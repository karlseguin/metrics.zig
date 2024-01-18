const std = @import("std");
const Allocator = std.mem.Allocator;

const m = @import("metric.zig");
const Metric = m.Metric;
const MetricVec = m.MetricVec;

const Opts = struct {
	help: ?[]const u8 = null,
};

pub fn Histogram(comptime V: type, comptime upper_bounds_: ?[]const V) type {
	assertHistogramType(V);
	const upper_bounds = upper_bounds_ orelse defaultUpperBounds(V);
	assertUpperBounds(upper_bounds);

	return union(enum) {
		noop: void,
		impl: *Impl,

		const Self = @This();

		pub fn init(allocator: Allocator, comptime name: []const u8, opts: Opts) !Self {
			const impl = try allocator.create(Impl);
			errdefer allocator.destroy(impl);
			impl.* = try Impl.init(allocator, name, opts);
			return .{.impl = impl};
		}

		pub fn observe(self: Self, value: V) void {
			switch (self) {
				.noop => {},
				.impl => |impl| impl.observe(value),
			}
		}

		pub fn write(self: Self, writer: anytype) !void {
			switch (self) {
				.noop => {},
				.impl => |impl| return impl.write(writer),
			}
		}

		pub fn deinit(self: Self, allocator: Allocator) void {
			switch (self) {
				.noop => {},
				.impl => |impl| {
					impl.deinit(allocator);
					allocator.destroy(impl);
				},
			}
		}

		const Impl = struct {
			metric: Metric,
			sum: V,
			count: usize,
			buckets: [upper_bounds.len]V,
			output_sum_prefix: []const u8,
			output_count_prefix: []const u8,
			output_bucket_prefixes: [upper_bounds.len][]const u8,
			output_bucket_inf_prefix: []const u8,


			fn init(allocator: Allocator, comptime name: []const u8, opts: Opts) !Impl {
				const metric = try Metric.init(allocator, name, .histogram, opts);
				errdefer metric.deinit(allocator);

				const output_sum_prefix = try std.fmt.allocPrint(allocator, "\n{s}_sum ", .{name});
				errdefer allocator.free(output_sum_prefix);

				const output_count_prefix = try std.fmt.allocPrint(allocator, "\n{s}_count ", .{name});
				errdefer allocator.free(output_count_prefix);

				const output_bucket_inf_prefix = try std.fmt.allocPrint(allocator, "{s}_bucket{{le=\"+Inf\"}} ", .{name});
				errdefer allocator.free(output_bucket_inf_prefix);

				var output_bucket_prefixes: [upper_bounds.len][]const u8 = undefined;
				var initialized: usize = 0;
				errdefer {
					for (0..initialized) |i| {
						allocator.free(output_bucket_prefixes[i]);
					}
				}

				for (upper_bounds, 0..) |upper, i| {
					output_bucket_prefixes[i] = try std.fmt.allocPrint(allocator, "{s}_bucket{{le=\"{d}\"}} ", .{name, upper});
					initialized += 1;
				}

				return .{
					.sum = 0,
					.count = 0,
					.metric = metric,
					.output_sum_prefix = output_sum_prefix,
					.output_count_prefix = output_count_prefix,
					.output_bucket_prefixes = output_bucket_prefixes,
					.output_bucket_inf_prefix = output_bucket_inf_prefix,
					.buckets = std.mem.zeroes([upper_bounds.len]V),
				};
			}

			fn deinit(self: Impl, allocator: Allocator) void {
				self.metric.deinit(allocator);
				allocator.free(self.output_sum_prefix);
				allocator.free(self.output_count_prefix);
				allocator.free(self.output_bucket_inf_prefix);
				for (self.output_bucket_prefixes) |obf| {
					allocator.free(obf);
				}
			}

			pub fn observe(self: *Impl, value: V) void {
				_ = @atomicRmw(usize, &self.count, .Add, 1, .Monotonic);
				_ = @atomicRmw(V, &self.sum, .Add, value, .Monotonic);

				const idx = blk: {
					for (upper_bounds, 0..) |upper, i| {
						if (value < upper) {
							break :blk i;
						}
					}
					// this is our implicit bucket to +Inf. Implicit because the count
					// and sum, updated above, will contain this entry
					return;
				};

				_ = @atomicRmw(V, &self.buckets[idx], .Add, 1, .Monotonic);
			}

			pub fn write(self: *Impl, writer: anytype) !void {
				const metric = &self.metric;
				try metric.write(writer);

				var sum: V = 0;
				for (self.output_bucket_prefixes, 0..) |prefix, i| {
					sum += @atomicRmw(V, &self.buckets[i], .Xchg, 0, .Monotonic);
					try writer.writeAll(prefix);
					try m.write(sum, writer);
					try writer.writeByte('\n');
				}

				const total_count = @atomicRmw(usize, &self.count, .Xchg, 0, .Monotonic);
				{
					// write +Inf
					try writer.writeAll(self.output_bucket_inf_prefix);
					try std.fmt.formatInt(total_count, 10, .lower, .{}, writer);
				}

				{
					//write sum
					// this includes a leading newline, hence we didn't need to write
					// it after our output_bucket_inf_prefix
					try writer.writeAll(self.output_sum_prefix);
					try m.write(@atomicRmw(V, &self.sum, .Xchg, 0, .Monotonic), writer);
				}

				{
					//write count
					// this includes a leading newline, hence we didn't need to write
					// it after our output_sum_prefix
					try writer.writeAll(self.output_count_prefix);
					try std.fmt.formatInt(total_count, 10, .lower, .{}, writer);
					try writer.writeByte('\n');
				}
			}
		};
	};
}

fn assertHistogramType(comptime T: type) void {
	switch (@typeInfo(T)) {
		.Float => return,
		.Int => |int| {
			if (int.signedness == .unsigned) return;
		},
		else => {},
	}
	@compileError("Histogram metric must be an unsigned integer or a float, got: " ++ @typeName(T));
}

fn defaultUpperBounds(comptime T: type) []const T {
	switch (@typeInfo(T)) {
		.Float => |float| switch (float.bits) {
			32 => return &.{0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10},
			64 => return &.{0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10},
			else => @compileError("An explicit list of buckets must be provided for non 32-bit and 64-bit histograms, got: " ++ @typeName(T)),
		},
		.Int => |int| {
			if (int.signedness == .unsigned) {
				if (int.bits >= 16) {
					return &.{0, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000};
				}
				@compileError("An explicit list of buckets must be provided for histograms with integers less than 16 bits, got: " ++ @typeName(T));
			}
		},
		else => {},
	}
	@compileError("Histogram type must be an unsigned integer or float, got: " ++ @typeName(T));
}

fn assertUpperBounds(upper_bounds: anytype) void {
	if (upper_bounds.len == 0) {
		@compileError("Histogram upper bound cannot be empty");
	}
	var last = upper_bounds[0];
	for (upper_bounds[1..]) |value| {
		if (value < last) {
			@compileError("Histogram upper bounds must be in ascending order");
		}
		last = value;
	}
}

const t = @import("t.zig");
test "Histogram: noop " {
	// these should just not crash
	var h = Histogram(u32, null){.noop = {}};
	defer h.deinit(t.allocator);
	h.observe(2);

	var arr = std.ArrayList(u8).init(t.allocator);
	defer arr.deinit();
	try h.write(arr.writer());
	try t.expectEqual(0, arr.items.len);
}

test "Histogram" {
	var h = try Histogram(f64, null).init(t.allocator, "hst_1", .{});
	defer h.deinit(t.allocator);

	var i: f64 = 0.001;
	for (0..1000) |_| {
		i = i + i / 100;
		h.observe(i);
	}

	var arr = std.ArrayList(u8).init(t.allocator);
	defer arr.deinit();
	try h.write(arr.writer());
	try t.expectString(\\# TYPE hst_1 histogram
\\hst_1_bucket{le="0.005"} 161
\\hst_1_bucket{le="0.01"} 231
\\hst_1_bucket{le="0.025"} 323
\\hst_1_bucket{le="0.05"} 393
\\hst_1_bucket{le="0.1"} 462
\\hst_1_bucket{le="0.25"} 554
\\hst_1_bucket{le="0.5"} 624
\\hst_1_bucket{le="1"} 694
\\hst_1_bucket{le="2.5"} 786
\\hst_1_bucket{le="5"} 855
\\hst_1_bucket{le="10"} 925
\\hst_1_bucket{le="+Inf"} 1000
\\hst_1_sum 2116.7737194191777
\\hst_1_count 1000
\\
, arr.items);
}
