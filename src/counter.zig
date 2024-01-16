const std = @import("std");
const Allocator = std.mem.Allocator;

const m = @import("metric.zig");
const Metric = m.Metric;
const MetricVec = m.MetricVec;

const Opts = struct {
	help: ?[]const u8 = null,
};

pub const Counter = union(enum) {
	noop: void,
	impl: *Impl,

	pub fn init(allocator: Allocator, comptime name: []const u8, opts: Opts) !Counter {
		const impl = try allocator.create(Impl);
		errdefer allocator.destroy(impl);
		impl.* = try Impl.init(allocator, name, opts);
		return .{.impl = impl};
	}

	pub fn incr(self: Counter) void {
		switch (self) {
			.noop => {},
			.impl => |impl| impl.incr(),
		}
	}

	pub fn incrBy(self: Counter, count: usize) void {
		switch (self) {
			.noop => {},
			.impl => |impl| impl.incrBy(count),
		}
	}

	pub fn write(self: Counter, writer: anytype) !void {
		switch (self) {
			.noop => {},
			.impl => |impl| return impl.write(writer),
		}
	}

	pub fn deinit(self: Counter, allocator: Allocator) void {
		switch (self) {
			.noop => {},
			.impl => |impl| {
				impl.deinit(allocator);
				allocator.destroy(impl);
			},
		}
	}

	const Impl = struct {
		count: usize,
		metric: Metric,

		fn init(allocator: Allocator, comptime name: []const u8, opts: Opts) !Impl {
			return .{
				.count = 0,
				.metric = try Metric.init(allocator, name, .counter, opts),
			};
		}

		fn deinit(self: Impl, allocator: Allocator) void {
			self.metric.deinit(allocator);
		}

		pub fn incr(self: *Impl) void {
			self.incrBy(1);
		}

		pub fn incrBy(self: *Impl, count: usize) void {
			_ = @atomicRmw(usize, &self.count, .Add, count, .Monotonic);
		}

		pub fn write(self: *const Impl, writer: anytype) !void {
			const metric = &self.metric;
			try metric.write(writer);

			try writer.writeAll(metric.name);
			const count = @atomicLoad(usize, &self.count, .Monotonic);
			try std.fmt.formatInt(count, 10, .lower, .{}, writer);
			return writer.writeByte('\n');
		}
	};
};

// Counter with labels
pub fn CounterVec(comptime T: type) type {
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

		pub fn deinit(self: Self, allocator: Allocator) void {
			switch (self) {
				.noop => {},
				.impl => |impl| {
					impl.deinit();
					allocator.destroy(impl);
				},
			}
		}

		pub fn incr(self: Self, labels: T) !void {
			switch (self) {
				.noop => {},
				.impl => |impl| return impl.incr(labels),
			}
		}

		pub fn incrBy(self: Self, labels: T, count: usize) !void {
			switch (self) {
				.noop => {},
				.impl => |impl| return impl.incrBy(labels, count),
			}
		}

		pub fn write(self: Self, writer: anytype) !void {
			switch (self) {
				.noop => {},
				.impl => |impl| return impl.write(writer),
			}
		}

		const Impl = struct {
			vec: MetricVec(T),
			allocator: Allocator,
			mutex: std.Thread.Mutex,
			values: MetricVec(T).HashMap(CounterVecValue),

			const V = MetricVec(T).V;

			fn init(allocator: Allocator, comptime name: []const u8, opts: Opts) !Impl {
				return .{
					.mutex = .{},
					.allocator = allocator,
					.vec = try MetricVec(T).init(allocator, name, .counter, opts),
					.values = MetricVec(T).HashMap(CounterVecValue){},
				};
			}

			fn deinit(self: *Impl) void {
				const allocator = self.allocator;
				self.vec.deinit(allocator);

				var it = self.values.iterator();
				while (it.next()) |kv| {
					MetricVec(T).free(allocator, kv.key_ptr.*);
					allocator.free(kv.value_ptr.attributes);
				}
				self.values.deinit(allocator);
			}

			pub fn incr(self: *Impl, labels: T) !void {
				return self.incrBy(labels, 1);
			}

			pub fn incrBy(self: *Impl, labels: T, count: usize) !void {
				const allocator = self.allocator;

				self.mutex.lock();
				defer self.mutex.unlock();
				const gop = try self.values.getOrPut(allocator, labels);
				if (gop.found_existing) {
					gop.value_ptr.count += count;
					return;
				}

				const counter = CounterVecValue{
					.count = count,
					.attributes = try MetricVec(T).buildAttributes(allocator, labels),
				};

				gop.value_ptr.* = counter;
				gop.key_ptr.* = try MetricVec(T).dupe(allocator, labels);
			}

			pub fn write(self: *Impl, writer: anytype) !void {
				const vec = &self.vec;
				try vec.write(writer);
				const name = vec.name;

				// TODO: reduce this lock!
				self.mutex.lock();
				defer self.mutex.unlock();

				var it = self.values.iterator();
				while (it.next()) |kv| {
					try writer.writeAll(name);

					const value = kv.value_ptr.*;
					try writer.writeAll(value.attributes);
					try std.fmt.formatInt(value.count, 10, .lower, .{}, writer);
					try writer.writeByte('\n');
				}
			}
		};
	};
}

const CounterVecValue = struct {
	count: usize,
	attributes: []const u8,
};

const t = @import("t.zig");
test "Counter: noop incr/incrBy" {
	// these should just not crash
	var c = Counter{.noop = {}};
	defer c.deinit(t.allocator);
	c.incr();
	c.incrBy(10);

	var arr = std.ArrayList(u8).init(t.allocator);
	defer arr.deinit();
	try c.write(arr.writer());
	try t.expectEqual(0, arr.items.len);
}

test "Counter: incr/incrBy" {
	var c = try Counter.init(t.allocator, "t1", .{});
	defer c.deinit(t.allocator);
	c.incr();
	try t.expectEqual(1, c.impl.count);
	c.incrBy(10);
	try t.expectEqual(11, c.impl.count);
}

test "Counter: write" {
	var arr = std.ArrayList(u8).init(t.allocator);
	defer arr.deinit();

	var c = try Counter.init(t.allocator, "metric_cnt_1_x", .{});
	defer c.deinit(t.allocator);

	{
		c.incr();
		try c.write(arr.writer());
		try t.expectString("# TYPE metric_cnt_1_x counter\nmetric_cnt_1_x 1\n", arr.items);
	}

	{
		arr.clearRetainingCapacity();
		c.incrBy(399929123);
		try c.write(arr.writer());
		try t.expectString("# TYPE metric_cnt_1_x counter\nmetric_cnt_1_x 399929124\n", arr.items);
	}
}

test "CounterVec: noop incr/incrBy" {
	// these should just not crash
	var c = CounterVec(struct{id: u32}){.noop = {}};
	defer c.deinit(t.allocator);
	try c.incr(.{.id = 3});
	try c.incrBy(.{.id = 10}, 20);

	var arr = std.ArrayList(u8).init(t.allocator);
	defer arr.deinit();
	try c.write(arr.writer());
	try t.expectEqual(0, arr.items.len);
}


test "CounterVec: incr/incrBy + write" {
	var arr = std.ArrayList(u8).init(t.allocator);
	defer arr.deinit();

	const preamble = "# HELP counter_vec_1 h1\n# TYPE counter_vec_1 counter\n";

	// these should just not crash
	var c = try CounterVec(struct{id: []const u8}).init(t.allocator, "counter_vec_1", .{.help = "h1"});
	defer c.deinit(t.allocator);

	try c.incr(.{.id = "a"});
	try c.write(arr.writer());
	try t.expectString(preamble ++ "counter_vec_1{id=\"a\"} 1\n", arr.items);

	arr.clearRetainingCapacity();
	try c.incr(.{.id = "b"});
	try c.incr(.{.id = "a"});
	try c.write(arr.writer());
	try t.expectString(preamble ++ "counter_vec_1{id=\"b\"} 1\ncounter_vec_1{id=\"a\"} 2\n", arr.items);

	arr.clearRetainingCapacity();
	try c.incrBy(.{.id = "a"}, 20);
	try c.write(arr.writer());
	try t.expectString(preamble ++ "counter_vec_1{id=\"b\"} 1\ncounter_vec_1{id=\"a\"} 22\n", arr.items);
}
