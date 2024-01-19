const std = @import("std");
const Allocator = std.mem.Allocator;

const m = @import("metric.zig");
const Metric = m.Metric;
const MetricVec = m.MetricVec;

const Opts = struct {
	help: ?[]const u8 = null,
};

pub fn Counter(comptime V: type) type {
	assertCounterType(V);
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

		pub fn incr(self: Self) void {
			switch (self) {
				.noop => {},
				.impl => |impl| impl.incr(),
			}
		}

		pub fn incrBy(self: Self, count: V) void {
			switch (self) {
				.noop => {},
				.impl => |impl| impl.incrBy(count),
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
			count: V,
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

			pub fn incrBy(self: *Impl, count: V) void {
				_ = @atomicRmw(V, &self.count, .Add, count, .Monotonic);
			}

			pub fn write(self: *const Impl, writer: anytype) !void {
				const metric = &self.metric;
				try metric.write(writer);

				const count = @atomicLoad(V, &self.count, .Monotonic);
				try m.write(count, writer);
				return writer.writeByte('\n');
			}
		};
	};
}

// Counter with labels
pub fn CounterVec(comptime V: type, comptime L: type) type {
	assertCounterType(V);
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

		pub fn incr(self: Self, labels: L) !void {
			switch (self) {
				.noop => {},
				.impl => |impl| return impl.incr(labels),
			}
		}

		pub fn incrBy(self: Self, labels: L, count: V) !void {
			switch (self) {
				.noop => {},
				.impl => |impl| return impl.incrBy(labels, count),
			}
		}

		pub fn remove(self: Self, labels: L) void {
			switch (self) {
				.noop => {},
				.impl => |impl| impl.remove(labels),
			}
		}

		pub fn write(self: Self, writer: anytype) !void {
			switch (self) {
				.noop => {},
				.impl => |impl| return impl.write(writer),
			}
		}

		const Impl = struct {
			vec: MetricVec(L),
			allocator: Allocator,
			lock: std.Thread.RwLock,
			values: MetricVec(L).HashMap(Value),

			const Value = struct {
				count: V,
				attributes: []const u8,
			};

			fn init(allocator: Allocator, comptime name: []const u8, opts: Opts) !Impl {
				return .{
					.lock = .{},
					.allocator = allocator,
					.vec = try MetricVec(L).init(allocator, name, .counter, opts),
					.values = MetricVec(L).HashMap(Value){},
				};
			}

			fn deinit(self: *Impl) void {
				const allocator = self.allocator;
				self.vec.deinit(allocator);

				var it = self.values.iterator();
				while (it.next()) |kv| {
					MetricVec(L).free(allocator, kv.key_ptr.*);
					allocator.free(kv.value_ptr.attributes);
				}
				self.values.deinit(allocator);
			}

			pub fn incr(self: *Impl, labels: L) !void {
				return self.incrBy(labels, 1);
			}

			pub fn incrBy(self: *Impl, labels: L, count: V) !void {
				const allocator = self.allocator;

				{
					self.lock.lockShared();
					defer self.lock.unlockShared();
					if (self.values.getPtr(labels)) |existing| {
						existing.count += count;
						return;
					}
				}

				// It's possible that another thread will come in and create this
				// missing label, and we'll check for that, but we'll assume not and
				// do our allocations here, outside of any locks.
				const attributes = try MetricVec(L).buildAttributes(allocator, labels);
				errdefer allocator.free(attributes);

				const owned_labels = try MetricVec(L).dupe(allocator, labels);
				errdefer MetricVec(L).free(allocator, owned_labels);

				const counter = Value{
					.count = count,
					.attributes = attributes,
				};

				self.lock.lock();
				defer self.lock.unlock();

				const gop = try self.values.getOrPut(allocator, owned_labels);
				if (gop.found_existing) {
					MetricVec(L).free(allocator, owned_labels);
					allocator.free(attributes);
					gop.value_ptr.count += count;
				}

				gop.value_ptr.* = counter;
			}

			pub fn remove(self: *Impl, labels: L) void {
				const kv = blk: {
					self.lock.lock();
					defer self.lock.unlock();
					break :blk self.values.fetchRemove(labels) orelse return;
				};

				const allocator = self.allocator;
				MetricVec(L).free(allocator, kv.key);
				allocator.free(kv.value.attributes);
			}

			pub fn write(self: *Impl, writer: anytype) !void {
				const vec = &self.vec;
				try vec.write(writer);
				const name = vec.name;

				self.lock.lockShared();
				defer self.lock.unlockShared();

				var it = self.values.iterator();
				while (it.next()) |kv| {
					try writer.writeAll(name);

					const value = kv.value_ptr.*;
					try writer.writeAll(value.attributes);
					try m.write(value.count, writer);
					try writer.writeByte('\n');
				}
			}
		};
	};
}

fn assertCounterType(comptime T: type) void {
	switch (@typeInfo(T)) {
		.Float => return,
		.Int => |int| {
			if (int.signedness == .unsigned) return;
		},
		else => {},
	}
	@compileError("Counter metric must be an unsigned integer or a float, got: " ++ @typeName(T));
}

const t = @import("t.zig");
test "Counter: noop incr/incrBy" {
	// these should just not crash
	var c = Counter(u32){.noop = {}};
	defer c.deinit(t.allocator);
	c.incr();
	c.incrBy(10);

	var arr = std.ArrayList(u8).init(t.allocator);
	defer arr.deinit();
	try c.write(arr.writer());
	try t.expectEqual(0, arr.items.len);
}

test "Counter: incr/incrBy" {
	var c = try Counter(u32).init(t.allocator, "t1", .{});
	defer c.deinit(t.allocator);
	c.incr();
	try t.expectEqual(1, c.impl.count);
	c.incrBy(10);
	try t.expectEqual(11, c.impl.count);
}

test "Counter: write" {
	var arr = std.ArrayList(u8).init(t.allocator);
	defer arr.deinit();

	var c = try Counter(u32).init(t.allocator, "metric_cnt_1_x", .{});
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

test "Counter: float incr/incrBy" {
	var c = try Counter(f32).init(t.allocator, "t1", .{});
	defer c.deinit(t.allocator);
	c.incr();
	try t.expectEqual(1, c.impl.count);
	c.incrBy(12.1);
	try t.expectEqual(13.1, c.impl.count);
}

test "Counter: float write" {
	var arr = std.ArrayList(u8).init(t.allocator);
	defer arr.deinit();

	var c = try Counter(f64).init(t.allocator, "metric_cnt_2_x", .{});
	defer c.deinit(t.allocator);

	{
		c.incr();
		try c.write(arr.writer());
		try t.expectString("# TYPE metric_cnt_2_x counter\nmetric_cnt_2_x 1\n", arr.items);
	}

	{
		arr.clearRetainingCapacity();
		c.incrBy(123.991);
		try c.write(arr.writer());
		try t.expectString("# TYPE metric_cnt_2_x counter\nmetric_cnt_2_x 124.991\n", arr.items);
	}
}

test "CounterVec: noop incr/incrBy" {
	// these should just not crash
	var c = CounterVec(u32, struct{id: u32}){.noop = {}};
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
	var c = try CounterVec(u64, struct{id: []const u8}).init(t.allocator, "counter_vec_1", .{.help = "h1"});
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

	arr.clearRetainingCapacity();
	c.remove(.{.id = "not_found"});
	c.remove(.{.id = "a"});
	try c.write(arr.writer());
	try t.expectString(preamble ++ "counter_vec_1{id=\"b\"} 1\n", arr.items);
}

test "CounterVec: float incr/incrBy + write" {
	var arr = std.ArrayList(u8).init(t.allocator);
	defer arr.deinit();

	const preamble = "# HELP counter_vec_xx_2 h1\n# TYPE counter_vec_xx_2 counter\n";

	// these should just not crash
	var c = try CounterVec(f32, struct{id: []const u8}).init(t.allocator, "counter_vec_xx_2", .{.help = "h1"});
	defer c.deinit(t.allocator);

	try c.incr(.{.id = "a"});
	try c.write(arr.writer());
	try t.expectString(preamble ++ "counter_vec_xx_2{id=\"a\"} 1\n", arr.items);

	arr.clearRetainingCapacity();
	try c.incr(.{.id = "b"});
	try c.incr(.{.id = "a"});
	try c.write(arr.writer());
	try t.expectString(preamble ++ "counter_vec_xx_2{id=\"b\"} 1\ncounter_vec_xx_2{id=\"a\"} 2\n", arr.items);

	arr.clearRetainingCapacity();
	try c.incrBy(.{.id = "a"}, 0.25);
	try c.write(arr.writer());
	try t.expectString(preamble ++ "counter_vec_xx_2{id=\"b\"} 1\ncounter_vec_xx_2{id=\"a\"} 2.25\n", arr.items);
}
