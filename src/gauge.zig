const std = @import("std");
const Allocator = std.mem.Allocator;

const m = @import("metric.zig");
const Metric = m.Metric;
const MetricVec = m.MetricVec;

const Opts = struct {
	help: ?[]const u8 = null,
};

pub fn Gauge(comptime V: type) type {
	assertGaugeType(V);
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

		pub fn set(self: Self, value: V) void {
			switch (self) {
				.noop => {},
				.impl => |impl| impl.set(value),
			}
		}

		pub fn incrBy(self: Self, value: V) void {
			switch (self) {
				.noop => {},
				.impl => |impl| impl.incrBy(value),
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
			value: V,
			metric: Metric,

			fn init(allocator: Allocator, comptime name: []const u8, opts: Opts) !Impl {
				return .{
					.value = 0,
					.metric = try Metric.init(allocator, name, .gauge, opts),
				};
			}

			fn deinit(self: Impl, allocator: Allocator) void {
				self.metric.deinit(allocator);
			}

			pub fn incr(self: *Impl) void {
				self.incrBy(1);
			}

			pub fn incrBy(self: *Impl, value: V) void {
				_ = @atomicRmw(V, &self.value, .Add, value, .Monotonic);
			}

			pub fn set(self: *Impl, value: V) void {
				@atomicStore(V, &self.value, value, .Monotonic);
			}

			pub fn write(self: *const Impl, writer: anytype) !void {
				const metric = &self.metric;
				try metric.write(writer);
				try m.write(@atomicLoad(V, &self.value, .Monotonic), writer);
				return writer.writeByte('\n');
			}
		};
	};
}

// Gauge with labels
pub fn GaugeVec(comptime V: type, comptime L: type) type {
	assertGaugeType(V);
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

		// could get the allocator from impl.allocator, but taking it as a parameter
		// makes the API the same between Gauge and GaugeVec
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

		pub fn incrBy(self: Self, labels: L, value: V) !void {
			switch (self) {
				.noop => {},
				.impl => |impl| return impl.incrBy(labels, value),
			}
		}

		pub fn set(self: Self, labels: L, value: V) !void {
			switch (self) {
				.noop => {},
				.impl => |impl| return impl.set(labels, value),
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
				value: V,
				attributes: []const u8,
			};

			fn init(allocator: Allocator, comptime name: []const u8, opts: Opts) !Impl {
				return .{
					.lock = .{},
					.allocator = allocator,
					.vec = try MetricVec(L).init(allocator, name, .gauge, opts),
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

			pub fn incrBy(self: *Impl, labels: L, value: V) !void {
				return self.withValue(labels, value, atomicIncrCallback, incrCallback);
			}

			fn atomicIncrCallback(value: V, entry: *Value) void {
				entry.value += value;
			}

			fn incrCallback(value: V, entry: *Value) void {
				entry.value += value;
			}

			pub fn set(self: *Impl, labels: L, value: V) !void {
				return self.withValue(labels, value, atomicSetCallback, setCallback);
			}

			fn setCallback(value: V, entry: *Value) void {
				entry.value = value;
			}

			fn atomicSetCallback(value: V, entry: *Value) void {
				entry.value = value;
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
					try m.write(value.value, writer);
					try writer.writeByte('\n');
				}
			}

			// value is only used if this is the first time we've seen this label.
			// if we've already seen this label, and thus have an existing entry in
			// our map, then fa() or f() is executed. fa() is called when an atomic
			// update is necessary, and f() is called when an atomic update isn't (
			// because a mutex is being held).
			fn withValue(self: *Impl, labels: L, value: V, comptime fa: fn(V, *Value) void, comptime f: fn(V, *Value) void) !void {
				const allocator = self.allocator;

				{
					self.lock.lockShared();
					defer self.lock.unlockShared();
					if (self.values.getPtr(labels)) |existing| {
						fa(value, existing);
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

				const gauge = Value{
					.value = value,
					.attributes = attributes,
				};

				self.lock.lock();
				defer self.lock.unlock();

				const gop = try self.values.getOrPut(allocator, owned_labels);
				if (gop.found_existing) {
					MetricVec(L).free(allocator, owned_labels);
					allocator.free(attributes);
					f(value, gop.value_ptr);
				}
				gop.value_ptr.* = gauge;
			}
		};
	};
}

fn assertGaugeType(comptime T: type) void {
	switch (@typeInfo(T)) {
		.Float, .Int => return,
		else => {},
	}
	@compileError("Gauge metric must be an integer or float, got: " ++ @typeName(T));
}

const t = @import("t.zig");
test "Gauge: noop incr/incrBy/set" {
	// these should just not crash
	var c = Gauge(u32){.noop = {}};
	defer c.deinit(t.allocator);
	c.incr();
	c.incrBy(10);
	c.set(100);

	var arr = std.ArrayList(u8).init(t.allocator);
	defer arr.deinit();
	try c.write(arr.writer());
	try t.expectEqual(0, arr.items.len);
}

test "Gauge: incr/incrBy/set" {
	var g = try Gauge(i32).init(t.allocator, "t1", .{});
	defer g.deinit(t.allocator);

	g.incr();
	try t.expectEqual(1, g.impl.value);

	g.incrBy(10);
	try t.expectEqual(11, g.impl.value);

	g.incrBy(-2);
	try t.expectEqual(9, g.impl.value);

	g.set(-10);
	try t.expectEqual(-10, g.impl.value);
}

test "Gauge: write" {
	var arr = std.ArrayList(u8).init(t.allocator);
	defer arr.deinit();

	var g = try Gauge(i32).init(t.allocator, "metric_grp_1_x", .{});
	defer g.deinit(t.allocator);

	{
		g.incr();
		try g.write(arr.writer());
		try t.expectString("# TYPE metric_grp_1_x gauge\nmetric_grp_1_x 1\n", arr.items);
	}

	{
		arr.clearRetainingCapacity();
		g.incrBy(399929123);
		try g.write(arr.writer());
		try t.expectString("# TYPE metric_grp_1_x gauge\nmetric_grp_1_x 399929124\n", arr.items);
	}

	{
		arr.clearRetainingCapacity();
		g.set(-329);
		try g.write(arr.writer());
		try t.expectString("# TYPE metric_grp_1_x gauge\nmetric_grp_1_x -329\n", arr.items);
	}
}

test "Gauge: float incr/incrBy/set" {
	var c = try Gauge(f32).init(t.allocator, "t1", .{});
	defer c.deinit(t.allocator);
	c.incr();
	try t.expectEqual(1, c.impl.value);
	c.incrBy(-3.9);
	try t.expectEqual(-2.9, c.impl.value);
	c.set(99.9);
	try t.expectEqual(99.9, c.impl.value);
}

test "Gauge: float write" {
	var arr = std.ArrayList(u8).init(t.allocator);
	defer arr.deinit();

	var c = try Gauge(f64).init(t.allocator, "metric_g_2_x", .{});
	defer c.deinit(t.allocator);

	{
		c.incr();
		try c.write(arr.writer());
		try t.expectString("# TYPE metric_g_2_x gauge\nmetric_g_2_x 1\n", arr.items);
	}

	{
		arr.clearRetainingCapacity();
		c.incrBy(-9.2);
		try c.write(arr.writer());
		try t.expectString("# TYPE metric_g_2_x gauge\nmetric_g_2_x -8.2\n", arr.items);
	}

	{
		arr.clearRetainingCapacity();
		c.set(8.888);
		try c.write(arr.writer());
		try t.expectString("# TYPE metric_g_2_x gauge\nmetric_g_2_x 8.888\n", arr.items);
	}
}

test "GaugeVec: noop incr/incrBy/set" {
	// these should just not crash
	var g = GaugeVec(u32, struct{id: u32}){.noop = {}};
	defer g.deinit(t.allocator);
	try g.incr(.{.id = 3});
	try g.incrBy(.{.id = 10}, 20);
	try g.set(.{.id = 3}, 11);

	var arr = std.ArrayList(u8).init(t.allocator);
	defer arr.deinit();
	try g.write(arr.writer());
	try t.expectEqual(0, arr.items.len);
}

test "GaugeVec: incr/incrBy/set + write" {
	var arr = std.ArrayList(u8).init(t.allocator);
	defer arr.deinit();

	const preamble = "# HELP gauge_vec_1 h1\n# TYPE gauge_vec_1 gauge\n";

	// these should just not crash
	var g = try GaugeVec(i64, struct{id: []const u8}).init(t.allocator, "gauge_vec_1", .{.help = "h1"});
	defer g.deinit(t.allocator);

	try g.incr(.{.id = "a"});
	try g.write(arr.writer());
	try t.expectString(preamble ++ "gauge_vec_1{id=\"a\"} 1\n", arr.items);

	arr.clearRetainingCapacity();
	try g.incr(.{.id = "b"});
	try g.incr(.{.id = "a"});
	try g.write(arr.writer());
	try t.expectString(preamble ++ "gauge_vec_1{id=\"b\"} 1\ngauge_vec_1{id=\"a\"} 2\n", arr.items);

	arr.clearRetainingCapacity();
	try g.incrBy(.{.id = "a"}, 20);
	try g.set(.{.id = "c"}, 5);
	try g.set(.{.id = "b"}, -33);
	try g.write(arr.writer());
	try t.expectString(preamble ++ "gauge_vec_1{id=\"b\"} -33\ngauge_vec_1{id=\"a\"} 22\ngauge_vec_1{id=\"c\"} 5\n", arr.items);

	arr.clearRetainingCapacity();
	g.remove(.{.id = "not_found"});
	g.remove(.{.id = "a"});
	try g.write(arr.writer());
	try t.expectString(preamble ++ "gauge_vec_1{id=\"b\"} -33\ngauge_vec_1{id=\"c\"} 5\n", arr.items);
}

test "GaugeVec: float incr/incrBy/set + write" {
	var arr = std.ArrayList(u8).init(t.allocator);
	defer arr.deinit();

	const preamble = "# HELP gauge_vec_xx_2 h1\n# TYPE gauge_vec_xx_2 gauge\n";

	// these should just not crash
	var g = try GaugeVec(f64, struct{id: []const u8}).init(t.allocator, "gauge_vec_xx_2", .{.help = "h1"});
	defer g.deinit(t.allocator);

	try g.incr(.{.id = "a"});
	try g.write(arr.writer());
	try t.expectString(preamble ++ "gauge_vec_xx_2{id=\"a\"} 1\n", arr.items);

	arr.clearRetainingCapacity();
	try g.incr(.{.id = "b"});
	try g.incr(.{.id = "a"});
	try g.set(.{.id = "c\nc"}, 0.011);
	try g.write(arr.writer());
	try t.expectString(preamble ++ "gauge_vec_xx_2{id=\"b\"} 1\ngauge_vec_xx_2{id=\"a\"} 2\ngauge_vec_xx_2{id=\"c\\nc\"} 0.011\n", arr.items);

	arr.clearRetainingCapacity();
	try g.incrBy(.{.id = "a"}, 0.25);
	g.remove(.{.id = "c\nc"});
	try g.write(arr.writer());
	try t.expectString(preamble ++ "gauge_vec_xx_2{id=\"b\"} 1\ngauge_vec_xx_2{id=\"a\"} 2.25\n", arr.items);
}
