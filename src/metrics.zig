const std = @import("std");

const counter = @import("counter.zig");
pub const Counter = counter.Counter;
pub const CounterVec = counter.CounterVec;

const gauge = @import("gauge.zig");
pub const Gauge = gauge.Gauge;
pub const GaugeVec = gauge.GaugeVec;

const histogram = @import("histogram.zig");
pub const Histogram = histogram.Histogram;
pub const HistogramVec = histogram.HistogramVec;

// This allows a library developer to safely use a library-wide metrics
// instance by defaulting all metrics to "noop" variants. Library developers
// can then expose a function to the main application, say:
//   try thelib.initializeMetrics(allocator)
// which then initializes the metrics instance to real implementations.
// Whether or not the application calls in initializeMetrics, the library
// can safely use the metrics instance, sine this function initialized it to
// noop.
pub fn initializeNoop(comptime T: type) T {
	switch (@typeInfo(T)) {
		.Struct => |struct_info| {
			var m: T = undefined;
			inline for (struct_info.fields) |field| {
				switch (@typeInfo(field.type)) {
					.Union => @field(m, field.name) = .{.noop = {}},
					else => {
						if (field.default_value) |default_value_ptr| {
							const default_value = @as(*align(1) const field.type, @ptrCast(default_value_ptr)).*;
							@field(m, field.name) = default_value;
						}
					},
				}
			}
			return m;
		},
		else => @compileError("initializeNoop expects a struct")
	}
}

pub fn write(metrics: anytype, writer: anytype) !void {
	const fields = @typeInfo(@TypeOf(metrics)).Struct.fields;
	inline for (fields) |f| {
		try @field(metrics, f.name).write(writer);
	}
}

test {
	std.testing.refAllDecls(@This());
}

const t = @import("t.zig");
test "initializeNoop" {
	const x = initializeNoop(struct{
		status: u16 = 33,
		hits: CounterVec(u32, struct{status: u16}),
		active: Gauge(u64),
		latency: Histogram(u32, &.{0, 2}),
	});
	try t.expectEqual(33, x.status);
	try t.expectEqual(.noop, std.meta.activeTag(x.hits));
	try t.expectEqual(.noop, std.meta.activeTag(x.active));
	try t.expectEqual(.noop, std.meta.activeTag(x.latency));
}
