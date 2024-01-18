const std = @import("std");

const counter = @import("counter.zig");
pub const Counter = counter.Counter;
pub const CounterVec = counter.CounterVec;

const gauge = @import("gauge.zig");
pub const Gauge = gauge.Gauge;
pub const GaugeVec = gauge.GaugeVec;

const histogram = @import("histogram.zig");
pub const Histogram = histogram.Histogram;

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
			var t: T = undefined;
			inline for (struct_info.fields) |field| {
				@field(t, field.name) = .{.noop = {}};
			}
			return t;
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
