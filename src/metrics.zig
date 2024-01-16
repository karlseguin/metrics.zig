const std = @import("std");

pub const Counter = @import("counter.zig").Counter;

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
	inline for (metrics) |m| {
		@compileLog(m);
	}
	_ = writer;
}

test {
	std.testing.refAllDecls(@This());
}
