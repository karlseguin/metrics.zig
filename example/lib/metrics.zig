const std = @import("std");
const m = @import("metrics");

const Allocator = std.mem.Allocator;

// public to be exposed to other files within this library, not to be exposed
// directly to the application.
var metrics = m.initializeNoop(Metrics);

const Metrics = struct {
	hits: Hits,
	active: Active,
	latency: Latency,

    const HitsLabels = struct{status: u16, path: []const u8};
    const LatencyLabels = struct{path: []const u8};

	const Hits = m.CounterVec(u32, HitsLabels);
	const Active = m.Gauge(u32);
	const Latency = m.HistogramVec(
		f64,
		LatencyLabels,
		&.{0.05, 0.10, 0.50, 1, 2.5, 5, 10},
	);
};

pub fn hit(labels: Metrics.HitsLabels) !void {
	return metrics.hits.incr(labels);
}

pub fn active(value: u32) void {
	metrics.active.set(value);
}

pub fn latency(labels: Metrics.LatencyLabels, value: f32) !void {
	return metrics.latency.observe(labels, value);
}

pub fn initialize(allocator: Allocator, comptime opts: m.RegistryOpts) !void {
	metrics = .{
		.hits = try Metrics.Hits.init(allocator, "lib_hits", .{}, opts),
		.active = Metrics.Active.init("lib_active", .{}, opts),
		.latency = try Metrics.Latency.init(allocator, "lib_latency", .{}, opts),
	};
}

pub fn write(writer: anytype) !void {
	return m.write(&metrics, writer);
}
