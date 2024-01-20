# Prometheus Metric Library for Zig
This library is meant to be easy for both library developers and application developers to use. I do hope that this can be even more streamlined when comptime allocations. 

It supports, counters, gauges and histograms and the labeled-variant of each.

## Metric Setup
Setup is a bit tedious, and I welcome suggestions for improvement. 

Let's start with a basic example:

```zig
const m = @import("metrics");

// defaults to noop metrics, making this safe to use
// whether or not initializeMetrics is called
pub var metrics = m.initializeNoop(Metrics);

const Metrics = struct {
    hits: m.Counter(u32),
    connected: m.Gauge(u16),
};

// meant to be called once on application startup
pub fn initializeMetrics(allocator: std.mem.Allocator) !void {
    metrics = .{
        .hits = try m.Counter(u32).init(allocator, "hits", .{}),
        .connected = try m.Gauge(u16).init(allocator, "connected", .{}),
    };
}

// thread safe
pub fn writeMetrics(writer: anytype) !void {
    return m.write(metrics, writer);
}
```

The call to `m.initializeNoop(Metrics)` creates a `Metrics` and initializes each metric (`hits` and `connected`) to a "noop" implementation (tagged unions are used). The `initializeMetrics` is called on application startup and sets these metrics to real implementation.

For library developers, this means `metrics` is always safe to use. For application developers, it gives them control over which metrics to enable .

### Note for Library Developers
Library developers are free to change the above as needed. However, having libraries consistently expose an `initializeMetrics` and `writeMetrics` should help application developers.

Library developers should ask their users to call `try initializeMetrics(allocator)` on startup and `try writeMetrics(writer)` to generate the metrics.

### Labels (vector-metrics)
Every metric type supports a vectored variant. This allows labels to be attached to metrics. As you'll see in the metric API section, most vectored metrics methods can fail (as they may need to do allocation for new label values).

```zig
pub var metrics = m.initializeNoop(Metrics);

const Metrics = struct {
    hits: m.CounterVec(u32, struct{status: u16, name: []const u8}),
};

pub fn initializeMetrics(allocator: std.mem.Allocator) !void {
    metrics = .{
        .hits = try m.CounterVec(u32, struct{status: u16, name: []const u8}).init(allocator, "hits", .{}),
    };
}
```

The labels are strongly types. Valid label types are: `ErrorSet`, `Enum`, `Type`, `Bool`, `Int` and `[]const u8`

The `CounterVec(u32, ...)` has to be typed twice: once in the definition of `Metrics` and once in `initializeMetrics`. This can be improved slightly.

```zig
const Metrics = struct {
    hits: Hits,

    const Hits = m.CounterVec(u32, struct{status: u16, name: []const u8});
};

pub fn initializeMetrics(allocator: std.mem.Allocator) !void {
    metrics = .{
        .hits = try Metrics.Hits.init(allocator, "hits", .{}),
    };
}
```

Internally, every metric is a union between a "noop" and an actual implementation. This allows metrics to be globally initialized as noop and then enabled on startup. The benefit of this approach is that library developers can safely and easily use their metrics whether or not the application has enabled them.

### Histograms
Histograms are setup like `Counter` and `Gauge`, and have a vectored-variant, but they require a comptime list of buckets:

```zig
const Metrics = struct {
    latency: Latency,

    const Latency = m.Histogram(f32, &.{0.005, 0.01, 0.05, 0.1, 0.25, 1, 5, 10});
};

pub fn initializeMetrics(allocator: std.mem.Allocator) !void {
    metrics = .{
        .latency = try Metrics.Latency.init(allocator, "hits", .{}),
    };
}
```

The `HistogramVec` is even more verbose, requiring the label struct and bucket list:

```zig
const Metrics = struct {
    latency: Latency,

    const Latency = m.HistogramVec(
        u32,
        struct{path: []const u8},
        &.{5, 10, 25, 50, 100, 250, 500, 1000}
    );
};

pub fn initializeMetrics(allocator: std.mem.Allocator) !void {
    metrics = .{
        .latency = try Metrics.Latency.init(allocator, "hits", .{}),
    };
}
```

## Metrics

### Utility
The package exposes the following utility functions.

#### `initializeNoop(T) T`
Creates an initializes metric `T` with `noop` implementation of every metric field. `T` should contain only metrics (`Counter`, `Gauge`, `Historgram` or their vectored variants) and primitive fields (int, bool, []const u8, enum, float). 

`initializeNoop(T)` will set any non-metric field to its default value.

This method is designed to allow a global "metrics" instance to exist and be safe to use within libraries.

#### `write(metrics: anytype, writer anytype) !void`
Calls the `write(writer) !void` method on every metric field within `metrics`.

Library developers are expected to wrap this method in a `writeMetric(writer: anytype) !void` function.

### Counter(T)
A `Counter(T)` is used for incrementing values. `T` can be an unsigned integer or a float. Its two main methods are `incr()` and `incrBy(value: T)`. `incr()` is a short version of `incrBy(1)`.

#### `init(allocator: Allocator, comptime name: []const, opts: Opts) !Counter(T)`
Initializes the counter. Name must be given at comptime. Opts is:
* `help: ?[]const` - optional help text to include in the prometheus output

#### `deinit(self: Counter(T), allocator: Allocator) void`
Deallocates the counter

#### `incr(self: Counter(T)) void`
Increments the counter by 1.

#### `incrBy(self: Counter(T), value: T) void`
Increments the counter by `value`.

#### `write(self: Counter(T), writer: anytype) !void`
Writes the counter to `writer`.

### CounterVec(T, L)
A `CounterVec(T, L)` is used for incrementing values with labels. `T` can be an unsigned integer or a float. `L` must be a struct where the field names and types will define the lables. Its two main methods are `incr(labels: L)` and `incrBy(labels: L, value: T)`. `incr(L)` is a short version of `incrBy(L, 1)`.

#### `init(allocator: Allocator, comptime name: []const, opts: Opts) !CounterVec(T, L)`
Initializes the counter. Name must be given at comptime. Opts is:
* `help: ?[]const` - optional help text to include in the prometheus output

#### `deinit(self: CounterVec(T, L), allocator: Allocator) void`
Deallocates the counter

#### `incr(self: CounterVec(T, L), labels: L) !void`
Increments the counter by 1. Vectored metrics can fail.

#### `incrBy(self: CounterVec(T, L), labels: L, value: T) !void`
Increments the counter by `value`. Vectored metrics can fail.

#### `write(self: CounterVec(T, L), writer: anytype) !void`
Writes the counter to `writer`.

### Gauge(T)
A `Gauge(T)` is used for setting values. `T` can be an integer or a float. Its main methods are `incr()`, `incrBy(value: T)` and `set(value: T)`. `incr()` is a short version of `incrBy(1)`.

#### `init(allocator: Allocator, comptime name: []const, opts: Opts) !Gauge(T)`
Initializes the gauge. Name must be given at comptime. Opts is:
* `help: ?[]const` - optional help text to include in the prometheus output

#### `deinit(self: Gauge(T), allocator: Allocator) void`
Deallocates the gauge

#### `incr(self: Gauge(T)) void`
Increments the gauge by 1.

#### `incrBy(self: Gauge(T), value: T) void`
Increments the gauge by `value`.

#### `set(self: Gauge(T), value: T) void`
Sets the the gauge to `value`.

#### `write(self: Gauge(T), writer: anytype) !void`
Writes the gauge to `writer`.

### GaugeVec(T, L)
A `GaugeVec(T, L)` is used for incrementing values with labels. `T` can be an integer or a float. `L` must be a struct where the field names and types will define the lables. Its main methods are `incr(labels: L)`, `incrBy(labels: L, value: T)` and `set(labels: L, value: T)`. `incr(L)` is a short version of `incrBy(L, 1)`.

#### `init(allocator: Allocator, comptime name: []const, opts: Opts) !GaugeVec(T, L)`
Initializes the gauge. Name must be given at comptime. Opts is:
* `help: ?[]const` - optional help text to include in the prometheus output

#### `deinit(self: GaugeVec(T, L), allocator: Allocator) void`
Deallocates the gauge

#### `incr(self: GaugeVec(T, L), labels: L) !void`
Increments the gauge by 1. Vectored metrics can fail.

#### `incrBy(self: GaugeVec(T, L), labels: L, value: T) !void`
Increments the gauge by `value`. Vectored metrics can fail.

#### `set(self: GaugeVec(T, L), labels: L, value: T) !void`
Sets the gauge to `value`. Vectored metrics can fail.

#### `write(self: GaugeVec(T, L), writer: anytype) !void`
Writes the gauge to `writer`.

### Histogram(T, []T)
A `Histogram(T, []T)` is used  to track the size and frequency of events. `T` can be an unsigned integer or a float. Its main methods is `observe(T)`.

Observed valued will fall within one of the provided buckets, `[]T`. The buckets must be in ascending order. A final "infinite" bucket *should not* be provided.

#### `init(allocator: Allocator, comptime name: []const, opts: Opts) !Histogram(T, []T)`
Initializes the histogram. Name must be given at comptime. Opts is:
* `help: ?[]const` - optional help text to include in the prometheus output

#### `deinit(self: Histogram(T, []T), allocator: Allocator) void`
Deallocates the histogram

#### `observe(self: Histogram(T, []T), value: T) void`
Observes `value`, bucketing it based on the provided comptime buckets.

#### `write(self: Histogram(T, []T), writer: anytype) !void`
Writes the histogram to `writer`.

### Histogram(T, L, []T)
A `Histogram(T, L, []T)` is used  to track the size and frequency of events. `T` can be an unsigned integer or a float. `L` must be a struct where the field names and types will define the lables. Its main methods is `observe(T)`.

Observed valued will fall within one of the provided buckets, `[]T`. The buckets must be in ascending order. A final "infinite" bucket *should not* be provided.

#### `init(allocator: Allocator, comptime name: []const, opts: Opts) !Histogram(T, L, []T)`
Initializes the histogram. Name must be given at comptime. Opts is:
* `help: ?[]const` - optional help text to include in the prometheus output

#### `deinit(self: Histogram(T, L, []T), allocator: Allocator) void`
Deallocates the histogram

#### `observe(self: Histogram(T, L, []T), value: T) void`
Observes `value`, bucketing it based on the provided comptime buckets.

#### `write(self: Histogram(T, L, []T), writer: anytype) !void`
Writes the histogram to `writer`.
