//! Simple spin-based synchronization primitives for Zig 0.16+.
//! std.Thread.Mutex and std.Thread.RwLock were removed in 0.16;
//! the new std.Io.Mutex/RwLock require an Io context which is too
//! heavy a dependency for a metrics library.  These lightweight
//! spin implementations cover the low-contention use-case well.

const std = @import("std");

/// A simple blocking mutex backed by an atomic boolean.
pub const Mutex = struct {
    state: std.atomic.Value(bool) = std.atomic.Value(bool).init(false),

    pub const init: Mutex = .{};

    pub fn lock(m: *Mutex) void {
        while (true) {
            // try to go from unlocked (false) -> locked (true)
            if (m.state.cmpxchgWeak(false, true, .acquire, .monotonic) == null) return;
            // spin
            std.Thread.yield() catch {};
        }
    }

    pub fn unlock(m: *Mutex) void {
        m.state.store(false, .release);
    }
};

/// A simple blocking readers-writer lock backed by an atomic i32.
///
/// Encoding:
///   -1  = writer holds the lock
///   >=0 = number of concurrent readers
pub const RwLock = struct {
    state: std.atomic.Value(i32) = std.atomic.Value(i32).init(0),

    pub const init: RwLock = .{};

    // --- exclusive (write) lock ---

    pub fn lock(rw: *RwLock) void {
        // Spin until we can go from 0 -> -1
        while (true) {
            if (rw.state.cmpxchgWeak(0, -1, .acquire, .monotonic) == null) return;
            std.Thread.yield() catch {};
        }
    }

    pub fn unlock(rw: *RwLock) void {
        _ = rw.state.swap(0, .release);
    }

    // --- shared (read) lock ---

    pub fn lockShared(rw: *RwLock) void {
        while (true) {
            const s = rw.state.load(.monotonic);
            if (s >= 0) {
                // try to add a reader
                if (rw.state.cmpxchgWeak(s, s + 1, .acquire, .monotonic) == null) return;
            }
            // writer holds lock or CAS failed — spin
            std.Thread.yield() catch {};
        }
    }

    pub fn unlockShared(rw: *RwLock) void {
        _ = rw.state.fetchSub(1, .release);
    }
};
