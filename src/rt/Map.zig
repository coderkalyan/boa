const std = @import("std");
const InternPool = @import("../InternPool.zig");

const Allocator = std.mem.Allocator;
const Hasher = std.hash.Wyhash;
const Map = @This();

count: u32,
capacity: u32,
entries: [*]Entry,

const Entry = struct {
    key: InternPool.Index,
    value: u32,
};

const sizes = comptime init_sizes: {
    var sizes: [0]
};

// pub fn init(gpa: Allocator, comptime capacity: u32) !Map {
//     std.debug.assert(capacity & (capacity - 1) == 0);
//     const entries = try gpa.alloc(Entry, capacity);
//     @memset(entries, .{});
//
//     return .{
//         .entries = entries,
//     };
// }
//
// pub fn deinit(map: *Map, gpa: Allocator) void {
//     gpa.free(map.entries);
// }
//
// inline fn reduce(map: *const Map, hash: u64) u32 {
//     const mul = @mulWithOverflow(hash, map.entries.len);
//     return @truncate(mul[0] >> 32);
// }
//
// inline fn hashKey(key: InternPool.Index) u64 {
//     var hasher = Hasher.init(0);
//     std.hash.autoHash(&hasher, @intFromEnum(key));
//     return hasher.final();
// }
//
// pub fn put(map: *Map, key: InternPool.Index, value: u32) !void {
//     const hash = hashKey(key);
//     var base = map.reduce(hash);
//
//     const probe_limit = @log2(map.entries.len);
//     for (0..probe_limit)
// }
//
// pub fn get(map: *const Map, key: InternPool.Index) ?u32 {
//     const hash = hashKey(key);
//     const base = map.reduce(hash);
//
//     for (0..map.entries.len) |_| {
//         const entry = map.entries[base];
//         if (entry.key == key) return entry.value;
//
//         base += 1;
//         if (base == map.entries.len) base = 0;
//     }
// }
