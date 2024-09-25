const std = @import("std");
const PageBumpAllocator = @import("../PageBumpAllocator.zig");
const InternPool = @import("../InternPool.zig");
const Object = @import("object.zig").Object;

const Allocator = std.mem.Allocator;

pub fn CacheStub(comptime cache_type: CacheType) type {
    return struct {
        head: ?*Entry,
        fallback: Entry.Method,

        const Entry = CacheEntry(cache_type);
        const Stub = @This();

        pub fn call(stub: *Stub, key: Entry.Key) Entry.Method {
            var entry_ptr: ?*const Entry = stub.head;
            while (entry_ptr.*) |entry| {
                if (entry.key == key) return entry.method;
                entry_ptr = entry.next;
            }
        }
    };
}

pub fn CacheEntry(comptime cache_type: CacheType) type {
    return switch (cache_type) {
        .load_attr => struct {
            key: Key,
            data: Data,
            method: Method,
            next: *@This(),

            const Key = InternPool.Index;
            const Data = u64;
            const Method = *const fn (object: *const Object, attr: InternPool.Index) u64;
        },
    };
}

pub const CacheType = enum {
    load_attr,
};
