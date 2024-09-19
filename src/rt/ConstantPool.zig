const std = @import("std");
const InternPool = @import("../InternPool.zig");
const String = @import("string.zig").String;

const Allocator = std.mem.Allocator;
const ConstantPool = @This();

gpa: Allocator,
// while this function takes in an allocator, it is highly recommended to use with
// a contiguous allocator like the internal PageBumpAllocator, ArenaAllocator, or
// FixedBufferAllocator
pba: Allocator,
map: std.AutoHashMapUnmanaged(InternPool.Index, *anyopaque),

pub fn init(gpa: Allocator, pba: Allocator) ConstantPool {
    return .{
        .gpa = gpa,
        .pba = pba,
        .map = .{},
    };
}

// uniques an intern pool index and gives a heap allocated pointer to put in the
// interpreter.
pub fn put(self: *ConstantPool, pool: *InternPool, ip: InternPool.Index) !*anyopaque {
    const pba = self.pba;
    const gop = try self.map.getOrPut(self.gpa, ip);

    if (!gop.found_existing) {
        switch (pool.get(ip)) {
            .ty, .ir, .bytecode => unreachable,
            .tv => |tv| switch (pool.get(tv.ty).ty) {
                .nonetype => unreachable,
                .int => {
                    const ptr = try pba.create(u64);
                    ptr.* = tv.val.int;
                    gop.value_ptr.* = ptr;
                },
                .float => {
                    const ptr = try pba.create(f64);
                    ptr.* = tv.val.float;
                    gop.value_ptr.* = ptr;
                },
                .bool => unreachable,
                .str => unreachable, // implemented in .str TODO: change this?
                .@"union", .any => unreachable, // TODO: unimplemented
            },
            .str => |bytes| {
                // load a string literal from the intern pool and construct a string
                // on the heap
                const str = String.init(pba, bytes) catch unreachable;
                gop.value_ptr.* = @constCast(str);
            },
            .function => |fi| gop.value_ptr.* = pool.functionPtr(fi),
        }
    }

    return gop.value_ptr.*;
}
