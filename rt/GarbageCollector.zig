const std = @import("std");
const builtin = @import("builtin");

const mem = std.mem;
const posix = std.posix;
const Allocator = std.mem.Allocator;
const asBytes = mem.asBytes;
const GarbageCollector = @This();

const space_size = 4 * 1024 * 1024 * 1024;
const chunk_size = 512 * 1024;
const stride = @alignOf(u64);

// used to store temporary data during garbage collection
arena: std.heap.ArenaAllocator,
// underlying virtual address space of size 2 * space_size (from mmap)
map: []align(mem.page_size) u8,
// the chunk of `map` space that we're currently allocating to
from_space: []align(mem.page_size) u64,
// the chunk of `map` space that we reserve for garbage collection
to_space: []align(mem.page_size) u64,
// current location in from_space we are allocating to
write_ptr: [*]u64,
// function that returns an iterator
push_fn: PushFn,
// opaque user data passed to the push function
user_context: ?*anyopaque,
scavenge_cycles: u32,

pub const Queue = std.ArrayList(**align(stride) anyopaque);
// when mode is .root, object_ptr and len are undefined and should not be used
pub const PushFn = *const fn (
    queue: *Queue,
    data: PushData,
) Allocator.Error!void;
pub const PushData = union(enum) {
    // push the root set
    root: struct {
        ctx: ?*anyopaque,
    },
    // push an object's pointers
    object: struct {
        ctx: ?*anyopaque,
        ptr: *anyopaque,
        len: usize,
    },
};

pub fn init(gpa: Allocator, push_fn: PushFn, user_context: ?*anyopaque) !GarbageCollector {
    var arena = std.heap.ArenaAllocator.init(gpa);
    errdefer arena.deinit();

    const map = try posix.mmap(
        null,
        2 * space_size,
        posix.PROT.READ | posix.PROT.WRITE,
        .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
        -1,
        0,
    );

    // divide the mapped region into two spaces of space_size
    const space_size_u64 = space_size / @sizeOf(u64);
    const a_start: [*]align(mem.page_size) u64 = @ptrCast(map.ptr);
    const b_start: [*]align(mem.page_size) u64 = @ptrCast(map.ptr + space_size);
    const a_space = a_start[0..space_size_u64];
    const b_space = b_start[0..space_size_u64];

    return .{
        .arena = arena,
        .map = map,
        .from_space = a_space,
        .to_space = b_space,
        .write_ptr = a_space.ptr,
        .push_fn = push_fn,
        .user_context = user_context,
        .scavenge_cycles = 0,
    };
}

pub fn deinit(gc: *GarbageCollector) void {
    posix.munmap(gc.map);
    gc.arena.deinit();
}

pub fn allocator(gc: *GarbageCollector) Allocator {
    return .{
        .ptr = gc,
        .vtable = &.{
            .alloc = alloc,
            .resize = resize,
            .free = free,
        },
    };
}

pub fn scavenge(gc: *GarbageCollector) Allocator.Error!void {
    // implementation of cheney's algorithm for mark and sweep:
    // 1) starting at a root set of accessible pointers (i.e. from the stack and
    // globals) traverse the graph of pointers to mark all reachable
    // objects as live
    // 2) for each live object (residing in the from_space), copy it into
    // the to_space contiguously (defragment), and write the new pointer to
    // (forwarding pointer) to the object header in the from_space
    // the next time the object is visited, simply update its pointer using
    // the forwarding pointer
    // 3) swap buffers, switching the from_space and to_space

    _ = gc.arena.reset(.retain_capacity);
    var queue = Queue.init(gc.arena.allocator());

    var write_ptr: [*]u8 = @ptrCast(gc.to_space.ptr);
    try gc.push_fn(&queue, .{ .root = .{ .ctx = gc.user_context } });
    while (queue.popOrNull()) |object_ref| {
        // because this is a moving GC, we receive a pointer to the pointer (to the object)
        // check if the object has been moved already
        const object_ptr: [*]align(stride) u8 = @ptrCast(object_ref.*);
        const header_ptr = &(@as([*]u64, @ptrCast(object_ptr)) - 1)[0];

        const header = header_ptr.*;
        const object_len = header >> 32;
        const object_align = (header >> 24) & 0xff;
        const object_moved = (header & 0x01) == 1;
        if (!object_moved) {
            // first time seeing the object, so move it
            write_ptr += @sizeOf(u64);
            const align_mask = object_align - 1;
            write_ptr = @ptrFromInt((@intFromPtr(write_ptr) + align_mask) & ~align_mask);

            // std.debug.print("{*}\n", .{(@as([*]u64, @ptrCast(write_ptr)) - 1)});
            (@as([*]u64, @ptrCast(@alignCast(write_ptr))) - 1)[0] = header;
            const src: [*]align(stride) const u8 = @ptrCast(object_ptr);
            const dest: [*]align(stride) u8 = @ptrCast(@alignCast(write_ptr));
            @memcpy(dest[0..object_len], src[0..object_len]);

            // update its forwarding pointer
            header_ptr.* = @intFromPtr(write_ptr) | 0x1;
            write_ptr += object_len;
        }

        // unconditionally update the object_ref pointer using the forwarding pointer
        object_ref.* = @ptrFromInt(header_ptr.* & ~@as(u64, 0x1));
    }

    // align the write_ptr up to stride
    const align_mask: u64 = stride - 1;
    write_ptr = @ptrFromInt((@intFromPtr(write_ptr) + align_mask) & ~align_mask);

    // trigger seg faults on non-moved data for testing
    if (builtin.mode == .Debug) @memset(gc.from_space, 0);
    // swap from and to spaces
    const to_space = gc.to_space;
    gc.to_space = gc.from_space;
    gc.from_space = to_space;
    gc.write_ptr = @ptrCast(@alignCast(write_ptr));

    const scavenge_cycles = @addWithOverflow(gc.scavenge_cycles, 1);
    gc.scavenge_cycles = scavenge_cycles[0];
}

fn alloc(ctx: *anyopaque, len: usize, obj_align: u8, ret_addr: usize) ?[*]u8 {
    _ = ret_addr;
    const gc: *GarbageCollector = @ptrCast(@alignCast(ctx));

    // crude threshold - if we cross a chunk_size boundary, schedule a
    // garbage collection event
    const current_offset = @intFromPtr(gc.write_ptr) - @intFromPtr(gc.from_space.ptr);
    const len_lower_bound = len + @sizeOf(u64);
    const next_offset = current_offset + len_lower_bound;
    const chunk_mask = ~(@as(usize, chunk_size) - 1);
    if ((current_offset & chunk_mask) != (next_offset & chunk_mask)) {
        gc.scavenge() catch return null;
    }

    // we always allocate at write_ptr, but first:
    // 1) align it as required (by default, it has @alignOf(u64) alignment)
    // 2) reserve a single usize header to store size/align information
    // and the forwarding pointer during GC
    var write_ptr = gc.write_ptr;

    // first, allocate at least usize bytes for the header
    // if we add more bytes later to align, we will still have enough space
    write_ptr += @sizeOf(u64);

    // now align up to the requested alignment
    const ptr_align = @max(@sizeOf(u64), obj_align);
    const align_mask = (@as(usize, 1) << @truncate(ptr_align)) - 1;
    const aligned_ptr = (@intFromPtr(write_ptr) + align_mask) & ~align_mask;
    write_ptr = @ptrFromInt(aligned_ptr);

    const object_ptr = write_ptr;
    const header_ptr = &(write_ptr - 1)[0];
    // TODO: implement a large object allocator so we know for sure
    // len is not greater than maxInt(u32)
    std.debug.assert(len <= std.math.maxInt(u32));
    const header_len = @as(u64, len) << 32;
    const header_align = @as(u32, ptr_align) << 24;
    const header_moved = @intFromBool(false);
    header_ptr.* = header_len | header_align | header_moved;

    // bump up our write pointer and save context
    write_ptr += len / @sizeOf(u64);
    gc.write_ptr = write_ptr;

    return @ptrCast(object_ptr);
}

fn resize(ctx: *anyopaque, buf: []u8, buf_align: u8, new_len: usize, ret_addr: usize) bool {
    _ = ctx;
    _ = buf;
    _ = buf_align;
    _ = new_len;
    _ = ret_addr;
    return false;
}

fn free(ctx: *anyopaque, buf: []u8, buf_align: u8, ret_addr: usize) void {
    _ = ctx;
    _ = buf;
    _ = buf_align;
    _ = ret_addr;
}

test "basic alloc" {
    var gc = try GarbageCollector.init(std.testing.allocator, undefined, undefined);
    defer gc.deinit();
    const gca = gc.allocator();

    // allocate a simple integer and make sure it can be written to
    const int_ptr = try gca.create(u64);
    int_ptr.* = 0xcafeb0bacafeb0ba;
    try std.testing.expectEqual(0xcafeb0bacafeb0ba, int_ptr.*);

    // allocate an aligned float array
    const floats = try gca.alignedAlloc(f64, 32, 4);
    const floats_ptr = @intFromPtr(floats.ptr);
    const align_mask: u64 = 31;
    try std.testing.expectEqual(floats_ptr, floats_ptr & ~align_mask);
    floats[0] = 1.0;
    floats[1] = 2.0;
    floats[2] = 3.0;
    floats[3] = 4.0;
    try std.testing.expectEqualSlices(f64, &.{ 1.0, 2.0, 3.0, 4.0 }, floats);

    // allocate a string
    const String = @import("string.zig").String;
    const string_ptr = try String.init(gca, "Hello, world!");
    try std.testing.expectEqualSlices(u8, "Hello, world!", string_ptr.bytes());
}

const TestStack = std.SegmentedList(*align(@alignOf(u64)) anyopaque, 4);
fn pushFn(queue: *Queue, data: PushData) Allocator.Error!void {
    switch (data) {
        .root => |root| {
            const stack: *TestStack = @ptrCast(@alignCast(root.ctx));
            try queue.ensureUnusedCapacity(stack.count());
            var iterator = stack.iterator(0);
            while (iterator.next()) |ptr| {
                queue.appendAssumeCapacity(ptr);
            }
        },
        .object => std.debug.assert(false),
    }
}

test "stack scavenge" {
    const gpa = std.testing.allocator;
    var stack: TestStack = .{};
    defer stack.deinit(gpa);

    var gc = try GarbageCollector.init(gpa, pushFn, &stack);
    defer gc.deinit();
    const gca = gc.allocator();

    // allocate two strings and push them to the stack
    const String = @import("string.zig").String;
    const hello = try String.init(gca, "Hello, ");
    const world = try String.init(gca, "world!");
    try std.testing.expectEqual("Hello, ".len, hello.len);
    try std.testing.expectEqualSlices(u8, "Hello, ", hello.bytes());
    try std.testing.expectEqual("world!".len, world.len);
    try std.testing.expectEqualSlices(u8, "world!", world.bytes());
    try stack.append(gpa, @constCast(@ptrCast(@alignCast(hello))));
    try stack.append(gpa, @constCast(@ptrCast(@alignCast(world))));

    // run a GC cycle - both pointers should still be alive
    try gc.scavenge();
    const hello_ptr: *const String = @ptrCast(@alignCast(stack.at(0).*));
    const world_ptr: *const String = @ptrCast(@alignCast(stack.at(1).*));
    try std.testing.expectEqual("Hello, ".len, hello_ptr.len);
    try std.testing.expectEqualSlices(u8, "Hello, ", hello_ptr.bytes());
    try std.testing.expectEqual("world!".len, world_ptr.len);
    try std.testing.expectEqualSlices(u8, "world!", world_ptr.bytes());

    // create a new string by catenating the two source strings, and delete both of them
    stack.clearRetainingCapacity();
    const hello_world = try String.catenate(gca, hello_ptr, world_ptr);
    try stack.append(gpa, @constCast(@ptrCast(@alignCast(hello_world))));

    // run another GC cycle, and make sure only the new pointer is alive
    try gc.scavenge();
    const hello_world_ptr: *const String = @ptrCast(@alignCast(stack.at(0).*));
    try std.testing.expectEqual("Hello, world!".len, hello_world_ptr.len);
    try std.testing.expectEqualSlices(u8, "Hello, world!", hello_world_ptr.bytes());

    // make sure it destroyed the old pointers (debug mode)
    try std.testing.expectEqual(0, hello_ptr.len);
    try std.testing.expectEqual(0, world_ptr.len);
    var alloc_len: u64 = 8 + @sizeOf(String) + hello_world_ptr.len;
    alloc_len = (alloc_len + stride - 1) & ~(@as(u64, stride) - 1);
    try std.testing.expectEqual(alloc_len, @intFromPtr(gc.write_ptr) - @intFromPtr(gc.from_space.ptr));

    // throw out existing stuff, and create two references to a single new object
    stack.clearRetainingCapacity();
    const duplicate = try String.init(gca, "duplicate");
    try stack.append(gpa, @constCast(@ptrCast(@alignCast(duplicate))));
    try stack.append(gpa, @constCast(@ptrCast(@alignCast(duplicate))));

    // run another GC cycle, and make sure both references are alive
    try gc.scavenge();
    const dup1: *const String = @ptrCast(@alignCast(stack.at(0).*));
    const dup2: *const String = @ptrCast(@alignCast(stack.at(1).*));
    try std.testing.expectEqual("duplicate".len, dup1.len);
    try std.testing.expectEqualSlices(u8, "duplicate", dup1.bytes());
    try std.testing.expectEqual("duplicate".len, dup2.len);
    try std.testing.expectEqualSlices(u8, "duplicate", dup2.bytes());
}
