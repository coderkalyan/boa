const std = @import("std");
const posix = std.posix;

const Allocator = std.mem.Allocator;
const PageBumpAllocator = @This();

// this is the internal tracking page (chunk) size, a multiple of the real OS page size
const page_size = 512 * 1024;

const vtable: Allocator.VTable = .{
    .alloc = alloc,
    .resize = resize,
    .free = free,
};

page: []u8 = &.{},
bump: usize = 0,

pub fn allocator(pba: *PageBumpAllocator) Allocator {
    return .{
        .ptr = pba,
        .vtable = &vtable,
    };
}

fn alloc(ctx: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
    _ = ret_addr;
    const pba: *PageBumpAllocator = @ptrCast(@alignCast(ctx));

    // align as required
    var ptr = pba.page.ptr + pba.bump;
    const align_mask = (@as(usize, 1) << @truncate(ptr_align)) - 1;
    ptr = @ptrFromInt((@intFromPtr(ptr) + align_mask) & ~align_mask);
    pba.bump = @intFromPtr(ptr) - @intFromPtr(pba.page.ptr);

    if (pba.bump + len <= pba.page.len) {
        pba.bump += len;
        return ptr;
    }

    // ran out of memory, forget about the existing page and mmap a new one
    const page = posix.mmap(
        null,
        page_size,
        posix.PROT.READ | posix.PROT.WRITE,
        .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
        -1,
        0,
    ) catch return null;

    // we don't care about alignment above the page size, so just put the object
    // at the start and return
    pba.* = .{ .page = page, .bump = len };
    return page.ptr;
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

test "allocate" {
    var page_bump: PageBumpAllocator = .{};
    const pba = page_bump.allocator();

    const int_ptr = try pba.create(u32);
    int_ptr.* = 0xcafeb0ba;

    const String = @import("string.zig").String;
    const string = try String.init(pba, "Hello, world!");
    try std.testing.expect(std.mem.eql(u8, string.bytes(), "Hello, world!"));

    const floats = try pba.alloc(f32, 4);
    floats[0] = 1.0;
    floats[1] = 2.0;
    floats[2] = 3.0;
    floats[3] = 4.0;

    try std.testing.expectEqual(1.0, floats[0]);
    try std.testing.expectEqual(2.0, floats[1]);
    try std.testing.expectEqual(3.0, floats[2]);
    try std.testing.expectEqual(4.0, floats[3]);
}
