const std = @import("std");

const Allocator = std.mem.Allocator;

pub const String = extern struct {
    len: usize,
    ptr: void align(@alignOf(u8)),

    pub fn init(gpa: Allocator, slice: []const u8) !*const String {
        const str = try alloc(gpa, slice.len);
        @memcpy(str.bytesMut(), slice);
        return str;
    }

    fn alloc(gpa: Allocator, len: usize) !*String {
        const size = len + @sizeOf(String);
        const container = try gpa.alignedAlloc(u8, @alignOf(String), size);
        const str: *String = @ptrCast(@alignCast(container));
        str.len = len;

        return str;
    }

    pub fn bytes(s: *const String) []const u8 {
        const start: [*]const u8 = @ptrCast(&s.ptr);
        return start[0..s.len];
    }

    fn bytesMut(s: *String) []u8 {
        const start: [*]u8 = @ptrCast(&s.ptr);
        return start[0..s.len];
    }

    pub fn catenate(gpa: Allocator, a: *const String, b: *const String) !*const String {
        const str = try alloc(gpa, a.len + b.len);
        const bytes_mut = str.bytesMut();
        @memcpy(bytes_mut[0..a.len], a.bytes());
        @memcpy(bytes_mut[a.len..str.len], b.bytes());
        return str;
    }

    pub fn repeat(gpa: Allocator, template: *const String, count: usize) !*const String {
        const str = try alloc(gpa, template.len * count);
        const bytes_mut = str.bytesMut();
        for (0..count) |i| {
            const start = template.len * i;
            @memcpy(bytes_mut[start .. start + template.len], template.bytes());
        }
        return str;
    }
};
