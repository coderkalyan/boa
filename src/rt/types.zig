const std = @import("std");
const Object = @import("object.zig").Object;

const Allocator = std.mem.Allocator;

// TODO: this should be extern?
pub const Context = struct {
    ipool: *InternPool,
    global: *Object,
    gpa: Allocator,
    pba: Allocator,
    gca: Allocator,
};

pub const CompilationInfo = opaque {};

pub const FunctionInfo = extern struct {
    comp: *CompilationInfo,
    bytecode: [*]i32,
    frame_size: u32,
};

pub const InternPool = opaque {
    pub const Index = enum(u32) { _ };
};

pub const List = extern struct {
    ptr: [*]align(alignment) u8,
    len: usize,
    capacity: usize,
    kind: Kind,

    const alignment = @alignOf(u64);
    const initial_capacity = 4;
    pub const Kind = enum(usize) {
        int,
    };

    pub fn init(gpa: Allocator) !List {
        const element_size = @sizeOf(i64);
        const elements = try gpa.alignedAlloc(u8, alignment, element_size * initial_capacity);
        return .{
            .ptr = elements.ptr,
            .len = 0,
            .capacity = elements.len,
            .kind = .int,
        };
    }
};
