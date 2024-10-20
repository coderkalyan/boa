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
