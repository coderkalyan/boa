const std = @import("std");
const Ast = @import("../Ast.zig");
const Ir = @import("../ir/Ir.zig");
const InternPool = @import("../InternPool.zig");
const Bytecode = @import("../bc/Bytecode.zig");
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

pub const FunctionInfo = extern struct {
    tree: *const Ast,
    ir: *const Ir,
    bytecode: [*]const i32,
    node: Ast.Node.Index,
    frame_size: u32,
    state: State,

    pub const State = enum(u32) {
        lazy,
        interpreted,
        optimized_lite,
        optimized_full,
    };
};
