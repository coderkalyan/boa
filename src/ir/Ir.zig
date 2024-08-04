const std = @import("std");
const builtin = @import("builtin");
const InternPool = @import("../InternPool.zig");
const Ast = @import("../Ast.zig");

pool: *InternPool,
tree: *const Ast,
insts: List.Slice,
extra: []const u32,

pub const Ir = @This();
pub const List = std.MultiArrayList(Inst);
pub const Index = enum(u32) { _ };
pub const ExtraIndex = enum(u32) { _ };

pub const Inst = struct {
    tag: Tag,
    payload: Payload,

    pub const Tag = enum(u8) {
        // (u64) integer constant
        // .int
        // iconst,
        // float constant
        // .float
        // fconst,
        // boolean constant
        // .bool
        // bconst,
        // None constant
        // .none
        // nconst,
        constant,

        // unary addition
        // .unary
        pos,
        // negation (unary subtraction)
        // .unary
        neg,
        // bitwise invert
        // .unary
        binv,
        // boolean invert
        // .unary
        not,

        // addition
        // .binary
        add,
        // subtraction
        // .binary
        sub,
        // multiplication
        // .binary
        mul,
        // division
        // .binary
        div,
        // modulo
        // .binary
        mod,
        // raise to power
        // .binary
        pow,
        // matrix multiplication
        // .binary
        matmul,
        // bitwise or
        // .binary
        bor,
        // bitwise and
        // .binary
        band,
        // bitwise xor
        // .binary
        xor,
        // boolean or
        // .binary
        lor,
        // boolean and
        // .binary
        land,
        // equal
        // .binary
        eq,
        // not equal
        // .binary
        ne,
        // less than
        // .binary
        lt,
        // greater than
        // .binary
        gt,
        // less than equal
        // .binary
        le,
        // greater than equal
        // .binary
        ge,

        // return a value
        ret,
    };

    pub const Payload = union {
        placeholder: void,
        ip: InternPool.Index,
        // int: u64,
        // float: f64,
        // bool: bool,
        unary: Index,
        binary: struct {
            l: Index,
            r: Index,
        },

        comptime {
            if (builtin.mode != .Debug) {
                std.debug.assert(@sizeOf(Payload) <= 8);
            }
        }
    };

    pub const ExtraSlice = struct {
        start: ExtraIndex,
        end: ExtraIndex,
    };
};

pub fn extraData(ir: *const Ir, comptime T: type, index: ExtraIndex) T {
    var result: T = undefined;
    const fields = std.meta.fields(T);
    const base: u32 = @intFromEnum(index);
    inline for (fields, 0..) |field, i| {
        switch (field.type) {
            inline else => @field(result, field.name) = @enumFromInt(ir.extra[base + i]),
        }
    }
    return result;
}

pub fn extraSlice(ir: *const Ir, slice: Inst.ExtraSlice) []const u32 {
    const start: u32 = @intFromEnum(slice.start);
    const end: u32 = @intFromEnum(slice.end);
    return ir.extra[start..end];
}
