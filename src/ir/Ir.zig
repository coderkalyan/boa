const std = @import("std");
const builtin = @import("builtin");
const InternPool = @import("../InternPool.zig");
const Ast = @import("../Ast.zig");
const Liveness = @import("Liveness.zig");

pool: *InternPool,
tree: *const Ast,
insts: List.Slice,
extra: []const u32,
block: ExtraIndex,
liveness: Liveness,

pub const Ir = @This();
pub const List = std.MultiArrayList(Inst);
pub const Index = enum(u32) { _ };
pub const ExtraIndex = enum(u32) { _ };

pub const Inst = struct {
    tag: Tag,
    payload: Payload,

    pub const Tag = enum(u8) {
        // constant (immediate)
        // .ip
        constant,

        // int to float
        // .unary
        itof,
        // float to int
        // .unary
        ftoi,

        // negation (unary subtraction)
        // .unary
        neg,
        // bitwise invert
        // .unary
        binv,
        // boolean invert
        // .unary
        lnot,

        // .binary
        // addition
        add,
        // subtraction
        sub,
        // multiplication
        mul,
        // division
        div,
        // modulo
        mod,
        // raise to power
        pow,
        // matrix multiplication
        // matmul,
        // bitwise or
        bor,
        // bitwise and
        band,
        // bitwise xor
        bxor,
        // logical shift left
        sll,
        // arithmetic shift right
        sra,
        // boolean or
        lor,
        // boolean and
        land,
        // equal
        eq,
        // not equal
        ne,
        // less than
        lt,
        // greater than
        gt,
        // less than equal
        le,
        // greater than equal
        ge,

        // return a value
        ret,

        // allocate a stack slot for a variable
        // .ip = type of variable
        alloc,
        // load a value from a stack slot
        // .unary = alloc inst
        load,
        // store a value in a stack slot
        // .binary = alloc inst and operand to store
        store,
        // free a previously allocated stack slot
        // .unary = alloc inst
        dealloc,

        // if else
        // .op_extra = condition inst and BranchDouble extra
        branch_double,

        phiarg,
        phi,
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
        op_extra: struct {
            op: Index,
            extra: ExtraIndex,
        },

        comptime {
            if (builtin.mode != .Debug) {
                std.debug.assert(@sizeOf(Payload) <= 8);
            }
        }

        // we can't use std.meta.Tag since the union isn't tagged, but
        // this is used for comptime meta programming
        const Tag = enum {
            placeholder,
            ip,
            unary,
            binary,
            op_extra,
        };
    };

    pub const ExtraSlice = struct {
        start: ExtraIndex,
        end: ExtraIndex,
    };

    pub const BranchDouble = struct {
        exec_true: ExtraIndex,
        exec_false: ExtraIndex,
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

pub fn typeOf(ir: *const Ir, inst: Index) InternPool.Index {
    const index = @intFromEnum(inst);
    const tag = ir.insts.items(.tag)[index];
    const payload = ir.insts.items(.payload)[index];

    return switch (tag) {
        .constant => ir.pool.get(payload.ip).tv.ty,
        .itof => .float,
        .ftoi => .int,
        .binv => .int,
        .lnot => .bool,
        .add,
        .sub,
        .mul,
        .div,
        .mod,
        .pow,
        => ir.typeOf(payload.binary.l),
        .neg => ir.typeOf(payload.unary),
        .bor, .band, .bxor, .sll, .sra => .int,
        .lor, .land => .bool,
        .eq, .ne, .lt, .gt, .le, .ge => .bool,
        .ret => ir.typeOf(payload.unary),
        .alloc => unreachable,
        .load => ir.insts.items(.payload)[@intFromEnum(payload.unary)].ip,
        .store => ir.typeOf(payload.binary.r),
        .dealloc => unreachable,
        // TODO: confirm, but we shouldn't use result of if
        // this may change when adding phi insts
        .branch_double => unreachable,
        .phiarg => ir.typeOf(payload.unary),
        // TODO: union the types
        .phi => ir.typeOf(payload.binary.l),
    };
}

pub fn payloadTag(tag: Inst.Tag) Inst.Payload.Tag {
    return switch (tag) {
        .constant => .ip,
        .itof,
        .ftoi,
        .neg,
        .binv,
        .lnot,
        .phiarg,
        => .unary,
        .add,
        .sub,
        .mul,
        .div,
        .mod,
        .pow,
        .bor,
        .band,
        .bxor,
        .sll,
        .sra,
        .lor,
        .land,
        .eq,
        .ne,
        .lt,
        .gt,
        .le,
        .ge,
        .phi,
        => .binary,
        .ret => .unary,
        .alloc => .ip,
        .load => .unary,
        .store => .binary,
        .dealloc => .unary,
        .branch_double => .op_extra,
    };
}

pub inline fn instTag(ir: *const Ir, inst: Index) Inst.Tag {
    const index = @intFromEnum(inst);
    return ir.insts.items(.tag)[index];
}

pub inline fn instPayload(ir: *const Ir, inst: Index) Inst.Payload {
    const index = @intFromEnum(inst);
    return ir.insts.items(.payload)[index];
}
