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
        // unary addition
        // .unary
        // TODO: check if this is needed
        // pos,

        // negation (unary subtraction)
        // .unary
        ineg,
        fneg,
        // bitwise invert
        // .unary
        binv,
        // boolean invert
        // .unary
        lnot,

        // addition
        // .binary
        add,
        // iadd,
        // fadd,
        // subtraction
        // .binary
        sub,
        // isub,
        // fsub,
        // multiplication
        // .binary
        mul,
        // imul,
        // fmul,
        // division
        // .binary
        div,
        // idiv,
        // fdiv,
        // modulo
        // .binary
        mod,
        // imod,
        // fmod,
        // raise to power
        // .binary
        pow,
        // ipow,
        // fpow,
        // matrix multiplication
        // .binary
        // matmul,
        // bitwise or
        // .binary
        bor,
        // bitwise and
        // .binary
        band,
        // bitwise xor
        // .binary
        bxor,
        // boolean or
        // .binary
        lor,
        // boolean and
        // .binary
        land,
        // equal
        // .binary
        ieq,
        feq,
        beq,
        // not equal
        // .binary
        ine,
        fne,
        bne,
        // less than
        // .binary
        ilt,
        flt,
        blt,
        // greater than
        // .binary
        igt,
        fgt,
        bgt,
        // less than equal
        // .binary
        ile,
        fle,
        ble,
        // greater than equal
        // .binary
        ige,
        fge,
        bge,

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

        // we can't use std.meta.Tag since the union isn't tagged, but
        // this is used for comptime meta programming
        const Tag = enum {
            placeholder,
            ip,
            unary,
            binary,
        };
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

pub fn typeOf(ir: *const Ir, inst: Index) InternPool.Index {
    const index = @intFromEnum(inst);
    const tag = ir.insts.items(.tag)[index];
    const payload = ir.insts.items(.payload)[index];

    return switch (tag) {
        .constant => ir.pool.get(payload.ip).tv.ty,
        .itof => .float,
        .ftoi => .int,
        .ineg => .int,
        .fneg => .float,
        .binv => .int,
        .lnot => .bool,
        .add,
        .sub,
        .mul,
        .div,
        .mod,
        .pow,
        => ir.typeOf(payload.binary.l),
        // .iadd => .int,
        // .fadd => .float,
        // .isub => .int,
        // .fsub => .float,
        // .imul => .int,
        // .fmul => .float,
        // .idiv => .int,
        // .fdiv => .float,
        // .imod => .int,
        // .fmod => .float,
        // .ipow => .int,
        // .fpow => .float,
        .bor, .band, .bxor => .int,
        .lor, .land => .bool,
        .ieq, .feq, .beq, .ine, .fne, .bne => .bool,
        .ilt, .flt, .blt, .igt, .fgt, .bgt => .bool,
        .ile, .fle, .ble, .ige, .fge, .bge => .bool,
        .ret => ir.typeOf(payload.unary),
        .alloc => unreachable,
        .load => ir.insts.items(.payload)[@intFromEnum(payload.unary)].ip,
        .store => ir.typeOf(payload.binary.r),
        .dealloc => unreachable,
    };
}

pub fn payloadTag(tag: Inst.Tag) Inst.Payload.Tag {
    return switch (tag) {
        .constant => .ip,
        .itof,
        .ftoi,
        .ineg,
        .fneg,
        .binv,
        .lnot,
        => .unary,
        .add,
        .sub,
        .mul,
        .div,
        .mod,
        .pow,
        // .iadd,
        // .fadd,
        // .isub,
        // .fsub,
        // .idiv,
        // .fdiv,
        // .imul,
        // .fmul,
        // .imod,
        // .fmod,
        // .ipow,
        // .fpow,
        .bor,
        .band,
        .bxor,
        .lor,
        .land,
        .ieq,
        .feq,
        .beq,
        .ine,
        .fne,
        .bne,
        .ilt,
        .flt,
        .blt,
        .igt,
        .fgt,
        .bgt,
        .ile,
        .fle,
        .ble,
        .ige,
        .fge,
        .bge,
        => .binary,
        .ret => .unary,
        .alloc => .ip,
        .load => .unary,
        .store => .binary,
        .dealloc => .unary,
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
