const std = @import("std");
const builtin = @import("builtin");
const InternPool = @import("../InternPool.zig");
const Ast = @import("../Ast.zig");
const Liveness = @import("Liveness.zig");

pool: *InternPool,
tree: *const Ast,
insts: List.Slice,
extra: []const u32,
blocks: []const Block,
liveness: Liveness,

pub const Ir = @This();
pub const List = std.MultiArrayList(Inst);
pub const Index = enum(u32) { _ };
pub const ExtraIndex = enum(u32) { _ };
pub const BlockIndex = enum(u32) { _ };

pub const Block = struct {
    insts: Inst.ExtraSlice,
};

pub const Inst = struct {
    tag: Tag,
    payload: Payload,

    pub const Tag = enum(u8) {
        // constant (immediate)
        // .ip
        constant,

        // extract an argument
        arg,
        // reference a builtin (must be called)
        // .ip
        builtin,
        // initialize a list
        list_init,
        // calculate the pointer to a variable in the current "context" object
        // by identifier
        // .slot
        context_ptr,
        // calculate the pointer to an object field, by identifier
        // .unary_ip
        attribute_ptr,
        // calculate the pointer to a list element, by index
        // .unary
        element_ptr,
        // load the value from a ptr - exact semantics depends on operand type
        // operand must be one of:
        // * context_ptr
        // * attribute_ptr
        // * element_ptr
        load,
        // store a value to a ptr - exact semantics depends on the operand type
        // operand must be one of:
        // * context_ptr
        // * attribute_ptr
        // * element_ptr
        store,

        // TODO: only encode destination type
        // int to float
        // .unary
        itof,
        // float to int
        // .unary
        ftoi,
        // int to bool
        // .unary
        itob,
        // bool to int
        // .unary
        btoi,
        // cast to any
        any,

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

        // function call
        call,

        // return a value
        ret,
        // jump unconditionally to a new block
        jmp,
        // branch conditionally to one of two blocks
        br,
        // select between two arguments based on entry block
        phi,
    };

    pub const Payload = union {
        placeholder: void,
        ip: InternPool.Index,
        slot: u32,
        unary: Index,
        binary: struct {
            l: Index,
            r: Index,
        },
        unary_extra: struct {
            op: Index,
            extra: ExtraIndex,
        },
        block: BlockIndex,
        extra: ExtraIndex,
        unary_ip: struct {
            op: Index,
            ip: InternPool.Index,
        },
        arg: struct {
            position: u32,
            ty: InternPool.Index,
        },

        comptime {
            if (builtin.mode == .ReleaseFast) {
                std.debug.assert(@sizeOf(Payload) <= 8);
            }
        }

        // we can't use std.meta.Tag since the union isn't tagged, but
        // this is used for comptime meta programming
        const Tag = enum {
            placeholder,
            ip,
            slot,
            unary,
            binary,
            unary_extra,
            block,
            extra,
            unary_ip,
            arg,
        };
    };

    pub const ExtraSlice = struct {
        start: ExtraIndex,
        end: ExtraIndex,
    };

    pub const Branch = struct {
        exec_if: BlockIndex,
        exec_else: BlockIndex,
    };

    pub const Phi = struct {
        ty: InternPool.Index,
        src1: Index,
        block1: BlockIndex,
        src2: Index,
        block2: BlockIndex,
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
        .constant => switch (ir.pool.get(payload.ip)) {
            .tv => |tv| tv.ty,
            else => .str,
        },
        .arg => payload.arg.ty,
        .builtin => unreachable, // TODO: implement this
        .context_ptr, .attribute_ptr, .element_ptr => unreachable,
        .load => payload.unary_ip.ip,
        .store => unreachable,
        .list_init => {
            // TODO: implement non empty list initializers
            return .list_int;
        },
        .itof => .float,
        .ftoi => .int,
        .itob => .bool,
        .btoi => .int,
        .any => .any,
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
        .eq, .ne, .lt, .gt, .le, .ge => .bool,
        .call => {
            // const target = payload.unary_extra.op;
            // std.debug.print("{}\n", .{ir.instTag(target)});
            // if (ir.instTag(target) == .constant) {
            //     const findex = ir.pool.get(ir.instPayload(target).ip).function;
            //     const fi_ptr = ir.pool.functionPtr(findex);
            //     return fi_ptr.return_type;
            // }

            return .int; // .any;
        },
        .ret => ir.typeOf(payload.unary),
        .jmp, .br => unreachable,
        .phi => ir.extraData(Inst.Phi, payload.extra).ty,
    };
}

pub fn payloadTag(tag: Inst.Tag) Inst.Payload.Tag {
    return switch (tag) {
        .constant, .builtin => .ip,
        .context_ptr => .slot,
        .attribute_ptr, .load => .unary_ip,
        .list_init => .extra,
        .itof,
        .ftoi,
        .itob,
        .btoi,
        .any,
        .neg,
        .binv,
        .lnot,
        => .unary,
        .arg => .arg,
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
        .eq,
        .ne,
        .lt,
        .gt,
        .le,
        .ge,
        .phi,
        .store,
        .element_ptr,
        => .binary,
        .call => .unary_extra,
        .ret => .unary,
        .jmp => .block,
        .br => .unary_extra,
    };
}

pub fn operands(ir: *const Ir, inst: Ir.Index, ops: *[2]Ir.Index) []const Index {
    const index = @intFromEnum(inst);
    const tag = ir.insts.items(.tag)[index];
    const payload = ir.insts.items(.payload)[index];

    switch (tag) {
        .constant,
        .arg,
        .builtin,
        .context_ptr,
        => return &.{},
        .attribute_ptr, .load => {
            ops[0] = payload.unary_ip.op;
            return ops[0..1];
        },
        .list_init => {
            const slice = ir.extraData(Ir.Inst.ExtraSlice, payload.extra);
            const elements: []const Ir.Index = @ptrCast(ir.extraSlice(slice));
            return elements;
        },
        .itof,
        .ftoi,
        .itob,
        .btoi,
        .any,
        .neg,
        .binv,
        .lnot,
        .ret,
        => {
            ops[0] = payload.unary;
            return ops[0..1];
        },
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
        .eq,
        .ne,
        .lt,
        .gt,
        .le,
        .ge,
        .store,
        .element_ptr,
        => {
            ops[0] = payload.binary.l;
            ops[1] = payload.binary.r;
            return ops[0..2];
        },
        .call => {
            // TODO: this should probably include the callee ptr
            const slice = ir.extraData(Ir.Inst.ExtraSlice, payload.unary_extra.extra);
            const args: []const Ir.Index = @ptrCast(ir.extraSlice(slice));
            return args;
        },
        .phi => {
            const phi = ir.extraData(Ir.Inst.Phi, payload.extra);
            ops[0] = phi.src1;
            ops[1] = phi.src2;
            return ops[0..2];
        },
        .jmp => return &.{},
        .br => {
            ops[0] = payload.unary_extra.op;
            return ops[0..1];
        },
    }
}

pub inline fn instTag(ir: *const Ir, inst: Index) Inst.Tag {
    const index = @intFromEnum(inst);
    return ir.insts.items(.tag)[index];
}

pub inline fn instPayload(ir: *const Ir, inst: Index) Inst.Payload {
    const index = @intFromEnum(inst);
    return ir.insts.items(.payload)[index];
}
