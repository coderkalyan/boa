const std = @import("std");
const Ir = @import("Ir.zig");
const IrGen = @import("IrGen.zig");
const InternPool = @import("../InternPool.zig");

const BlockBuilder = @This();
const Index = Ir.Index;
const ExtraIndex = Ir.ExtraIndex;
const BlockIndex = Ir.BlockIndex;

ig: *IrGen,
index: BlockIndex,
insts: std.ArrayListUnmanaged(Index),
slot: u32,

pub fn init(ig: *IrGen, index: BlockIndex, slot: u32) BlockBuilder {
    return .{
        .ig = ig,
        .index = index,
        .insts = .{},
        .slot = slot,
    };
}

pub fn seal(bb: *BlockBuilder) !BlockIndex {
    const ig = bb.ig;
    const index = bb.index;
    const i = @intFromEnum(bb.index);

    // if the block doesn't end in a terminator, add an implicit
    // return none
    if (bb.insts.items.len == 0) {
        _ = try bb.unary(.ret, try bb.constant(.none));
    } else {
        switch (ig.getTempIr().instTag(bb.insts.items[bb.insts.items.len - 1])) {
            .jmp, .br, .ret => {},
            else => _ = try bb.unary(.ret, try bb.constant(.none)),
        }
    }

    const insts = try ig.addSlice(@ptrCast(bb.insts.items));
    ig.blocks.items[i] = .{ .insts = insts };
    try ig.free_builders.append(ig.arena, bb.slot);
    return index;
}

pub fn add(bb: *BlockBuilder, inst: Ir.Inst) !Index {
    const ig = bb.ig;
    try ig.insts.ensureUnusedCapacity(ig.gpa, 1);
    try bb.insts.ensureUnusedCapacity(ig.gpa, 1);

    const len: u32 = @intCast(ig.insts.len);
    const index: Index = @enumFromInt(len);
    ig.insts.appendAssumeCapacity(inst);
    bb.insts.appendAssumeCapacity(index);
    return index;
}

pub fn constant(bb: *BlockBuilder, ip: InternPool.Index) !Index {
    return bb.add(.{ .tag = .constant, .payload = .{ .ip = ip } });
}

pub fn arg(bb: *BlockBuilder, position: u32, ty: InternPool.Index) !Index {
    return bb.add(.{
        .tag = .arg,
        .payload = .{
            .arg = .{
                .position = position,
                .ty = ty,
            },
        },
    });
}

pub fn builtin(bb: *BlockBuilder, ip: InternPool.Index) !Index {
    return bb.add(.{ .tag = .builtin, .payload = .{ .ip = ip } });
}

pub fn unary(bb: *BlockBuilder, tag: Ir.Inst.Tag, operand: Index) !Index {
    return bb.add(.{ .tag = tag, .payload = .{ .unary = operand } });
}

pub fn binary(bb: *BlockBuilder, tag: Ir.Inst.Tag, l: Index, r: Index) !Index {
    return bb.add(.{ .tag = tag, .payload = .{ .binary = .{ .l = l, .r = r } } });
}

pub fn jmp(bb: *BlockBuilder, block: BlockIndex) !Index {
    return bb.add(.{ .tag = .jmp, .payload = .{ .block = block } });
}

pub fn br(
    bb: *BlockBuilder,
    condition: Index,
    exec_if: BlockIndex,
    exec_else: BlockIndex,
) !Index {
    const extra_ptr = try bb.ig.addExtra(Ir.Inst.Branch{
        .exec_if = exec_if,
        .exec_else = exec_else,
    });
    return bb.add(.{
        .tag = .br,
        .payload = .{
            .unary_extra = .{
                .op = condition,
                .extra = extra_ptr,
            },
        },
    });
}

pub fn phi(
    bb: *BlockBuilder,
    ty: InternPool.Index,
    src1: Index,
    block1: BlockIndex,
    src2: Index,
    block2: BlockIndex,
) !Index {
    const extra_ptr = try bb.ig.addExtra(Ir.Inst.Phi{
        .ty = ty,
        .src1 = src1,
        .block1 = block1,
        .src2 = src2,
        .block2 = block2,
    });
    return bb.add(.{ .tag = .phi, .payload = .{ .extra = extra_ptr } });
}

pub fn ldGlobal(bb: *BlockBuilder, ident: InternPool.Index) !Index {
    return bb.add(.{ .tag = .ld_global, .payload = .{ .ip = ident } });
}

pub fn stGlobal(bb: *BlockBuilder, val: Index, ident: InternPool.Index) !Index {
    return bb.add(.{
        .tag = .st_global,
        .payload = .{
            .unary_ip = .{
                .op = val,
                .ip = ident,
            },
        },
    });
}

pub fn call(bb: *BlockBuilder, ptr: Index, args: []const Index) !Index {
    const slice = try bb.ig.addSlice(@ptrCast(args));
    const extra = try bb.ig.addExtra(slice);
    return bb.add(.{
        .tag = .call,
        .payload = .{
            .unary_extra = .{ .op = ptr, .extra = extra },
        },
    });
}
