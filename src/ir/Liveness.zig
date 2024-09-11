const std = @import("std");
const Ir = @import("Ir.zig");

const Allocator = std.mem.Allocator;
const Liveness = @This();

dead: []const u8,
special: std.AutoHashMapUnmanaged(Ir.Index, ExtraIndex),
extra: []const u32,

pub const ExtraIndex = enum(u32) { _ };

pub fn deadBits(liveness: *const Liveness, inst: Ir.Index) u4 {
    const i = @intFromEnum(inst);
    const elem = liveness.dead[i / 2];

    if (i % 2 == 0) {
        return @truncate(elem);
    } else {
        return @truncate(elem >> 4);
    }
}

const Analysis = struct {
    gpa: Allocator,
    arena: Allocator,
    ir: *const Ir,
    dead: []u8,
    live_set: std.AutoHashMapUnmanaged(Ir.Index, void),

    pub fn init(gpa: Allocator, arena: Allocator, ir: *const Ir) !Analysis {
        const dead = try gpa.alloc(u8, ir.insts.len);
        errdefer gpa.free(dead);

        @memset(dead, 0);
        return .{
            .gpa = gpa,
            .arena = arena,
            .ir = ir,
            .dead = dead,
            .live_set = .{},
        };
    }

    pub fn deinit(self: *Analysis) void {
        self.gpa.free(self.dead);
    }

    fn analyzeBlock(analysis: *Analysis, index: Ir.ExtraIndex) Allocator.Error!void {
        const ir = analysis.ir;

        const block = ir.extraData(Ir.Inst.ExtraSlice, index);
        const insts = ir.extraSlice(block);
        var i: u32 = @intCast(insts.len);
        while (i > 0) {
            i -= 1;
            const inst: Ir.Index = @enumFromInt(insts[i]);
            try analysis.analyzeInst(inst);
        }
    }

    fn markLive(
        analysis: *Analysis,
        comptime num_operands: u32,
        operands: *const [num_operands]Ir.Index,
    ) !u4 {
        std.debug.assert(num_operands <= 2);
        var bits: u4 = 0;
        inline for (operands, 0..) |operand, i| {
            if (!analysis.live_set.contains(operand)) {
                bits |= 1 << i;
                try analysis.live_set.put(analysis.arena, operand, {});
            }
        }

        return bits;
    }

    fn analyzeInst(analysis: *Analysis, inst: Ir.Index) !void {
        const ir = analysis.ir;
        var bits: u4 = 0x0;

        // instruction is unused, since it is defined here
        // but we haven't seen it used below (moving backwards)
        if (!analysis.live_set.contains(inst)) {
            bits |= 0x8;
        } else {
            _ = analysis.live_set.remove(inst);
        }

        // based on the number of operands the instruction has,
        // check each of them against the live set, and if they
        // don't exist, add them and mark the "dead" bit
        const tag = ir.instTag(inst);
        const payload = ir.instPayload(inst);
        switch (tag) {
            // zero operands
            .constant => {},
            // unary (one operand)
            .itof,
            .ftoi,
            .neg,
            .binv,
            .lnot,
            .ret,
            => bits |= try analysis.markLive(1, &.{payload.unary}),
            // binary (two operands)
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
            .phi_if_else,
            .phi_entry_if,
            .phi_entry_else,
            .phi_entry_body_body,
            .phi_entry_body_exit,
            => bits |= try analysis.markLive(2, &.{ payload.binary.l, payload.binary.r }),
            .if_else => {
                // check if the condition dies
                bits |= try analysis.markLive(1, &.{payload.op_extra.op});
                const if_else = ir.extraData(Ir.Inst.IfElse, payload.op_extra.extra);
                try analysis.analyzeBlock(if_else.exec_true);
                try analysis.analyzeBlock(if_else.exec_false);
            },
            // TODO: what to do here?
            .loop => {
                const loop = ir.extraData(Ir.Inst.Loop, payload.op_extra.extra);
                bits |= try analysis.markLive(1, &.{payload.op_extra.op});
                try analysis.analyzeBlock(loop.condition);
                try analysis.analyzeBlock(loop.body);
                try analysis.analyzeBlock(loop.phi_block);
            },
        }

        const elem: u8 = if (@intFromEnum(inst) % 2 == 0) bits else @as(u8, bits) << 4;
        analysis.dead[@intFromEnum(inst) / 2] |= elem;
    }

    inline fn setBits(analysis: *Analysis, inst: Ir.Index, bits: u4) void {
        const i = @intFromEnum(inst);
        const existing = analysis.dead[i / 2];
        const extended: u8 = bits;
        if (i % 2 == 0) {
            analysis.dead[i / 2] = (existing & 0xf0) | extended;
        } else {
            analysis.dead[i / 2] = (existing & 0x0f) | (extended << 4);
        }
    }

    fn unaryOp(analysis: *Analysis, inst: Ir.Index) !void {
        const payload = analysis.ir.instPayload(inst);
        var bits: u4 = 0;

        if (!analysis.live_set.contains(payload.unary)) {
            bits |= 0x1;
            analysis.live_set.put(analysis.arena, payload.unary);
        }
        if (!analysis.live_set.remove(inst)) bits |= 0x8;

        analysis.setBits(inst, bits);
    }

    fn binaryOp(analysis: *Analysis, inst: Ir.Index) !void {
        const payload = analysis.ir.instPayload(inst);
        var bits: u4 = 0;

        if (!analysis.live_set.contains(payload.binary.l)) {
            bits |= 0x1;
            analysis.live_set.put(analysis.arena, payload.binary.l);
        }
        if (!analysis.live_set.contains(payload.binary.r)) {
            bits |= 0x2;
            analysis.live_set.put(analysis.arena, payload.binary.r);
        }
        if (!analysis.live_set.remove(inst)) bits |= 0x8;

        analysis.setBits(inst, bits);
    }

    fn ifElse(analysis: *Analysis, inst: Ir.Index) !void {
        const payload = analysis.ir.instPayload(inst);
    }
};

pub fn analyze(gpa: Allocator, temp_ir: *const Ir) !Liveness {
    var arena_allocator = std.heap.ArenaAllocator.init(gpa);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    var analysis = try Analysis.init(gpa, arena, temp_ir);
    errdefer analysis.deinit();

    try analysis.analyzeBlock(temp_ir.block);

    return .{
        .dead = analysis.dead,
        .special = .{},
        .extra = &.{},
    };
}

// pub fn addExtra(ig: *IrGen, extra: anytype) !Ir.ExtraIndex {
//     const len: u32 = @intCast(ig.extra.items.len);
//     const fields = std.meta.fields(@TypeOf(extra));
//     try ig.extra.ensureUnusedCapacity(ig.gpa, fields.len);
//     inline for (fields) |field| {
//         switch (field.type) {
//             inline else => {
//                 const num: u32 = @intFromEnum(@field(extra, field.name));
//                 ig.extra.appendAssumeCapacity(@bitCast(num));
//             },
//         }
//     }
//     return @enumFromInt(len);
// }
//
