const std = @import("std");
const Ir = @import("Ir.zig");

const Allocator = std.mem.Allocator;
const Liveness = @This();

dead: []const u8,
special: std.AutoHashMapUnmanaged(Ir.Index, ExtraIndex),
extra: []const u32,

pub const ExtraIndex = enum(u32) { _ };

pub const IfElse = struct {
    dead_start: ExtraIndex,
    dead_end: ExtraIndex,
};

pub fn deadBits(liveness: *const Liveness, inst: Ir.Index) u4 {
    const i = @intFromEnum(inst);
    const elem = liveness.dead[i / 2];

    if (i % 2 == 0) {
        return @truncate(elem);
    } else {
        return @truncate(elem >> 4);
    }
}

pub fn extraData(liveness: *const Liveness, comptime T: type, index: ExtraIndex) T {
    var result: T = undefined;
    const fields = std.meta.fields(T);
    const base: u32 = @intFromEnum(index);
    inline for (fields, 0..) |field, i| {
        switch (field.type) {
            inline else => @field(result, field.name) = @enumFromInt(liveness.extra[base + i]),
        }
    }
    return result;
}

const Analysis = struct {
    gpa: Allocator,
    arena: Allocator,

    ir: *const Ir,
    dead: []u8,
    special: *std.AutoHashMapUnmanaged(Ir.Index, ExtraIndex),
    extra: *std.ArrayListUnmanaged(u32),
    scratch: *std.ArrayListUnmanaged(u32),

    live_set: std.AutoHashMapUnmanaged(Ir.Index, void),
    live_out: std.AutoHashMapUnmanaged(Ir.Index, void),

    pub fn init(
        gpa: Allocator,
        arena: Allocator,
        ir: *const Ir,
        dead: []u8,
        special: *std.AutoHashMapUnmanaged(Ir.Index, ExtraIndex),
        extra: *std.ArrayListUnmanaged(u32),
        scratch: *std.ArrayListUnmanaged(u32),
    ) Analysis {
        return .{
            .gpa = gpa,
            .arena = arena,
            .ir = ir,
            .dead = dead,
            .special = special,
            .extra = extra,
            .scratch = scratch,
            .live_set = .{},
            .live_out = .{},
        };
    }

    fn initChild(analysis: *Analysis) Analysis {
        return .{
            .gpa = analysis.gpa,
            .arena = analysis.arena,
            .ir = analysis.ir,
            .dead = analysis.dead,
            .special = analysis.special,
            .extra = analysis.extra,
            .scratch = analysis.scratch,
            .live_set = .{},
            .live_out = .{},
        };
    }

    inline fn seenBefore(analysis: *const Analysis, inst: Ir.Index) bool {
        return analysis.live_set.contains(inst); // or analysis.live_out.contains(inst);
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

    fn deadBits(analysis: *const Analysis, inst: Ir.Index) u4 {
        const i = @intFromEnum(inst);
        const elem = analysis.dead[i / 2];

        if (i % 2 == 0) {
            return @truncate(elem);
        } else {
            return @truncate(elem >> 4);
        }
    }

    fn addExtra(analysis: *Analysis, extra: anytype) Allocator.Error!ExtraIndex {
        const fields = std.meta.fields(@TypeOf(extra));
        try analysis.extra.ensureUnusedCapacity(analysis.gpa, fields.len);
        const len: u32 = @intCast(analysis.extra.items.len);
        inline for (fields) |field| {
            switch (field.type) {
                inline else => {
                    const num: u32 = @intFromEnum(@field(extra, field.name));
                    analysis.extra.appendAssumeCapacity(@bitCast(num));
                },
            }
        }
        return @enumFromInt(len);
    }

    fn analyze(analysis: *Analysis, block_extra: Ir.ExtraIndex) Allocator.Error!void {
        const ir = analysis.ir;
        const block = ir.extraData(Ir.Inst.ExtraSlice, block_extra);
        const insts = ir.extraSlice(block);

        var i: u32 = @intCast(insts.len);
        while (i > 0) {
            i -= 1;
            const inst: Ir.Index = @enumFromInt(insts[i]);
            try analysis.analyzeInst(inst);
        }
    }

    fn analyzeInst(analysis: *Analysis, inst: Ir.Index) !void {
        const ir = analysis.ir;
        const tag = ir.instTag(inst);
        switch (tag) {
            .constant => try analysis.constant(inst),
            .itof,
            .ftoi,
            .neg,
            .binv,
            .lnot,
            .ret,
            => try analysis.unaryOp(inst),
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
            => try analysis.binaryOp(inst),
            .if_else => try analysis.ifElse(inst),
            else => {},
            // // TODO: what to do here?
            // .loop => {
            //     const loop = ir.extraData(Ir.Inst.Loop, payload.op_extra.extra);
            //     bits |= try analysis.markLive(1, &.{payload.op_extra.op});
            //     try analysis.analyzeBlock(loop.condition);
            //     try analysis.analyzeBlock(loop.body);
            //     try analysis.analyzeBlock(loop.phi_block);
            // },
        }
    }

    fn constant(analysis: *Analysis, inst: Ir.Index) !void {
        var bits: u4 = 0;
        if (!analysis.live_set.remove(inst)) bits |= 0x8;
        analysis.setBits(inst, bits);
    }

    fn unaryOp(analysis: *Analysis, inst: Ir.Index) !void {
        const payload = analysis.ir.instPayload(inst);
        var bits: u4 = 0;

        if (!analysis.seenBefore(payload.unary)) {
            bits |= 0x1;
            try analysis.live_set.put(analysis.arena, payload.unary, {});
        }
        if (!analysis.live_set.remove(inst)) bits |= 0x8;

        analysis.setBits(inst, bits);
    }

    fn binaryOp(analysis: *Analysis, inst: Ir.Index) !void {
        const payload = analysis.ir.instPayload(inst);
        var bits: u4 = 0;

        if (!analysis.seenBefore(payload.binary.l)) {
            bits |= 0x1;
            try analysis.live_set.put(analysis.arena, payload.binary.l, {});
        }
        if (!analysis.seenBefore(payload.binary.r)) {
            bits |= 0x2;
            try analysis.live_set.put(analysis.arena, payload.binary.r, {});
        }
        if (!analysis.live_set.remove(inst)) bits |= 0x8;

        analysis.setBits(inst, bits);
    }

    fn ifElse(analysis: *Analysis, inst: Ir.Index) !void {
        const payload = analysis.ir.instPayload(inst);
        const if_else = analysis.ir.extraData(Ir.Inst.IfElse, payload.op_extra.extra);

        var true_analysis = Analysis.initChild(analysis);
        // try true_analysis.analyzeBranchPhis(.branch_if, if_else.phis);
        try true_analysis.analyzeBranchPhis(if_else.phis);
        try true_analysis.analyze(if_else.exec_true);

        var false_analysis = Analysis.initChild(analysis);
        // try false_analysis.analyzeBranchPhis(.branch_else, if_else.phis);
        try false_analysis.analyzeBranchPhis(if_else.phis);
        try false_analysis.analyze(if_else.exec_false);

        try analysis.transferIfElseDeadBits(
            inst,
            &true_analysis.live_set,
            &false_analysis.live_set,
        );

        var bits: u4 = 0;
        if (!analysis.seenBefore(payload.op_extra.op)) {
            bits |= 0x1;
            try analysis.live_set.put(analysis.arena, payload.op_extra.op, {});
        }
        analysis.setBits(inst, bits);
    }

    // const PhiContext = enum {
    //     branch_if,
    //     branch_else,
    // };
    //
    // fn phiOp(analysis: *Analysis, comptime context: PhiContext, inst: Ir.Index) !void {
    //     const payload = analysis.ir.instPayload(inst);
    //     var bits: u4 = 0;
    //
    //     switch (context) {
    //         .branch_if => if (!analysis.seenBefore(payload.binary.l)) {
    //             bits |= 0x1;
    //             try analysis.live_set.put(analysis.arena, payload.binary.l, {});
    //         },
    //         .branch_else => if (!analysis.seenBefore(payload.binary.r)) {
    //             bits |= 0x2;
    //             try analysis.live_set.put(analysis.arena, payload.binary.r, {});
    //         },
    //     }
    //
    //     analysis.setBits(inst, bits);
    // }

    fn analyzeBranchPhis(
        analysis: *Analysis,
        // comptime context: PhiContext,
        phis_extra: Ir.ExtraIndex,
    ) !void {
        const ir = analysis.ir;
        const extra_slice = ir.extraData(Ir.Inst.ExtraSlice, phis_extra);
        const phis = ir.extraSlice(extra_slice);
        for (phis) |phi| {
            try analysis.live_set.put(analysis.arena, @enumFromInt(phi), {});
            // try analysis.phiOp(context, @enumFromInt(phi));
            try analysis.binaryOp(@enumFromInt(phi));
        }
    }

    fn transferIfElseDeadBits(
        analysis: *Analysis,
        inst: Ir.Index,
        if_live_set: *std.AutoHashMapUnmanaged(Ir.Index, void),
        else_live_set: *std.AutoHashMapUnmanaged(Ir.Index, void),
    ) !void {
        // instructions that are still live once completing the if else statement
        // are declared outside, so must die in the if_else statement itself
        const payload = analysis.ir.instPayload(inst);
        const if_else = analysis.ir.extraData(Ir.Inst.IfElse, payload.op_extra.extra);

        // first add live insts to the if_else statement
        const scratch_top = analysis.scratch.items.len;
        defer analysis.scratch.shrinkRetainingCapacity(scratch_top);
        var it = if_live_set.keyIterator();
        while (it.next()) |live| {
            std.debug.print("transfer: %{}\n", .{live.*});
            try analysis.scratch.append(analysis.arena, @intFromEnum(live.*));
            try analysis.live_set.put(analysis.arena, live.*, {});
        }
        it = else_live_set.keyIterator();
        while (it.next()) |live| {
            std.debug.print("transfer: %{}\n", .{live.*});
            try analysis.scratch.append(analysis.arena, @intFromEnum(live.*));
            try analysis.live_set.put(analysis.arena, live.*, {});
        }

        const start: u32 = @intCast(analysis.extra.items.len);
        try analysis.extra.appendSlice(analysis.gpa, analysis.scratch.items[scratch_top..]);
        const end: u32 = @intCast(analysis.extra.items.len);
        const extra = try analysis.addExtra(IfElse{
            .dead_start = @enumFromInt(start),
            .dead_end = @enumFromInt(end),
        });
        try analysis.special.put(analysis.gpa, inst, extra);

        // now clear the set bits for these
        try analysis.clearBlock(if_else.exec_true, if_live_set);
        try analysis.clearBlock(if_else.exec_false, else_live_set);
    }

    fn clearBlock(
        analysis: *Analysis,
        block_extra: Ir.ExtraIndex,
        live_set: *std.AutoHashMapUnmanaged(Ir.Index, void),
    ) !void {
        const ir = analysis.ir;
        const block = ir.extraData(Ir.Inst.ExtraSlice, block_extra);
        const insts = ir.extraSlice(block);

        var i: u32 = @intCast(insts.len);
        while (i > 0) {
            i -= 1;
            const inst: Ir.Index = @enumFromInt(insts[i]);
            const tag = ir.instTag(inst);
            switch (tag) {
                .constant => try analysis.constant(inst),
                .itof,
                .ftoi,
                .neg,
                .binv,
                .lnot,
                .ret,
                => try analysis.clearUnary(inst, live_set),
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
                => try analysis.clearBinary(inst, live_set),
                else => unreachable, // TODO: implement
            }
        }
    }

    fn clearUnary(
        analysis: *Analysis,
        inst: Ir.Index,
        live_set: *std.AutoHashMapUnmanaged(Ir.Index, void),
    ) !void {
        const payload = analysis.ir.instPayload(inst);
        var bits = analysis.deadBits(inst);

        if ((bits & 0x1 != 0) and live_set.contains(payload.unary)) bits &= ~@as(u4, 0x1);
        analysis.setBits(inst, bits);
    }

    fn clearBinary(
        analysis: *Analysis,
        inst: Ir.Index,
        live_set: *std.AutoHashMapUnmanaged(Ir.Index, void),
    ) !void {
        const payload = analysis.ir.instPayload(inst);
        var bits = analysis.deadBits(inst);

        if ((bits & 0x1 != 0) and live_set.contains(payload.binary.l)) bits &= ~@as(u4, 0x1);
        if ((bits & 0x2 != 0) and live_set.contains(payload.binary.r)) bits &= ~@as(u4, 0x2);
        analysis.setBits(inst, bits);
    }
};

pub fn analyze(gpa: Allocator, ir: *const Ir) !Liveness {
    // var arena_allocator = std.heap.ArenaAllocator.init(gpa);
    // defer arena_allocator.deinit();
    // const arena = arena_allocator.allocator();

    const dead = try gpa.alloc(u8, ir.insts.len);
    errdefer gpa.free(dead);
    @memset(dead, 0);
    const special: std.AutoHashMapUnmanaged(Ir.Index, ExtraIndex) = .{};
    var extra: std.ArrayListUnmanaged(u32) = .{};
    // var scratch: std.ArrayListUnmanaged(u32) = .{};

    // var analysis = Analysis.init(gpa, arena, ir, dead, &special, &extra, &scratch);
    // try analysis.analyze(ir.block);

    return .{
        .dead = dead,
        .special = special,
        .extra = try extra.toOwnedSlice(gpa),
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
