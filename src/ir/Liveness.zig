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

    live_in: []LiveSet,
    live_out: []LiveSet,

    const LiveSet = std.AutoHashMapUnmanaged(Ir.Index, void);

    pub fn init(
        gpa: Allocator,
        arena: Allocator,
        ir: *const Ir,
        dead: []u8,
        special: *std.AutoHashMapUnmanaged(Ir.Index, ExtraIndex),
        extra: *std.ArrayListUnmanaged(u32),
        scratch: *std.ArrayListUnmanaged(u32),
    ) !Analysis {
        const live_in = try arena.alloc(LiveSet, ir.blocks.len);
        const live_out = try arena.alloc(LiveSet, ir.blocks.len);
        @memset(live_in, .{});
        @memset(live_out, .{});

        return .{
            .gpa = gpa,
            .arena = arena,
            .ir = ir,
            .dead = dead,
            .special = special,
            .extra = extra,
            .scratch = scratch,
            .live_in = live_in,
            .live_out = live_out,
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

    fn analyzeBlock(analysis: *Analysis, block: Ir.BlockIndex) !bool {
        const ir = analysis.ir;
        const n = @intFromEnum(block);
        const insts: []const Ir.Index = @ptrCast(ir.extraSlice(ir.blocks[n].insts));
        const live_in = &analysis.live_in[n];
        const live_out = &analysis.live_out[n];
        var dirty = false;
        var it: LiveSet.KeyIterator = undefined;

        // keep a copy of the live in set for later on
        const scratch_top = analysis.scratch.items.len;
        defer analysis.scratch.shrinkRetainingCapacity(scratch_top);

        var def: LiveSet = .{};
        try def.ensureTotalCapacity(analysis.arena, @intCast(insts.len));
        for (insts) |inst| def.putAssumeCapacity(inst, {});

        // live_in[block] := use[block] U (live_out[block] - def[block])
        for (insts) |inst| {
            var ops: [2]Ir.Index = undefined;
            for (ir.operands(inst, &ops)) |operand| {
                if (def.contains(operand)) continue;
                const gop = try live_in.getOrPut(analysis.arena, operand);
                dirty = dirty or !gop.found_existing;
            }
        }
        it = live_out.keyIterator();
        while (it.next()) |key| {
            if (def.contains(key.*)) continue;
            const gop = try live_in.getOrPut(analysis.arena, key.*);
            dirty = dirty or !gop.found_existing;
        }

        // live_out[block] = U {live_in[s] foreach s in succ[block]}
        const terminator = insts[insts.len - 1];
        const successors: []const Ir.BlockIndex = switch (ir.instTag(terminator)) {
            .ret => &.{},
            .jmp => &.{ir.instPayload(terminator).block},
            .br => blocks: {
                const branch = ir.extraData(Ir.Inst.Branch, ir.instPayload(terminator).unary_extra.extra);
                break :blocks &.{ branch.exec_if, branch.exec_else };
            },
            else => unreachable,
        };
        for (successors) |successor| {
            it = analysis.live_in[@intFromEnum(successor)].keyIterator();
            while (it.next()) |key| {
                const gop = try live_out.getOrPut(analysis.arena, key.*);
                dirty = dirty or !gop.found_existing;
            }
        }

        return dirty;
    }

    fn markDeadBits(analysis: *Analysis, block: Ir.BlockIndex) Allocator.Error!void {
        const ir = analysis.ir;
        const n = @intFromEnum(block);
        const insts = ir.extraSlice(ir.blocks[n].insts);
        const live_out = &analysis.live_out[n];

        var i: u32 = @intCast(insts.len);
        while (i > 0) {
            i -= 1;
            const inst: Ir.Index = @enumFromInt(insts[i]);
            try analysis.markInst(live_out, inst);
        }
    }

    fn markInst(analysis: *Analysis, live_out: *LiveSet, inst: Ir.Index) !void {
        const ir = analysis.ir;
        const tag = ir.instTag(inst);
        switch (tag) {
            .constant => try analysis.constant(live_out, inst),
            .itof,
            .ftoi,
            .neg,
            .binv,
            .lnot,
            => try analysis.unaryOp(live_out, inst),
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
            => try analysis.binaryOp(live_out, inst),
            .phi => try analysis.phi(live_out, inst),
            .jmp => {},
            .br => try analysis.br(live_out, inst),
            .ret => try analysis.ret(live_out, inst),
        }
    }

    fn constant(analysis: *Analysis, live_out: *LiveSet, inst: Ir.Index) !void {
        var bits: u4 = 0;
        if (!live_out.remove(inst)) bits |= 0x8;
        analysis.setBits(inst, bits);
    }

    fn unaryOp(analysis: *Analysis, live_out: *LiveSet, inst: Ir.Index) !void {
        const payload = analysis.ir.instPayload(inst);
        var bits: u4 = 0;

        if (!live_out.contains(payload.unary)) {
            bits |= 0x1;
            try live_out.put(analysis.arena, payload.unary, {});
        }
        if (!live_out.remove(inst)) bits |= 0x8;

        analysis.setBits(inst, bits);
    }

    fn binaryOp(analysis: *Analysis, live_out: *LiveSet, inst: Ir.Index) !void {
        const payload = analysis.ir.instPayload(inst);
        var bits: u4 = 0;

        if (!live_out.contains(payload.binary.l)) {
            bits |= 0x1;
            try live_out.put(analysis.arena, payload.binary.l, {});
        }
        if (!live_out.contains(payload.binary.r)) {
            bits |= 0x2;
            try live_out.put(analysis.arena, payload.binary.r, {});
        }
        if (!live_out.remove(inst)) bits |= 0x8;

        analysis.setBits(inst, bits);
    }

    fn phi(analysis: *Analysis, live_out: *LiveSet, inst: Ir.Index) !void {
        const payload = analysis.ir.instPayload(inst);
        const phi_data = analysis.ir.extraData(Ir.Inst.Phi, payload.extra);
        var bits: u4 = 0;

        if (!live_out.contains(phi_data.src1)) {
            bits |= 0x1;
            try live_out.put(analysis.arena, phi_data.src1, {});
        }
        if (!live_out.contains(phi_data.src2)) {
            bits |= 0x2;
            try live_out.put(analysis.arena, phi_data.src2, {});
        }
        if (!live_out.remove(inst)) bits |= 0x8;

        analysis.setBits(inst, bits);
    }

    fn br(analysis: *Analysis, live_out: *LiveSet, inst: Ir.Index) !void {
        const payload = analysis.ir.instPayload(inst);
        var bits: u4 = 0;

        if (!live_out.contains(payload.unary_extra.op)) {
            bits |= 0x1;
            try live_out.put(analysis.arena, payload.unary_extra.op, {});
        }

        analysis.setBits(inst, bits);
    }

    fn ret(analysis: *Analysis, live_out: *LiveSet, inst: Ir.Index) !void {
        const payload = analysis.ir.instPayload(inst);
        var bits: u4 = 0;

        if (!live_out.contains(payload.unary)) {
            bits |= 0x1;
            try live_out.put(analysis.arena, payload.unary, {});
        }

        analysis.setBits(inst, bits);
    }
};

pub fn analyze(gpa: Allocator, ir: *const Ir) !Liveness {
    var arena_allocator = std.heap.ArenaAllocator.init(gpa);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    const dead = try gpa.alloc(u8, ir.insts.len);
    errdefer gpa.free(dead);
    @memset(dead, 0);
    var special: std.AutoHashMapUnmanaged(Ir.Index, ExtraIndex) = .{};
    var extra: std.ArrayListUnmanaged(u32) = .{};
    var scratch: std.ArrayListUnmanaged(u32) = .{};

    var analysis = try Analysis.init(gpa, arena, ir, dead, &special, &extra, &scratch);
    while (true) {
        // for (0..10) |_| {
        var dirty = false;
        var i = ir.blocks.len;
        while (i > 0) {
            i -= 1;
            const block_dirty = try analysis.analyzeBlock(@enumFromInt(i));
            dirty = dirty or block_dirty;

            // std.debug.print("live_in for block{}\n", .{i});
            // var it = analysis.live_in[i].keyIterator();
            // while (it.next()) |key| std.debug.print("%{}\n", .{key.*});
            // std.debug.print("live_out for block{}\n", .{i});
            // it = analysis.live_out[i].keyIterator();
            // while (it.next()) |key| std.debug.print("%{}\n", .{key.*});
            // std.debug.print("\n", .{});
        }

        // break;
        if (!dirty) break;
    }

    var i = ir.blocks.len;
    while (i > 0) {
        i -= 1;
        std.debug.print("live_in for block{}\n", .{i});
        var it = analysis.live_in[i].keyIterator();
        while (it.next()) |key| std.debug.print("%{}\n", .{key.*});
        std.debug.print("live_out for block{}\n", .{i});
        it = analysis.live_out[i].keyIterator();
        while (it.next()) |key| std.debug.print("%{}\n", .{key.*});
        try analysis.markDeadBits(@enumFromInt(i));
    }

    return .{
        .dead = dead,
        .special = special,
        .extra = try extra.toOwnedSlice(gpa),
    };
}
