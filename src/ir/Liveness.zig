const std = @import("std");
const Ir = @import("Ir.zig");

const Allocator = std.mem.Allocator;
const Liveness = @This();

dead: []const u8,
special: std.AutoHashMapUnmanaged(Ir.Index, ExtraIndex),
extra: []const u32,

pub const ExtraIndex = enum(u32) { _ };
pub const ExtraSlice = struct {
    start: ExtraIndex,
    end: ExtraIndex,
};

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

pub fn extraSlice(liveness: *const Liveness, slice: ExtraSlice) []const u32 {
    const start: u32 = @intFromEnum(slice.start);
    const end: u32 = @intFromEnum(slice.end);
    return liveness.extra[start..end];
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

    pub fn addExtra(analysis: *Analysis, extra: anytype) !ExtraIndex {
        const len: u32 = @intCast(analysis.extra.items.len);
        const fields = std.meta.fields(@TypeOf(extra));
        try analysis.extra.ensureUnusedCapacity(analysis.gpa, fields.len);
        inline for (fields) |field| {
            switch (field.type) {
                inline else => {
                    const num = @intFromEnum(@field(extra, field.name));
                    analysis.extra.appendAssumeCapacity(num);
                },
            }
        }
        return @enumFromInt(len);
    }

    pub fn addSlice(analysis: *Analysis, slice: []const u32) !ExtraIndex {
        const start: u32 = @intCast(analysis.extra.items.len);
        try analysis.extra.appendSlice(analysis.gpa, slice);
        const end: u32 = @intCast(analysis.extra.items.len);

        return analysis.addExtra(ExtraSlice{
            .start = @enumFromInt(start),
            .end = @enumFromInt(end),
        });
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

    fn compareLiveSets(a: *const LiveSet, b: *const LiveSet) bool {
        const a_count = a.count();
        const b_count = b.count();
        if (a_count != b_count) return false;

        var it = a.keyIterator();
        while (it.next()) |key_ptr| if (!b.contains(key_ptr.*)) return false;
        return true;
    }

    fn analyzeBlock(analysis: *Analysis, block: Ir.BlockIndex) !bool {
        const ir = analysis.ir;
        const n = @intFromEnum(block);
        const insts: []const Ir.Index = @ptrCast(ir.extraSlice(ir.blocks[n].insts));
        // var live_in: LiveSet = .{};
        // var live_out: LiveSet = .{};
        const live_in = &analysis.live_in[n];
        const live_out = &analysis.live_out[n];
        var dirty = false;
        var it: LiveSet.KeyIterator = undefined;

        // keep a copy of the live in set for later on
        const scratch_top = analysis.scratch.items.len;
        defer analysis.scratch.shrinkRetainingCapacity(scratch_top);

        var def: LiveSet = .{};
        try def.ensureTotalCapacity(analysis.arena, @intCast(insts.len));

        // live_in[block] := use[block] U (live_out[block] - def[block])
        for (insts) |inst| {
            var ops: [2]Ir.Index = undefined;
            for (ir.operands(inst, &ops)) |operand| {
                if (def.contains(operand)) continue;
                const gop = try live_in.getOrPut(analysis.arena, operand);
                dirty = dirty or !gop.found_existing;
            }
            def.putAssumeCapacity(inst, {});
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
                if (analysis.filterPhi(block, successor, key.*)) continue;
                const gop = try live_out.getOrPut(analysis.arena, key.*);
                dirty = dirty or !gop.found_existing;
            }
        }

        // dirty = !compareLiveSets(&analysis.live_in[n], &live_in);
        // dirty = dirty or !compareLiveSets(&analysis.live_out[n], &live_out);
        // analysis.live_in[n] = live_in;
        // analysis.live_out[n] = live_out;
        return dirty;
    }

    fn filterPhi(analysis: *const Analysis, block: Ir.BlockIndex, successor: Ir.BlockIndex, key: Ir.Index) bool {
        const ir = analysis.ir;

        const n = @intFromEnum(successor);
        const insts: []const Ir.Index = @ptrCast(ir.extraSlice(ir.blocks[n].insts));
        for (insts) |inst| {
            if (ir.instTag(inst) != .phi) break;
            const phi_data = ir.extraData(Ir.Inst.Phi, ir.instPayload(inst).extra);
            if (phi_data.src1 == key and phi_data.block1 != block) return true;
            if (phi_data.src2 == key and phi_data.block2 != block) return true;
        }

        return false;
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
            .ld_global => try analysis.ldGlobal(live_out, inst),
            .st_global => try analysis.stGlobal(live_out, inst),
            .arg => try analysis.argInst(live_out, inst),
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
            .eq,
            .ne,
            .lt,
            .gt,
            .le,
            .ge,
            => try analysis.binaryOp(live_out, inst),
            .call => try analysis.call(live_out, inst),
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

    fn ldGlobal(analysis: *Analysis, live_out: *LiveSet, inst: Ir.Index) !void {
        var bits: u4 = 0;
        if (!live_out.remove(inst)) bits |= 0x8;
        analysis.setBits(inst, bits);
    }

    fn stGlobal(analysis: *Analysis, live_out: *LiveSet, inst: Ir.Index) !void {
        const payload = analysis.ir.instPayload(inst);
        var bits: u4 = 0;

        if (!live_out.contains(payload.unary_ip.op)) {
            bits |= 0x1;
            try live_out.put(analysis.arena, payload.unary_ip.op, {});
        }

        analysis.setBits(inst, bits);
    }

    fn argInst(analysis: *Analysis, live_out: *LiveSet, inst: Ir.Index) !void {
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
        if (!live_out.contains(inst)) bits |= 0x8;

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
        if (!live_out.contains(inst)) bits |= 0x8;

        analysis.setBits(inst, bits);
    }

    fn call(analysis: *Analysis, live_out: *LiveSet, inst: Ir.Index) !void {
        const ir = analysis.ir;
        const payload = ir.instPayload(inst).unary_extra;
        const slice = ir.extraData(Ir.Inst.ExtraSlice, payload.extra);
        const args: []const Ir.Index = @ptrCast(ir.extraSlice(slice));
        var bits: u4 = 0;

        if (!live_out.contains(payload.op)) {
            bits |= 0x1;
            try live_out.put(analysis.arena, payload.op, {});
        }

        const scratch_top = analysis.scratch.items.len;
        defer analysis.scratch.shrinkRetainingCapacity(scratch_top);
        try analysis.scratch.ensureUnusedCapacity(analysis.arena, args.len);
        try live_out.ensureUnusedCapacity(analysis.arena, @intCast(args.len));
        for (args) |arg| {
            if (!live_out.contains(arg)) {
                analysis.scratch.appendAssumeCapacity(@intFromBool(true));
                live_out.putAssumeCapacity(arg, {});
            } else {
                analysis.scratch.appendAssumeCapacity(@intFromBool(false));
            }
        }

        if (!live_out.contains(inst)) bits |= 0x8;
        analysis.setBits(inst, bits);
        const extra = try analysis.addSlice(analysis.scratch.items[scratch_top..]);
        try analysis.special.put(analysis.gpa, inst, extra);
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
        if (!live_out.contains(inst)) bits |= 0x8;

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
        var dirty = false;
        var i = ir.blocks.len;
        while (i > 0) {
            i -= 1;
            const block_dirty = try analysis.analyzeBlock(@enumFromInt(i));
            dirty = dirty or block_dirty;
        }

        if (!dirty) break;
    }

    var i = ir.blocks.len;
    while (i > 0) {
        i -= 1;
        try analysis.markDeadBits(@enumFromInt(i));
    }

    return .{
        .dead = dead,
        .special = special,
        .extra = try extra.toOwnedSlice(gpa),
    };
}
