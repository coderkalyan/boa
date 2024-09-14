const std = @import("std");
const Ir = @import("../ir/Ir.zig");

const Allocator = std.mem.Allocator;
const BlockIndex = Ir.BlockIndex;
const PrePass = @This();

order: []const BlockIndex,
phis: []const std.ArrayListUnmanaged(PhiMarker),
ranges: []const Ir.Index,

pub const PhiMarker = struct {
    operand: Ir.Index,
    phi: Ir.Index,
    dest_block: BlockIndex,
};

const Context = struct {
    arena: Allocator,
    ir: *const Ir,
    order: []BlockIndex,
    visited: []bool,
    phis: []std.ArrayListUnmanaged(PhiMarker),
    ranges: []Ir.Index,

    fn recordPostOrderDfs(self: *Context, current: BlockIndex, i: usize) !usize {
        const ir = self.ir;
        const c = @intFromEnum(current);
        if (self.visited[c]) return i;
        self.visited[c] = true;

        const insts: []const Ir.Index = @ptrCast(ir.extraSlice(ir.blocks[c].insts));
        // dfs
        const n = n: {
            const inst = insts[insts.len - 1];
            const tag = ir.instTag(inst);
            const payload = ir.instPayload(inst);
            break :n switch (tag) {
                .jmp => try self.recordPostOrderDfs(payload.block, i),
                .br => {
                    const extra = ir.instPayload(inst).unary_extra.extra;
                    const branch = ir.extraData(Ir.Inst.Branch, extra);
                    const temp = try self.recordPostOrderDfs(branch.exec_if, i);
                    break :n try self.recordPostOrderDfs(branch.exec_else, temp);
                },
                .ret => i,
                else => unreachable,
            };
        };

        // record postorder
        self.order[n - 1] = current;

        // mark live ranges and phi edges
        for (insts) |inst| {
            self.updateRanges(inst);
            if (ir.instTag(inst) == .phi) {
                const extra = ir.instPayload(inst).extra;
                const phi = ir.extraData(Ir.Inst.Phi, extra);
                const block1 = @intFromEnum(phi.block1);
                const block2 = @intFromEnum(phi.block2);
                try self.phis[block1].append(self.arena, .{ .operand = phi.src1, .phi = inst, .dest_block = current });
                try self.phis[block2].append(self.arena, .{ .operand = phi.src2, .phi = inst, .dest_block = current });
                continue;
            }
        }

        return n - 1;
    }

    inline fn markRangeEnd(self: *Context, inst: Ir.Index, end: Ir.Index) void {
        const index = @intFromEnum(inst);
        const cur = @intFromEnum(self.ranges[index]);
        self.ranges[index] = @enumFromInt(@max(cur, @intFromEnum(end)));
    }

    fn updateRanges(self: *Context, inst: Ir.Index) void {
        const ir = self.ir;
        const index = @intFromEnum(inst);
        const tag = ir.insts.items(.tag)[index];
        const payload = ir.insts.items(.payload)[index];
        const dead_bits = ir.liveness.deadBits(inst);

        switch (tag) {
            .constant => if (dead_bits & 0x8 != 0) self.markRangeEnd(inst, inst),
            .itof,
            .ftoi,
            .neg,
            .binv,
            .lnot,
            .ret,
            => {
                if (dead_bits & 0x1 != 0) self.markRangeEnd(payload.unary, inst);
                if (dead_bits & 0x8 != 0) self.markRangeEnd(inst, inst);
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
            => {
                if (dead_bits & 0x1 != 0) self.markRangeEnd(payload.binary.l, inst);
                if (dead_bits & 0x2 != 0) self.markRangeEnd(payload.binary.r, inst);
                if (dead_bits & 0x8 != 0) self.markRangeEnd(inst, inst);
            },
            .phi => {
                const phi = ir.extraData(Ir.Inst.Phi, payload.extra);
                if (dead_bits & 0x1 != 0) self.markRangeEnd(phi.src1, inst);
                if (dead_bits & 0x2 != 0) self.markRangeEnd(phi.src2, inst);
                if (dead_bits & 0x8 != 0) self.markRangeEnd(inst, inst);
            },
            .jmp => {},
            .br => {
                if (dead_bits & 0x1 != 0) self.markRangeEnd(payload.unary_extra.op, inst);
            },
        }
    }
};

pub fn analyze(arena: Allocator, ir: *const Ir, entry: BlockIndex) !PrePass {
    const order = try arena.alloc(BlockIndex, ir.blocks.len);
    const visited = try arena.alloc(bool, ir.blocks.len);
    const phis = try arena.alloc(std.ArrayListUnmanaged(PhiMarker), ir.blocks.len);
    @memset(phis, .{});
    const ranges = try arena.alloc(Ir.Index, ir.insts.len);
    @memset(ranges, @enumFromInt(0));

    var context: Context = .{
        .arena = arena,
        .ir = ir,
        .order = order,
        .visited = visited,
        .phis = phis,
        .ranges = ranges,
    };

    @memset(context.visited, false);
    _ = try context.recordPostOrderDfs(entry, ir.blocks.len);

    return .{
        .order = order,
        .phis = phis,
        .ranges = ranges,
    };
}
