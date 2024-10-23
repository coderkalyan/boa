const std = @import("std");
const Ir = @import("../ir/Ir.zig");
const Liveness = @import("../ir/Liveness.zig");

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
                    const temp = try self.recordPostOrderDfs(branch.exec_else, i);
                    break :n try self.recordPostOrderDfs(branch.exec_if, temp);
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
            .constant,
            .arg,
            .context_ptr,
            => if (dead_bits & 0x8 != 0) self.markRangeEnd(inst, inst),
            .builtin => {}, // nothing to do here
            .attribute_ptr, .load => {
                if (dead_bits & 0x1 != 0) self.markRangeEnd(payload.unary_ip.op, inst);
                if (dead_bits & 0x8 != 0) self.markRangeEnd(inst, inst);
            },
            .list_init => {
                if (dead_bits & 0x8 != 0) self.markRangeEnd(inst, inst);

                const slice = ir.extraData(Ir.Inst.ExtraSlice, payload.extra);
                const elements = ir.extraSlice(slice);

                const special = ir.liveness.special.get(inst).?;
                const dead_slice = ir.liveness.extraData(Liveness.ExtraSlice, special);
                const dead_elements = ir.liveness.extraSlice(dead_slice);
                for (elements, dead_elements) |element, dead_element| {
                    if (dead_element == 1) self.markRangeEnd(@enumFromInt(element), inst);
                }
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
                if (dead_bits & 0x1 != 0) self.markRangeEnd(payload.unary, inst);
                if (dead_bits & 0x8 != 0) self.markRangeEnd(inst, inst);
            },
            .call => {
                if (dead_bits & 0x1 != 0) self.markRangeEnd(payload.unary_extra.op, inst);
                if (dead_bits & 0x8 != 0) self.markRangeEnd(inst, inst);

                const slice = ir.extraData(Ir.Inst.ExtraSlice, payload.unary_extra.extra);
                const args = ir.extraSlice(slice);

                const special = ir.liveness.special.get(inst).?;
                const dead_slice = ir.liveness.extraData(Liveness.ExtraSlice, special);
                const dead_args = ir.liveness.extraSlice(dead_slice);
                for (args, dead_args) |arg, dead_arg| {
                    if (dead_arg == 1) self.markRangeEnd(@enumFromInt(arg), inst);
                }
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
    const last = try context.recordPostOrderDfs(entry, ir.blocks.len);

    // filter out unreachable blocks
    const out_order = try arena.alloc(BlockIndex, order.len - last);
    @memcpy(out_order, order[last..]);
    // std.debug.print("{any}\n", .{out_order});

    return .{
        .order = out_order,
        .phis = phis,
        .ranges = ranges,
    };
}
