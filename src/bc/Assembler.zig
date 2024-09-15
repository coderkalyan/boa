const std = @import("std");
const InternPool = @import("../InternPool.zig");
const Ir = @import("../ir/Ir.zig");
const Bytecode = @import("Bytecode.zig");
const PrePass = @import("PrePass.zig");

const Allocator = std.mem.Allocator;
const asBytes = std.mem.asBytes;
const Opcode = Bytecode.Opcode;
const Register = Bytecode.Register;

const Assembler = @This();

gpa: Allocator,
arena: Allocator,
pool: *InternPool,
ir: *const Ir,
code: Bytecode.List,
register_count: u32,
register_map: std.AutoHashMapUnmanaged(Ir.Index, Bytecode.Register),
free_registers: std.ArrayListUnmanaged(Bytecode.Register),
scratch: std.ArrayListUnmanaged(u32),
order: []const Ir.BlockIndex,
phis: []const std.ArrayListUnmanaged(PrePass.PhiMarker),
ranges: []const Ir.Index,
patch: []std.ArrayListUnmanaged(Patch),
block_starts: std.AutoHashMapUnmanaged(Ir.BlockIndex, u32),

const Patch = struct {
    ir_inst: Ir.Index,
    bc_inst: u32,
};

const Slot = struct {
    inst: Ir.Index,
    live: bool,
};

pub fn assemble(gpa: Allocator, pool: *InternPool, ir: *const Ir) !Bytecode {
    var arena_allocator = std.heap.ArenaAllocator.init(gpa);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    const entry: Ir.BlockIndex = @enumFromInt(0);
    const prepass = try PrePass.analyze(arena, ir, entry);
    // for (prepass.order) |i| std.debug.print("{}, ", .{@intFromEnum(i)});
    // std.debug.print("\n", .{});
    // for (prepass.ranges, 0..) |end, start| std.debug.print("%{}..%{}\n", .{ start, @intFromEnum(end) });
    // for (0..ir.blocks.len) |i| {
    //     std.debug.print("block{}: ", .{i});
    //     for (prepass.phis[i].items) |phi| std.debug.print("%{} -> %{}, ", .{ @intFromEnum(phi.operand), @intFromEnum(phi.phi) });
    //     std.debug.print("\n", .{});
    // }

    const patch = try arena.alloc(std.ArrayListUnmanaged(Patch), ir.blocks.len);
    @memset(patch, .{});

    var assembler: Assembler = .{
        .gpa = gpa,
        .arena = arena,
        .pool = pool,
        .ir = ir,
        .code = .{},
        .register_count = 0,
        .free_registers = .{},
        .register_map = .{},
        .scratch = .{},
        .order = prepass.order,
        .phis = prepass.phis,
        .ranges = prepass.ranges,
        .patch = patch,
        .block_starts = .{},
    };
    try assembler.add(.pool, .{ .dst = undefined, .ops = .{ .pool = pool } });
    for (0..prepass.order.len) |i| {
        const current = prepass.order[i];
        const next = if (i == prepass.order.len - 1) undefined else prepass.order[i + 1];
        try assembler.generateBlock(current, next);
    }

    return .{
        .register_count = assembler.register_count,
        .code = assembler.code.toOwnedSlice(),
    };
}

fn generateBlock(
    self: *Assembler,
    current_block: Ir.BlockIndex,
    next_block: Ir.BlockIndex,
) error{OutOfMemory}!void {
    const ir = self.ir;
    const n = @intFromEnum(current_block);
    try self.block_starts.put(self.arena, current_block, @intCast(self.code.len));

    const insts = ir.extraSlice(ir.blocks[n].insts);
    try self.startBlock(current_block);
    for (insts) |inst| {
        // TODO: clean this up
        switch (ir.instTag(@enumFromInt(inst))) {
            .jmp, .br, .ret => {},
            else => try self.generateInst(@enumFromInt(inst), current_block, next_block),
        }
    }
    try self.endBlock(current_block);
    for (insts) |inst| {
        // TODO: clean this up
        switch (ir.instTag(@enumFromInt(inst))) {
            .jmp,
            .br,
            .ret,
            => try self.generateInst(@enumFromInt(inst), current_block, next_block),
            else => {},
        }
    }
}

fn startBlock(self: *Assembler, block: Ir.BlockIndex) !void {
    for (self.patch[@intFromEnum(block)].items) |patch| {
        try self.patchInst(block, @intCast(self.code.len), patch);
    }
}

fn endBlock(self: *Assembler, block: Ir.BlockIndex) !void {
    const c = @intFromEnum(block);
    for (self.phis[c].items) |phi_marker| {
        // if (self.elideInst(phi_marker.phi)) continue;
        const src = self.register_map.get(phi_marker.operand).?;
        const mov = try self.reserve(.mov);
        self.update(mov, .{ .dst = undefined, .ops = .{ .unary = src } });
        try self.markPatch(block, phi_marker.dest_block, phi_marker.phi, mov);
    }
}

fn generateInst(self: *Assembler, inst: Ir.Index, current_block: Ir.BlockIndex, next_block: Ir.BlockIndex) !void {
    const ir = self.ir;
    // this isn't a full dead code elimination pass, since it doesn't reason
    // about dependency chains ending in an unused instruction
    // however it is good at cleaning up "useless" instructions, especially phis
    // at branch/loop exits which are used to export local variables across scopes,
    // if they aren't used
    // TODO: needs to drop dead operands
    // if (self.elideInst(inst)) return;

    const index = @intFromEnum(inst);
    switch (ir.insts.items(.tag)[index]) {
        .constant => try self.constant(inst),
        .ld_global => try self.ldGlobal(inst),
        .st_global => try self.stGlobal(inst),
        .itof,
        .ftoi,
        .neg,
        .binv,
        .lnot,
        => try self.unaryOp(inst),
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
        => try self.binaryOp(inst),
        .call => try self.call(inst),
        .jmp => try self.jmp(inst, current_block, next_block),
        .br => try self.br(inst, current_block, next_block),
        .ret => try self.ret(inst),
        .phi => {}, // implemented in startBlock
    }
}

inline fn add(self: *Assembler, tag: Bytecode.Inst.Tag, payload: Bytecode.Inst.Payload) !void {
    try self.code.append(self.gpa, .{ .tag = tag, .payload = payload });
}

inline fn reserve(self: *Assembler, tag: Bytecode.Inst.Tag) !u32 {
    const index: u32 = @intCast(self.code.len);
    try self.code.append(self.gpa, .{ .tag = tag, .payload = undefined });
    return index;
}

inline fn update(self: *Assembler, inst: u32, payload: Bytecode.Inst.Payload) void {
    self.code.items(.payload)[inst] = payload;
}

fn markPatch(self: *Assembler, current_block: Ir.BlockIndex, target_block: Ir.BlockIndex, ir_inst: Ir.Index, bc_inst: u32) !void {
    const patch: Patch = .{ .ir_inst = ir_inst, .bc_inst = bc_inst };
    if (self.block_starts.get(target_block)) |loc| {
        try self.patchInst(current_block, loc, patch);
    } else {
        try self.patch[@intFromEnum(target_block)].append(self.arena, patch);
    }
}

fn patchInst(self: *Assembler, current_block: Ir.BlockIndex, loc: u32, patch: Patch) !void {
    const ir = self.ir;
    const payload = ir.instPayload(patch.ir_inst);
    switch (ir.instTag(patch.ir_inst)) {
        .jmp => self.update(patch.bc_inst, .{ .dst = undefined, .ops = .{ .target = loc } }),
        .br => {
            const branch = ir.extraData(Ir.Inst.Branch, payload.unary_extra.extra);
            if (branch.exec_if == current_block) {
                self.code.items(.payload)[patch.bc_inst].ops.branch.target = loc;
            }
        },
        .phi => {
            if (self.register_map.get(patch.ir_inst)) |dst| {
                self.code.items(.payload)[patch.bc_inst].dst = dst;
            } else {
                const dst = try self.allocate();
                try self.register_map.put(self.arena, patch.ir_inst, dst);
                self.code.items(.payload)[patch.bc_inst].dst = dst;
            }
        },
        else => unreachable,
    }
}

fn allocate(self: *Assembler) !Register {
    // try to allocate an existing register (doesn't matter which)
    // before increasing our stack frame size
    if (self.free_registers.items.len > 0) {
        const register = self.free_registers.swapRemove(0);
        return register;
    }

    // no free register found, so "spill" by increasing our stack frame size
    const register: Register = @enumFromInt(self.register_count);
    self.register_count += 1;
    try self.free_registers.ensureTotalCapacity(self.arena, self.register_count);
    return register;
}

fn deallocate(self: *Assembler, register: Register) void {
    self.free_registers.appendAssumeCapacity(register);
}

fn assign(self: *Assembler, inst: Ir.Index) !Bytecode.Register {
    if (self.free_registers.items.len > 0) {
        const register = self.free_registers.swapRemove(0);
        self.register_map.putAssumeCapacity(inst, register);
        return register;
    }

    const index: u32 = self.register_count;
    self.register_count += 1;
    try self.free_registers.ensureUnusedCapacity(self.arena, 1);
    try self.register_map.ensureTotalCapacity(self.arena, @intCast(self.free_registers.capacity));

    const register: Bytecode.Register = @enumFromInt(index);
    self.register_map.putAssumeCapacity(inst, register);
    return register;
}

fn unassign(self: *Assembler, inst: Ir.Index) void {
    // if (inst != self.ranges[@intFromEnum(cur)]) return;
    const index = self.register_map.get(inst).?;
    std.debug.assert(self.register_map.remove(inst));
    self.free_registers.appendAssumeCapacity(index);
}

fn unassignRegister(self: *Assembler, index: Bytecode.Register) void {
    self.free_registers.appendAssumeCapacity(index);
}

inline fn rangeEnd(self: *Assembler, inst: Ir.Index) Ir.Index {
    return self.ranges[@intFromEnum(inst)];
}

fn elideInst(self: *Assembler, inst: Ir.Index) bool {
    // instructions with side effects (like writing to memory or
    // changing control flow) cannot be elided
    // otherwise, instructions that die immediately (never use) are elided
    return switch (self.ir.instTag(inst)) {
        .br, .jmp, .ret => false,
        else => self.rangeEnd(inst) == inst,
    };
}

fn constant(self: *Assembler, inst: Ir.Index) !void {
    const ip = self.ir.instPayload(inst).ip;
    switch (self.pool.get(ip)) {
        .tv => |tv| {
            const wide = switch (tv.ty) {
                .int => tv.val.int > std.math.maxInt(u32),
                .float => true,
                else => false,
            };
            if (wide) {
                var imm: [8]u8 = undefined;
                switch (tv.ty) {
                    .int => @memcpy(&imm, asBytes(&tv.val.int)),
                    .float => @memcpy(&imm, asBytes(&tv.val.float)),
                    else => unreachable,
                }
                const dst = try self.allocate();
                try self.register_map.put(self.arena, inst, dst);
                try self.add(.ldw, .{
                    .dst = dst,
                    .ops = .{ .wimm = imm },
                });
            } else {
                var imm: [4]u8 = undefined;
                const val: u32 = switch (tv.ty) {
                    .nonetype => undefined,
                    .bool => @intFromBool(tv.val.bool),
                    .int => @intCast(tv.val.int),
                    else => unreachable,
                };
                @memcpy(&imm, asBytes(&val));
                const dst = try self.allocate();
                try self.register_map.put(self.arena, inst, dst);
                try self.add(.ld, .{
                    .dst = dst,
                    .ops = .{ .imm = imm },
                });
            }
        },
        .function => |index| {
            const function_ptr = self.pool.functionPtr(index);
            var imm: [8]u8 = undefined;
            @memcpy(&imm, asBytes(&function_ptr));
            const dst = try self.allocate();
            try self.register_map.put(self.arena, inst, dst);
            try self.add(.ldw, .{
                .dst = dst,
                .ops = .{ .wimm = imm },
            });
        },
        else => unreachable,
    }
}

fn ldGlobal(self: *Assembler, inst: Ir.Index) !void {
    const ip = self.ir.instPayload(inst).ip;
    const dst = try self.allocate();
    try self.add(.ld_global, .{ .dst = dst, .ops = .{ .ip = ip } });
}

fn stGlobal(self: *Assembler, inst: Ir.Index) !void {
    const payload = self.ir.instPayload(inst).unary_ip;
    const ip = payload.ip;

    const val = self.register_map.get(payload.op).?;
    if (self.rangeEnd(payload.op) == inst) self.deallocate(val);

    try self.add(.st_global, .{
        .dst = undefined,
        .ops = .{ .store = .{ .ip = ip, .val = val } },
    });
}

fn call(self: *Assembler, inst: Ir.Index) !void {
    _ = self;
    _ = inst;
    // const ip = self.ir.instPayload(inst).ip;
    // const dst = try self.allocate();
    // try self.add(.ld_global, .{ .dst = dst, .ops = .{ .ip = ip } });
}

fn binaryOp(self: *Assembler, inst: Ir.Index) !void {
    const binary = self.ir.instPayload(inst).binary;
    const ty = self.pool.get(self.ir.typeOf(binary.l)).ty;
    const tag: Bytecode.Inst.Tag = switch (ty) {
        .int => switch (self.ir.instTag(inst)) {
            .add => .iadd,
            .sub => .isub,
            .mul => .imul,
            .div => .idiv,
            .mod => .imod,
            .pow => .ipow,
            .band => .band,
            .bor => .bor,
            .bxor => .bxor,
            .sll => .sll,
            .sra => .sra,
            .eq => .ieq,
            .ne => .ine,
            .lt => .ilt,
            .gt => .igt,
            .le => .ile,
            .ge => .ige,
            else => unreachable,
        },
        .float => switch (self.ir.instTag(inst)) {
            .add => .fadd,
            .sub => .fsub,
            .mul => .fmul,
            .div => .fdiv,
            .mod => .fmod,
            .pow => .fpow,
            .eq, .ne => unreachable,
            .lt => .flt,
            .gt => .fgt,
            .le => .fle,
            .ge => .fge,
            else => unreachable,
        },
        else => unreachable,
    };

    // std.debug.print("binary: {s}, %{}\n", .{ @tagName(self.ir.instTag(inst)), binary.l });
    const l = self.register_map.get(binary.l).?;
    const r = self.register_map.get(binary.r).?;
    if (self.rangeEnd(binary.l) == inst) self.deallocate(l);
    if (self.rangeEnd(binary.r) == inst) self.deallocate(r);

    const dst = try self.allocate();
    try self.register_map.put(self.arena, inst, dst);
    try self.add(tag, .{
        .dst = dst,
        .ops = .{
            .binary = .{
                .op1 = l,
                .op2 = r,
            },
        },
    });
}

fn unaryOp(self: *Assembler, inst: Ir.Index) !void {
    const unary = self.ir.instPayload(inst).unary;
    const ty = self.pool.get(self.ir.typeOf(unary)).ty;
    const tag: Bytecode.Inst.Tag = switch (self.ir.instTag(inst)) {
        .neg => switch (ty) {
            .int => .ineg,
            .float => .fneg,
            else => unreachable,
        },
        .binv => .binv,
        .lnot => .lnot,
        .itof => .itof,
        .ftoi => .ftoi,
        else => unreachable,
    };

    const operand = self.register_map.get(unary).?;
    if (self.rangeEnd(unary) == inst) self.deallocate(operand);

    const dst = try self.allocate();
    try self.register_map.put(self.arena, inst, dst);
    try self.add(tag, .{
        .dst = dst,
        .ops = .{ .unary = operand },
    });
}

fn jmp(self: *Assembler, inst: Ir.Index, current_block: Ir.BlockIndex, next_block: Ir.BlockIndex) !void {
    const target_block = self.ir.instPayload(inst).block;
    if (next_block == target_block) return;

    const jump = try self.reserve(.jump);
    try self.markPatch(current_block, target_block, inst, jump);
}

fn br(self: *Assembler, inst: Ir.Index, current_block: Ir.BlockIndex, next_block: Ir.BlockIndex) !void {
    const payload = self.ir.instPayload(inst).unary_extra;
    const branch = self.ir.extraData(Ir.Inst.Branch, payload.extra);

    const operand = self.register_map.get(payload.op).?;
    if (self.rangeEnd(payload.op) == inst) self.deallocate(operand);

    // TODO: an easy optimization here is to negate the operand if needed
    if (branch.exec_if != next_block) {
        const bc_inst = try self.reserve(.branch);
        self.code.items(.payload)[bc_inst].ops = .{
            .branch = .{
                .condition = operand,
                .target = undefined,
            },
        };
        try self.markPatch(current_block, branch.exec_if, inst, bc_inst);
    }
}

fn ret(self: *Assembler, inst: Ir.Index) !void {
    _ = inst;
    try self.add(.exit, undefined);
}
