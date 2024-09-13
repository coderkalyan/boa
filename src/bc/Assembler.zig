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
// some instructions, like phis, keep track of the mapping
// from Ir index to bytecode index, so they can be updated later
inst_map: std.AutoHashMapUnmanaged(Ir.Index, u32),
scratch: std.ArrayListUnmanaged(u32),
order: []const Ir.BlockIndex,
phis: []const std.ArrayListUnmanaged(PrePass.PhiMarker),

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
    for (prepass.order) |i| std.debug.print("{}, ", .{@intFromEnum(i)});
    std.debug.print("\n", .{});
    for (prepass.ranges, 0..) |end, start| std.debug.print("%{}..%{}\n", .{ start, @intFromEnum(end) });
    // for (0..ir.blocks.len) |i| {
    //     std.debug.print("block{}: ", .{i});
    //     for (prepass.phis[i].items) |phi| std.debug.print("%{} -> %{}, ", .{ @intFromEnum(phi.operand), @intFromEnum(phi.phi) });
    //     std.debug.print("\n", .{});
    // }

    var assembler: Assembler = .{
        .gpa = gpa,
        .arena = arena,
        .pool = pool,
        .ir = ir,
        .code = .{},
        .register_count = 0,
        .free_registers = .{},
        .register_map = .{},
        .inst_map = .{},
        .scratch = .{},
        .order = prepass.order,
        .phis = prepass.phis,
    };

    for (prepass.order) |block| try assembler.generateBlock(block);
    // try assembler.add(.exit, undefined);

    return .{
        .ir = ir,
        .code = assembler.code.toOwnedSlice(),
    };
}

fn updateLiveRangeCount(self: *Assembler, block: Ir.BlockIndex) !void {
    const ir = self.ir;
    const n = @intFromEnum(block);
    if (self.visited[n]) return;
    self.visited[n] = true;

    const insts: []const Ir.Index = @ptrCast(ir.extraSlice(ir.blocks[n].insts));
    for (insts) |inst| {
        var ops: [2]Ir.Index = undefined;
        for (ir.operands(inst, &ops)) |operand| {
            self.live_range_count[@intFromEnum(operand)] += 1;
        }

        switch (ir.instTag(inst)) {
            .jmp => try self.updateLiveRangeCount(ir.instPayload(inst).block),
            .br => {
                const extra = ir.instPayload(inst).unary_extra.extra;
                const branch = ir.extraData(Ir.Inst.Branch, extra);
                try self.updateLiveRangeCount(branch.exec_if);
                try self.updateLiveRangeCount(branch.exec_else);
            },
            else => {},
        }
    }
}

fn generateBlock(self: *Assembler, block: Ir.BlockIndex) error{OutOfMemory}!void {
    const ir = self.ir;
    const n = @intFromEnum(block);
    if (self.visited[n]) return;
    self.visited[n] = true;

    const insts = ir.extraSlice(ir.blocks[n].insts);
    for (insts) |inst| {
        try self.generateInst(block, @enumFromInt(inst));
    }
}

fn generateInst(self: *Assembler, block: Ir.BlockIndex, inst: Ir.Index) !void {
    const ir = self.ir;
    const dead_bits = ir.liveness.deadBits(inst);

    const index = @intFromEnum(inst);
    switch (ir.insts.items(.tag)[index]) {
        .constant => try self.constant(inst),
        .itof => try self.itof(inst),
        .ftoi => try self.ftoi(inst),
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
        .lor => try self.lor(inst),
        .land => try self.land(inst),
        .jmp => try self.jmp(block, inst),
        .br => try self.br(block, inst),
        // .ret => unreachable, // TODO: implement
        else => {},
        // .if_else => try self.ifElse(inst),
        // .loop => try self.loop(inst),
        // .phi_if_else,
        // .phi_entry_if,
        // .phi_entry_else,
        // .phi_entry_body_body,
        // .phi_entry_body_exit,
        // => {},
    }

    // switch (ir.insts.items(.tag)[index]) {
    //     .if_else, .loop => {},
    //     else => if (dead_bits & 0x8 != 0) self.unassign(inst),
    // }
    if (dead_bits & 0x8 != 0) self.unassign(inst);
}

inline fn add(self: *Assembler, tag: Bytecode.Inst.Tag, payload: Bytecode.Inst.Payload) !void {
    try self.code.append(self.gpa, .{ .tag = tag, .payload = payload });
}

inline fn reserve(self: *Assembler, tag: Bytecode.Inst.Tag) !Bytecode.Register {
    const index: u32 = @intCast(self.code.len);
    try self.code.append(self.gpa, .{ .tag = tag, .payload = undefined });
    return @enumFromInt(index);
}

inline fn update(self: *Assembler, inst: Bytecode.Register, payload: Bytecode.Inst.Payload) void {
    const i = @intFromEnum(inst);
    self.code.items(.payload)[i] = payload;
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
    const i = @intFromEnum(inst);
    self.live_range_count[i] -= 1;
    if (self.live_range_count[i] > 0) return;

    if (!self.register_map.contains(inst)) std.debug.print("no: {}\n", .{i});
    const index = self.register_map.get(inst).?;
    std.debug.assert(self.register_map.remove(inst));
    self.free_registers.appendAssumeCapacity(index);
}

fn unassignRegister(self: *Assembler, index: Bytecode.Register) void {
    self.free_registers.appendAssumeCapacity(index);
}

fn getSlot(self: *Assembler, inst: Ir.Index) Bytecode.Register {
    if (!self.register_map.contains(inst)) std.debug.print("no: {}\n", .{@intFromEnum(inst)});
    return self.register_map.get(inst).?;
}

fn constant(self: *Assembler, inst: Ir.Index) !void {
    const ip = self.ir.instPayload(inst).ip;
    const tv = self.pool.get(ip).tv;

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
}

fn itof(self: *Assembler, inst: Ir.Index) !void {
    const unary = self.ir.instPayload(inst).unary;
    const operand = self.register_map.get(unary).?;
    const dead_bits = self.ir.liveness.deadBits(inst);
    if (dead_bits & 0x1 != 0) self.deallocate(operand);
    const dst = try self.allocate();
    try self.register_map.put(self.arena, inst, dst);
    try self.add(.itof, .{
        .dst = dst,
        .ops = .{ .unary = operand },
    });
}

fn ftoi(self: *Assembler, inst: Ir.Index) !void {
    const unary = self.ir.instPayload(inst).unary;
    const operand = self.register_map.get(unary).?;
    const dead_bits = self.ir.liveness.deadBits(inst);
    if (dead_bits & 0x1 != 0) self.deallocate(operand);
    const dst = try self.allocate();
    try self.register_map.put(self.arena, inst, dst);
    try self.add(.ftoi, .{
        .dst = dst,
        .ops = .{ .unary = operand },
    });
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

    std.debug.print("binary: {s}, %{}\n", .{ @tagName(self.ir.instTag(inst)), binary.l });
    const l = self.register_map.get(binary.l).?;
    const r = self.register_map.get(binary.r).?;
    const dead_bits = self.ir.liveness.deadBits(inst);
    if (dead_bits & 0x1 != 0) self.deallocate(l);
    if (dead_bits & 0x2 != 0) self.deallocate(r);

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
        else => unreachable,
    };

    const operand = self.register_map.get(unary).?;
    const dead_bits = self.ir.liveness.deadBits(inst);
    if (dead_bits & 0x1 != 0) self.deallocate(operand);

    const dst = try self.allocate();
    try self.register_map.put(self.arena, inst, dst);
    try self.add(tag, .{
        .dst = dst,
        .ops = .{ .unary = operand },
    });
}

fn jmp(self: *Assembler, block: Ir.BlockIndex, inst: Ir.Index) !void {
    const jump = self.ir.instPayload(inst).block;
    const next: Ir.BlockIndex = @enumFromInt(@intFromEnum(block) + 1);
    if (jump != next) {
        try self.add(.jump, .{
            .dst = undefined,
            .ops = .{ .target = @intCast(self.code.len) },
        });
    }

    try self.generateBlock(jump);
}

fn br(self: *Assembler, block: Ir.BlockIndex, inst: Ir.Index) !void {
    _ = self;
    _ = block;
    _ = inst;
    // const payload = self.ir.instPayload(inst).unary_extra;
    // const branch = self.ir.extraData(Ir.Inst.Branch, payload.extra);
    //
    // const operand = self.register_map.get(payload.op).?;
    // const dead_bits = self.ir.liveness.deadBits(inst);
    // if (dead_bits & 0x1 != 0) self.deallocate(operand);
    // const next: Ir.BlockIndex = @enumFromInt(@intFromEnum(block) + 1);

    // if (jump != next) {
    //     try self.add(.jump, .{
    //         .dst = undefined,
    //         .ops = .{ .target = @intCast(self.code.len) },
    //     });
    // }

    // try self.generateBlock(jump);
}
