const std = @import("std");
const InternPool = @import("../InternPool.zig");
const Ir = @import("../ir/Ir.zig");
const Bytecode = @import("Bytecode.zig");
const PrePass = @import("PrePass.zig");
const Liveness = @import("../ir/Liveness.zig");
const ConstantPool = @import("../rt/ConstantPool.zig");

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
ic_count: u32,
constant_pool: *ConstantPool,
constants: std.ArrayListUnmanaged(*anyopaque),
constant_map: std.AutoHashMapUnmanaged(InternPool.Index, u32),

const Patch = struct {
    ir_inst: Ir.Index,
    bc_inst: u32,
};

const Slot = struct {
    inst: Ir.Index,
    live: bool,
};

pub fn assemble(
    gpa: Allocator,
    pool: *InternPool,
    constant_pool: *ConstantPool,
    ir: *const Ir,
) !Bytecode {
    var arena_allocator = std.heap.ArenaAllocator.init(gpa);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    const entry: Ir.BlockIndex = @enumFromInt(0);
    const prepass = try PrePass.analyze(arena, ir, entry);

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
        .ic_count = 0,
        .constant_pool = constant_pool,
        .constants = .{},
        .constant_map = .{},
    };

    const pool_ptr: usize = @intFromPtr(pool);
    try assembler.code.appendSlice(assembler.gpa, &.{
        .{ .imm = @truncate(pool_ptr) },
        .{ .imm = @truncate(pool_ptr >> 32) },
    });

    // _ = try assembler.code.addOne(assembler.gpa);
    // _ = try assembler.code.addOne(assembler.gpa);

    for (0..prepass.order.len) |i| {
        const current = prepass.order[i];
        const next = if (i == prepass.order.len - 1) undefined else prepass.order[i + 1];
        try assembler.generateBlock(current, next);
    }

    const entry_pc: usize = 2;
    // const constants = try assembler.constants.toOwnedSlice(assembler.gpa);
    // const ptr: usize = @intFromPtr(constants.ptr);
    // assembler.code.items[2] = .{ .imm = @truncate(ptr) };
    // assembler.code.items[3] = .{ .imm = @truncate(ptr >> 32) };
    // try assembler.code.ensureUnusedCapacity(assembler.gpa, assembler.constants.items.len * @sizeOf(usize) / @sizeOf(u32));
    // for (assembler.constants.items) |item| {
    //     const ptr = @intFromPtr(item);
    //     std.debug.print("adding constant {x} at index {}\n", .{ ptr, entry_pc });
    //     try assembler.code.appendSlice(assembler.gpa, entry_pc, &.{
    //         .{ .imm = @truncate(ptr) },
    //         .{ .imm = @truncate(ptr >> 32) },
    //     });
    //     entry_pc += 2;
    // }

    return .{
        .register_count = @intCast(assembler.register_count),
        .ic_count = assembler.ic_count,
        .code = try assembler.code.toOwnedSlice(assembler.gpa),
        .entry_pc = entry_pc,
    };
}

fn generateBlock(
    self: *Assembler,
    current_block: Ir.BlockIndex,
    next_block: Ir.BlockIndex,
) error{OutOfMemory}!void {
    const ir = self.ir;
    const n = @intFromEnum(current_block);
    try self.block_starts.put(self.arena, current_block, @intCast(self.code.items.len));

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
        try self.patchInst(block, @intCast(self.code.items.len), patch);
    }
}

fn endBlock(self: *Assembler, block: Ir.BlockIndex) !void {
    const c = @intFromEnum(block);
    for (self.phis[c].items) |phi_marker| {
        // if (self.elideInst(phi_marker.phi)) continue;
        const src = self.register_map.get(phi_marker.operand).?;
        const mov = try self.reserveMov(src);
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
        .arg => try self.argInst(inst),
        .builtin => {}, // nothing here, used by call
        .itof,
        .ftoi,
        .itob,
        .btoi,
        .any,
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
        .jmp => self.updateJump(patch.bc_inst, loc - patch.bc_inst),
        .br => {
            const branch = ir.extraData(Ir.Inst.Branch, payload.unary_extra.extra);
            if (branch.exec_if == current_block) {
                self.updateBranch(patch.bc_inst, loc - patch.bc_inst);
            }
        },
        .phi => {
            if (self.register_map.get(patch.ir_inst)) |dst| {
                self.updateMov(patch.bc_inst, dst);
            } else {
                const dst = try self.allocate();
                try self.register_map.put(self.arena, patch.ir_inst, dst);
                self.updateMov(patch.bc_inst, dst);
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
    const register: Register = @intCast(self.register_count);
    self.register_count += 1;
    try self.free_registers.ensureTotalCapacity(self.arena, self.register_count);
    return register;
}

fn deallocate(self: *Assembler, register: Register) void {
    if (register < 0) return; // TODO: find out why this is a thing
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

const Immediate = union(enum) {
    none: void,
    bool: bool,
    int: u32,
    float: f32,
};

fn addLd(self: *Assembler, dst: Register, imm: Immediate) !void {
    var dest: u32 = undefined;
    switch (imm) {
        .none => {},
        .bool => |b| dest = @intFromBool(b),
        .int => |i| dest = i,
        .float => |f| @memcpy(asBytes(&dest), asBytes(&f)),
    }

    try self.code.appendSlice(self.gpa, &.{
        .{ .opcode = .ld },
        .{ .register = dst },
        .{ .imm = dest },
    });
}

fn addLdi(self: *Assembler, dst: Register, ip: InternPool.Index) !void {
    const gop = try self.constant_map.getOrPut(self.arena, ip);
    if (!gop.found_existing) {
        const ptr = try self.constant_pool.put(self.pool, ip);
        const index: u32 = @intCast(self.constants.items.len);
        try self.constants.append(self.gpa, ptr);
        gop.value_ptr.* = index;
    }

    // TODO: clean this up
    const ptr = @intFromPtr(self.constants.items[gop.value_ptr.*]);
    try self.code.appendSlice(self.gpa, &.{
        .{ .opcode = .ldw },
        .{ .register = dst },
        .{ .imm = @truncate(ptr) },
        .{ .imm = @truncate(ptr >> 32) },
    });
}

fn addLdg(self: *Assembler, dst: Register, ip: InternPool.Index, ic: u32) !void {
    try self.code.appendSlice(self.gpa, &.{
        .{ .opcode = .ldg },
        .{ .register = dst },
        .{ .ip = ip },
        .{ .count = ic },
    });
}

fn addStg(self: *Assembler, src: Register, ip: InternPool.Index) !void {
    try self.code.appendSlice(self.gpa, &.{
        .{ .opcode = .stg },
        .{ .register = src },
        .{ .ip = ip },
    });
}

fn addUnary(self: *Assembler, opcode: Opcode, dst: Register, src: Register) !void {
    try self.code.appendSlice(self.gpa, &.{
        .{ .opcode = opcode },
        .{ .register = dst },
        .{ .register = src },
    });
}

fn addBinary(self: *Assembler, opcode: Opcode, dst: Register, src1: Register, src2: Register) !void {
    try self.code.appendSlice(self.gpa, &.{
        .{ .opcode = opcode },
        .{ .register = dst },
        .{ .register = src1 },
        .{ .register = src2 },
    });
}

fn addPrint(self: *Assembler, opcode: Opcode, src: Register) !void {
    try self.code.appendSlice(self.gpa, &.{
        .{ .opcode = opcode },
        .{ .register = src },
    });
}

fn addJump(self: *Assembler, target: u32) !void {
    try self.code.appendSlice(self.gpa, &.{
        .{ .opcode = .jump },
        .{ .target = target },
    });
}

fn addBranch(self: *Assembler, cond: Register, target: u32) !void {
    try self.code.appendSlice(self.gpa, &.{
        .{ .opcode = .branch },
        .{ .register = cond },
        .{ .target = target },
    });
}

fn addRet(self: *Assembler, val: Register) !void {
    try self.code.appendSlice(self.gpa, &.{
        .{ .opcode = .ret },
        .{ .register = val },
    });
}

fn reserveJump(self: *Assembler) !u32 {
    const top: u32 = @intCast(self.code.items.len);
    const slot = try self.code.addManyAsSlice(self.gpa, 2);
    slot[0] = .{ .opcode = .jump };
    return top;
}

fn reserveBranch(self: *Assembler, cond: Register) !u32 {
    const top: u32 = @intCast(self.code.items.len);
    const slot = try self.code.addManyAsSlice(self.gpa, 3);
    slot[0] = .{ .opcode = .branch };
    slot[1] = .{ .register = cond };
    return top;
}

fn reserveMov(self: *Assembler, src: Register) !u32 {
    const top: u32 = @intCast(self.code.items.len);
    const slot = try self.code.addManyAsSlice(self.gpa, 3);
    slot[0] = .{ .opcode = .mov };
    slot[2] = .{ .register = src };
    return top;
}

fn updateJump(self: *const Assembler, slot: u32, target: u32) void {
    self.code.items[slot + 1] = .{ .target = target };
}

fn updateBranch(self: *const Assembler, slot: u32, target: u32) void {
    self.code.items[slot + 2] = .{ .target = target };
}

fn updateMov(self: *const Assembler, slot: u32, dst: Register) void {
    self.code.items[slot + 1] = .{ .register = dst };
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
                const dst = try self.allocate();
                try self.register_map.put(self.arena, inst, dst);
                try self.addLdi(dst, ip);
            } else {
                const imm: Immediate = switch (tv.ty) {
                    .nonetype => .{ .none = {} },
                    .bool => .{ .bool = tv.val.bool },
                    .int => .{ .int = @truncate(tv.val.int) },
                    else => unreachable,
                };

                const dst = try self.allocate();
                try self.register_map.put(self.arena, inst, dst);
                try self.addLd(dst, imm);
            }
        },
        .function => {
            const dst = try self.allocate();
            try self.register_map.put(self.arena, inst, dst);
            try self.addLdi(dst, ip);
        },
        .str => {
            const dst = try self.allocate();
            try self.register_map.put(self.arena, inst, dst);
            try self.addLdi(dst, ip);
        },
        else => unreachable,
    }
}

fn ldGlobal(self: *Assembler, inst: Ir.Index) !void {
    const ip = self.ir.instPayload(inst).ip;
    const dst = try self.allocate();
    try self.register_map.put(self.arena, inst, dst);
    try self.addLdg(dst, ip, self.ic_count);
    self.ic_count += 1;
}

fn stGlobal(self: *Assembler, inst: Ir.Index) !void {
    const payload = self.ir.instPayload(inst).unary_ip;
    const ip = payload.ip;

    const val = self.register_map.get(payload.op).?;
    if (self.rangeEnd(payload.op) == inst) self.deallocate(val);
    try self.addStg(val, ip);
}

fn argInst(self: *Assembler, inst: Ir.Index) !void {
    const arg = self.ir.instPayload(inst).arg;
    const pos: i32 = @intCast(arg.position);
    // arguments are indexed as -1, -2, -3... but:
    // fp[-1] is the return register
    // fp[-2] is the saved frame pointer
    // fp[-3] is the saved stack pointer
    // fp[-3] is the saved instruction pointer
    // so we start at -5
    try self.register_map.put(self.arena, inst, -5 - pos);
}

fn call(self: *Assembler, inst: Ir.Index) !void {
    const ir = self.ir;
    const payload = ir.instPayload(inst).unary_extra;
    const ptr = payload.op;
    const slice = ir.extraData(Ir.Inst.ExtraSlice, payload.extra);
    const args = ir.extraSlice(slice);

    if (ir.instTag(ptr) == .builtin) {
        try self.expandBuiltin(inst);
        return;
    }

    const target = self.register_map.get(ptr).?;
    if (self.rangeEnd(ptr) == inst) self.deallocate(target);
    const dst = try self.allocate();
    try self.register_map.put(self.arena, inst, dst);

    try self.code.ensureUnusedCapacity(self.gpa, 4 + args.len);
    self.code.appendSliceAssumeCapacity(&.{
        .{ .opcode = .call },
        .{ .register = target },
        .{ .register = dst },
        .{ .count = @intCast(args.len) },
    });
    for (args) |ir_arg| {
        const arg = self.register_map.get(@enumFromInt(ir_arg)).?;
        if (self.rangeEnd(ptr) == inst) self.deallocate(arg);
        self.code.appendAssumeCapacity(.{ .register = arg });
    }
}

fn expandBuiltin(self: *Assembler, inst: Ir.Index) !void {
    const ir = self.ir;
    const payload = ir.instPayload(inst).unary_extra;
    const builtin = payload.op;

    const ip = self.ir.instPayload(builtin).ip;
    switch (ip) {
        .builtin_print => try self.builtinPrint(inst),
        .builtin_len => try self.builtinLen(inst),
        else => unreachable,
    }
}

fn builtinPrint(self: *Assembler, inst: Ir.Index) !void {
    const ir = self.ir;
    const payload = ir.instPayload(inst).unary_extra;
    const slice = ir.extraData(Ir.Inst.ExtraSlice, payload.extra);
    const args: []const Ir.Index = @ptrCast(ir.extraSlice(slice));

    for (args) |arg| {
        const ty = ir.typeOf(arg);
        const operand = self.register_map.get(arg).?;
        if (self.rangeEnd(arg) == inst) self.deallocate(operand);

        const opcode: Opcode = switch (ty) {
            .nonetype => unreachable, // TODO: unimplemented
            .int => .pint,
            .float => .pfloat,
            .bool => .pfloat,
            .str => .pstr,
            else => unreachable,
        };
        try self.addPrint(opcode, operand);
    }
}

fn builtinLen(self: *Assembler, inst: Ir.Index) !void {
    const ir = self.ir;
    const payload = ir.instPayload(inst).unary_extra;
    const slice = ir.extraData(Ir.Inst.ExtraSlice, payload.extra);
    const args: []const Ir.Index = @ptrCast(ir.extraSlice(slice));

    std.debug.assert(args.len == 1);
    std.debug.assert(ir.typeOf(args[0]) == .str);
    const str = args[0];
    const operand = self.register_map.get(str).?;
    if (self.rangeEnd(str) == inst) self.deallocate(operand);

    const dst = try self.allocate();
    try self.register_map.put(self.arena, inst, dst);
    try self.addUnary(.strlen, dst, operand);
}

fn binaryOp(self: *Assembler, inst: Ir.Index) !void {
    const binary = self.ir.instPayload(inst).binary;
    const ty = self.pool.get(self.ir.typeOf(binary.l)).ty;
    const tag: Bytecode.Opcode = switch (ty) {
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
        .str => switch (self.ir.instTag(inst)) {
            .add => .strcat,
            .mul => .strrep,
            else => unreachable,
        },
        else => unreachable,
    };

    const l = self.register_map.get(binary.l).?;
    const r = self.register_map.get(binary.r).?;
    if (self.rangeEnd(binary.l) == inst) self.deallocate(l);
    if (self.rangeEnd(binary.r) == inst) self.deallocate(r);

    const dst = try self.allocate();
    try self.register_map.put(self.arena, inst, dst);
    try self.addBinary(tag, dst, l, r);
}

fn unaryOp(self: *Assembler, inst: Ir.Index) !void {
    const unary = self.ir.instPayload(inst).unary;
    const ty = self.pool.get(self.ir.typeOf(unary)).ty;
    const tag: Bytecode.Opcode = switch (self.ir.instTag(inst)) {
        .neg => switch (ty) {
            .int => .ineg,
            .float => .fneg,
            else => unreachable,
        },
        .binv => .binv,
        .lnot => .lnot,
        .itof => .itof,
        .ftoi => .ftoi,
        .itob, .btoi => {
            const operand = self.register_map.get(unary).?;
            try self.register_map.put(self.arena, inst, operand);
            return;
        },
        else => unreachable,
    };

    const operand = self.register_map.get(unary).?;
    if (self.rangeEnd(unary) == inst) self.deallocate(operand);

    const dst = try self.allocate();
    try self.register_map.put(self.arena, inst, dst);
    try self.addUnary(tag, dst, operand);
}

fn jmp(self: *Assembler, inst: Ir.Index, current_block: Ir.BlockIndex, next_block: Ir.BlockIndex) !void {
    const target_block = self.ir.instPayload(inst).block;
    if (next_block == target_block) return;

    const jump = try self.reserveJump();
    try self.markPatch(current_block, target_block, inst, jump);
}

fn br(self: *Assembler, inst: Ir.Index, current_block: Ir.BlockIndex, next_block: Ir.BlockIndex) !void {
    const payload = self.ir.instPayload(inst).unary_extra;
    const branch = self.ir.extraData(Ir.Inst.Branch, payload.extra);

    const operand = self.register_map.get(payload.op).?;
    if (self.rangeEnd(payload.op) == inst) self.deallocate(operand);

    // TODO: an easy optimization here is to negate the operand if needed
    if (branch.exec_if != next_block) {
        const bc_inst = try self.reserveBranch(operand);
        try self.markPatch(current_block, branch.exec_if, inst, bc_inst);
    }
}

fn ret(self: *Assembler, inst: Ir.Index) !void {
    const unary = self.ir.instPayload(inst).unary;
    const operand = self.register_map.get(unary).?;
    if (self.rangeEnd(unary) == inst) self.deallocate(operand);
    try self.addRet(operand);
}
