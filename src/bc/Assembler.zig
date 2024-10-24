const std = @import("std");
const InternPool = @import("../InternPool.zig");
const Ir = @import("../ir/Ir.zig");
const Bytecode = @import("Bytecode.zig");
const PrePass = @import("PrePass.zig");
const Liveness = @import("../ir/Liveness.zig");
const String = @import("../rt/string.zig").String;
const types = @import("../rt/types.zig");
const builtins = @import("../rt/builtins.zig");
const compile = @import("../compile.zig");

const Allocator = std.mem.Allocator;
const asBytes = std.mem.asBytes;
const Opcode = Bytecode.Opcode;
const Register = i32; //Bytecode.Register;
const FunctionInfo = types.FunctionInfo;

const Assembler = @This();

gpa: Allocator,
arena: Allocator,
pool: *InternPool,
ir: *const Ir,
code: std.ArrayListUnmanaged(i32),
register_count: u32,
register_map: std.AutoHashMapUnmanaged(Ir.Index, i32),
free_registers: std.ArrayListUnmanaged(i32),
scratch: std.ArrayListUnmanaged(u32),
order: []const Ir.BlockIndex,
phis: []const std.ArrayListUnmanaged(PrePass.PhiMarker),
ranges: []const Ir.Index,
// TODO: this can probably be cleaned up
patch: []std.ArrayListUnmanaged(Patch),
inverts: std.AutoHashMapUnmanaged(Ir.Index, u32),
block_starts: std.AutoHashMapUnmanaged(Ir.BlockIndex, u32),
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
        .inverts = .{},
        .block_starts = .{},
        .constants = .{},
        .constant_map = .{},
    };

    for (0..prepass.order.len) |i| {
        const current = prepass.order[i];
        const next = if (i == prepass.order.len - 1) undefined else prepass.order[i + 1];
        try assembler.generateBlock(current, next);
    }

    return .{
        .frame_size = @intCast(assembler.register_count),
        .code = try assembler.code.toOwnedSlice(assembler.gpa),
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
        if (self.elideInst(phi_marker.phi)) continue;
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
    if (self.elideInst(inst)) {
        return;
    }

    const index = @intFromEnum(inst);
    switch (ir.insts.items(.tag)[index]) {
        .constant => try self.constant(inst),
        .arg => try self.argInst(inst),
        .builtin => {}, // nothing here, used by call
        .context_ptr, .element_ptr, .attribute_ptr => {}, // nothing here, used by load
        .load => try self.load(inst),
        .store => try self.store(inst),
        .list_init => try self.listInit(inst),
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
    const offset = @as(i32, @intCast(loc)) - @as(i32, @intCast(patch.bc_inst));
    switch (ir.instTag(patch.ir_inst)) {
        .jmp => self.updateJump(patch.bc_inst, offset),
        .br => {
            const branch = ir.extraData(Ir.Inst.Branch, payload.unary_extra.extra);
            if (branch.exec_else == current_block) {
                self.updateBranch(patch.bc_inst, offset);
            } else if (branch.exec_if == current_block) {
                self.updateJump(patch.bc_inst, offset);
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
    const unused = switch (self.ir.instTag(inst)) {
        .br, .jmp, .ret, .call, .store => false,
        else => self.rangeEnd(inst) == inst,
    };
    if (!unused) return false;

    switch (self.ir.instTag(inst)) {
        .constant, .arg, .builtin, .context_ptr, .attribute_ptr, .element_ptr => {},
        .list_init => {
            const payload = self.ir.instPayload(inst);
            const slice = self.ir.extraData(Ir.Inst.ExtraSlice, payload.extra);
            const src_elements: []const Ir.Index = @ptrCast(self.ir.extraSlice(slice));

            for (src_elements) |src_element| {
                const element = self.register_map.get(src_element).?;
                if (self.rangeEnd(src_element) == inst) self.deallocate(element);
            }
        },
        .load,
        .itof,
        .ftoi,
        .itob,
        .btoi,
        .any,
        .neg,
        .binv,
        .lnot,
        => {
            const unary = self.ir.instPayload(inst).unary;
            const operand = self.register_map.get(unary).?;
            if (self.rangeEnd(unary) == inst) self.deallocate(operand);
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
            const binary = self.ir.instPayload(inst).binary;
            const l = self.register_map.get(binary.l).?;
            const r = self.register_map.get(binary.r).?;
            if (self.rangeEnd(binary.l) == inst) self.deallocate(l);
            if (self.rangeEnd(binary.r) == inst) self.deallocate(r);
        },
        // .call => {
        // const payload = self.ir.instPayload(inst).unary_extra;
        // const ptr = payload.op;
        // const slice = self.ir.extraData(Ir.Inst.ExtraSlice, payload.extra);
        // const src_args: []const Ir.Index = @ptrCast(self.ir.extraSlice(slice));
        //
        // const target = self.register_map.get(ptr).?;
        // if (self.rangeEnd(ptr) == inst) self.deallocate(target);
        // for (src_args) |src_arg| {
        //     const arg = self.register_map.get(src_arg).?;
        //     if (self.rangeEnd(src_arg) == inst) self.deallocate(arg);
        // }
        // },
        .phi => {},
        .call, .store => unreachable,
        .ret, .jmp, .br => unreachable,
    }

    return true;
}

const Immediate = union(enum) {
    none: void,
    bool: bool,
    int: i32,
    float: f32,
};

fn addLd(self: *Assembler, dst: Register, in_imm: Immediate) !void {
    var imm: i32 = undefined;
    switch (in_imm) {
        .none => {},
        .bool => |b| imm = @intFromBool(b),
        .int => |i| imm = i,
        .float => |f| @memcpy(asBytes(&imm), asBytes(&f)),
    }

    try self.code.appendSlice(self.gpa, &.{
        @intFromEnum(Opcode.ld),
        dst,
        imm,
    });
}

fn addLdi(self: *Assembler, dst: Register, ip: InternPool.Index) !void {
    const pool = self.pool;
    const val: i64 = switch (pool.get(ip)) {
        .ty, .ir, .bytecode => unreachable,
        .tv => |tv| switch (pool.get(tv.ty).ty) {
            .nonetype => unreachable,
            .int => tv.val.int,
            .float => @bitCast(tv.val.float),
            .bool => unreachable,
            .str => unreachable, // implemented in .str TODO: change this?
            .list => unreachable, // TODO: implement this
            .@"union", .any, .object => unreachable, // TODO: unimplemented
        },
        // load a string literal from the intern pool and construct a string
        // on the heap
        // TODO: use page bump allocator
        .str => |bytes| @bitCast(@intFromPtr(try String.init(self.gpa, bytes))),
        // TODO: this should happen at "runtime" through callrt
        .function => |fi| ptr: {
            const function = pool.functionPtr(fi);
            const state: compile.CompilationInfo.State = switch (function.state) {
                .lazy => .lazy,
                .interpreted => .interpreted,
                .optimized_lite => .optimized_lite,
                .optimized_full => .optimized_full,
            };
            const ir = if (function.state == .lazy) undefined else pool.irPtr(function.ir);
            const bytecode = if (function.state == .lazy) undefined else pool.bytecodePtr(function.bytecode);
            const code = if (function.state == .lazy) undefined else pool.bytecodePtr(function.bytecode).code.ptr;
            const frame_size = if (function.state == .lazy) undefined else pool.bytecodePtr(function.bytecode).frame_size;

            const comp = try self.gpa.create(compile.CompilationInfo);
            comp.* = .{
                .tree = function.tree,
                .ir = ir,
                .bytecode = bytecode,
                .node = function.node,
                .global = function.global,
                .state = state,
            };
            const function_info = try self.gpa.create(FunctionInfo);
            function_info.* = .{
                .comp = @ptrCast(@alignCast(comp)),
                .bytecode = code,
                .frame_size = frame_size,
            };

            break :ptr @bitCast(@intFromPtr(function_info));
        },
    };

    try self.code.appendSlice(self.gpa, &.{
        @intFromEnum(Opcode.ldw),
        dst,
        @truncate(val),
        @truncate(val >> 32),
    });
}

fn addLdg(self: *Assembler, dst: Register, ip: InternPool.Index) !void {
    try self.code.appendSlice(self.gpa, &.{
        @intFromEnum(Opcode.ldg_init),
        dst,
        @bitCast(@intFromEnum(ip)),
        undefined, // inline cache key (lower 32 bits of shape pointer)
        undefined, // inline cache value (attribute index)
    });
}

fn addStg(self: *Assembler, src: Register, ip: InternPool.Index) !void {
    try self.code.appendSlice(self.gpa, &.{
        @intFromEnum(Opcode.stg_init),
        src,
        @bitCast(@intFromEnum(ip)),
        undefined, // inline cache key (lower 32 bits of shape pointer)
        undefined, // inline cache value (attribute index)
    });
}

fn addLdCtx(self: *Assembler, dst: Register, slot: u32) !void {
    try self.code.appendSlice(self.gpa, &.{
        @intFromEnum(Opcode.ld_ctx),
        dst,
        @bitCast(slot),
    });
}

fn addStCtx(self: *Assembler, src: Register, slot: u32) !void {
    try self.code.appendSlice(self.gpa, &.{
        @intFromEnum(Opcode.st_ctx),
        src,
        @bitCast(slot),
    });
}

fn addUnary(self: *Assembler, opcode: Opcode, dst: Register, src: Register) !void {
    try self.code.appendSlice(self.gpa, &.{
        @intFromEnum(opcode),
        dst,
        src,
    });
}

fn addBinary(self: *Assembler, opcode: Opcode, dst: Register, src1: Register, src2: Register, ir_inst: Ir.Index) !void {
    switch (opcode) {
        .ieq, .ine, .ilt, .igt, .ile, .ige, .flt, .fgt, .fle, .fge => {
            const bc_inst: u32 = @intCast(self.code.items.len);
            try self.inverts.put(self.arena, ir_inst, bc_inst);
        },
        else => {},
    }

    try self.code.appendSlice(self.gpa, &.{
        @intFromEnum(opcode),
        dst,
        src1,
        src2,
    });
}

fn addJump(self: *Assembler, target: i32) !void {
    try self.code.appendSlice(self.gpa, &.{
        @intFromEnum(Opcode.jmp),
        target,
    });
}

fn addBranch(self: *Assembler, cond: Register, target: i32) !void {
    try self.code.appendSlice(self.gpa, &.{
        @intFromEnum(Opcode.br),
        cond,
        target,
    });
}

fn addPush(self: *Assembler, args: []const Register) !void {
    switch (args.len) {
        0 => {},
        1 => {
            try self.code.appendSlice(self.gpa, &.{
                @intFromEnum(Opcode.push_one),
                @intCast(args[0]),
            });
        },
        else => {
            try self.code.ensureUnusedCapacity(self.gpa, args.len + 2);
            self.code.appendSliceAssumeCapacity(&.{
                @intFromEnum(Opcode.push_multi),
                @intCast(args.len),
            });
            self.code.appendSliceAssumeCapacity(args);
        },
    }
}

fn addPop(self: *Assembler, len: usize) !void {
    switch (len) {
        0 => {},
        1 => try self.code.append(self.gpa, @intFromEnum(Opcode.pop_one)),
        else => try self.code.appendSlice(self.gpa, &.{
            @intFromEnum(Opcode.pop_multi),
            @intCast(len),
        }),
    }
}

fn addCall(self: *Assembler, target: Register, dst: Register) !void {
    try self.code.appendSlice(self.gpa, &.{
        @intFromEnum(Opcode.call_init),
        target,
        dst,
        0, // cache key for function info
    });
}

fn addCallRt(self: *Assembler, id: builtins.Id, dst: Register) !void {
    try self.code.appendSlice(self.gpa, &.{
        @intFromEnum(Opcode.callrt),
        @intFromEnum(id),
        dst,
    });
}

fn callRuntimeArgs(self: *Assembler, id: builtins.Id, dst: Register, in_args: []const Register) !void {
    const scratch_top = self.scratch.items.len;
    defer self.scratch.shrinkRetainingCapacity(scratch_top);
    try self.scratch.ensureUnusedCapacity(self.arena, in_args.len);
    var i: usize = in_args.len;
    while (i > 0) {
        i -= 1;
        self.scratch.appendAssumeCapacity(@bitCast(in_args[i]));
    }

    const args = self.scratch.items[scratch_top..];
    try self.addPush(@ptrCast(args));
    try self.addCallRt(id, dst);
    try self.addPop(args.len);
}

fn addRet(self: *Assembler, val: Register) !void {
    try self.code.appendSlice(self.gpa, &.{
        @intFromEnum(Opcode.ret),
        val,
    });
}

fn reserveJump(self: *Assembler) !u32 {
    const top: u32 = @intCast(self.code.items.len);
    const slot = try self.code.addManyAsSlice(self.gpa, 2);
    slot[0] = @intFromEnum(Opcode.jmp);
    return top;
}

fn reserveBranch(self: *Assembler, cond: Register) !u32 {
    const top: u32 = @intCast(self.code.items.len);
    const slot = try self.code.addManyAsSlice(self.gpa, 3);
    slot[0] = @intFromEnum(Opcode.br);
    slot[1] = cond;
    return top;
}

fn reserveMov(self: *Assembler, src: Register) !u32 {
    const top: u32 = @intCast(self.code.items.len);
    const slot = try self.code.addManyAsSlice(self.gpa, 3);
    slot[0] = @intFromEnum(Opcode.mov);
    slot[2] = src;
    return top;
}

fn updateJump(self: *const Assembler, slot: u32, target: i32) void {
    self.code.items[slot + 1] = target;
}

fn updateBranch(self: *const Assembler, slot: u32, target: i32) void {
    self.code.items[slot + 2] = target;
}

fn updateMov(self: *const Assembler, slot: u32, dst: Register) void {
    self.code.items[slot + 1] = dst;
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
    try self.addLdg(dst, ip);
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
    // fp[-2] is the saved stack pointer
    // fp[-3] is the saved frame pointer
    // fp[-4] is the saved instruction pointer
    // so we start at -5
    try self.register_map.put(self.arena, inst, -5 - pos);
}

fn load(self: *Assembler, inst: Ir.Index) !void {
    const ir = self.ir;
    const payload = ir.instPayload(inst).unary_ip;
    switch (ir.instTag(payload.op)) {
        .context_ptr => {
            const slot = ir.instPayload(payload.op).slot;
            const dst = try self.allocate();
            try self.register_map.put(self.arena, inst, dst);
            try self.addLdCtx(dst, slot);
        },
        else => unreachable,
    }
}

fn store(self: *Assembler, inst: Ir.Index) !void {
    const ir = self.ir;
    const binary = ir.instPayload(inst).binary;
    switch (ir.instTag(binary.l)) {
        .context_ptr => {
            const slot = ir.instPayload(binary.l).slot;
            const val = self.register_map.get(binary.r).?;
            if (self.rangeEnd(binary.r) == inst) self.deallocate(val);
            try self.addStCtx(val, slot);
        },
        else => unreachable,
    }
}

fn listInit(self: *Assembler, inst: Ir.Index) !void {
    const ir = self.ir;
    const payload = ir.instPayload(inst);
    const slice = ir.extraData(Ir.Inst.ExtraSlice, payload.extra);
    const src_elements: []const Ir.Index = @ptrCast(ir.extraSlice(slice));

    const scratch_top = self.scratch.items.len;
    try self.scratch.ensureUnusedCapacity(self.arena, src_elements.len);
    defer self.scratch.shrinkRetainingCapacity(scratch_top);
    for (src_elements) |src_element| {
        const element = self.register_map.get(src_element).?;
        if (self.rangeEnd(src_element) == inst) self.deallocate(element);
        self.scratch.appendAssumeCapacity(@bitCast(element));
    }

    const elements: []const i32 = @ptrCast(self.scratch.items[scratch_top..]);
    _ = elements;
    const dst = try self.allocate();
    try self.register_map.put(self.arena, inst, dst);
    try self.callRuntimeArgs(.list_init, dst, &.{});
}

fn call(self: *Assembler, inst: Ir.Index) !void {
    const ir = self.ir;
    const payload = ir.instPayload(inst).unary_extra;
    const ptr = payload.op;
    const slice = ir.extraData(Ir.Inst.ExtraSlice, payload.extra);
    const src_args: []const Ir.Index = @ptrCast(ir.extraSlice(slice));

    if (ir.instTag(ptr) == .builtin) {
        try self.expandBuiltin(inst);
        return;
    }

    const target = self.register_map.get(ptr).?;
    if (self.rangeEnd(ptr) == inst) self.deallocate(target);
    const dst = try self.allocate();
    try self.register_map.put(self.arena, inst, dst);

    const scratch_top = self.scratch.items.len;
    try self.scratch.ensureUnusedCapacity(self.arena, src_args.len);
    defer self.scratch.shrinkRetainingCapacity(scratch_top);
    var i = src_args.len;
    while (i > 0) {
        i -= 1;
        const src_arg = src_args[i];
        const arg = self.register_map.get(src_arg).?;
        if (self.rangeEnd(src_arg) == inst) self.deallocate(arg);
        self.scratch.appendAssumeCapacity(@bitCast(arg));
    }

    const args: []const i32 = @ptrCast(self.scratch.items[scratch_top..]);
    try self.addPush(args);
    try self.addCall(target, dst);
    try self.addPop(args.len);
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

        // we don't use it, but the callrt interface needs a return register
        const dst = try self.allocate();
        switch (ty) {
            .nonetype => unreachable, // TODO: unimplemented
            .int => try self.callRuntimeArgs(.print1_int, dst, &.{operand}),
            .float => try self.callRuntimeArgs(.print1_float, dst, &.{operand}),
            .bool => try self.callRuntimeArgs(.print1_bool, dst, &.{operand}),
            .str => try self.callRuntimeArgs(.print1_str, dst, &.{operand}),
            else => unreachable,
        }
        self.deallocate(dst);
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
    try self.callRuntimeArgs(.strlen, dst, &.{operand});
}

fn binaryOp(self: *Assembler, inst: Ir.Index) !void {
    const binary = self.ir.instPayload(inst).binary;
    const ty = self.pool.get(self.ir.typeOf(binary.l)).ty;

    const l = self.register_map.get(binary.l).?;
    const r = self.register_map.get(binary.r).?;
    if (self.rangeEnd(binary.l) == inst) self.deallocate(l);
    if (self.rangeEnd(binary.r) == inst) self.deallocate(r);
    const dst = try self.allocate();
    try self.register_map.put(self.arena, inst, dst);

    const tag: Bytecode.Opcode = switch (ty) {
        .int => switch (self.ir.instTag(inst)) {
            .add => .iadd,
            .sub => .isub,
            .mul => .imul,
            .div => .idiv,
            .mod => .imod,
            .pow => return self.callRuntimeArgs(.ipow, dst, &.{ l, r }),
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
            .pow => return self.callRuntimeArgs(.fpow, dst, &.{ l, r }),
            .eq, .ne => unreachable,
            .lt => .flt,
            .gt => .fgt,
            .le => .fle,
            .ge => .fge,
            else => unreachable,
        },
        .str => switch (self.ir.instTag(inst)) {
            .add => return self.callRuntimeArgs(.strcat, dst, &.{ l, r }),
            .mul => return self.callRuntimeArgs(.strrep, dst, &.{ l, r }),
            else => unreachable,
        },
        else => unreachable,
    };

    try self.addBinary(tag, dst, l, r, inst);
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

    // we branch on the inverted form of the instruction, but if the conditions
    // are right, we can flip the instruction in place
    var in_place = false;
    if (self.rangeEnd(payload.op) == inst) {
        in_place = switch (self.ir.instTag(payload.op)) {
            .eq, .ne, .lt, .gt, .le, .ge => true,
            else => false,
        };
    }

    const inv = if (in_place) reg: {
        const bc_inst = self.inverts.get(payload.op).?;
        const old_opcode: Bytecode.Opcode = @enumFromInt(self.code.items[bc_inst]);
        const new_inst: Bytecode.Opcode = switch (old_opcode) {
            .ieq => .ine,
            .ine => .ieq,
            .ilt => .ige,
            .igt => .ile,
            .ile => .igt,
            .ige => .ilt,
            .flt => .fge,
            .fgt => .fle,
            .fle => .fgt,
            .fge => .flt,
            else => unreachable,
        };

        self.code.items[bc_inst] = @intFromEnum(new_inst);
        break :reg operand;
    } else reg: {
        const inv = try self.allocate();
        _ = try self.addUnary(.lnot, inv, operand);
        self.deallocate(inv);
        break :reg inv;
    };

    {
        const bc_inst = try self.reserveBranch(inv);
        try self.markPatch(current_block, branch.exec_else, inst, bc_inst);
    }

    if (branch.exec_if != next_block) {
        const bc_inst = try self.reserveJump();
        try self.markPatch(current_block, branch.exec_if, inst, bc_inst);
    }
}

fn ret(self: *Assembler, inst: Ir.Index) !void {
    const unary = self.ir.instPayload(inst).unary;
    const operand = self.register_map.get(unary).?;
    if (self.rangeEnd(unary) == inst) self.deallocate(operand);
    try self.addRet(operand);
}
