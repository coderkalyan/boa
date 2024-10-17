// Compiles a function to native machine code using LLVM.

const std = @import("std");
const mvll = @import("../mvll/root.zig");
const Ir = @import("../ir/Ir.zig");
const InternPool = @import("../InternPool.zig");
const c = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/Target.h");
    @cInclude("llvm-c/TargetMachine.h");
    @cInclude("llvm-c/Analysis.h");
    @cInclude("llvm-c/BitWriter.h");
});

const Allocator = std.mem.Allocator;
const Context = mvll.Context;
const Module = mvll.Module;
const Builder = mvll.Builder;
const BasicBlock = mvll.BasicBlock;
const Type = mvll.Type;
const Value = mvll.Value;
const Target = mvll.Target;
const TargetMachine = mvll.TargetMachine;
const Compiler = @This();

arena: Allocator,
ctx: *Context,
module: *Module,
builder: *Builder,
target_machine: *TargetMachine,
function: *Value,
ir: *const Ir,
pool: *InternPool,
blocks: []const *BasicBlock,
map: std.AutoHashMapUnmanaged(Ir.Index, *Value),
visited: []bool,

fn init(arena: Allocator, module_name: [:0]const u8, pool: *InternPool, ir: *const Ir) !Compiler {
    if (c.LLVMInitializeNativeTarget() != 0) return error.TargetInitFailed;
    if (c.LLVMInitializeNativeAsmParser() != 0) return error.TargetInitFailed;
    if (c.LLVMInitializeNativeAsmPrinter() != 0) return error.TargetInitFailed;
    const ctx = Context.init();
    errdefer ctx.deinit();

    const module = Module.init(module_name, ctx);
    errdefer module.deinit();
    const builder = Builder.init(ctx);
    errdefer builder.deinit();

    const target_triple = c.LLVMGetDefaultTargetTriple();
    const target = try getTargetFromTriple(target_triple);
    const target_machine = TargetMachine.init(
        target,
        target_triple,
        "generic",
        "",
        .aggressive,
        .static,
        .jit_default,
    );

    const entry_type = ctx.function(ctx.void(), &.{}, false);
    const function = module.addFunction("jit_entry", entry_type);

    // allocate LLVM basic blocks for each of the IR blocks
    const blocks = try arena.alloc(*BasicBlock, ir.blocks.len);
    errdefer arena.free(blocks);
    // entry block is implicitly the 0th ir block
    for (blocks) |*block| block.* = BasicBlock.append(ctx, function, "");

    // used to run dfs without loops
    // TODO: can use a bitmap
    const visited = try arena.alloc(bool, ir.blocks.len);
    errdefer arena.free(visited);

    return .{
        .arena = arena,
        .ctx = ctx,
        .module = module,
        .builder = builder,
        .target_machine = target_machine,
        .function = function,
        .pool = pool,
        .ir = ir,
        .blocks = blocks,
        .map = .{},
        .visited = visited,
    };
}

fn getTargetFromTriple(target_triple: [*c]const u8) !*Target {
    var msg: [*c]u8 = null;
    defer c.LLVMDisposeMessage(msg);
    var target: *Target = undefined;
    const rc = c.LLVMGetTargetFromTriple(target_triple, @ptrCast(&target), &msg);
    if (rc != 0) {
        std.debug.print("Get target error: {s}\n", .{msg});
        return error.GetTargetFailed;
    }

    return target;
}

fn compileBlocksDfs(self: *Compiler, block: Ir.BlockIndex) !void {
    const block_index = @intFromEnum(block);
    if (self.visited[block_index]) return;
    self.visited[block_index] = true;

    try self.compileBlock(block);

    // rather than the linear order of the blocks, following the dag
    // structure prevents use before define (except for phis, which contain
    // back-edges)
    const ir = self.ir;
    const insts: []const Ir.Index = @ptrCast(ir.extraSlice(ir.blocks[block_index].insts));
    const last = insts[insts.len - 1];
    switch (ir.instTag(last)) {
        .ret => {},
        .jmp => {
            const target = ir.instPayload(last).block;
            try self.compileBlocksDfs(target);
        },
        .br => {
            const payload = ir.instPayload(last).unary_extra;
            const branch = ir.extraData(Ir.Inst.Branch, payload.extra);
            try self.compileBlocksDfs(branch.exec_if);
            try self.compileBlocksDfs(branch.exec_else);
        },
        else => unreachable,
    }
}

fn compilePhisDfs(self: *Compiler, block: Ir.BlockIndex) !void {
    const block_index = @intFromEnum(block);
    if (self.visited[block_index]) return;
    self.visited[block_index] = true;

    const ir = self.ir;
    const insts: []const Ir.Index = @ptrCast(ir.extraSlice(ir.blocks[block_index].insts));
    for (insts) |inst| {
        if (ir.instTag(inst) == .phi) self.phiInst(inst);
    }

    // rather than the linear order of the blocks, following the dag
    // structure prevents use before define (except for phis, which contain
    // back-edges)
    const last = insts[insts.len - 1];
    switch (ir.instTag(last)) {
        .ret => {},
        .jmp => {
            const target = ir.instPayload(last).block;
            try self.compilePhisDfs(target);
        },
        .br => {
            const payload = ir.instPayload(last).unary_extra;
            const branch = ir.extraData(Ir.Inst.Branch, payload.extra);
            try self.compilePhisDfs(branch.exec_if);
            try self.compilePhisDfs(branch.exec_else);
        },
        else => unreachable,
    }
}

fn compileBlock(self: *Compiler, block: Ir.BlockIndex) !void {
    const ir = self.ir;
    const block_index = @intFromEnum(block);
    self.builder.positionAtEnd(self.blocks[block_index]);
    const insts: []const Ir.Index = @ptrCast(ir.extraSlice(ir.blocks[block_index].insts));
    try self.map.ensureUnusedCapacity(self.arena, @intCast(insts.len));

    for (insts) |inst| {
        switch (ir.instTag(inst)) {
            .constant => self.constant(inst),
            .ld_global, .st_global => {},
            .arg => self.argInst(inst),
            .builtin => {},
            .itof,
            .ftoi,
            .itob,
            .btoi,
            .neg,
            .binv,
            .lnot,
            => self.unaryOp(inst),
            .any => {},
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
            => self.binaryOp(inst),
            .call => {},
            .ret => self.ret(inst),
            .jmp => self.jmp(inst),
            .br => self.br(inst),
            .phi => self.phiStub(inst),
        }
    }
}

fn constant(self: *Compiler, inst: Ir.Index) void {
    const ip = self.ir.instPayload(inst).ip;
    const value = switch (self.pool.get(ip)) {
        .tv => |tv| switch (tv.ty) {
            .int => self.builder.iconst(self.ctx.int(64), @bitCast(tv.val.int), true),
            .float => self.builder.fconst(self.ctx.float(.double), tv.val.float),
            .bool => self.builder.iconst(self.ctx.int(1), @intFromBool(tv.val.bool), false),
            .nonetype => self.builder.iconst(self.ctx.int(64), 0, false), // TODO: fix this
            else => {
                std.debug.print("{}\n", .{tv});
                unreachable;
            },
        },
        else => unreachable,
    };

    self.map.putAssumeCapacity(inst, value);
}

fn argInst(self: *Compiler, inst: Ir.Index) void {
    const arg = self.ir.instPayload(inst).arg;
    const value = self.function.param(@intCast(arg.position));
    self.map.putAssumeCapacity(inst, value);
}

fn unaryOp(self: *Compiler, inst: Ir.Index) void {
    const unary = self.ir.instPayload(inst).unary;
    const ty = self.pool.get(self.ir.typeOf(unary)).ty;
    const src = self.map.get(unary).?;
    const value = switch (self.ir.instTag(inst)) {
        .itof => self.builder.cast(.sitofp, src, self.ctx.int(64), ""),
        .ftoi => self.builder.cast(.fptosi, src, self.ctx.float(.double), ""),
        .itob => self.builder.cast(.truncate, src, self.ctx.int(1), ""),
        .btoi => self.builder.cast(.zext, src, self.ctx.int(64), ""),
        .neg => switch (ty) {
            .int => self.builder.neg(src, ""),
            .float => self.builder.fneg(src, ""),
            else => unreachable,
        },
        .binv => val: {
            const minus_one = self.builder.iconst(self.ctx.int(64), @bitCast(@as(i64, -1)), true);
            break :val self.builder.binary(.xor, src, minus_one, "");
        },
        .lnot => val: {
            const zero = self.builder.iconst(self.ctx.int(64), 0, false);
            break :val self.builder.icmp(.eq, src, zero, "");
        },
        else => unreachable,
    };
    self.map.putAssumeCapacity(inst, value);
}

fn binaryOp(self: *Compiler, inst: Ir.Index) void {
    const binary = self.ir.instPayload(inst).binary;
    const ty = self.pool.get(self.ir.typeOf(binary.l)).ty;
    const l = self.map.get(binary.l).?;
    const r = self.map.get(binary.r).?;

    const value = switch (ty) {
        .int => switch (self.ir.instTag(inst)) {
            .add => self.builder.binary(.add, l, r, ""),
            .sub => self.builder.binary(.sub, l, r, ""),
            .mul => self.builder.binary(.mul, l, r, ""),
            .div => self.builder.binary(.sdiv, l, r, ""),
            .mod => self.builder.binary(.srem, l, r, ""),
            .pow => unreachable, // TODO: runtime
            .bor => self.builder.binary(.@"or", l, r, ""),
            .band => self.builder.binary(.@"and", l, r, ""),
            .bxor => self.builder.binary(.xor, l, r, ""),
            .sll => self.builder.binary(.sll, l, r, ""),
            .sra => self.builder.binary(.sra, l, r, ""),
            .eq => self.builder.icmp(.eq, l, r, ""),
            .ne => self.builder.icmp(.ne, l, r, ""),
            .lt => self.builder.icmp(.slt, l, r, ""),
            .gt => self.builder.icmp(.sgt, l, r, ""),
            .le => self.builder.icmp(.sle, l, r, ""),
            .ge => self.builder.icmp(.sge, l, r, ""),
            else => unreachable,
        },
        .float => undefined,
        // .float => switch (self.ir.instTag(inst)) {
        //     .add => self.builder.binary(.fadd, l, r, ""),
        //     .sub => self.builder.binary(.fsub, l, r, ""),
        //     .mul => self.builder.binary(.fmul, l, r, ""),
        //     .div => self.builder.binary(.fdiv, l, r, ""),
        //     .mod => self.builder.binary(.frem, l, r, ""),
        //     .pow => unreachable, // TODO: runtime
        //     .lt => self.builder.fcmp(.olt, l, r, ""),
        //     .gt => self.builder.fcmp(.ogt, l, r, ""),
        //     .le => self.builder.fcmp(.ole, l, r, ""),
        //     .ge => self.builder.fcmp(.oge, l, r, ""),
        //     else => unreachable,
        // },
        else => unreachable,
    };
    self.map.putAssumeCapacity(inst, value);
}

fn ret(self: *Compiler, inst: Ir.Index) void {
    // const unary = self.ir.instPayload(inst).unary;
    // const src = self.map.get(unary).?;
    // const value = self.builder.ret(src);
    // TODO: implement return values
    const value = self.builder.ret(null);
    self.map.putAssumeCapacity(inst, value);
}

fn jmp(self: *Compiler, inst: Ir.Index) void {
    const target = self.ir.instPayload(inst).block;
    const block = self.blocks[@intFromEnum(target)];
    _ = self.builder.br(block);
}

fn br(self: *Compiler, inst: Ir.Index) void {
    const payload = self.ir.instPayload(inst).unary_extra;
    const branch = self.ir.extraData(Ir.Inst.Branch, payload.extra);
    const cond = self.map.get(payload.op).?;

    const exec_if = self.blocks[@intFromEnum(branch.exec_if)];
    const exec_else = self.blocks[@intFromEnum(branch.exec_else)];
    _ = self.builder.condBr(cond, exec_if, exec_else);
}

fn phiStub(self: *Compiler, inst: Ir.Index) void {
    const payload = self.ir.instPayload(inst).extra;
    const data = self.ir.extraData(Ir.Inst.Phi, payload);

    const ty = switch (self.pool.get(data.ty).ty) {
        .int => self.ctx.int(64),
        .float => self.ctx.float(.double),
        .bool => self.ctx.int(1),
        .nonetype => self.ctx.int(64), // TODO: fix this
        else => unreachable,
    };
    const phi = self.builder.phi(ty, "");
    self.map.putAssumeCapacity(inst, phi);
}

fn phiInst(self: *Compiler, inst: Ir.Index) void {
    const payload = self.ir.instPayload(inst).extra;
    const data = self.ir.extraData(Ir.Inst.Phi, payload);
    const phi = self.map.get(inst).?;

    const src1 = self.map.get(data.src1).?;
    const block1 = self.blocks[@intFromEnum(data.block1)];
    c.LLVMAddIncoming(@ptrCast(phi), @constCast(@ptrCast(&src1)), @constCast(@ptrCast(&block1)), 1);

    const src2 = self.map.get(data.src2).?;
    const block2 = self.blocks[@intFromEnum(data.block2)];
    c.LLVMAddIncoming(@ptrCast(phi), @constCast(@ptrCast(&src2)), @constCast(@ptrCast(&block2)), 1);
}

fn verify(self: *Compiler) !void {
    var msg: [*c]u8 = null;
    defer c.LLVMDisposeMessage(msg);
    const rc = c.LLVMVerifyModule(@ptrCast(self.module), c.LLVMReturnStatusAction, &msg);

    if (rc != 0) {
        std.debug.print("IR verification error:\n{s}", .{msg});
        c.LLVMDumpModule(@ptrCast(self.module));
        return error.VerifyIrFailed;
    }
}

fn finalize(self: *Compiler, gpa: Allocator) ![]align(@alignOf(u64)) const u8 {
    var memory_buffer: *mvll.MemoryBuffer = undefined;
    var msg: [*c]u8 = null;
    defer c.LLVMDisposeMessage(msg);
    // TODO: there may be a better way to do this, for example performance issues
    // mentioned in mesa: https://gitlab.freedesktop.org/mesa/mesa/-/merge_requests/26336
    const rc = c.LLVMTargetMachineEmitToMemoryBuffer(
        @ptrCast(self.target_machine),
        @ptrCast(self.module),
        c.LLVMAssemblyFile,
        &msg,
        @ptrCast(&memory_buffer),
    );

    if (rc != 0) {
        std.debug.print("Emit error: {s}\n", .{msg});
        return error.EmitFailed;
    }

    defer memory_buffer.deinit();
    const size = memory_buffer.size();
    const out_buffer = try gpa.alignedAlloc(u8, @alignOf(u64), size);
    errdefer gpa.free(out_buffer);

    @memcpy(out_buffer, memory_buffer.start()[0..size]);
    return out_buffer;
}

fn deinit(self: *Compiler) void {
    self.builder.deinit();
    self.module.deinit();
    self.ctx.deinit();
}

pub fn compile(gpa: Allocator, pool: *InternPool, ir: *const Ir) !void {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    var compiler = try Compiler.init(arena.allocator(), "jit", pool, ir);
    defer compiler.deinit();

    const entry: Ir.BlockIndex = @enumFromInt(0);
    @memset(compiler.visited, false);
    try compiler.compileBlocksDfs(entry);
    @memset(compiler.visited, false);
    try compiler.compilePhisDfs(entry);

    try compiler.verify();
    compiler.module.printToStderr();
    try compiler.module.printToFile("/tmp/foo.ll");

    // const buffer = try compiler.finalize(gpa);
    // for (buffer) |byte| std.debug.print("0x{x} ", .{byte});
    // std.debug.print("{s}\n", .{buffer});
    // std.debug.print("\n", .{});
}
