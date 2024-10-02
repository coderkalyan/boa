// Macro assembler for the boa runtime VM. It uses LLVM to emit bytecode handlers at compile time,
// which are then linked statically into the boa executable.

const std = @import("std");
const mvll = @import("mvll/root.zig");
const Bytecode = @import("Bytecode.zig");
const c = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/Target.h");
    @cInclude("llvm-c/TargetMachine.h");
    @cInclude("llvm-c/Analysis.h");
    @cInclude("llvm-c/BitWriter.h");
});

const Allocator = std.mem.Allocator;
const Opcode = Bytecode.Opcode;
const Context = mvll.Context;
const Module = mvll.Module;
const Builder = mvll.Builder;
const BasicBlock = mvll.BasicBlock;
const Type = mvll.Type;
const Value = mvll.Value;
const Target = mvll.Target;
const TargetMachine = mvll.TargetMachine;

const Assembler = struct {
    ctx: *Context,
    module: *Module,
    builder: *Builder,
    // This type depends on target data layout and is cached for convenience
    usize_type: *Type,

    const Error = Allocator.Error || error{TargetInitFailed};
    const HandlerGeneratorFn = *const fn (self: *Assembler) Error!*Value;
    const address_space = 0;
    const jump_table_name = "jump_table";
    const handlers = .{
        .{ .opcode = .ld, .words = 3, .generator = ld, .next = .default },
        .{ .opcode = .ldw, .words = 4, .generator = ldw, .next = .default },
        .{ .opcode = .mov, .words = 3, .generator = mov, .next = .default },
        .{ .opcode = .ineg, .words = 3, .generator = ineg, .next = .default },
        .{ .opcode = .fneg, .words = 3, .generator = fneg, .next = .default },
        .{ .opcode = .binv, .words = 3, .generator = binv, .next = .default },
        .{ .opcode = .lnot, .words = 3, .generator = lnot, .next = .default },
        .{ .opcode = .exit, .words = 1, .generator = exit, .next = .none },
        .{ .opcode = .trap, .words = 1, .generator = trap, .next = .none },
    };

    comptime {
        std.debug.assert(handlers.len == std.meta.tags(Opcode).len);
        for (handlers, std.meta.tags(Opcode)) |handler, opcode| {
            std.debug.assert(handler.opcode == opcode);
        }
    }

    pub const Param = enum(u8) {
        ip = 0,
        fp = 1,
        sp = 2,
        ctx = 3,
    };

    pub fn init(module_name: [:0]const u8) !Assembler {
        if (c.LLVMInitializeNativeTarget() != 0) return error.TargetInitFailed;
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
            .default,
            .default,
        );
        const target_data = c.LLVMCreateTargetDataLayout(@ptrCast(target_machine));
        const usize_type = c.LLVMIntPtrTypeInContext(@ptrCast(ctx), target_data);
        const ptr_size = c.LLVMABISizeOfType(target_data, @ptrCast(ctx.ptr(address_space)));

        declareJumpTable(ctx, module, handlers.len);
        defineDefaultTrapInner(ctx, module, builder, ptr_size);
        inline for (handlers) |handler| {
            declareHandler(
                ctx,
                module,
                ptr_size,
                handler.opcode,
                handler.words,
            );
        }

        return .{
            .ctx = ctx,
            .module = module,
            .builder = builder,
            .usize_type = @ptrCast(usize_type),
        };
    }

    pub fn deinit(assembler: *Assembler) void {
        assembler.builder.deinit();
        assembler.module.deinit();
        assembler.ctx.deinit();
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

    fn handlerType(ctx: *Context) *Type {
        const parameter_types = &.{
            // instruction pointer to the current instruction
            ctx.ptr(address_space),
            // frame pointer to the base of the current stack frame
            ctx.ptr(address_space),
            // stack pointer to the top of the current stack frame
            ctx.ptr(address_space),
            // pointer to global interpreter context (shared across function calls)
            ctx.ptr(address_space),
        };

        return ctx.function(ctx.void(), parameter_types, false);
    }

    fn declareHandler(ctx: *Context, module: *Module, ptr_size: usize, comptime opcode: Opcode, words: usize) void {
        const handler = module.addFunction("interpreter_" ++ @tagName(opcode), handlerType(ctx));

        const nonnull = "nonnull";
        const readonly = "readonly";
        const deref = "dereferenceable";

        // instruction pointer attributes: nonnull, readonly, dereferenceable(words), align 4
        const ip_param_index = @intFromEnum(Param.ip);
        const ip_param = handler.param(ip_param_index);
        c.LLVMAddAttributeAtIndex(@ptrCast(handler), ip_param_index + 1, enumAttribute(ctx, nonnull, null));
        c.LLVMAddAttributeAtIndex(@ptrCast(handler), ip_param_index + 1, enumAttribute(ctx, readonly, null));
        c.LLVMSetParamAlignment(@ptrCast(ip_param), 4);
        const deref_bytes = words * ptr_size;
        c.LLVMAddAttributeAtIndex(@ptrCast(handler), ip_param_index + 1, enumAttribute(ctx, deref, @intCast(deref_bytes)));
        c.LLVMSetValueName2(@ptrCast(ip_param), "ip", "ip".len);

        // frame pointer attributes: nonnull, align 8
        const fp_param_index = @intFromEnum(Param.fp);
        const fp_param = c.LLVMGetParam(@ptrCast(handler), fp_param_index);
        c.LLVMAddAttributeAtIndex(@ptrCast(handler), fp_param_index + 1, enumAttribute(ctx, nonnull, null));
        c.LLVMSetParamAlignment(fp_param, 8);
        c.LLVMSetValueName2(fp_param, "fp", "fp".len);

        // stack pointer attributes: nonnull, align 8
        const sp_param_index = @intFromEnum(Param.sp);
        const sp_param = c.LLVMGetParam(@ptrCast(handler), sp_param_index);
        c.LLVMAddAttributeAtIndex(@ptrCast(handler), sp_param_index + 1, enumAttribute(ctx, nonnull, null));
        c.LLVMSetParamAlignment(sp_param, 8);
        c.LLVMSetValueName2(fp_param, "sp", "sp".len);

        // ctx pointer attributes: nonnull, align 8
        const ctx_param_index = @intFromEnum(Param.ctx);
        const ctx_param = c.LLVMGetParam(@ptrCast(handler), ctx_param_index);
        c.LLVMAddAttributeAtIndex(@ptrCast(handler), ctx_param_index + 1, enumAttribute(ctx, nonnull, null));
        c.LLVMSetParamAlignment(ctx_param, 8);
        c.LLVMSetValueName2(ctx_param, "ctx", "ctx".len);

        // set the calling convention to GHC to avoid callee saved registers
        c.LLVMSetFunctionCallConv(@ptrCast(handler), c.LLVMGHCCallConv);
    }

    fn enumAttribute(ctx: *Context, name: []const u8, val: ?u32) c.LLVMAttributeRef {
        return c.LLVMCreateEnumAttribute(
            @ptrCast(ctx),
            c.LLVMGetEnumAttributeKindForName(name.ptr, @intCast(name.len)),
            val orelse 0,
        );
    }

    fn declareJumpTable(ctx: *Context, module: *Module, count: u32) void {
        const ty = c.LLVMArrayType2(@ptrCast(ctx.ptr(address_space)), count);
        const jump_table = c.LLVMAddGlobal(@ptrCast(module), ty, jump_table_name);
        c.LLVMSetLinkage(jump_table, c.LLVMPrivateLinkage);
        c.LLVMSetUnnamedAddress(jump_table, c.LLVMLocalUnnamedAddr);
    }

    fn defineDefaultTrapInner(ctx: *Context, module: *Module, builder: *Builder, ptr_size: usize) void {
        const handler = module.addFunction("interpreter_trap_inner", handlerType(ctx));
        c.LLVMSetFunctionCallConv(@ptrCast(handler), c.LLVMCCallConv);
        c.LLVMSetLinkage(@ptrCast(handler), c.LLVMExternalWeakLinkage);

        const nonnull = "nonnull";
        const readonly = "readonly";
        const deref = "dereferenceable";

        // instruction pointer attributes: nonnull, readonly, dereferenceable(words), align 4
        const ip_param_index = @intFromEnum(Param.ip);
        const ip_param = handler.param(ip_param_index);
        c.LLVMAddAttributeAtIndex(@ptrCast(handler), ip_param_index + 1, enumAttribute(ctx, nonnull, null));
        c.LLVMAddAttributeAtIndex(@ptrCast(handler), ip_param_index + 1, enumAttribute(ctx, readonly, null));
        c.LLVMSetParamAlignment(@ptrCast(ip_param), 4);
        const deref_bytes = 1 * ptr_size;
        c.LLVMAddAttributeAtIndex(@ptrCast(handler), ip_param_index + 1, enumAttribute(ctx, deref, @intCast(deref_bytes)));
        c.LLVMSetValueName2(@ptrCast(ip_param), "ip", "ip".len);

        // frame pointer attributes: nonnull, align 8
        const fp_param_index = @intFromEnum(Param.fp);
        const fp_param = c.LLVMGetParam(@ptrCast(handler), fp_param_index);
        c.LLVMAddAttributeAtIndex(@ptrCast(handler), fp_param_index + 1, enumAttribute(ctx, nonnull, null));
        c.LLVMSetParamAlignment(fp_param, 8);
        c.LLVMSetValueName2(fp_param, "fp", "fp".len);

        // stack pointer attributes: nonnull, align 8
        const sp_param_index = @intFromEnum(Param.sp);
        const sp_param = c.LLVMGetParam(@ptrCast(handler), sp_param_index);
        c.LLVMAddAttributeAtIndex(@ptrCast(handler), sp_param_index + 1, enumAttribute(ctx, nonnull, null));
        c.LLVMSetParamAlignment(sp_param, 8);
        c.LLVMSetValueName2(fp_param, "sp", "sp".len);

        // ctx pointer attributes: nonnull, align 8
        const ctx_param_index = @intFromEnum(Param.ctx);
        const ctx_param = c.LLVMGetParam(@ptrCast(handler), ctx_param_index);
        c.LLVMAddAttributeAtIndex(@ptrCast(handler), ctx_param_index + 1, enumAttribute(ctx, nonnull, null));
        c.LLVMSetParamAlignment(ctx_param, 8);
        c.LLVMSetValueName2(ctx_param, "ctx", "ctx".len);

        const entry_block = BasicBlock.append(ctx, handler, "entry");
        builder.positionAtEnd(entry_block);
        _ = builder.ret(null);
    }

    pub fn assemble(self: *Assembler) !void {
        var ptrs: [handlers.len]*Value = undefined;
        inline for (handlers, 0..) |handler, i| {
            ptrs[i] = try HandlerGenerator(handler.opcode, handler.words, handler.generator, handler.next)(self);
        }

        const jump_table = c.LLVMGetNamedGlobal(@ptrCast(self.module), jump_table_name);
        const initializer = c.LLVMConstArray2(@ptrCast(self.ctx.ptr(address_space)), @ptrCast(&ptrs), @intCast(ptrs.len));
        c.LLVMSetInitializer(jump_table, initializer);

        try self.entry();
    }

    pub fn entry(self: *Assembler) !void {
        const name = "interpreter_entry";
        const function = self.module.addFunction(name, handlerType(self.ctx));

        const nonnull = "nonnull";
        const readonly = "readonly";

        // instruction pointer attributes: nonnull, readonly, dereferenceable(words), align 4
        const ip_param_index = @intFromEnum(Param.ip);
        const ip_param = function.param(ip_param_index);
        c.LLVMAddAttributeAtIndex(@ptrCast(function), ip_param_index + 1, enumAttribute(self.ctx, nonnull, null));
        c.LLVMAddAttributeAtIndex(@ptrCast(function), ip_param_index + 1, enumAttribute(self.ctx, readonly, null));
        c.LLVMSetParamAlignment(@ptrCast(ip_param), 4);
        c.LLVMSetValueName2(@ptrCast(ip_param), "ip", "ip".len);

        // frame pointer attributes: nonnull, align 8
        const fp_param_index = @intFromEnum(Param.fp);
        const fp_param = c.LLVMGetParam(@ptrCast(function), fp_param_index);
        c.LLVMAddAttributeAtIndex(@ptrCast(function), fp_param_index + 1, enumAttribute(self.ctx, nonnull, null));
        c.LLVMSetParamAlignment(fp_param, 8);
        c.LLVMSetValueName2(fp_param, "fp", "fp".len);

        // stack pointer attributes: nonnull, align 8
        const sp_param_index = @intFromEnum(Param.sp);
        const sp_param = c.LLVMGetParam(@ptrCast(function), sp_param_index);
        c.LLVMAddAttributeAtIndex(@ptrCast(function), sp_param_index + 1, enumAttribute(self.ctx, nonnull, null));
        c.LLVMSetParamAlignment(sp_param, 8);
        c.LLVMSetValueName2(fp_param, "sp", "sp".len);

        // ctx pointer attributes: nonnull, align 8
        const ctx_param_index = @intFromEnum(Param.ctx);
        const ctx_param = c.LLVMGetParam(@ptrCast(function), ctx_param_index);
        c.LLVMAddAttributeAtIndex(@ptrCast(function), ctx_param_index + 1, enumAttribute(self.ctx, nonnull, null));
        c.LLVMSetParamAlignment(ctx_param, 8);
        c.LLVMSetValueName2(ctx_param, "ctx", "ctx".len);

        // set the calling convention to C so it can be called from zig
        c.LLVMSetFunctionCallConv(@ptrCast(function), c.LLVMCCallConv);

        const entry_block = BasicBlock.append(self.ctx, function, "entry");
        self.builder.positionAtEnd(entry_block);

        const ip = self.param(.ip);
        const fp = self.param(.fp);
        const sp = self.param(.sp);
        const ctx = self.param(.ctx);

        const args = .{ ip, fp, sp, ctx };
        const next_opcode = self.builder.load(self.ctx.int(32), ip, "opcode.next");
        // load the handler pointer from the jump table using the opcode
        const jump_table: *Value = @ptrCast(c.LLVMGetNamedGlobal(@ptrCast(self.module), jump_table_name));
        const handler_ptr = self.builder.gep(.inbounds, self.ctx.ptr(address_space), jump_table, &.{next_opcode}, "handler.ptr");
        const handler = self.builder.load(self.ctx.ptr(address_space), handler_ptr, "handler");
        const handler_call = self.builder.call(handlerType(self.ctx), handler, &args, "");
        c.LLVMSetInstructionCallConv(@ptrCast(handler_call), c.LLVMGHCCallConv);
        _ = self.builder.ret(null);
    }

    fn HandlerGenerator(
        comptime opcode: Opcode,
        comptime words: usize,
        comptime body: *const fn (self: *Assembler) Error!void,
        comptime next: enum { none, default },
    ) HandlerGeneratorFn {
        return struct {
            pub fn generator(self: *Assembler) !*Value {
                const name = "interpreter_" ++ @tagName(opcode);
                const handler = self.module.getNamedFunction(name);

                // set the function to internal linkage here, not in the declaration
                // because internal functions need to be defined
                c.LLVMSetLinkage(@ptrCast(handler), c.LLVMInternalLinkage);
                c.LLVMSetUnnamedAddress(@ptrCast(handler), c.LLVMLocalUnnamedAddr);

                const entry_block = BasicBlock.append(self.ctx, handler, "entry");
                self.builder.positionAtEnd(entry_block);
                try @call(.always_inline, body, .{self});

                switch (next) {
                    .none => {},
                    .default => self.tail(self.offset(words)),
                }

                _ = self.builder.ret(null);
                return handler;
            }
        }.generator;
    }

    fn ld(self: *Assembler) !void {
        const imm = self.read(self.offset(2), .sext, "imm");
        self.store(self.read(self.offset(1), .sext, "dst.reg"), imm, .integer, "dst");
    }

    fn ldw(self: *Assembler) !void {
        // read the lower and upper 4 byte chunks
        const lower = self.read(self.offset(2), .zext, "lower");
        const upper = self.read(self.offset(3), .zext, "upper");
        // assemble the complete immediate
        const sll = self.builder.binary(.sll, upper, self.iconst(32), "upper.sll");
        const imm = self.builder.binary(.@"or", lower, sll, "imm");
        self.store(self.read(self.offset(1), .sext, "dst.reg"), imm, .integer, "dst");
    }

    fn mov(self: *Assembler) !void {
        const src = self.load(self.read(self.offset(2), .sext, "src.reg"), .integer, "src");
        self.store(self.read(self.offset(1), .sext, "dst.reg"), src, .integer, "dst");
    }

    fn ineg(self: *Assembler) !void {
        const src = self.load(self.read(self.offset(2), .sext, "src.reg"), .integer, "src");
        const neg = self.builder.neg(src, "ineg");
        self.store(self.read(self.offset(1), .sext, "dst.reg"), neg, .integer, "dst");
    }

    fn fneg(self: *Assembler) !void {
        const src = self.load(self.read(self.offset(2), .sext, "src.reg"), .double, "src");
        const neg = self.builder.fneg(src, "fneg");
        self.store(self.read(self.offset(1), .sext, "dst.reg"), neg, .double, "dst");
    }

    fn binv(self: *Assembler) !void {
        const src = self.load(self.read(self.offset(2), .sext, "src.reg"), .integer, "src");
        const mask = self.iconst(-1);
        const inv = self.builder.binary(.xor, src, mask, "binv");
        self.store(self.read(self.offset(1), .sext, "dst.reg"), inv, .integer, "dst");
    }

    fn lnot(self: *Assembler) !void {
        const src = self.load(self.read(self.offset(2), .sext, "src.reg"), .integer, "src");
        const zero = self.iconst(0);
        const not = self.builder.icmp(.eq, src, zero, "lnot");
        self.store(self.read(self.offset(1), .sext, "dst.reg"), not, .integer, "dst");
    }

    fn exit(self: *Assembler) !void {
        _ = self;
    }

    fn trap(self: *Assembler) !void {
        const ip = self.param(.ip);
        const fp = self.param(.fp);
        const sp = self.param(.sp);
        const ctx = self.param(.ctx);

        // tail call the handler
        const args = .{ ip, fp, sp, ctx };
        const inner = self.module.getNamedFunction("interpreter_trap_inner");
        const handler_call = self.builder.call(handlerType(self.ctx), inner, &args, "");
        c.LLVMSetInstructionCallConv(@ptrCast(handler_call), c.LLVMCCallConv);
    }

    fn param(self: *Assembler, comptime p: Param) *Value {
        const function = self.builder.current().parent();
        return function.param(@intFromEnum(p));
    }

    fn offset(self: *Assembler, value: isize) *Value {
        return self.builder.iconst(self.usize_type, @bitCast(value), true);
    }

    fn iconst(self: *Assembler, value: i64) *Value {
        return self.builder.iconst(self.ctx.int(64), @bitCast(value), true);
    }

    const Extend = enum { none, zext, sext };
    fn read(self: *Assembler, ip_offset: *Value, comptime extend: Extend, comptime name: [:0]const u8) *Value {
        const ip = self.param(.ip);
        const gep = self.builder.gep(.inbounds, self.ctx.int(32), ip, &.{ip_offset}, name ++ ".ptr");
        const raw = self.builder.load(self.ctx.int(32), gep, if (extend == .none) name else name ++ ".i32");
        return switch (extend) {
            .none => raw,
            .zext => self.builder.cast(.zext, raw, self.ctx.int(64), name),
            .sext => self.builder.cast(.sext, raw, self.ctx.int(64), name),
        };
    }

    fn loadBase(self: *Assembler, base: *Value, ip_offset: *Value, comptime ty: Type.Kind, comptime name: [:0]const u8) *Value {
        const gep = self.builder.gep(.inbounds, self.ctx.int(64), base, &.{ip_offset}, name ++ ".ptr");
        const value = self.builder.load(self.ctx.int(64), gep, if (ty == .integer) name else name ++ ".i64");
        c.LLVMSetAlignment(@ptrCast(value), @alignOf(u64));
        return switch (ty) {
            .integer => value,
            .double => self.builder.cast(.bitcast, value, self.ctx.float(.double), name),
            .pointer => self.builder.cast(.inttoptr, value, self.ctx.ptr(address_space), name),
            else => unreachable,
        };
    }

    fn load(self: *Assembler, ip_offset: *Value, comptime ty: Type.Kind, comptime name: [:0]const u8) *Value {
        const fp = self.param(.fp);
        return self.loadBase(fp, ip_offset, ty, name);
    }

    fn storeBase(self: *Assembler, base: *Value, ip_offset: *Value, value: *Value, comptime ty: Type.Kind, comptime name: [:0]const u8) void {
        const gep = self.builder.gep(.inbounds, self.ctx.int(64), base, &.{ip_offset}, name ++ ".ptr");
        const value_int = switch (ty) {
            .integer => value,
            .double => self.builder.cast(.bitcast, value, self.ctx.int(64), name ++ ".int"),
            .pointer => self.builder.cast(.ptrtoint, value, self.ctx.int(64), name ++ ".int"),
            else => unreachable,
        };
        const store_inst = self.builder.store(value_int, gep);
        c.LLVMSetAlignment(@ptrCast(store_inst), @alignOf(u64));
    }

    fn store(self: *Assembler, ip_offset: *Value, value: *Value, comptime ty: Type.Kind, comptime name: [:0]const u8) void {
        const fp = self.param(.fp);
        self.storeBase(fp, ip_offset, value, ty, name);
    }

    fn tail(self: *Assembler, ip_offset: *Value) void {
        const ip = self.param(.ip);
        const fp = self.param(.fp);
        const sp = self.param(.sp);
        const ctx = self.param(.ctx);

        // calculate pointer to the start of the next instruction
        // and use it to read the opcode of the next instruction
        const next_ip = self.builder.gep(.inbounds, self.ctx.int(32), ip, &.{ip_offset}, "ip.next");
        const next_opcode = self.builder.load(self.ctx.int(32), next_ip, "opcode.next");

        // load the handler pointer from the jump table using the opcode
        const jump_table: *Value = @ptrCast(c.LLVMGetNamedGlobal(@ptrCast(self.module), jump_table_name));
        const handler_ptr = self.builder.gep(.inbounds, self.ctx.ptr(address_space), jump_table, &.{next_opcode}, "handler.ptr");
        const handler = self.builder.load(self.ctx.ptr(address_space), handler_ptr, "handler");

        // tail call the handler
        const args = .{ next_ip, fp, sp, ctx };
        const handler_call = self.builder.call(handlerType(self.ctx), handler, &args, "");
        c.LLVMSetTailCallKind(@ptrCast(handler_call), c.LLVMTailCallKindMustTail);
        c.LLVMSetInstructionCallConv(@ptrCast(handler_call), c.LLVMGHCCallConv);
    }

    pub fn finalize(self: *Assembler, bc_name: [:0]const u8) !void {
        try self.verify();

        const rc = c.LLVMWriteBitcodeToFile(@ptrCast(self.module), bc_name);
        if (rc != 0) return error.WriteBitcodeFailed;
    }

    fn verify(self: *Assembler) !void {
        var msg: [*c]u8 = null;
        defer c.LLVMDisposeMessage(msg);
        const rc = c.LLVMVerifyModule(@ptrCast(self.module), c.LLVMReturnStatusAction, &msg);

        if (rc != 0) {
            std.debug.print("IR verification error:\n{s}", .{msg});
            c.LLVMDumpModule(@ptrCast(self.module));
            return error.VerifyIrFailed;
        }
    }
};

test "read ir" {
    var assembler = try Assembler.init("interpreter");
    const function_type = assembler.ctx.function(
        assembler.ctx.void(),
        &.{
            assembler.ctx.ptr(Assembler.address_space), // ip
            assembler.ctx.ptr(Assembler.address_space), // fp
            assembler.ctx.ptr(Assembler.address_space), // sp
            assembler.ctx.ptr(Assembler.address_space), // ctx
        },
        false,
    );

    const read_none = assembler.module.addFunction("read_none", function_type);
    var entry = BasicBlock.append(assembler.ctx, read_none, "entry");
    assembler.builder.positionAtEnd(entry);
    var offset = assembler.builder.iconst(assembler.ctx.int(32), 2, false);
    _ = assembler.read(offset, .none, "read.none");

    const read_zext = assembler.module.addFunction("read_zext", function_type);
    entry = BasicBlock.append(assembler.ctx, read_zext, "entry");
    assembler.builder.positionAtEnd(entry);
    offset = assembler.builder.iconst(assembler.ctx.int(32), 2, false);
    _ = assembler.read(offset, .zext, "read.zext");

    const read_sext = assembler.module.addFunction("read_sext", function_type);
    entry = BasicBlock.append(assembler.ctx, read_sext, "entry");
    assembler.builder.positionAtEnd(entry);
    offset = assembler.builder.iconst(assembler.ctx.int(32), 2, false);
    _ = assembler.read(offset, .sext, "read.sext");

    {
        const actual = std.mem.span(read_none.printToString());
        const expected =
            \\define void @read_none(ptr %0, ptr %1, ptr %2, ptr %3) {
            \\entry:
            \\  %read.none.ptr = getelementptr inbounds i32, ptr %0, i32 2
            \\  %read.none = load i32, ptr %read.none.ptr, align 4
            \\}
            \\
        ;
        try std.testing.expectEqualSlices(u8, expected, actual);
    }

    {
        const actual = std.mem.span(read_zext.printToString());
        const expected =
            \\define void @read_zext(ptr %0, ptr %1, ptr %2, ptr %3) {
            \\entry:
            \\  %read.zext.ptr = getelementptr inbounds i32, ptr %0, i32 2
            \\  %read.zext.i32 = load i32, ptr %read.zext.ptr, align 4
            \\  %read.zext = zext i32 %read.zext.i32 to i64
            \\}
            \\
        ;
        try std.testing.expectEqualSlices(u8, expected, actual);
    }

    {
        const actual = std.mem.span(read_sext.printToString());
        const expected =
            \\define void @read_sext(ptr %0, ptr %1, ptr %2, ptr %3) {
            \\entry:
            \\  %read.sext.ptr = getelementptr inbounds i32, ptr %0, i32 2
            \\  %read.sext.i32 = load i32, ptr %read.sext.ptr, align 4
            \\  %read.sext = sext i32 %read.sext.i32 to i64
            \\}
            \\
        ;
        try std.testing.expectEqualSlices(u8, expected, actual);
    }
}

test "load ir" {
    var assembler = try Assembler.init("interpreter");
    const function_type = assembler.ctx.function(
        assembler.ctx.void(),
        &.{
            assembler.ctx.ptr(Assembler.address_space), // ip
            assembler.ctx.ptr(Assembler.address_space), // fp
            assembler.ctx.ptr(Assembler.address_space), // sp
            assembler.ctx.ptr(Assembler.address_space), // ctx
        },
        false,
    );

    {
        const load_base = assembler.module.addFunction("load_base", function_type);
        const entry = BasicBlock.append(assembler.ctx, load_base, "entry");
        assembler.builder.positionAtEnd(entry);
        const fp = assembler.param(.fp);
        const offset = assembler.builder.iconst(assembler.ctx.int(32), 2, false);
        _ = assembler.loadBase(fp, offset, .integer, "load");

        const actual = std.mem.span(load_base.printToString());
        const expected =
            \\define void @load_base(ptr %0, ptr %1, ptr %2, ptr %3) {
            \\entry:
            \\  %load.ptr = getelementptr inbounds i64, ptr %1, i32 2
            \\  %load = load i64, ptr %load.ptr, align 8
            \\}
            \\
        ;
        try std.testing.expectEqualSlices(u8, expected, actual);
    }

    {
        const load_fp = assembler.module.addFunction("load_fp", function_type);
        const entry = BasicBlock.append(assembler.ctx, load_fp, "entry");
        assembler.builder.positionAtEnd(entry);
        const offset = assembler.builder.iconst(assembler.ctx.int(32), 2, false);
        _ = assembler.load(offset, .integer, "load");

        const actual = std.mem.span(load_fp.printToString());
        const expected =
            \\define void @load_fp(ptr %0, ptr %1, ptr %2, ptr %3) {
            \\entry:
            \\  %load.ptr = getelementptr inbounds i64, ptr %1, i32 2
            \\  %load = load i64, ptr %load.ptr, align 8
            \\}
            \\
        ;
        try std.testing.expectEqualSlices(u8, expected, actual);
    }

    {
        const load_float = assembler.module.addFunction("load_float", function_type);
        const entry = BasicBlock.append(assembler.ctx, load_float, "entry");
        assembler.builder.positionAtEnd(entry);
        const offset = assembler.builder.iconst(assembler.ctx.int(32), 2, false);
        _ = assembler.load(offset, .double, "load");

        const actual = std.mem.span(load_float.printToString());
        const expected =
            \\define void @load_float(ptr %0, ptr %1, ptr %2, ptr %3) {
            \\entry:
            \\  %load.ptr = getelementptr inbounds i64, ptr %1, i32 2
            \\  %load.i64 = load i64, ptr %load.ptr, align 8
            \\  %load = bitcast i64 %load.i64 to double
            \\}
            \\
        ;
        try std.testing.expectEqualSlices(u8, expected, actual);
    }

    {
        const load_ptr = assembler.module.addFunction("load_ptr", function_type);
        const entry = BasicBlock.append(assembler.ctx, load_ptr, "entry");
        assembler.builder.positionAtEnd(entry);
        const offset = assembler.builder.iconst(assembler.ctx.int(32), 2, false);
        _ = assembler.load(offset, .pointer, "load");

        const actual = std.mem.span(load_ptr.printToString());
        const expected =
            \\define void @load_ptr(ptr %0, ptr %1, ptr %2, ptr %3) {
            \\entry:
            \\  %load.ptr = getelementptr inbounds i64, ptr %1, i32 2
            \\  %load.i64 = load i64, ptr %load.ptr, align 8
            \\  %load = inttoptr i64 %load.i64 to ptr
            \\}
            \\
        ;
        try std.testing.expectEqualSlices(u8, expected, actual);
    }
}

test "store ir" {
    var assembler = try Assembler.init("interpreter");
    const function_type = assembler.ctx.function(
        assembler.ctx.void(),
        &.{
            assembler.ctx.ptr(Assembler.address_space), // ip
            assembler.ctx.ptr(Assembler.address_space), // fp
            assembler.ctx.ptr(Assembler.address_space), // sp
            assembler.ctx.ptr(Assembler.address_space), // ctx
        },
        false,
    );

    {
        const store_base = assembler.module.addFunction("store_base", function_type);
        const entry = BasicBlock.append(assembler.ctx, store_base, "entry");
        assembler.builder.positionAtEnd(entry);
        const fp = assembler.param(.fp);
        const value = assembler.builder.iconst(assembler.ctx.int(64), 100, false);
        const offset = assembler.builder.iconst(assembler.ctx.int(32), 2, false);
        _ = assembler.storeBase(fp, offset, value, .integer, "store");

        const actual = std.mem.span(store_base.printToString());
        const expected =
            \\define void @store_base(ptr %0, ptr %1, ptr %2, ptr %3) {
            \\entry:
            \\  %store.ptr = getelementptr inbounds i64, ptr %1, i32 2
            \\  store i64 100, ptr %store.ptr, align 8
            \\}
            \\
        ;
        try std.testing.expectEqualSlices(u8, expected, actual);
    }

    {
        const store_fp = assembler.module.addFunction("store_fp", function_type);
        const entry = BasicBlock.append(assembler.ctx, store_fp, "entry");
        assembler.builder.positionAtEnd(entry);
        const value = assembler.builder.iconst(assembler.ctx.int(64), 100, false);
        const offset = assembler.builder.iconst(assembler.ctx.int(32), 2, false);
        _ = assembler.store(offset, value, .integer, "store");

        const actual = std.mem.span(store_fp.printToString());
        const expected =
            \\define void @store_fp(ptr %0, ptr %1, ptr %2, ptr %3) {
            \\entry:
            \\  %store.ptr = getelementptr inbounds i64, ptr %1, i32 2
            \\  store i64 100, ptr %store.ptr, align 8
            \\}
            \\
        ;
        try std.testing.expectEqualSlices(u8, expected, actual);
    }

    {
        const store_float = assembler.module.addFunction("store_float", function_type);
        const entry = BasicBlock.append(assembler.ctx, store_float, "entry");
        assembler.builder.positionAtEnd(entry);
        const value = assembler.builder.fconst(assembler.ctx.float(.double), 100.0);
        const offset = assembler.builder.iconst(assembler.ctx.int(32), 2, false);
        _ = assembler.store(offset, value, .double, "store");

        const actual = std.mem.span(store_float.printToString());
        const expected =
            \\define void @store_float(ptr %0, ptr %1, ptr %2, ptr %3) {
            \\entry:
            \\  %store.ptr = getelementptr inbounds i64, ptr %1, i32 2
            \\  store i64 4636737291354636288, ptr %store.ptr, align 8
            \\}
            \\
        ;
        try std.testing.expectEqualSlices(u8, expected, actual);
    }

    {
        const store_ptr = assembler.module.addFunction("store_ptr", function_type);
        const entry = BasicBlock.append(assembler.ctx, store_ptr, "entry");
        assembler.builder.positionAtEnd(entry);
        const value = assembler.builder.iconst(assembler.ctx.int(64), 0, false);
        const ptr = assembler.builder.cast(.inttoptr, value, assembler.ctx.ptr(Assembler.address_space), "value");
        const offset = assembler.builder.iconst(assembler.ctx.int(32), 2, false);
        _ = assembler.store(offset, ptr, .pointer, "store");

        const actual = std.mem.span(store_ptr.printToString());
        const expected =
            \\define void @store_ptr(ptr %0, ptr %1, ptr %2, ptr %3) {
            \\entry:
            \\  %store.ptr = getelementptr inbounds i64, ptr %1, i32 2
            \\  store i64 0, ptr %store.ptr, align 8
            \\}
            \\
        ;
        try std.testing.expectEqualSlices(u8, expected, actual);
    }
}

test "tail call ir" {
    var assembler = try Assembler.init("interpreter");
    const function_type = assembler.ctx.function(
        assembler.ctx.void(),
        &.{
            assembler.ctx.ptr(Assembler.address_space), // ip
            assembler.ctx.ptr(Assembler.address_space), // fp
            assembler.ctx.ptr(Assembler.address_space), // sp
            assembler.ctx.ptr(Assembler.address_space), // ctx
        },
        false,
    );

    {
        const tail_call = assembler.module.addFunction("tail_call", function_type);
        const entry = BasicBlock.append(assembler.ctx, tail_call, "entry");
        assembler.builder.positionAtEnd(entry);
        assembler.tail(assembler.offset(3));

        const actual = std.mem.span(tail_call.printToString());
        const expected =
            \\define void @tail_call(ptr %0, ptr %1, ptr %2, ptr %3) {
            \\entry:
            \\  %ip.next = getelementptr inbounds i32, ptr %0, i64 3
            \\  %opcode.next = load i32, ptr %ip.next, align 4
            \\  %handler.ptr = getelementptr inbounds ptr, ptr @jump_table, i32 %opcode.next
            \\  %handler = load ptr, ptr %handler.ptr, align 8
            \\  musttail call ghccc void %handler(ptr %ip.next, ptr %1, ptr %2, ptr %3)
            \\}
            \\
        ;
        try std.testing.expectEqualSlices(u8, expected, actual);
    }
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const args = try std.process.argsAlloc(arena.allocator());

    var assembler = try Assembler.init("interpreter");
    try assembler.assemble();
    // assembler.module.printToStderr();
    try assembler.finalize(args[1]);
}
