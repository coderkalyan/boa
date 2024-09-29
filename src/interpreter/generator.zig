const std = @import("std");
const BuiltinIndex = @import("builtins.zig").BuiltinIndex;
const c = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/Error.h");
    @cInclude("llvm-c/Analysis.h");
    @cInclude("llvm-c/BitWriter.h");
    @cInclude("llvm-c/TargetMachine.h");
    @cInclude("llvm-c/Target.h");
});

const Allocator = std.mem.Allocator;
const Error = Allocator.Error | Generator.Error;

const Generator = struct {
    context: Context,
    module: Module,
    builder: Builder,
    target_machine: TargetMachine,
    target_data: TargetData,
    // type of each wordcode atom
    word_type: Type,
    // type of array index
    usize_type: Type,
    // type of opaque pointer
    ptr_type: Type,
    // type of f64 (double precision float)
    f64_type: Type,
    // type of bool (u1)
    bool_type: Type,
    // type of bytecode handler function
    handler_type: Type,
    // list of builtins
    builtins: []const Builtin,

    const Context = c.LLVMContextRef;
    const Module = c.LLVMModuleRef;
    const Builder = c.LLVMBuilderRef;
    const Type = c.LLVMTypeRef;
    const Value = c.LLVMValueRef;
    const Attribute = c.LLVMAttributeRef;
    const Target = c.LLVMTargetRef;
    const TargetMachine = c.LLVMTargetMachineRef;
    const TargetData = c.LLVMTargetDataRef;
    const HandlerGeneratorFn = *const fn (self: *Generator) Generator.Error!Value;
    const Builtin = struct { ty: Type, ptr: Value };
    const Error = error{
        WriteBitcodeFailed,
        VerifyIrFailed,
    };

    const address_space = 0;
    const ip_param_index = 0;
    const fp_param_index = 1;
    const sp_param_index = 2;
    const ctx_param_index = 3;

    pub fn init(arena: Allocator, module_name: [:0]const u8) !Generator {
        if (c.LLVMInitializeNativeTarget() != 0) return error.TargetInitFailed;

        const context = c.LLVMContextCreate();
        errdefer c.LLVMContextDispose(context);
        const module = c.LLVMModuleCreateWithNameInContext(module_name, context);
        const builder = c.LLVMCreateBuilderInContext(context);

        const target_triple = c.LLVMGetDefaultTargetTriple();
        const target = try getTargetFromTriple(target_triple);
        const target_machine = c.LLVMCreateTargetMachine(target, target_triple, "generic", "", c.LLVMCodeGenLevelAggressive, c.LLVMRelocDefault, c.LLVMCodeModelDefault);
        const target_data = c.LLVMCreateTargetDataLayout(target_machine);

        const word_type = c.LLVMInt32TypeInContext(context);
        const usize_type = c.LLVMIntPtrTypeInContext(context, target_data);
        const ptr_type = c.LLVMPointerTypeInContext(context, address_space);
        const f64_type = c.LLVMDoubleTypeInContext(context);
        const bool_type = c.LLVMInt1TypeInContext(context);
        const handler_type = handlerType(context);

        const builtins = try declareBuiltins(arena, context, module);

        return .{
            .context = context,
            .module = module,
            .builder = builder,
            .target_machine = target_machine,
            .target_data = target_data,
            .word_type = word_type,
            .usize_type = usize_type,
            .ptr_type = ptr_type,
            .f64_type = f64_type,
            .bool_type = bool_type,
            .handler_type = handler_type,
            .builtins = builtins,
        };
    }

    fn getTargetFromTriple(target_triple: [*c]const u8) !Target {
        var msg: [*c]u8 = null;
        defer c.LLVMDisposeMessage(msg);
        var target: Target = null;
        const rc = c.LLVMGetTargetFromTriple(target_triple, &target, &msg);
        if (rc != 0) {
            std.debug.print("Get target error: {s}\n", .{msg});
            return error.GetTargetFailed;
        }

        return target;
    }

    fn declareBuiltins(arena: Allocator, context: Context, module: Module) ![]const Builtin {
        const ptr_type = c.LLVMPointerTypeInContext(context, address_space);
        const u32_type = c.LLVMInt32TypeInContext(context);
        const u64_type = c.LLVMInt64TypeInContext(context);
        const void_type = c.LLVMVoidTypeInContext(context);

        var builtins = std.ArrayList(Builtin).init(arena);
        const tvs = &.{
            .{ .push_args, .{ ptr_type, u64_type, ptr_type, ptr_type }, void_type },
            .{ .eval_callable, .{ ptr_type, ptr_type, ptr_type }, ptr_type },
            .{ .trap, .{u32_type}, void_type },
            .{ .attr_index_or_panic, .{ ptr_type, u32_type }, u64_type },
            .{ .attr_index_or_insert, .{ ptr_type, u32_type }, u64_type },
            .{ .load_index, .{ ptr_type, u64_type }, u64_type },
            .{ .store_index, .{ ptr_type, u64_type, u64_type }, void_type },
            .{ .pint, .{u64_type}, void_type },
        };
        try builtins.ensureTotalCapacity(tvs.len);
        inline for (comptime std.meta.tags(BuiltinIndex), tvs) |tag, tv| {
            std.debug.assert(tag == tv[0]);
            const ty = functionType(&tv[1], tv[2]);
            builtins.appendAssumeCapacity(.{
                .ty = ty,
                .ptr = c.LLVMAddFunction(module, "rt_" ++ @tagName(tag), ty),
            });
        }

        return builtins.toOwnedSlice();
    }

    fn builtinDecl(self: *Generator, comptime index: BuiltinIndex) Builtin {
        return self.builtins[@intFromEnum(index)];
    }

    fn functionType(parameter_types: []const Type, return_type: Type) Type {
        return c.LLVMFunctionType(
            return_type,
            @constCast(parameter_types.ptr),
            @intCast(parameter_types.len),
            0,
        );
    }

    fn handlerType(context: Context) Type {
        const parameter_types: []const Type = &.{
            // instruction pointer to the current instruction
            c.LLVMPointerTypeInContext(context, address_space),
            // frame pointer to the base of the current stack frame
            c.LLVMPointerTypeInContext(context, address_space),
            // stack pointer to the top of the current stack frame
            c.LLVMPointerTypeInContext(context, address_space),
            // pointer to global interpreter context (shared across function calls)
            c.LLVMPointerTypeInContext(context, address_space),
        };
        const return_type = c.LLVMVoidTypeInContext(context);
        const function_type = c.LLVMFunctionType(
            return_type,
            @constCast(parameter_types.ptr),
            @intCast(parameter_types.len),
            0,
        );

        return function_type;
    }

    fn addHandler(self: *Generator, comptime name: [:0]const u8, words: u32) !Value {
        // start by creating the base handler declaration
        const handler = c.LLVMAddFunction(
            self.module,
            "interpreter_" ++ name,
            self.handler_type,
        );

        const non_null = "nonnull";
        const read_only = "readonly";
        const deref = "dereferenceable";

        // instruction pointer attributes: nonnull, readonly, dereferenceable(words), align 4
        const ip_param = c.LLVMGetParam(handler, ip_param_index);
        c.LLVMAddAttributeAtIndex(handler, ip_param_index + 1, self.enumAttribute(non_null, null));
        c.LLVMAddAttributeAtIndex(handler, ip_param_index + 1, self.enumAttribute(read_only, null));
        c.LLVMSetParamAlignment(ip_param, 4);
        const deref_bytes = words * c.LLVMABISizeOfType(self.target_data, self.ptr_type);
        c.LLVMAddAttributeAtIndex(handler, ip_param_index + 1, self.enumAttribute(deref, @intCast(deref_bytes)));
        c.LLVMSetValueName2(ip_param, "ip", "ip".len);

        // frame pointer attributes: nonnull, align 8
        const fp_param = c.LLVMGetParam(handler, fp_param_index);
        c.LLVMAddAttributeAtIndex(handler, fp_param_index + 1, self.enumAttribute(non_null, null));
        c.LLVMSetParamAlignment(fp_param, 8);
        c.LLVMSetValueName2(fp_param, "fp", "fp".len);

        // stack pointer attributes: nonnull, align 8
        const sp_param = c.LLVMGetParam(handler, sp_param_index);
        c.LLVMAddAttributeAtIndex(handler, sp_param_index + 1, self.enumAttribute(non_null, null));
        c.LLVMSetParamAlignment(sp_param, 8);
        c.LLVMSetValueName2(fp_param, "sp", "sp".len);

        // ctx pointer attributes: nonnull, align 8
        const ctx_param = c.LLVMGetParam(handler, ctx_param_index);
        c.LLVMAddAttributeAtIndex(handler, ctx_param_index + 1, self.enumAttribute(non_null, null));
        c.LLVMSetParamAlignment(ctx_param, 8);
        c.LLVMSetValueName2(ctx_param, "ctx", "ctx".len);

        // set the function to internal for optimization
        c.LLVMSetLinkage(handler, c.LLVMInternalLinkage);
        c.LLVMSetUnnamedAddress(handler, c.LLVMLocalUnnamedAddr);

        // and set its calling convention to GHC to avoid callee saved registers
        c.LLVMSetFunctionCallConv(handler, c.LLVMGHCCallConv);

        return handler;
    }

    fn enumAttribute(self: *Generator, name: []const u8, val: ?u32) Attribute {
        return c.LLVMCreateEnumAttribute(
            self.context,
            c.LLVMGetEnumAttributeKindForName(
                name.ptr,
                @intCast(name.len),
            ),
            val orelse 0,
        );
    }

    pub fn generate(self: *Generator) !void {
        const generators = .{
            HandlerGenerator("ld", 3, ld),
            HandlerGenerator("ldw", 4, ldw),
            HandlerGenerator("ldg", 4, ldg),
            HandlerGenerator("stg", 3, stg),
            HandlerGenerator("mov", 3, mov),
            HandlerGenerator("itof", 3, itof),
            HandlerGenerator("ftoi", 3, ftoi),
            HandlerGenerator("ineg", 3, ineg),
            HandlerGenerator("fneg", 3, fneg),
            HandlerGenerator("binv", 3, binv),
            HandlerGenerator("lnot", 3, lnot),
            HandlerGenerator("iadd", 4, BinaryIntHandler(c.LLVMAdd)),
            HandlerGenerator("fadd", 4, BinaryFloatHandler(c.LLVMFAdd)),
            HandlerGenerator("isub", 4, BinaryIntHandler(c.LLVMSub)),
            HandlerGenerator("fsub", 4, BinaryFloatHandler(c.LLVMFSub)),
            HandlerGenerator("imul", 4, BinaryIntHandler(c.LLVMMul)),
            HandlerGenerator("fmul", 4, BinaryFloatHandler(c.LLVMFMul)),
            HandlerGenerator("idiv", 4, BinaryIntHandler(c.LLVMSDiv)),
            HandlerGenerator("fdiv", 4, BinaryFloatHandler(c.LLVMFDiv)),
            HandlerGenerator("imod", 4, BinaryIntHandler(c.LLVMSRem)),
            HandlerGenerator("fmod", 4, BinaryFloatHandler(c.LLVMFRem)),
            trap, // ipow
            trap, // fpow
            HandlerGenerator("bor", 4, BinaryIntHandler(c.LLVMOr)),
            HandlerGenerator("band", 4, BinaryIntHandler(c.LLVMAnd)),
            HandlerGenerator("bxor", 4, BinaryIntHandler(c.LLVMXor)),
            HandlerGenerator("sll", 4, BinaryIntHandler(c.LLVMShl)),
            HandlerGenerator("sra", 4, BinaryIntHandler(c.LLVMAShr)),
            HandlerGenerator("ieq", 4, BinaryIntCompareHandler(c.LLVMIntEQ)),
            HandlerGenerator("ine", 4, BinaryIntCompareHandler(c.LLVMIntNE)),
            HandlerGenerator("ilt", 4, BinaryIntCompareHandler(c.LLVMIntSLT)),
            HandlerGenerator("flt", 4, BinaryFloatCompareHandler(c.LLVMRealOLT)),
            HandlerGenerator("igt", 4, BinaryIntCompareHandler(c.LLVMIntSGT)),
            HandlerGenerator("fgt", 4, BinaryFloatCompareHandler(c.LLVMRealOGT)),
            HandlerGenerator("ile", 4, BinaryIntCompareHandler(c.LLVMIntSLE)),
            HandlerGenerator("fle", 4, BinaryFloatCompareHandler(c.LLVMRealOLE)),
            HandlerGenerator("ige", 4, BinaryIntCompareHandler(c.LLVMIntSGE)),
            HandlerGenerator("fge", 4, BinaryFloatCompareHandler(c.LLVMRealOGE)),
            call1,
            call,
            trap,
            trap,
            trap,
            // HandlerGenerator("callrt0", 2, callrt0),
            // HandlerGenerator("callrt1", 2, callrt0),
            // HandlerGenerator("callrt", 2, callrt0),
            HandlerGenerator("pint", 2, pint), // TODO: pint
            trap, // TODO: pfloat
            trap, // TODO: pbool
            trap, // TODO: pstr
            trap, // TODO: strlen
            trap, // TODO: strcat
            trap, // TODO: strrep
            br,
            jmp,
            ret,
            exit,
        };
        self.declareJumpTable(generators.len);
        self.declareRtHandler();

        _ = try self.addHandler("trampoline", 1);
        var handlers: [generators.len]Value = undefined;
        inline for (generators, 0..) |generator, i| {
            handlers[i] = try generator(self);
        }
        self.defineJumpTable(&handlers);

        _ = try self.entry();
        _ = try self.trampoline();
    }

    fn HandlerGenerator(
        comptime name: [:0]const u8,
        comptime words: u32,
        comptime body: *const fn (self: *Generator) Generator.Error!void,
    ) HandlerGeneratorFn {
        return struct {
            pub fn generator(self: *Generator) !Value {
                const handler = try self.addHandler(name, words);
                const entry_block = c.LLVMAppendBasicBlock(handler, "entry");
                c.LLVMPositionBuilderAtEnd(self.builder, entry_block);

                try @call(.always_inline, body, .{self});

                self.tailCallNext(words);
                _ = c.LLVMBuildRetVoid(self.builder);
                return handler;
            }
        }.generator;
    }

    fn entry(self: *Generator) !Value {
        const handler = try self.addHandler("entry", 1);
        c.LLVMSetFunctionCallConv(handler, c.LLVMCCallConv);
        c.LLVMSetLinkage(handler, c.LLVMExternalLinkage);
        c.LLVMSetUnnamedAddr(handler, @intFromBool(false));
        const entry_block = c.LLVMAppendBasicBlock(handler, "entry");
        c.LLVMPositionBuilderAtEnd(self.builder, entry_block);

        // don't actually tail call, since this is a different calling convention
        self.tailCallNext(0);
        const tail_call = c.LLVMGetLastInstruction(entry_block);
        c.LLVMSetTailCallKind(tail_call, c.LLVMTailCallKindNone);

        _ = c.LLVMBuildRetVoid(self.builder);
        return handler;
    }

    fn trampoline(self: *Generator) !Value {
        // const function = try self.addHandler("trampoline", 1);
        const function = c.LLVMGetNamedFunction(self.module, "interpreter_trampoline");
        // this handler has to be visible externally so the runtime can
        // point to it, but is otherwise the same
        c.LLVMSetLinkage(function, c.LLVMExternalLinkage);
        c.LLVMSetUnnamedAddr(function, @intFromBool(false));
        const entry_block = c.LLVMAppendBasicBlock(function, "entry");
        c.LLVMPositionBuilderAtEnd(self.builder, entry_block);

        var fp = c.LLVMGetParam(function, fp_param_index);
        var sp = c.LLVMGetParam(function, sp_param_index);
        const ctx = c.LLVMGetParam(function, ctx_param_index);

        // pop the register count to calculate the stack pointer
        const register_count = self.pop(&sp);

        // the runtime will have pushed the bytecode head to the VM stack,
        // so we can simply pop it and tail call the first opcode
        const ip_int = self.pop(&sp);
        const ip = c.LLVMBuildIntToPtr(self.builder, ip_int, self.ptr_type, "ip");
        // and use it to read the opcode of the next instruction (to lookup the handler)
        const next_opcode = c.LLVMBuildLoad2(self.builder, self.word_type, ip, "opcode.next");

        // load the handler pointer from the jump table using the opcode
        const jump_table = c.LLVMGetNamedGlobal(self.module, "jump_table");
        const handler_ptr = c.LLVMBuildInBoundsGEP2(self.builder, self.ptr_type, jump_table, @constCast(&next_opcode), 1, "handler.ptr");
        const handler = c.LLVMBuildLoad2(self.builder, self.ptr_type, handler_ptr, "handler");

        // tail call the handler
        fp = sp;
        sp = c.LLVMBuildInBoundsGEP2(self.builder, self.usize_type, sp, @constCast(&register_count), 1, "sp");
        const args: []const Value = &.{ ip, fp, sp, ctx };
        const handler_call = c.LLVMBuildCall2(self.builder, self.handler_type, handler, @constCast(args.ptr), @intCast(args.len), "");
        c.LLVMSetTailCallKind(handler_call, c.LLVMTailCallKindMustTail);
        c.LLVMSetInstructionCallConv(handler_call, c.LLVMGHCCallConv);

        _ = c.LLVMBuildRetVoid(self.builder);
        return handler;
    }

    fn ld(self: *Generator) !void {
        const immediate = self.read(2, .sext, "imm");
        self.storeOperand(1, immediate, "dst");
    }

    fn ldw(self: *Generator) !void {
        // read the lower and upper 4 byte chunks
        const lower = self.read(2, .zext, "lower");
        const upper = self.read(3, .zext, "upper");
        // assemble the complete immediate
        const four_bytes = c.LLVMConstInt(self.usize_type, 32, @intFromBool(false));
        const shl = c.LLVMBuildBinOp(self.builder, c.LLVMShl, upper, four_bytes, "upper.sll");
        const immediate = c.LLVMBuildBinOp(self.builder, c.LLVMOr, lower, shl, "imm");
        // and store the assembled immediate on stack
        self.storeOperand(1, immediate, "dst");
    }

    fn ldg(self: *Generator) !void {
        const insert_block = c.LLVMGetInsertBlock(self.builder);
        const handler = c.LLVMGetBasicBlockParent(insert_block);
        const ctx = c.LLVMGetParam(handler, ctx_param_index);
        // read the intern pool index of the attribute
        const attr = self.read(2, .none, "attr");
        // call the runtime to look it up and store in the destination register
        const index = self.callRuntime(.attr_index_or_panic, &.{ ctx, attr });
        const value = self.callRuntime(.load_index, &.{ ctx, index });
        self.storeOperand(1, value, "dst");
    }

    fn stg(self: *Generator) !void {
        const insert_block = c.LLVMGetInsertBlock(self.builder);
        const handler = c.LLVMGetBasicBlockParent(insert_block);
        const ctx = c.LLVMGetParam(handler, ctx_param_index);
        // read the intern pool index of the attribute
        const attr = self.read(2, .none, "attr");
        // call the runtime to look it up
        const index = self.callRuntime(.attr_index_or_insert, &.{ ctx, attr });
        const value = self.loadOperand(1, "src");
        self.callRuntimeVoid(.store_index, &.{ ctx, index, value });
    }

    fn mov(self: *Generator) !void {
        const src = self.loadOperand(2, "src");
        self.storeOperand(1, src, "dst");
    }

    fn itof(self: *Generator) !void {
        const src = self.loadOperand(2, "src");
        const conversion = c.LLVMBuildSIToFP(self.builder, src, self.f64_type, "itof");
        const bitcast = c.LLVMBuildBitCast(self.builder, conversion, self.usize_type, "bitcast");
        self.storeOperand(1, bitcast, "dst");
    }

    fn ftoi(self: *Generator) !void {
        const src_int = self.loadOperand(2, "src.int");
        const src = c.LLVMBuildBitCast(self.builder, src_int, self.f64_type, "src");
        const conversion = c.LLVMBuildFPToSI(self.builder, src, self.usize_type, "ftoi");
        self.storeOperand(1, conversion, "dst");
    }

    fn ineg(self: *Generator) !void {
        const src = self.loadOperand(2, "src");
        const neg = c.LLVMBuildNUWNeg(self.builder, src, "ineg");
        self.storeOperand(1, neg, "dst");
    }

    fn fneg(self: *Generator) !void {
        const src_int = self.loadOperand(2, "src.int");
        const src = c.LLVMBuildBitCast(self.builder, src_int, self.f64_type, "src");
        const neg = c.LLVMBuildFNeg(self.builder, src, "fneg");
        const neg_int = c.LLVMBuildBitCast(self.builder, neg, self.usize_type, "fneg.int");
        self.storeOperand(1, neg_int, "dst");
    }

    fn binv(self: *Generator) !void {
        const src = self.loadOperand(2, "src");
        const mask = c.LLVMConstInt(self.usize_type, @bitCast(@as(i64, -1)), @intFromBool(true));
        const inv = c.LLVMBuildXor(self.builder, src, mask, "binv");
        self.storeOperand(1, inv, "dst");
    }

    fn lnot(self: *Generator) !void {
        const src_int = self.loadOperand(2, "src.int");
        const src = c.LLVMBuildTruncOrBitCast(self.builder, src_int, self.bool_type, "src");
        const zero = c.LLVMConstInt(self.bool_type, 0, @intFromBool(false));
        const not = c.LLVMBuildICmp(self.builder, c.LLVMIntEQ, src, zero, "lnot");
        self.storeOperand(1, not, "dst");
    }

    fn BinaryIntHandler(comptime opcode: c.LLVMOpcode) *const fn (self: *Generator) Generator.Error!void {
        return struct {
            pub fn generator(self: *Generator) Generator.Error!void {
                const src1 = self.loadOperand(2, "src1");
                const src2 = self.loadOperand(3, "src2");
                const op = c.LLVMBuildBinOp(self.builder, opcode, src1, src2, "binary");
                self.storeOperand(1, op, "dst");
            }
        }.generator;
    }

    fn BinaryFloatHandler(comptime opcode: c.LLVMOpcode) *const fn (self: *Generator) Generator.Error!void {
        return struct {
            pub fn generator(self: *Generator) Generator.Error!void {
                const src1_int = self.loadOperand(2, "src1.int");
                const src1 = c.LLVMBuildBitCast(self.builder, src1_int, self.f64_type, "src1");
                const src2_int = self.loadOperand(3, "src2.int");
                const src2 = c.LLVMBuildBitCast(self.builder, src2_int, self.f64_type, "src2");
                const op = c.LLVMBuildBinOp(self.builder, opcode, src1, src2, "binary");
                const op_int = c.LLVMBuildBitCast(self.builder, op, self.usize_type, "binary.int");
                self.storeOperand(1, op_int, "dst");
            }
        }.generator;
    }

    fn BinaryIntCompareHandler(comptime predicate: c.LLVMIntPredicate) *const fn (self: *Generator) Generator.Error!void {
        return struct {
            pub fn generator(self: *Generator) Generator.Error!void {
                const src1 = self.loadOperand(2, "src1");
                const src2 = self.loadOperand(3, "src2");
                const op = c.LLVMBuildICmp(self.builder, predicate, src1, src2, "icmp");
                const op_int = c.LLVMBuildZExt(self.builder, op, self.usize_type, "icmp.zext");
                self.storeOperand(1, op_int, "dst");
            }
        }.generator;
    }

    fn BinaryFloatCompareHandler(comptime predicate: c.LLVMRealPredicate) *const fn (self: *Generator) Generator.Error!void {
        return struct {
            pub fn generator(self: *Generator) Generator.Error!void {
                const src1_int = self.loadOperand(2, "src1.int");
                const src1 = c.LLVMBuildBitCast(self.builder, src1_int, self.f64_type, "src1");
                const src2_int = self.loadOperand(3, "src2.int");
                const src2 = c.LLVMBuildBitCast(self.builder, src2_int, self.f64_type, "src2");
                const op = c.LLVMBuildFCmp(self.builder, predicate, src1, src2, "fcmp");
                const op_int = c.LLVMBuildZExt(self.builder, op, self.usize_type, "fcmp.zext");
                self.storeOperand(1, op_int, "dst");
            }
        }.generator;
    }

    pub fn pint(self: *Generator) !void {
        const value = self.loadOperand(1, "int");
        self.callRuntimeVoid(.pint, &.{value});
    }

    pub fn br(self: *Generator) !Value {
        const handler = try self.addHandler("br", 3);
        const entry_block = c.LLVMAppendBasicBlock(handler, "entry");
        c.LLVMPositionBuilderAtEnd(self.builder, entry_block);

        const predicate_int = self.loadOperand(1, "predicate.int");
        const zero = c.LLVMConstInt(self.usize_type, 0, @intFromBool(false));
        const predicate = c.LLVMBuildICmp(self.builder, c.LLVMIntNE, predicate_int, zero, "predicate");
        // const predicate = c.LLVMBuildTruncOrBitCast(self.builder, predicate_int, self.bool_type, "predicate");

        const taken = c.LLVMAppendBasicBlock(handler, "taken");
        const skipped = c.LLVMAppendBasicBlock(handler, "skipped");
        _ = c.LLVMBuildCondBr(self.builder, predicate, taken, skipped);

        c.LLVMPositionBuilderAtEnd(self.builder, taken);
        const target = self.read(2, .sext, "target");
        self.tailCallTarget(target);
        _ = c.LLVMBuildRetVoid(self.builder);

        c.LLVMPositionBuilderAtEnd(self.builder, skipped);
        self.tailCallNext(3);
        _ = c.LLVMBuildRetVoid(self.builder);

        return handler;
    }

    pub fn jmp(self: *Generator) !Value {
        const handler = try self.addHandler("jmp", 2);
        const entry_block = c.LLVMAppendBasicBlock(handler, "entry");
        c.LLVMPositionBuilderAtEnd(self.builder, entry_block);

        const target = self.loadOperand(2, "target");
        self.tailCallTarget(target);

        _ = c.LLVMBuildRetVoid(self.builder);
        return handler;
    }

    pub fn call1(self: *Generator) !Value {
        const handler = try self.addHandler("call1", 3);
        const entry_block = c.LLVMAppendBasicBlock(handler, "entry");
        c.LLVMPositionBuilderAtEnd(self.builder, entry_block);
        const ip = c.LLVMGetParam(handler, ip_param_index);
        const fp = c.LLVMGetParam(handler, fp_param_index);
        var sp = c.LLVMGetParam(handler, sp_param_index);
        const ctx = c.LLVMGetParam(handler, ctx_param_index);

        const ssp = sp;
        // call the runtime to push arguments onto the stack
        const arg_count = c.LLVMConstInt(self.usize_type, 1, @intFromBool(false));
        const arg = self.loadOperand(3, "arg");
        _ = c.LLVMBuildStore(self.builder, arg, sp);
        sp = c.LLVMBuildInBoundsGEP2(self.builder, self.usize_type, sp, @constCast(&arg_count), 1, "sp.args");

        // push saved ip, fp, and return register
        const sip_offset = c.LLVMConstInt(self.usize_type, 4, @intFromBool(false));
        const sip = c.LLVMBuildInBoundsGEP2(self.builder, self.word_type, ip, @constCast(&sip_offset), 1, "sip");
        const sfp = fp;
        const return_reg = self.read(2, .sext, "ret.reg");
        self.push(&sp, sip);
        self.push(&sp, sfp);
        self.push(&sp, ssp);
        self.push(&sp, return_reg);

        // TODO: frame pointer should be stack pointer, and sp pushed further
        // fp = sp;

        // call the runtime to evaluate the callable
        const callable_int = self.loadOperand(1, "callable.int");
        const callable = c.LLVMBuildIntToPtr(self.builder, callable_int, self.ptr_type, "callable");
        var target = self.callRuntime(.eval_callable, &.{ ctx, callable, sp });
        target = c.LLVMGetNamedFunction(self.module, "interpreter_trampoline");
        const two = c.LLVMConstInt(self.usize_type, 2, @intFromBool(false));
        sp = c.LLVMBuildInBoundsGEP2(self.builder, self.usize_type, sp, @constCast(&two), 1, "sp.next");

        // tail call the target function
        const poison = c.LLVMGetPoison(self.ptr_type);
        const args: []const Value = &.{ poison, fp, sp, ctx };
        const target_call = c.LLVMBuildCall2(self.builder, self.handler_type, target, @constCast(args.ptr), @intCast(args.len), "");
        c.LLVMSetTailCallKind(target_call, c.LLVMTailCallKindMustTail);
        c.LLVMSetInstructionCallConv(target_call, c.LLVMGHCCallConv);

        _ = c.LLVMBuildRetVoid(self.builder);
        return handler;
    }

    pub fn call(self: *Generator) !Value {
        const handler = try self.addHandler("call", 4);
        const entry_block = c.LLVMAppendBasicBlock(handler, "entry");
        c.LLVMPositionBuilderAtEnd(self.builder, entry_block);
        const ip = c.LLVMGetParam(handler, ip_param_index);
        const fp = c.LLVMGetParam(handler, fp_param_index);
        var sp = c.LLVMGetParam(handler, sp_param_index);
        const ctx = c.LLVMGetParam(handler, ctx_param_index);

        const ssp = sp;
        // call the runtime to push arguments onto the stack
        const arg_count = self.read(3, .zext, "arg.count");
        const arg_offset = c.LLVMConstInt(self.usize_type, 4, @intFromBool(false));
        const arg_start = c.LLVMBuildInBoundsGEP2(self.builder, self.word_type, ip, @constCast(&arg_offset), 1, "arg.start");
        self.callRuntimeVoid(.push_args, &.{ arg_start, arg_count, fp, sp });
        sp = c.LLVMBuildInBoundsGEP2(self.builder, self.usize_type, sp, @constCast(&arg_count), 1, "sp.args");

        // push saved ip, fp, and return register
        const sip = c.LLVMBuildInBoundsGEP2(self.builder, self.word_type, arg_start, @constCast(&arg_count), 1, "sip");
        const sfp = fp;
        const return_reg = self.read(2, .sext, "ret.reg");
        self.push(&sp, sip);
        self.push(&sp, sfp);
        self.push(&sp, ssp);
        self.push(&sp, return_reg);

        // TODO: frame pointer should be stack pointer, and sp pushed further
        // fp = sp;

        // call the runtime to evaluate the callable
        const callable_int = self.loadOperand(1, "callable.int");
        const callable = c.LLVMBuildIntToPtr(self.builder, callable_int, self.ptr_type, "callable");
        const target = self.callRuntime(.eval_callable, &.{ ctx, callable, sp });
        const two = c.LLVMConstInt(self.usize_type, 2, @intFromBool(false));
        sp = c.LLVMBuildInBoundsGEP2(self.builder, self.usize_type, sp, @constCast(&two), 1, "sp.next");

        // tail call the target function
        const poison = c.LLVMGetPoison(self.ptr_type);
        const args: []const Value = &.{ poison, fp, sp, ctx };
        const target_call = c.LLVMBuildCall2(self.builder, self.handler_type, target, @constCast(args.ptr), @intCast(args.len), "");
        c.LLVMSetTailCallKind(target_call, c.LLVMTailCallKindMustTail);
        c.LLVMSetInstructionCallConv(target_call, c.LLVMGHCCallConv);

        _ = c.LLVMBuildRetVoid(self.builder);
        return handler;
    }

    pub fn ret(self: *Generator) !Value {
        const function = try self.addHandler("ret", 2);
        const entry_block = c.LLVMAppendBasicBlock(function, "entry");
        c.LLVMPositionBuilderAtEnd(self.builder, entry_block);
        const fp = c.LLVMGetParam(function, fp_param_index);
        const ctx = c.LLVMGetParam(function, ctx_param_index);

        // pop the register file
        var sp = fp;

        // pop the return register, fp, and ip
        const return_reg = self.pop(&sp);
        const ssp_int = self.pop(&sp);
        const ssp = c.LLVMBuildIntToPtr(self.builder, ssp_int, self.ptr_type, "ssp");
        const sfp_int = self.pop(&sp);
        const sfp = c.LLVMBuildIntToPtr(self.builder, sfp_int, self.ptr_type, "sfp");
        const sip_int = self.pop(&sp);
        const sip = c.LLVMBuildIntToPtr(self.builder, sip_int, self.ptr_type, "sip");

        // save the return value in the return register
        const src = self.loadOperand(1, "src");
        const stack_gep = c.LLVMBuildInBoundsGEP2(self.builder, self.usize_type, sfp, @constCast(&return_reg), 1, "ret.val.ptr");
        const store_inst = c.LLVMBuildStore(self.builder, src, stack_gep);
        c.LLVMSetAlignment(store_inst, @alignOf(u64));

        // and use it to read the opcode of the next instruction (to lookup the handler)
        const next_opcode = c.LLVMBuildLoad2(self.builder, self.word_type, sip, "opcode.next");

        // load the handler pointer from the jump table using the opcode
        const jump_table = c.LLVMGetNamedGlobal(self.module, "jump_table");
        const handler_ptr = c.LLVMBuildInBoundsGEP2(self.builder, self.ptr_type, jump_table, @constCast(&next_opcode), 1, "handler.ptr");
        const handler = c.LLVMBuildLoad2(self.builder, self.ptr_type, handler_ptr, "handler");

        // tail call the handler
        const args: []const Value = &.{ sip, sfp, ssp, ctx };
        const handler_call = c.LLVMBuildCall2(self.builder, self.handler_type, handler, @constCast(args.ptr), @intCast(args.len), "");
        c.LLVMSetTailCallKind(handler_call, c.LLVMTailCallKindMustTail);
        c.LLVMSetInstructionCallConv(handler_call, c.LLVMGHCCallConv);

        _ = c.LLVMBuildRetVoid(self.builder);

        return function;
    }

    pub fn callrt0(self: *Generator) !void {
        const insert_block = c.LLVMGetInsertBlock(self.builder);
        const function = c.LLVMGetBasicBlockParent(insert_block);
        const ip = c.LLVMGetParam(function, ip_param_index);
        const fp = c.LLVMGetParam(function, fp_param_index);
        var sp = c.LLVMGetParam(function, sp_param_index);

        // push the builtin index so the handler knows which runtime function to dispatch
        const index = self.readImmediate(1, "index");
        self.push(&sp, index);

        const rt_handler = c.LLVMGetNamedFunction(self.module, "rt_handler");
        const args: []const Value = &.{ ip, fp, sp };
        const builtin_call = c.LLVMBuildCall2(self.builder, self.handler_type, rt_handler, @constCast(args.ptr), @intCast(args.len), "");
        c.LLVMSetInstructionCallConv(builtin_call, c.LLVMCCallConv);
    }

    pub fn exit(self: *Generator) !Value {
        const handler = try self.addHandler("exit", 1);
        const entry_block = c.LLVMAppendBasicBlock(handler, "entry");
        c.LLVMPositionBuilderAtEnd(self.builder, entry_block);
        _ = c.LLVMBuildRetVoid(self.builder);
        return handler;
    }

    pub fn trap(self: *Generator) !Value {
        const handler = try self.addHandler("exit", 1);
        const entry_block = c.LLVMAppendBasicBlock(handler, "entry");
        c.LLVMPositionBuilderAtEnd(self.builder, entry_block);

        const ip = c.LLVMGetParam(handler, ip_param_index);
        const opcode = c.LLVMBuildLoad2(self.builder, self.word_type, ip, "opcode");
        self.callRuntimeVoid(.trap, &.{opcode});
        _ = c.LLVMBuildUnreachable(self.builder);
        return handler;
    }

    fn param(self: *Generator, index: u32) Value {
        const insert_block = c.LLVMGetInsertBlock(self.builder);
        const function = c.LLVMGetBasicBlockParent(insert_block);
        return c.LLVMGetParam(function, index);
    }

    fn iconst(self: *Generator, ty: Type, value: u64, sext: bool) Value {
        _ = self;
        return c.LLVMConstInt(ty, value, @intFromBool(sext));
    }

    fn gep(self: *Generator, ty: Type, ptr: Value, offsets: []const Value, name: [:0]const u8) Value {
        return c.LLVMBuildInBoundsGEP2(
            self.builder,
            ty,
            ptr,
            @constCast(offsets.ptr),
            @intCast(offsets.len),
            name,
        );
    }

    const ExtendMode = enum { none, zext, sext };
    fn read(
        self: *Generator,
        offset: anytype,
        mode: ExtendMode,
        comptime name: [:0]const u8,
    ) Value {
        const ip = self.param(ip_param_index);
        const ptr_offset = switch (@TypeOf(offset)) {
            Value => offset,
            else => self.iconst(self.word_type, offset, true),
        };
        const ptr = self.gep(
            self.word_type,
            ip,
            &.{ptr_offset},
            name ++ ".ptr",
        );

        const raw = c.LLVMBuildLoad2(self.builder, self.word_type, ptr, name ++ ".word");
        return switch (mode) {
            .none => raw,
            .zext => c.LLVMBuildZExt(self.builder, raw, self.usize_type, name),
            .sext => c.LLVMBuildSExt(self.builder, raw, self.usize_type, name),
        };
    }

    fn load(self: *Generator, register: Value, comptime name: [:0]const u8) Value {
        const fp = self.param(fp_param_index);
        const stack_gep = c.LLVMBuildInBoundsGEP2(self.builder, self.usize_type, fp, @constCast(&register), 1, name ++ ".ptr");
        const value = c.LLVMBuildLoad2(self.builder, self.usize_type, stack_gep, name);
        c.LLVMSetAlignment(value, @alignOf(u64));
        return value;
    }

    fn store(self: *Generator, register: Value, value: Value, comptime name: [:0]const u8) void {
        const fp = self.param(fp_param_index);
        const stack_gep = c.LLVMBuildInBoundsGEP2(self.builder, self.usize_type, fp, @constCast(&register), 1, name ++ ".ptr");
        const store_inst = c.LLVMBuildStore(self.builder, value, stack_gep);
        c.LLVMSetAlignment(store_inst, @alignOf(u64));
    }

    fn loadOperand(self: *Generator, comptime offset: u32, comptime name: [:0]const u8) Value {
        const register = self.read(offset, .sext, name ++ ".reg");
        return self.load(register, name);
    }

    fn storeOperand(self: *Generator, comptime offset: u32, value: Value, comptime name: [:0]const u8) void {
        const register = self.read(offset, .sext, name ++ ".reg");
        self.store(register, value, name);
    }

    fn declareJumpTable(self: *Generator, count: u32) void {
        const jump_table_type = c.LLVMArrayType2(self.ptr_type, count);
        const jump_table = c.LLVMAddGlobal(self.module, jump_table_type, "jump_table");
        c.LLVMSetLinkage(jump_table, c.LLVMPrivateLinkage);
        c.LLVMSetUnnamedAddress(jump_table, c.LLVMLocalUnnamedAddr);
    }

    fn defineJumpTable(self: *Generator, handlers: []const Value) void {
        const jump_table = c.LLVMGetNamedGlobal(self.module, "jump_table");
        const pointers = c.LLVMConstArray2(self.ptr_type, @constCast(handlers.ptr), @intCast(handlers.len));
        c.LLVMSetInitializer(jump_table, pointers);
    }

    fn declareRtHandler(self: *Generator) void {
        const handler = c.LLVMAddFunction(self.module, "rt_handler", self.handler_type);
        c.LLVMSetLinkage(handler, c.LLVMExternalLinkage);
    }

    fn tailCallTarget(self: *Generator, offset: Value) void {
        const insert_block = c.LLVMGetInsertBlock(self.builder);
        const function = c.LLVMGetBasicBlockParent(insert_block);
        const ip = c.LLVMGetParam(function, ip_param_index);
        const fp = c.LLVMGetParam(function, fp_param_index);
        const sp = c.LLVMGetParam(function, sp_param_index);
        const ctx = c.LLVMGetParam(function, ctx_param_index);

        // load the pointer to the start of the next instruction (to pass into next handler)
        // and use it to read the opcode of the next instruction (to lookup the handler)
        const next_ip = c.LLVMBuildInBoundsGEP2(self.builder, self.word_type, ip, @constCast(&offset), 1, "ip.next");
        const next_opcode = c.LLVMBuildLoad2(self.builder, self.word_type, next_ip, "opcode.next");

        // load the handler pointer from the jump table using the opcode
        const jump_table = c.LLVMGetNamedGlobal(self.module, "jump_table");
        const handler_ptr = c.LLVMBuildInBoundsGEP2(self.builder, self.ptr_type, jump_table, @constCast(&next_opcode), 1, "handler.ptr");
        const handler = c.LLVMBuildLoad2(self.builder, self.ptr_type, handler_ptr, "handler");

        // tail call the handler
        const args: []const Value = &.{ next_ip, fp, sp, ctx };
        const handler_call = c.LLVMBuildCall2(self.builder, self.handler_type, handler, @constCast(args.ptr), @intCast(args.len), "");
        c.LLVMSetTailCallKind(handler_call, c.LLVMTailCallKindMustTail);
        c.LLVMSetInstructionCallConv(handler_call, c.LLVMGHCCallConv);
    }

    fn tailCallNext(self: *Generator, offset: u32) void {
        const insert_block = c.LLVMGetInsertBlock(self.builder);
        const function = c.LLVMGetBasicBlockParent(insert_block);
        const ip = c.LLVMGetParam(function, ip_param_index);
        const fp = c.LLVMGetParam(function, fp_param_index);
        const sp = c.LLVMGetParam(function, sp_param_index);
        const ctx = c.LLVMGetParam(function, ctx_param_index);

        // load the pointer to the start of the next instruction (to pass into next handler)
        // and use it to read the opcode of the next instruction (to lookup the handler)
        const offset_value = c.LLVMConstInt(self.usize_type, @intCast(offset), @intFromBool(true));
        const next_ip = c.LLVMBuildInBoundsGEP2(self.builder, self.word_type, ip, @constCast(&offset_value), 1, "ip.next");
        const next_opcode = c.LLVMBuildLoad2(self.builder, self.word_type, next_ip, "opcode.next");

        // load the handler pointer from the jump table using the opcode
        const jump_table = c.LLVMGetNamedGlobal(self.module, "jump_table");
        const handler_ptr = c.LLVMBuildInBoundsGEP2(self.builder, self.ptr_type, jump_table, @constCast(&next_opcode), 1, "handler.ptr");
        const handler = c.LLVMBuildLoad2(self.builder, self.ptr_type, handler_ptr, "handler");

        // tail call the handler
        const args: []const Value = &.{ next_ip, fp, sp, ctx };
        const handler_call = c.LLVMBuildCall2(self.builder, self.handler_type, handler, @constCast(args.ptr), @intCast(args.len), "");
        c.LLVMSetTailCallKind(handler_call, c.LLVMTailCallKindMustTail);
        c.LLVMSetInstructionCallConv(handler_call, c.LLVMGHCCallConv);
    }

    // fn tail(self: *Generator, ptr: Value, args: []const Value) void {
    //     const function_call = c.LLVMBuildCall2(self.builder, self.handler_type, )
    // }
    //
    fn callRuntimeVoid(self: *Generator, comptime index: BuiltinIndex, arguments: []const Value) void {
        const builtin = self.builtinDecl(index);
        const runtime_call = c.LLVMBuildCall2(
            self.builder,
            builtin.ty,
            builtin.ptr,
            @constCast(arguments.ptr),
            @intCast(arguments.len),
            "",
        );
        c.LLVMSetInstructionCallConv(runtime_call, c.LLVMCCallConv);
    }

    fn callRuntime(self: *Generator, comptime index: BuiltinIndex, arguments: []const Value) Value {
        const builtin = self.builtinDecl(index);
        const runtime_call = c.LLVMBuildCall2(
            self.builder,
            builtin.ty,
            builtin.ptr,
            @constCast(arguments.ptr),
            @intCast(arguments.len),
            "rt",
        );
        c.LLVMSetInstructionCallConv(runtime_call, c.LLVMCCallConv);

        return runtime_call;
    }

    fn push(self: *Generator, sp_ptr: *Value, value: Value) void {
        const sp = sp_ptr.*;
        _ = c.LLVMBuildStore(self.builder, value, sp);
        const one = c.LLVMConstInt(self.usize_type, 1, @intFromBool(false));
        const next_sp = c.LLVMBuildInBoundsGEP2(self.builder, self.usize_type, sp, @constCast(&one), 1, "sp.next");
        sp_ptr.* = next_sp;
    }

    fn pop(self: *Generator, sp_ptr: *Value) Value {
        const sp = sp_ptr.*;
        const minus_one = c.LLVMConstInt(self.usize_type, @bitCast(@as(i64, -1)), @intFromBool(true));
        const prev_sp = c.LLVMBuildInBoundsGEP2(self.builder, self.usize_type, sp, @constCast(&minus_one), 1, "sp.prev");
        const value = c.LLVMBuildLoad2(self.builder, self.usize_type, prev_sp, "pop");
        sp_ptr.* = prev_sp;
        return value;
    }

    pub fn finalize(self: *Generator, bc_name: [:0]const u8) !void {
        try self.verify();

        const rc = c.LLVMWriteBitcodeToFile(self.module, bc_name);
        if (rc != 0) return error.WriteBitcodeFailed;
    }

    fn verify(self: *Generator) !void {
        var msg: [*c]u8 = null;
        defer c.LLVMDisposeMessage(msg);
        const rc = c.LLVMVerifyModule(self.module, c.LLVMReturnStatusAction, &msg);

        if (rc != 0) {
            std.debug.print("IR verification error:\n{s}", .{msg});
            c.LLVMDumpModule(self.module);
            return error.VerifyIrFailed;
        }
    }
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const args = try std.process.argsAlloc(arena.allocator());

    var generator = try Generator.init(arena.allocator(), "interpreter");
    try generator.generate();
    try generator.finalize(args[1]);
}
