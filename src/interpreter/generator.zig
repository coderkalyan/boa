const std = @import("std");
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
    const Error = error{
        WriteBitcodeFailed,
        VerifyIrFailed,
    };

    const address_space = 0;
    const ip_param_index = 0;
    const fp_param_index = 1;

    pub fn init(module_name: [:0]const u8) !Generator {
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
        };
    }

    fn getTargetFromTriple(target_triple: [*c]const u8) !Target {
        var msg: [*c]u8 = null;
        defer c.LLVMDisposeMessage(msg);
        var target: Target = null;
        const ret = c.LLVMGetTargetFromTriple(target_triple, &target, &msg);
        if (ret != 0) {
            std.debug.print("Get target error: {s}\n", .{msg});
            return error.GetTargetFailed;
        }

        return target;
    }

    fn handlerType(context: Context) Type {
        const parameter_types: []const Type = &.{
            // instruction pointer to the current instruction
            c.LLVMPointerTypeInContext(context, address_space),
            // frame pointer to the base of the current stack frame
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

    fn addHandler(self: *Generator, name: [:0]const u8, words: u32) !Value {
        // start by creating the base handler declaration
        const handler = c.LLVMAddFunction(
            self.module,
            name,
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

        // stack pointer attributes: nonnull, align 8
        const fp_param = c.LLVMGetParam(handler, fp_param_index);
        c.LLVMAddAttributeAtIndex(handler, fp_param_index + 1, self.enumAttribute(non_null, null));
        c.LLVMSetParamAlignment(fp_param, 8);

        // set the function to internal for optimization
        // c.LLVMSetLinkage(handler, c.LLVMInternalLinkage);
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
            // TODO: ldi
            // TODO: ldg
            // TODO: stg
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
            // TODO: call
            // TODO: pint
            // TODO: pfloat
            // TODO: pbool
            // TODO: pstr
            // TODO: strlen
            // TODO: strcat
            // TODO: strrep
            br,
            jmp,
            // TODO: ret
            exit,
        };
        self.declareJumpTable(generators.len);

        var handlers: [generators.len]Value = undefined;
        inline for (generators, 0..) |generator, i| {
            handlers[i] = try generator(self);
        }
        self.defineJumpTable(&handlers);
    }

    fn HandlerGenerator(
        comptime name: [:0]const u8,
        comptime words: u32,
        comptime body: *const fn (self: *Generator) Generator.Error!void,
    ) HandlerGeneratorFn {
        return struct {
            pub fn generator(self: *Generator) !Value {
                const handler = try self.addHandler(name, words);
                const entry = c.LLVMAppendBasicBlock(handler, "entry");
                c.LLVMPositionBuilderAtEnd(self.builder, entry);

                try @call(.always_inline, body, .{self});

                self.tailCallNext(words);
                _ = c.LLVMBuildRetVoid(self.builder);
                return handler;
            }
        }.generator;
    }

    fn ld(self: *Generator) !void {
        const immediate = self.readImmediate(2, "imm");
        self.storeRegister(1, immediate, "dst");
    }

    fn mov(self: *Generator) !void {
        const src = self.loadRegister(2, "src");
        self.storeRegister(1, src, "dst");
    }

    fn itof(self: *Generator) !void {
        const src = self.loadRegister(2, "src");
        const conversion = c.LLVMBuildSIToFP(self.builder, src, self.f64_type, "itof");
        const bitcast = c.LLVMBuildBitCast(self.builder, conversion, self.usize_type, "bitcast");
        self.storeRegister(1, bitcast, "dst");
    }

    fn ftoi(self: *Generator) !void {
        const src_int = self.loadRegister(2, "src.int");
        const src = c.LLVMBuildBitCast(self.builder, src_int, self.f64_type, "src");
        const conversion = c.LLVMBuildFPToSI(self.builder, src, self.usize_type, "ftoi");
        self.storeRegister(1, conversion, "dst");
    }

    fn ineg(self: *Generator) !void {
        const src = self.loadRegister(2, "src");
        const neg = c.LLVMBuildNUWNeg(self.builder, src, "ineg");
        self.storeRegister(1, neg, "dst");
    }

    fn fneg(self: *Generator) !void {
        const src_int = self.loadRegister(2, "src.int");
        const src = c.LLVMBuildBitCast(self.builder, src_int, self.f64_type, "src");
        const neg = c.LLVMBuildFNeg(self.builder, src, "fneg");
        const neg_int = c.LLVMBuildBitCast(self.builder, neg, self.usize_type, "fneg.int");
        self.storeRegister(1, neg_int, "dst");
    }

    fn binv(self: *Generator) !void {
        const src = self.loadRegister(2, "src");
        const mask = c.LLVMConstInt(self.usize_type, @bitCast(@as(i64, -1)), @intFromBool(true));
        const inv = c.LLVMBuildXor(self.builder, src, mask, "binv");
        self.storeRegister(1, inv, "dst");
    }

    fn lnot(self: *Generator) !void {
        const src_int = self.loadRegister(2, "src.int");
        const src = c.LLVMBuildTruncOrBitCast(self.builder, src_int, self.bool_type, "src");
        const zero = c.LLVMConstInt(self.bool_type, 0, @intFromBool(false));
        const not = c.LLVMBuildICmp(self.builder, c.LLVMIntEQ, src, zero, "lnot");
        self.storeRegister(1, not, "dst");
    }

    fn BinaryIntHandler(comptime opcode: c.LLVMOpcode) *const fn (self: *Generator) Generator.Error!void {
        return struct {
            pub fn generator(self: *Generator) Generator.Error!void {
                const src1 = self.loadRegister(2, "src1");
                const src2 = self.loadRegister(3, "src2");
                const op = c.LLVMBuildBinOp(self.builder, opcode, src1, src2, "binary");
                self.storeRegister(1, op, "dst");
            }
        }.generator;
    }

    fn BinaryFloatHandler(comptime opcode: c.LLVMOpcode) *const fn (self: *Generator) Generator.Error!void {
        return struct {
            pub fn generator(self: *Generator) Generator.Error!void {
                const src1_int = self.loadRegister(2, "src1.int");
                const src1 = c.LLVMBuildBitCast(self.builder, src1_int, self.f64_type, "src1");
                const src2_int = self.loadRegister(3, "src2.int");
                const src2 = c.LLVMBuildBitCast(self.builder, src2_int, self.f64_type, "src2");
                const op = c.LLVMBuildBinOp(self.builder, opcode, src1, src2, "binary");
                const op_int = c.LLVMBuildBitCast(self.builder, op, self.usize_type, "binary.int");
                self.storeRegister(1, op_int, "dst");
            }
        }.generator;
    }

    fn BinaryIntCompareHandler(comptime predicate: c.LLVMIntPredicate) *const fn (self: *Generator) Generator.Error!void {
        return struct {
            pub fn generator(self: *Generator) Generator.Error!void {
                const src1 = self.loadRegister(2, "src1");
                const src2 = self.loadRegister(3, "src2");
                const op = c.LLVMBuildICmp(self.builder, predicate, src1, src2, "icmp");
                const op_int = c.LLVMBuildZExt(self.builder, op, self.usize_type, "icmp.zext");
                self.storeRegister(1, op_int, "dst");
            }
        }.generator;
    }

    fn BinaryFloatCompareHandler(comptime predicate: c.LLVMRealPredicate) *const fn (self: *Generator) Generator.Error!void {
        return struct {
            pub fn generator(self: *Generator) Generator.Error!void {
                const src1_int = self.loadRegister(2, "src1.int");
                const src1 = c.LLVMBuildBitCast(self.builder, src1_int, self.f64_type, "src1");
                const src2_int = self.loadRegister(3, "src2.int");
                const src2 = c.LLVMBuildBitCast(self.builder, src2_int, self.f64_type, "src2");
                const op = c.LLVMBuildFCmp(self.builder, predicate, src1, src2, "fcmp");
                const op_int = c.LLVMBuildZExt(self.builder, op, self.usize_type, "fcmp.zext");
                self.storeRegister(1, op_int, "dst");
            }
        }.generator;
    }

    pub fn br(self: *Generator) !Value {
        const handler = try self.addHandler("br", 3);
        const entry = c.LLVMAppendBasicBlock(handler, "entry");
        c.LLVMPositionBuilderAtEnd(self.builder, entry);

        const predicate_int = self.loadRegister(1, "predicate.int");
        const predicate = c.LLVMBuildTruncOrBitCast(self.builder, predicate_int, self.bool_type, "predicate");

        const taken = c.LLVMAppendBasicBlock(handler, "taken");
        const skipped = c.LLVMAppendBasicBlock(handler, "skipped");
        _ = c.LLVMBuildCondBr(self.builder, predicate, taken, skipped);

        c.LLVMPositionBuilderAtEnd(self.builder, taken);
        const target = self.loadRegister(2, "target");
        self.tailCallTarget(target);
        _ = c.LLVMBuildRetVoid(self.builder);

        c.LLVMPositionBuilderAtEnd(self.builder, skipped);
        self.tailCallNext(3);
        _ = c.LLVMBuildRetVoid(self.builder);

        return handler;
    }

    pub fn jmp(self: *Generator) !Value {
        const handler = try self.addHandler("jmp", 2);
        const entry = c.LLVMAppendBasicBlock(handler, "entry");
        c.LLVMPositionBuilderAtEnd(self.builder, entry);

        const target = self.loadRegister(2, "target");
        self.tailCallTarget(target);

        _ = c.LLVMBuildRetVoid(self.builder);
        return handler;
    }

    pub fn exit(self: *Generator) !Value {
        const handler = try self.addHandler("exit", 1);
        const entry = c.LLVMAppendBasicBlock(handler, "entry");
        c.LLVMPositionBuilderAtEnd(self.builder, entry);
        _ = c.LLVMBuildRetVoid(self.builder);
        return handler;
    }

    fn readImmediate(self: *Generator, offset: usize, comptime name: [:0]const u8) Value {
        const insert_block = c.LLVMGetInsertBlock(self.builder);
        const function = c.LLVMGetBasicBlockParent(insert_block);
        const ip = c.LLVMGetParam(function, ip_param_index);

        const offset_value = c.LLVMConstInt(self.word_type, @intCast(offset), @intFromBool(false));
        const gep = c.LLVMBuildInBoundsGEP2(self.builder, self.word_type, ip, @constCast(&offset_value), 1, name ++ ".ptr");
        const load = c.LLVMBuildLoad2(self.builder, self.word_type, gep, name ++ ".word");
        const sext = c.LLVMBuildSExt(self.builder, load, self.usize_type, name);
        return sext;
    }

    fn readRegister(self: *Generator, offset: usize, comptime name: [:0]const u8) Value {
        const insert_block = c.LLVMGetInsertBlock(self.builder);
        const function = c.LLVMGetBasicBlockParent(insert_block);
        const ip = c.LLVMGetParam(function, ip_param_index);

        // load the register index from code
        const offset_value = c.LLVMConstInt(self.word_type, @intCast(offset), @intFromBool(false));
        const code_gep = c.LLVMBuildInBoundsGEP2(self.builder, self.word_type, ip, @constCast(&offset_value), 1, name ++ ".ptr");
        const register = c.LLVMBuildLoad2(self.builder, self.word_type, code_gep, name);
        return register;
    }

    fn loadStack(self: *Generator, register: Value, comptime name: [:0]const u8) Value {
        const insert_block = c.LLVMGetInsertBlock(self.builder);
        const function = c.LLVMGetBasicBlockParent(insert_block);
        const fp = c.LLVMGetParam(function, fp_param_index);

        // load data from stack using the register offset
        const stack_gep = c.LLVMBuildInBoundsGEP2(self.builder, self.usize_type, fp, @constCast(&register), 1, name ++ ".ptr");
        const value = c.LLVMBuildLoad2(self.builder, self.usize_type, stack_gep, name);
        c.LLVMSetAlignment(value, @alignOf(u64));
        return value;
    }

    fn storeStack(self: *Generator, register: Value, value: Value, comptime name: [:0]const u8) void {
        const insert_block = c.LLVMGetInsertBlock(self.builder);
        const function = c.LLVMGetBasicBlockParent(insert_block);
        const fp = c.LLVMGetParam(function, fp_param_index);

        // load data from stack using the register offset
        const stack_gep = c.LLVMBuildInBoundsGEP2(self.builder, self.usize_type, fp, @constCast(&register), 1, name ++ ".ptr");
        const store = c.LLVMBuildStore(self.builder, value, stack_gep);
        c.LLVMSetAlignment(store, @alignOf(u64));
    }

    fn loadRegister(self: *Generator, comptime offset: u32, comptime name: [:0]const u8) Value {
        const register = self.readRegister(offset, name ++ ".reg");
        const value = self.loadStack(register, name);
        return value;
    }

    fn storeRegister(self: *Generator, comptime offset: u32, value: Value, comptime name: [:0]const u8) void {
        const register = self.readRegister(offset, name ++ ".reg");
        self.storeStack(register, value, name);
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

    fn tailCallTarget(self: *Generator, offset: Value) void {
        const insert_block = c.LLVMGetInsertBlock(self.builder);
        const function = c.LLVMGetBasicBlockParent(insert_block);
        const ip = c.LLVMGetParam(function, ip_param_index);
        const fp = c.LLVMGetParam(function, fp_param_index);

        // load the pointer to the start of the next instruction (to pass into next handler)
        // and use it to read the opcode of the next instruction (to lookup the handler)
        const next_ip = c.LLVMBuildInBoundsGEP2(self.builder, self.word_type, ip, @constCast(&offset), 1, "ip.next");
        const next_opcode = c.LLVMBuildLoad2(self.builder, self.word_type, next_ip, "opcode.next");

        // load the handler pointer from the jump table using the opcode
        const jump_table = c.LLVMGetNamedGlobal(self.module, "jump_table");
        const handler_ptr = c.LLVMBuildInBoundsGEP2(self.builder, self.word_type, jump_table, @constCast(&next_opcode), 1, "handler.ptr");
        const handler = c.LLVMBuildLoad2(self.builder, self.ptr_type, handler_ptr, "handler");

        // tail call the handler
        const args: []const Value = &.{ next_ip, fp };
        const call = c.LLVMBuildCall2(self.builder, self.handler_type, handler, @constCast(args.ptr), @intCast(args.len), "");
        c.LLVMSetTailCallKind(call, c.LLVMTailCallKindMustTail);
        c.LLVMSetInstructionCallConv(call, c.LLVMGHCCallConv);
    }

    fn tailCallNext(self: *Generator, offset: u32) void {
        const insert_block = c.LLVMGetInsertBlock(self.builder);
        const function = c.LLVMGetBasicBlockParent(insert_block);
        const ip = c.LLVMGetParam(function, ip_param_index);
        const fp = c.LLVMGetParam(function, fp_param_index);

        // load the pointer to the start of the next instruction (to pass into next handler)
        // and use it to read the opcode of the next instruction (to lookup the handler)
        const offset_value = c.LLVMConstInt(self.usize_type, @intCast(offset), @intFromBool(true));
        const next_ip = c.LLVMBuildInBoundsGEP2(self.builder, self.word_type, ip, @constCast(&offset_value), 1, "ip.next");
        const next_opcode = c.LLVMBuildLoad2(self.builder, self.word_type, next_ip, "opcode.next");

        // load the handler pointer from the jump table using the opcode
        const jump_table = c.LLVMGetNamedGlobal(self.module, "jump_table");
        const handler_ptr = c.LLVMBuildInBoundsGEP2(self.builder, self.word_type, jump_table, @constCast(&next_opcode), 1, "handler.ptr");
        const handler = c.LLVMBuildLoad2(self.builder, self.ptr_type, handler_ptr, "handler");

        // tail call the handler
        const args: []const Value = &.{ next_ip, fp };
        const call = c.LLVMBuildCall2(self.builder, self.handler_type, handler, @constCast(args.ptr), @intCast(args.len), "");
        c.LLVMSetTailCallKind(call, c.LLVMTailCallKindMustTail);
        c.LLVMSetInstructionCallConv(call, c.LLVMGHCCallConv);
    }

    pub fn finalize(self: *Generator, bc_name: [:0]const u8) !void {
        try self.verify();

        const ret = c.LLVMWriteBitcodeToFile(self.module, bc_name);
        if (ret != 0) return error.WriteBitcodeFailed;
    }

    fn verify(self: *Generator) !void {
        var msg: [*c]u8 = null;
        defer c.LLVMDisposeMessage(msg);
        const ret = c.LLVMVerifyModule(self.module, c.LLVMReturnStatusAction, &msg);

        if (ret != 0) {
            std.debug.print("IR verification error: {s}\n", .{msg});
            return error.VerifyIrFailed;
        }
    }
};

pub fn main() !void {
    var generator = try Generator.init("interpreter");
    try generator.generate();
    try generator.finalize("interpreter.bc");
}
