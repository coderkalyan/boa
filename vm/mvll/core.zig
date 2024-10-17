const std = @import("std");
const Allocator = std.mem.Allocator;

pub const DiagnosticHandler = *const fn (*DiagnosticInfo, *anyopaque) void;
pub const YieldCallback = *const fn (*Context, *anyopaque) void;

// LLVMBool is an int, but we don't expose it, instead wrapping functions that use it to get a type safe
// zig bool.
const Bool = c_int;

pub const MemoryBuffer = extern opaque {
    extern fn LLVMGetBufferStart(buf: *const MemoryBuffer) [*]u8;
    pub const start = LLVMGetBufferStart;

    extern fn LLVMGetBufferSize(buf: *const MemoryBuffer) usize;
    pub const size = LLVMGetBufferSize;

    extern fn LLVMDisposeMemoryBuffer(buf: *MemoryBuffer) void;
    pub const deinit = LLVMDisposeMemoryBuffer;
};

pub const Context = extern opaque {
    extern fn LLVMContextCreate() *Context;
    pub const init = LLVMContextCreate;

    extern fn LLVMGetGlobalContext() *Context;
    pub const global = LLVMGetGlobalContext;

    extern fn LLVMContextSetDiagnosticHandler(ctx: *Context, handler: DiagnosticHandler, diagnosticCtx: *anyopaque) void;
    pub const setDiagnosticHandler = LLVMContextSetDiagnosticHandler;

    extern fn LLVMContextGetDiagnosticHandler(ctx: *Context) DiagnosticHandler;
    pub const getDiagnosticHandler = LLVMContextGetDiagnosticHandler;

    extern fn LLVMContextGetDiagnosticContext(ctx: *Context) *anyopaque;
    pub const getDiagnosticContext = LLVMContextGetDiagnosticContext;

    extern fn LLVMContextSetYieldCallback(ctx: *Context, callback: YieldCallback, handle: *anyopaque) void;
    pub const setYieldCallback = LLVMContextSetYieldCallback;

    extern fn LLVMContextShouldDiscardValueNames(ctx: *Context) Bool;
    pub inline fn shouldDiscardValueNames(ctx: *Context) bool {
        return LLVMContextShouldDiscardValueNames(ctx) != 0;
    }

    extern fn LLVMContextSetDiscardValueNames(ctx: *Context, discard: Bool) void;
    pub inline fn setDiscardValueNames(ctx: *Context, discard: bool) void {
        LLVMContextSetDiscardValueNames(ctx, @intFromBool(discard));
    }

    extern fn LLVMContextDispose(ctx: *Context) void;
    pub const deinit = LLVMContextDispose;

    extern fn LLVMIntTypeInContext(ctx: *Context, bits: c_uint) *Type;
    pub const int = LLVMIntTypeInContext;

    extern fn LLVMHalfTypeInContext(ctx: *Context) *Type;
    extern fn LLVMBFloatTypeInContext(ctx: *Context) *Type;
    extern fn LLVMFloatTypeInContext(ctx: *Context) *Type;
    extern fn LLVMDoubleTypeInContext(ctx: *Context) *Type;
    extern fn LLVMX86FP80TypeInContext(ctx: *Context) *Type;
    extern fn LLVMFP128TypeInContext(ctx: *Context) *Type;
    extern fn LLVMPPCFP128TypeInContext(ctx: *Context) *Type;
    pub inline fn float(ctx: *Context, comptime kind: Type.Kind) *Type {
        return switch (kind) {
            .half => LLVMHalfTypeInContext(ctx),
            .bfloat => LLVMBFloatTypeInContext(ctx),
            .float => LLVMFloatTypeInContext(ctx),
            .double => LLVMDoubleTypeInContext(ctx),
            .x86_fp80 => LLVMX86FP80TypeInContext(ctx),
            .fp128 => LLVMFP128TypeInContext(ctx),
            .ppc_fp128 => LLVMPPCFP128TypeInContext(ctx),
            else => unreachable, // asserted at comptime
        };
    }

    extern fn LLVMFunctionType(return_type: *Type, param_types: [*]const *Type, param_count: c_uint, is_var_arg: Bool) *Type;
    pub inline fn function(ctx: *Context, return_type: *Type, param_types: []const *Type, is_var_arg: bool) *Type {
        _ = ctx;
        return LLVMFunctionType(return_type, param_types.ptr, @intCast(param_types.len), @intFromBool(is_var_arg));
    }

    extern fn LLVMStructTypeInContext(ctx: *Context, element_types: [*]const *Type, element_count: c_uint, pack: Bool) *Type;
    pub inline fn @"struct"(ctx: *Context, element_types: []const *Type, pack: bool) *Type {
        return LLVMStructTypeInContext(ctx, element_types.ptr, @intCast(element_types.len), @intFromBool(pack));
    }

    extern fn LLVMVoidTypeInContext(ctx: *Context) *Type;
    pub const @"void" = LLVMVoidTypeInContext;

    extern fn LLVMPointerTypeInContext(ctx: *Context, address_space: c_uint) *Type;
    pub const ptr = LLVMPointerTypeInContext;
};

// TODO: diagnostic severity
// TODO: all the enum attribute stuff

pub const Module = extern opaque {
    extern fn LLVMModuleCreateWithNameInContext(name: [*:0]const u8, ctx: *Context) *Module;
    pub const init = LLVMModuleCreateWithNameInContext;

    extern fn LLVMCloneModule(module: *Module) *Module;
    pub const clone = LLVMCloneModule;

    extern fn LLVMDisposeModule(module: *Module) void;
    pub const deinit = LLVMDisposeModule;

    extern fn LLVMGetModuleIdentifier(module: *Module, out_len: *usize) *u8;
    pub inline fn getIdentifier(module: *Module) []const u8 {
        var len: usize = undefined;
        const ptr = LLVMGetModuleIdentifier(module, &len);
        return ptr[0..len];
    }

    extern fn LLVMSetModuleIdentifier(module: *Module, id: *u8, len: usize) void;
    pub inline fn setIdentifier(module: *Module, name: []const u8) void {
        LLVMSetModuleIdentifier(module, name.ptr, name.len);
    }

    // TODO: source file name
    // TODO: data layout
    // TODO: target
    // TODO: module flags

    extern fn LLVMDumpModule(module: *Module) void;
    pub const printToStderr = LLVMDumpModule;

    extern fn LLVMPrintModuleToFile(module: *Module, filename: [*:0]const u8, msg: *[*:0]const u8) Bool;
    pub inline fn printToFile(module: *Module, filename: [:0]const u8) !void {
        var msg: [*:0]const u8 = undefined;
        // TODO: dispose
        // errdefer LLVMDisposeMessage(msg);

        if (LLVMPrintModuleToFile(module, filename.ptr, &msg) != 0) {
            return error.PrintModuleFailed;
        }
    }

    extern fn LLVMPrintModuleToString(module: *Module) [*:0]const u8;
    pub const printToString = LLVMPrintModuleToString;

    // TODO: inline asm

    extern fn LLVMGetModuleContext(module: *Module) *Context;
    pub const context = LLVMGetModuleContext;

    // TODO: named metadata
    // TODO: debug loc

    extern fn LLVMAddFunction(module: *Module, name: [*:0]const u8, ty: *Type) *Value;
    pub const addFunction = LLVMAddFunction;

    extern fn LLVMGetNamedFunction(module: *Module, name: [*:0]const u8) *Value;
    pub const getNamedFunction = LLVMGetNamedFunction;

    extern fn LLVMGetFirstFunction(module: *Module) *Value;
    pub const getFirstFunction = LLVMGetFirstFunction;

    extern fn LLVMGetLastFunction(module: *Module) *Value;
    pub const getLastFunction = LLVMGetLastFunction;

    extern fn LLVMGetNextFunction(module: *Module) *Value;
    pub const getNextFunction = LLVMGetNextFunction;

    extern fn LLVMGetPreviousFunction(module: *Module) *Value;
    pub const getPreviousFunction = LLVMGetPreviousFunction;
};

pub const Type = extern opaque {
    pub const Kind = enum(c_int) {
        void = 0,
        half = 1,
        float = 2,
        double = 3,
        x86_fp80 = 4,
        fp128 = 5,
        ppc_fp128 = 6,
        label = 7,
        integer = 8,
        function = 9,
        @"struct" = 10,
        array = 11,
        pointer = 12,
        vector = 13,
        metadata = 14,
        // 16 unused (deprecated)
        token = 16,
        scalable_vector = 17,
        bfloat = 18,
        x86_amx = 19,
        target_ext = 20,
    };

    extern fn LLVMGetTypeKind(ty: *Type) Kind;
    pub const kind = LLVMGetTypeKind;

    extern fn LLVMTypeIsSized(ty: *Type) Bool;
    pub inline fn sized(ty: *Type) bool {
        return LLVMTypeIsSized(ty) != 0;
    }

    extern fn LLVMGetTypeContext(ty: *Type) *Context;
    pub const context = LLVMGetTypeContext;

    extern fn LLVMDumpType(ty: *Type) void;
    pub const printToStderr = LLVMDumpType;

    extern fn LLVMPrintTypeToString(ty: *Type) [*:0]const u8;
    pub const printToString = LLVMPrintTypeToString;

    extern fn LLVMGetIntTypeWidth(ty: *Type) c_uint;
    pub const width = LLVMGetIntTypeWidth;

    extern fn LLVMIsFunctionVarArg(ty: *Type) Bool;
    // This function invokes undefined behavior if the type is not a function.
    pub inline fn isVarArg(ty: *Type) bool {
        return LLVMIsFunctionVarArg(ty) != 0;
    }

    extern fn LLVMGetReturnType(ty: *Type) *Type;
    // This function invokes undefined behavior if the type is not a function.
    pub const returnType = LLVMGetReturnType;

    extern fn LLVMCountParamTypes(ty: *Type) c_uint;
    pub const paramTypeCount = LLVMCountParamTypes;
    // This function invokes undefined behavior if the type is not a function.
    extern fn LLVMGetParamTypes(ty: *Type, dst: [*]*Type) void;
    // This function invokes undefined behavior if the type is not a function.
    pub inline fn paramTypesAlloc(ty: *Type, gpa: Allocator) ![]*Type {
        const count: c_uint = LLVMCountParamTypes(ty);
        const types = try gpa.alloc(*Type, count);
        LLVMGetParamTypes(ty, types.ptr);
        return types;
    }
};

pub const Value = extern opaque {
    pub const Kind = enum(c_int) {
        argument,
        basic_block,
        memory_use,
        memory_def,
        memory_phi,

        function,
        global_alias,
        global_ifunc,
        global_variable,
        block_address,
        constant_expr,
        constant_array,
        constant_struct,
        constant_vector,

        undef_value,
        constant_aggregate_zero,
        constant_data_array,
        constant_data_vector,
        constant_int_value,
        constant_fp_value,
        constant_pointer_null,
        constant_token_none,

        metadata_as_value,
        inline_asm,

        instruction,
        poison_value,
        constant_target_none,
        constant_ptr_auth,
    };

    pub const IntPredicate = enum(c_int) {
        eq = 32,
        ne,
        ugt,
        uge,
        ult,
        ule,
        sgt,
        sge,
        slt,
        sle,
    };

    pub const RealPredicate = enum(c_int) {
        false,
        oeq,
        ogt,
        oge,
        olt,
        ole,
        one,
        ord,
        uno,
        ueq,
        ugt,
        uge,
        ult,
        ule,
        une,
        true,
    };

    extern fn LLVMTypeOf(value: *Value) *Type;
    pub const @"type" = LLVMTypeOf;

    extern fn LLVMGetValueKind(value: *Value) Kind;
    pub const kind = LLVMGetValueKind;

    extern fn LLVMGetValueName2(value: *Value, out_len: *usize) [*]const u8;
    pub inline fn getName(value: *Value) []const u8 {
        var len: usize = undefined;
        const ptr = LLVMGetValueName2(value, &len);
        return ptr[0..len];
    }

    extern fn LLVMSetValueName2(value: *Value, ptr: [*]const u8, len: usize) void;
    pub inline fn setName(value: *Value, name: []const u8) void {
        return LLVMSetValueName2(value, name.ptr, name.len);
    }

    extern fn LLVMDumpValue(value: *Value) void;
    pub const printToStderr = LLVMDumpValue;

    extern fn LLVMPrintValueToString(value: *Value) [*:0]const u8;
    pub const printToString = LLVMPrintValueToString;

    // TODO: a lot more

    extern fn LLVMValueIsBasicBlock(value: *Value) Bool;
    pub inline fn isBasicBlock(value: *Value) bool {
        return LLVMValueIsBasicBlock(value) != 0;
    }

    extern fn LLVMValueAsBasicBlock(value: *Value) *BasicBlock;
    pub const asBlock = LLVMValueAsBasicBlock;

    // extern fn LLVMCountBasicBlocks(f: *Value) c_uint;
    // pub const blockCount = LLVMCountBasicBlocks;

    extern fn LLVMGetFirstBasicBlock(f: *Value) *BasicBlock;
    pub const firstBlock = LLVMGetFirstBasicBlock;

    extern fn LLVMGetLastBasicBlock(f: *Value) *BasicBlock;
    pub const lastBlock = LLVMGetLastBasicBlock;

    extern fn LLVMGetNextBasicBlock(f: *Value) *BasicBlock;
    pub const nextBlock = LLVMGetNextBasicBlock;

    extern fn LLVMGetPreviousBasicBlock(f: *Value) *BasicBlock;
    pub const previousBlock = LLVMGetPreviousBasicBlock;

    extern fn LLVMGetEntryBasicBlock(f: *Value) *BasicBlock;
    pub const entryBlock = LLVMGetEntryBasicBlock;

    extern fn LLVMAppendExistingBasicBlock(f: *Value, bb: *BasicBlock) void;
    pub const appendExistingBlock = LLVMAppendExistingBasicBlock;

    extern fn LLVMGetParam(f: *Value, index: c_uint) *Value;
    pub const param = LLVMGetParam;
};

pub const BasicBlock = extern opaque {
    extern fn LLVMBasicBlockAsValue(bb: *BasicBlock) *Value;
    pub const asValue = LLVMBasicBlockAsValue;

    extern fn LLVMGetBasicBlockName(bb: *BasicBlock) [*:0]const u8;
    pub const name = LLVMGetBasicBlockName;

    extern fn LLVMGetBasicBlockParent(bb: *BasicBlock) *Value;
    pub const parent = LLVMGetBasicBlockParent;

    extern fn LLVMGetBasicBlockTerminator(bb: *BasicBlock) *Value;
    pub const terminator = LLVMGetBasicBlockTerminator;

    extern fn LLVMCreateBasicBlockInContext(ctx: *Context, name: [*:0]const u8) *BasicBlock;
    pub const init = LLVMCreateBasicBlockInContext;

    extern fn LLVMAppendBasicBlockInContext(ctx: *Context, f: *Value, name: [*:0]const u8) *BasicBlock;
    pub const append = LLVMAppendBasicBlockInContext;

    extern fn LLVMInsertBasicBlockInContext(ctx: *Context, bb: *BasicBlock, name: [*:0]const u8) *BasicBlock;
    pub const insertBefore = LLVMInsertBasicBlockInContext;

    extern fn LLVMDeleteBasicBlock(bb: *BasicBlock) void;
    pub const delete = LLVMDeleteBasicBlock;

    extern fn LLVMRemoveBasicBlockFromParent(bb: *BasicBlock) void;
    pub const removeFromParent = LLVMRemoveBasicBlockFromParent;
};

pub const Metadata = extern opaque {};

pub const NamedMetadataNode = extern opaque {};

pub const ValueMetadataEntry = extern opaque {};

pub const Builder = extern opaque {
    pub const Opcode = enum(c_int) {
        // terminators
        ret = 1,
        br = 2,
        @"switch" = 3,
        indirect_br = 4,
        invoke = 5,
        // 6 is unused (deprecated)
        @"unreachable" = 7,
        call_br = 67,

        // unary
        fneg = 66,

        // binary
        add = 8,
        fadd = 9,
        sub = 10,
        fsub = 11,
        mul = 12,
        fmul = 13,
        udiv = 14,
        sdiv = 15,
        fdiv = 16,
        urem = 17,
        srem = 18,
        frem = 19,

        // logical
        sll = 20,
        srl = 21,
        sra = 22,
        @"and" = 23,
        @"or" = 24,
        xor = 25,

        // memory
        alloca = 26,
        load = 27,
        store = 28,
        gep = 29,

        // cast
        truncate = 30,
        zext = 31,
        sext = 32,
        fptoui = 33,
        fptosi = 34,
        uitofp = 35,
        sitofp = 36,
        fptrunc = 37,
        fpext = 38,
        ptrtoint = 39,
        inttoptr = 40,
        bitcast = 41,
        addrspace_cast = 60,

        // other
        icmp = 42,
        fcmp = 43,
        phi = 44,
        call = 45,
        select = 46,
        user_op1 = 47,
        user_op2 = 48,
        aarg = 49,
        extract_element = 50,
        insert_element = 51,
        shuffle_vector = 52,
        extract_value = 53,
        insert_value = 54,
        freeze = 68,

        // atomic
        fence = 55,
        atomic_cmpxchg = 56,
        atomic_rmw = 57,

        // exception handling
        @"resume" = 58,
        landing_pad = 59,
        cleanup_ret = 61,
        catch_ret = 62,
        catch_pad = 63,
        cleanup_pad = 64,
        catch_switch = 65,
    };

    extern fn LLVMCreateBuilderInContext(ctx: *Context) *Builder;
    pub const init = LLVMCreateBuilderInContext;

    extern fn LLVMPositionBuilder(builder: *Builder, block: *BasicBlock, inst: *Value) void;
    const position = LLVMPositionBuilder;

    extern fn LLVMPositionBuilderBefore(builder: *Builder, inst: *Value) void;
    pub const positionBefore = LLVMPositionBuilderBefore;

    extern fn LLVMPositionBuilderAtEnd(builder: *Builder, block: *BasicBlock) void;
    pub const positionAtEnd = LLVMPositionBuilderAtEnd;

    extern fn LLVMGetInsertBlock(builder: *Builder) *BasicBlock;
    pub const current = LLVMGetInsertBlock;

    extern fn LLVMClearInsertionPosition(builder: *Builder) void;
    pub const clearPosition = LLVMClearInsertionPosition;

    extern fn LLVMInsertIntoBuilder(builder: *Builder, inst: *Value) void;
    pub const insert = LLVMInsertIntoBuilder;

    extern fn LLVMInsertIntoBuilderWithName(builder: *Builder, inst: *Value, name: [*:0]const u8) void;
    pub const insertWithName = LLVMInsertIntoBuilderWithName;

    extern fn LLVMDisposeBuilder(builder: *Builder) void;
    pub const deinit = LLVMDisposeBuilder;

    // TODO: debug info, metadata, default fp math tag
    extern fn LLVMGetBuilderContext(builder: *Builder) *Context;
    pub const context = LLVMGetBuilderContext;

    extern fn LLVMBuildRetVoid(builder: *Builder) *Value;
    extern fn LLVMBuildRet(builder: *Builder, value: *Value) *Value;
    pub inline fn ret(builder: *Builder, value: ?*Value) *Value {
        if (value) |val| return LLVMBuildRet(builder, val);
        return LLVMBuildRetVoid(builder);
    }

    extern fn LLVMBuildBr(builder: *Builder, dest: *BasicBlock) *Value;
    pub const br = LLVMBuildBr;

    extern fn LLVMBuildCondBr(
        builder: *Builder,
        predicate: *Value,
        exec_if: *BasicBlock,
        exec_else: *BasicBlock,
    ) *Value;
    pub const condBr = LLVMBuildCondBr;

    // TODO: switch. indirect br, call br, invoke, resume

    extern fn LLVMBuildUnreachable(builder: *Builder) *Value;
    pub const @"unreachable" = LLVMBuildUnreachable;

    // TODO: all other exception handling

    extern fn LLVMBuildBinOp(builder: *Builder, op: Opcode, l: *Value, r: *Value, name: [*:0]const u8) *Value;
    pub const binary = LLVMBuildBinOp;

    extern fn LLVMBuildNeg(builder: *Builder, value: *Value, name: [*:0]const u8) *Value;
    pub const neg = LLVMBuildNeg;

    extern fn LLVMBuildNSWNeg(builder: *Builder, value: *Value, name: [*:0]const u8) *Value;
    pub const nswNeg = LLVMBuildNSWNeg;

    extern fn LLVMBuildFNeg(builder: *Builder, value: *Value, name: [*:0]const u8) *Value;
    pub const fneg = LLVMBuildFNeg;

    extern fn LLVMBuildNot(builder: *Builder, value: *Value, name: [*:0]const u8) *Value;
    pub const not = LLVMBuildNot;

    // TODO: non-negative, fast math, disjoint

    extern fn LLVMBuildMalloc(builder: *Builder, ty: *Type, name: [*:0]const u8) *Value;
    pub const malloc = LLVMBuildMalloc;

    extern fn LLVMBuildMemCpy(builder: *Builder, ty: *Type, name: [*:0]const u8) *Value;
    pub const memcopy = LLVMBuildMemCpy;

    extern fn LLVMBuildMemSet(builder: *Builder, ty: *Type, name: [*:0]const u8) *Value;
    pub const memset = LLVMBuildMemSet;

    extern fn LLVMBuildMemMove(builder: *Builder, ty: *Type, name: [*:0]const u8) *Value;
    pub const memmove = LLVMBuildMemMove;

    extern fn LLVMBuildAlloca(builder: *Builder, ty: *Type, name: [*:0]const u8) *Value;
    pub const alloca = LLVMBuildAlloca;

    extern fn LLVMBuildFree(builder: *Builder, ptr: *Value) *Value;
    pub const free = LLVMBuildFree;

    extern fn LLVMBuildLoad2(builder: *Builder, ty: *Type, ptr: *Value, name: [*:0]const u8) *Value;
    pub const load = LLVMBuildLoad2;

    extern fn LLVMBuildStore(builder: *Builder, value: *Value, ptr: *Value) *Value;
    pub const store = LLVMBuildStore;

    pub const GepBounds = enum {
        default,
        inbounds,
    };

    extern fn LLVMBuildGEP2(builder: *Builder, ty: *Type, ptr: *Value, indices: [*]*Value, num_indices: c_uint, name: [*:0]const u8) *Value;
    extern fn LLVMBuildInBoundsGEP2(builder: *Builder, ty: *Type, ptr: *Value, indices: [*]*Value, num_indices: c_uint, name: [*:0]const u8) *Value;
    pub inline fn gep(builder: *Builder, comptime bounds: GepBounds, ty: *Type, ptr: *Value, indices: []const *Value, name: [*:0]const u8) *Value {
        return switch (bounds) {
            .default => LLVMBuildGEP2(builder, ty, ptr, @constCast(indices.ptr), @intCast(indices.len), name),
            .inbounds => LLVMBuildInBoundsGEP2(builder, ty, ptr, @constCast(indices.ptr), @intCast(indices.len), name),
        };
    }

    extern fn LLVMBuildCast(builder: *Builder, op: Opcode, val: *Value, dest: *Type, name: [*:0]const u8) *Value;
    pub const cast = LLVMBuildCast;

    extern fn LLVMBuildICmp(builder: *Builder, op: Value.IntPredicate, l: *Value, r: *Value, name: [*:0]const u8) *Value;
    pub const icmp = LLVMBuildICmp;

    extern fn LLVMBuildFCmp(builder: *Builder, op: Value.RealPredicate, l: *Value, r: *Value, name: [*:0]const u8) *Value;
    pub const fcmp = LLVMBuildFCmp;

    extern fn LLVMBuildPhi(builder: *Builder, ty: *Type, name: [*:0]const u8) *Value;
    pub const phi = LLVMBuildPhi;

    extern fn LLVMBuildCall2(builder: *Builder, ty: *Type, f: *Value, args: [*]*Value, num_args: c_uint, name: [*:0]const u8) *Value;
    pub inline fn call(builder: *Builder, ty: *Type, f: *Value, args: []const *Value, name: [*:0]const u8) *Value {
        return LLVMBuildCall2(builder, ty, f, @constCast(args.ptr), @intCast(args.len), name);
    }

    extern fn LLVMConstInt(ty: *Type, n: c_ulonglong, sign_extend: Bool) *Value;
    pub inline fn iconst(builder: *Builder, ty: *Type, n: u64, sign_extend: bool) *Value {
        _ = builder;
        return LLVMConstInt(ty, @intCast(n), @intFromBool(sign_extend));
    }

    extern fn LLVMConstReal(ty: *Type, n: f64) *Value;
    pub inline fn fconst(builder: *Builder, ty: *Type, n: f64) *Value {
        _ = builder;
        return LLVMConstReal(ty, n);
    }
    // TODO: select, vaarg, elements, values, atomic
    // extern fn LLVMInsertExistingBasicBlockAfterInsertBlock(builder: *Builder, bb: *BasicBlock) void;
    // pub const insertBlock
};

pub const DIBuilder = extern opaque {};

pub const ModuleProvider = extern opaque {};

pub const PassManager = extern opaque {};

pub const Use = extern opaque {};

pub const OperandBundle = extern opaque {};

pub const Attribute = extern opaque {};

pub const DiagnosticInfo = extern opaque {};

pub const Comdat = extern opaque {};

pub const ModuleFlagEntry = extern opaque {};

pub const JITEventListener = extern opaque {};

pub const Binary = extern opaque {};

pub const DbgRecord = extern opaque {};

pub const Target = extern opaque {};

pub const TargetMachine = extern opaque {
    pub const OptimizationLevel = enum(c_int) {
        none,
        less,
        default,
        aggressive,
    };

    pub const RelocationMode = enum(c_int) {
        default,
        static,
        pic,
        dynamic_no_pic,
        ropi,
        rwpi,
        ropi_rwpi,
    };

    pub const CodeModel = enum(c_int) {
        default,
        jit_default,
        tiny,
        small,
        kernel,
        medium,
        large,
    };

    pub const FileType = enum(c_int) {
        assembly,
        object,
    };

    extern fn LLVMCreateTargetMachine(
        target: *Target,
        triple: [*:0]const u8,
        cpu: [*:0]const u8,
        features: [*:0]const u8,
        opt_level: OptimizationLevel,
        reloc: RelocationMode,
        model: CodeModel,
    ) *TargetMachine;
    pub const init = LLVMCreateTargetMachine;
};
