const core = @import("core.zig");
const Error = @import("error.zig").Error;

const Context = core.Context;
const Module = core.Module;
pub const MemoryBuffer = core.MemoryBuffer;

pub const JITTargetAddress = u64;
pub const ExecutorAddress = u64;

pub const JITSymbolGenericFlags = enum(c_uint) {
    none = 0,
    exported = 1,
    weak = 2,
    callable = 4,
    materialization_side_effects_only = 8,
};

pub const JITSymbolTargetFlags = u8;

pub const JITSymbolFlags = struct {
    generic_flags: u8,
    target_flags: u8,
};

pub const JITEvaluatedSymbol = struct {
    address: ExecutorAddress,
    flags: JITSymbolFlags,
};

pub const ExecutionSession = opaque {
    pub const ErrorReporter = *const fn (ctx: *anyopaque, err: Error) void;
    pub const LookupHandleResult = *const fn (err: *Error, result: [*]SymbolMapPair, len: usize, ctx: *anyopaque) void;

    extern fn LLVMOrcExecutionSessionSetErrorReporter(es: *ExecutionSession, reporter: ErrorReporter, ctx: *anyopaque) void;
    pub const SetErrorReporter = LLVMOrcExecutionSessionSetErrorReporter;

    extern fn LLVMOrcExecutionSessionGetSymbolStringPool(es: *ExecutionSession) *SymbolStringPool;
    pub const symbolStringPool = LLVMOrcExecutionSessionGetSymbolStringPool;

    extern fn LLVMOrcExecutionSessionIntern(es: *ExecutionSession, name: [*:0]const u8) *SymbolStringPool.Entry;
    pub const intern = LLVMOrcExecutionSessionIntern;

    extern fn LLVMOrcExecutionSessionLookup(
        es: *ExecutionSession,
        kind: LookupKind,
        order_ptr: [*]const JITDylib.SearchOrderElement,
        order_len: usize,
        symbols_ptr: [*]const LookupSetElement,
        symbols_len: usize,
        handler: LookupHandleResult,
        ctx: *anyopaque,
    ) void;
    pub inline fn lookup(es: *ExecutionSession, kind: LookupKind, order: JITDylib.SearchOrder, symbols: LookupSet, handler: LookupHandleResult, ctx: *anyopaque) void {
        LLVMOrcExecutionSessionLookup(es, kind, order.ptr, order.len, symbols.ptr, symbols.len, handler, ctx);
    }
};

pub const SymbolStringPool = opaque {
    pub const Entry = opaque {
        extern fn LLVMOrcRetainSymbolStringPoolEntry(s: *Entry) void;
        pub const retain = LLVMOrcRetainSymbolStringPoolEntry;

        extern fn LLVMOrcReleaseSymbolStringPoolEntry(s: *Entry) void;
        pub const release = LLVMOrcReleaseSymbolStringPoolEntry;

        extern fn LLVMOrcSymbolStringPoolEntryStr(s: *Entry) [*:0]const u8;
        pub const str = LLVMOrcSymbolStringPoolEntryStr;
    };

    extern fn LLVMOrcSymbolStringPoolClearDeadEntries(ssp: *SymbolStringPool) void;
    pub const clearDeadEntries = LLVMOrcSymbolStringPoolClearDeadEntries;
};

pub const SymbolFlagsMapPair = struct {
    name: *SymbolStringPool.Entry,
    flags: JITSymbolFlags,
};

pub const SymbolMapPair = struct {
    name: *SymbolStringPool.Entry,
    sym: JITEvaluatedSymbol,
};

pub const JITDylib = opaque {
    pub const LookupFlags = enum(c_uint) {
        match_exported_symbols_only,
        match_all_symbols,
    };

    pub const SearchOrderElement = struct {
        jd: *JITDylib,
        lookup_flags: LookupFlags,
    };

    pub const SearchOrder = []const SearchOrderElement;
};

pub const DependenceMapPair = struct {
    jd: *JITDylib,
    names: []*SymbolStringPool.Entry,
};

pub const LookupKind = enum(c_uint) {
    static,
    dl_sym,
};

pub const SymbolLookupFlags = enum(c_uint) {
    required_symbol,
    weakly_referenced_symbol,
};

pub const LookupSetElement = struct {
    name: *SymbolStringPool.Entry,
    flags: SymbolLookupFlags,
};

pub const LookupSet = []const LookupSetElement;

pub const MaterializationUnit = opaque {
    pub const Materialize = *const fn (ctx: *anyopaque, mr: *MaterializationResponsibility) void;
    pub const Discard = *const fn (ctx: *anyopaque, jd: *JITDylib, symbol: *SymbolStringPool.Entry) void;
    pub const Destroy = *const fn (ctx: *anyopaque) void;

    extern fn LLVMOrcDisposeMaterializationUnit(mu: *MaterializationUnit) void;
    pub const deinit = LLVMOrcDisposeMaterializationUnit;

    extern fn LLVMOrcCreateCustomMaterializationUnit(
        name: [*:0]const u8,
        ctx: *anyopaque,
        syms_ptr: [*]const SymbolFlagsMapPair,
        syms_len: usize,
        init_sym: *SymbolStringPool.Entry,
        materialize: Materialize,
        discard: Discard,
        destroy: Destroy,
    ) *MaterializationUnit;
    pub inline fn init(name: []const u8, ctx: *anyopaque, syms: []const SymbolFlagsMapPair, init_sym: *SymbolStringPool.Entry, materialize: Materialize, discard: Discard, destroy: Destroy) *MaterializationUnit {
        return LLVMOrcCreateCustomMaterializationUnit(name.ptr, ctx, syms.ptr, syms.len, init_sym, materialize, discard, destroy);
    }

    extern fn LLVMOrcAbsoluteSymbols(syms_ptr: [*]const SymbolFlagsMapPair, syms_len: usize) *MaterializationUnit;
    pub inline fn absoluteSymbols(syms: []const SymbolFlagsMapPair) *MaterializationUnit {
        return LLVMOrcAbsoluteSymbols(syms.ptr, syms.len);
    }

    // extern fn LLVMOrcLazyReexports(lctm: *LazyCallThroughManager, ism: *IndirectStubsManager, source: *JITDylib, aliases: )
};

pub const MaterializationResponsibility = opaque {
    extern fn LLVMOrcDisposeMaterializationResponsibility(mr: *MaterializationResponsibility) void;
    pub const deinit = LLVMOrcDisposeMaterializationResponsibility;

    extern fn LLVMOrcMaterializationResponsibilityGetTargetDylib(mr: *MaterializationResponsibility) *JITDylib;
    pub const targetDylib = LLVMOrcMaterializationResponsibilityGetTargetDylib;

    extern fn LLVMOrcMaterializationResponsibilityGetExecutionSession(mr: *MaterializationResponsibility) *ExecutionSession;
    pub const executionSession = LLVMOrcMaterializationResponsibilityGetExecutionSession;

    extern fn LLVMOrcMaterializationResponsibilityGetSymbols(mr: *MaterializationResponsibility, out_len: *usize) [*]const SymbolFlagsMapPair;
    pub inline fn symbols(mr: *MaterializationResponsibility) []const SymbolFlagsMapPair {
        var len: usize = undefined;
        const ptr = LLVMOrcMaterializationResponsibilityGetSymbols(mr, &len);
        return ptr[0..len];
    }
};

pub const DefinitionGenerator = opaque {
    extern fn LLVMOrcDisposeDefinitionGenerator(dg: *DefinitionGenerator) void;
    pub const deinit = LLVMOrcDisposeDefinitionGenerator;
};

pub const ResourceTracker = opaque {
    extern fn LLVMOrcReleaseResourceTracker(rt: *ResourceTracker) void;
    pub const release = LLVMOrcReleaseResourceTracker;

    extern fn LLVMOrcResourceTrackerTransferTo(src: *ResourceTracker, dst: *ResourceTracker) void;
    pub const transfer = LLVMOrcResourceTrackerTransferTo;

    extern fn LLVMOrcResourceTrackerRemove(rt: *ResourceTracker) *Error;
    pub const remove = LLVMOrcResourceTrackerRemove;
};

pub const LookupState = opaque {};

// TODO: generator function

pub const ThreadSafeContext = opaque {};

pub const ThreadSafeModule = opaque {};

pub const GenericIRModuleOperation = *const fn (ctx: *anyopaque, m: *Module) *Error;

pub const JITTargetMachineBuilder = opaque {};

pub const ObjectLayer = opaque {};

pub const ObjectLinkingLayer = opaque {};

pub const IRTransformLayer = opaque {
    pub const Transform = *const fn (
        ctx: *anyopaque,
        in_out_module: *ThreadSafeModule,
        mr: *MaterializationResponsibility,
    ) *Error;
};

pub const ObjectTransformLayer = opaque {
    pub const Transform = *const fn (
        ctx: *anyopaque,
        in_out_object: *MemoryBuffer,
    ) *Error;
};

pub const IndirectStubsManager = opaque {};

pub const LazyCallThroughManager = opaque {};

pub const DumpObjects = opaque {};

pub const LLJITBuilder = opaque {
    pub const ObjectLinkingLayerCreator = *const fn (ctx: *anyopaque, session: *ExecutionSession, triple: [*:0]const u8) *ObjectLayer;

    extern fn LLVMOrcCreateLLJITBuilder() *LLJITBuilder;
    pub const init = LLVMOrcCreateLLJITBuilder;

    extern fn LLVMOrcDisposeLLJITBuilder(builder: *LLJITBuilder) void;
    pub const deinit = LLVMOrcDisposeLLJITBuilder;

    extern fn LLVMOrcLLJITBuilderSetTargetMachineBuilder(builder: *LLJITBuilder, jtmb: *JITTargetMachineBuilder) void;
    pub const setTargetMachineBuilder = LLVMOrcLLJITBuilderSetTargetMachineBuilder;

    extern fn LLVMOrcLLJITBuilderSetObjectLinkingLayerCreator(builder: *LLJITBuilder, f: ObjectLinkingLayerCreator, ctx: *anyopaque) void;
    pub const setObjectLinkingLayerCreator = LLVMOrcLLJITBuilderSetObjectLinkingLayerCreator;
};

pub const LLJIT = opaque {
    extern fn LLVMOrcCreateLLJIT(out_lljit: *LLJIT, builder: ?*LLJITBuilder) *Error;
    pub const init = LLVMOrcCreateLLJIT;

    extern fn LLVMOrcDisposeLLJIT(lljit: *LLJIT) *Error;
    pub const deinit = LLVMOrcDisposeLLJIT;

    extern fn LLVMOrcLLJITGetExecutionSession(lljit: *LLJIT) *ExecutionSession;
    pub const executionSession = LLVMOrcLLJITGetExecutionSession;

    extern fn LLVMOrcLLJITGetMainJITDylib(lljit: *LLJIT) *JITDylib;
    pub const mainJITDylib = LLVMOrcLLJITGetMainJITDylib;

    extern fn LLVMOrcLLJITGetTripleString(lljit: *LLJIT) [*:0]const u8;
    pub const tripleString = LLVMOrcLLJITGetTripleString;

    extern fn LLVMOrcLLJITGetGlobalPrefix(lljit: *LLJIT) u8;
    pub const globalPrefix = LLVMOrcLLJITGetGlobalPrefix;

    extern fn LLVMOrcLLJITMangleAndIntern(lljit: *LLJIT, name: [*:0]const u8) *SymbolStringPool.Entry;
    pub const mangleAndIntern = LLVMOrcLLJITMangleAndIntern;

    extern fn LLVMOrcLLJITAddObjectFile(lljit: *LLJIT, dylib: *JITDylib, buffer: *MemoryBuffer) *Error;
    pub const addObjectFile = LLVMOrcLLJITAddObjectFile;

    extern fn LLVMOrcLLJITAddObjectFileWithRT(lljit: *LLJIT, rt: *ResourceTracker, buffer: *MemoryBuffer) *Error;
    pub const addObjectFileWithRT = LLVMOrcLLJITAddObjectFileWithRT;

    extern fn LLVMOrcLLJITAddLLVMIRModule(lljit: *LLJIT, dylib: *JITDylib, tsm: *ThreadSafeModule) *Error;
    pub const addLLVMIrModule = LLVMOrcLLJITAddLLVMIRModule;

    extern fn LLVMOrcLLJITAddLLVMIRModuleWithRT(lljit: *LLJIT, rt: *ResourceTracker, tsm: *ThreadSafeModule) *Error;
    pub const addLLVMIrModuleWithRT = LLVMOrcLLJITAddLLVMIRModuleWithRT;

    extern fn LLVMOrcLLJITLookup(lljit: *LLJIT, result: *ExecutorAddress, name: [*:0]const u8) *Error;
    pub const lookup = LLVMOrcLLJITLookup;

    extern fn LLVMOrcLLJITGetObjLinkingLayer(lljit: *LLJIT) *ObjectLayer;
    pub const objectLayer = LLVMOrcLLJITGetObjLinkingLayer;

    extern fn LLVMOrcLLJITGetObjTranformLayer(lljit: *LLJIT) *ObjectTransformLayer;
    pub const objectTransformLayer = LLVMOrcLLJITGetObjTranformLayer;

    extern fn LLVMOrcLLJITGetIRTranformLayer(lljit: *LLJIT) *IRTransformLayer;
    pub const irTransformLayer = LLVMOrcLLJITGetIRTranformLayer;

    extern fn LLVMOrcLLJITGetDataLayoutStr(lljit: *LLJIT) [*:0]const u8;
    pub const dataLayoutStr = LLVMOrcLLJITGetDataLayoutStr;
};
