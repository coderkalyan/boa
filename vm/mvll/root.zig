const std = @import("std");
const bindings = @import("bindings.zig");

pub const DiagnosticHandler = bindings.DiagnosticHandler;
pub const YieldCallback = bindings.YieldCallback;
pub const MemoryBuffer = bindings.MemoryBuffer;
pub const Context = bindings.Context;
pub const Module = bindings.Module;
pub const Type = bindings.Type;
pub const Value = bindings.Value;
pub const BasicBlock = bindings.BasicBlock;
pub const Metadata = bindings.Metadata;
pub const NamedMetadataNode = bindings.NamedMetadataNode;
pub const ValueMetadataEntry = bindings.ValueMetadataEntry;
pub const Builder = bindings.Builder;
pub const DIBuilder = bindings.DIBuilder;
pub const ModuleProvider = bindings.ModuleProvider;
pub const PassManager = bindings.PassManager;
pub const Use = bindings.Use;
pub const OperandBundle = bindings.OperandBundle;
pub const Attribute = bindings.Attribute;
pub const DiagnosticInfo = bindings.DiagnosticInfo;
pub const Comdat = bindings.Comdat;
pub const ModuleFlagEntry = bindings.ModuleFlagEntry;
pub const JITEventListener = bindings.JITEventListener;
pub const Binary = bindings.Binary;
pub const DbgRecord = bindings.DbgRecord;
pub const Target = bindings.Target;
pub const TargetMachine = bindings.TargetMachine;

test "hello" {
    const ctx = Context.init();
    defer ctx.deinit();

    const module = Module.init("hello", ctx);
    defer module.deinit();

    const function_type = ctx.function(ctx.void(), &.{ctx.int(32)}, false);
    const function = module.addFunction("hello", function_type);
    _ = function;

    std.debug.print("{s}", .{module.printToString()});
}
