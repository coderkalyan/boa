const std = @import("std");
const core = @import("core.zig");

pub const DiagnosticHandler = core.DiagnosticHandler;
pub const YieldCallback = core.YieldCallback;
pub const MemoryBuffer = core.MemoryBuffer;
pub const Context = core.Context;
pub const Module = core.Module;
pub const Type = core.Type;
pub const Value = core.Value;
pub const BasicBlock = core.BasicBlock;
pub const Metadata = core.Metadata;
pub const NamedMetadataNode = core.NamedMetadataNode;
pub const ValueMetadataEntry = core.ValueMetadataEntry;
pub const Builder = core.Builder;
pub const DIBuilder = core.DIBuilder;
pub const ModuleProvider = core.ModuleProvider;
pub const PassManager = core.PassManager;
pub const Use = core.Use;
pub const OperandBundle = core.OperandBundle;
pub const Attribute = core.Attribute;
pub const DiagnosticInfo = core.DiagnosticInfo;
pub const Comdat = core.Comdat;
pub const ModuleFlagEntry = core.ModuleFlagEntry;
pub const JITEventListener = core.JITEventListener;
pub const Binary = core.Binary;
pub const DbgRecord = core.DbgRecord;
pub const Target = core.Target;
pub const TargetMachine = core.TargetMachine;

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
