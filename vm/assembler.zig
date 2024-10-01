// Macro assembler for the boa runtime VM. It uses LLVM to emit bytecode handlers at compile time,
// which are then linked statically into the boa executable.

const std = @import("std");
const mvll = @import("mvll/root.zig");
const Bytecode = @import("Bytecode.zig");
const c = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/Target.h");
    @cInclude("llvm-c/TargetMachine.h");
});

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

    const address_space = 0;

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

        // const HandlerData = struct {};
        // const handlers: []HandlerData = .{
        //     .{ .ld, "ld" },
        // };

        // declareHandlers(&handlers);

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

    // pub fn assemble(self: *Assembler) void {}

    // fn handler(self: *Assembler, comptime name: [:0]const u8, deref: u32) *Value {
    //
    // }

    fn param(self: *Assembler, comptime p: Param) *Value {
        const function = self.builder.current().parent();
        return function.param(@intFromEnum(p));
    }

    const Extend = enum { none, zext, sext };
    fn read(self: *Assembler, offset: *Value, comptime extend: Extend, comptime name: [:0]const u8) *Value {
        const ip = self.param(.ip);
        const gep = self.builder.gep(.inbounds, self.ctx.int(32), ip, &.{offset}, name ++ ".ptr");
        const raw = self.builder.load(self.ctx.int(32), gep, if (extend == .none) name else name ++ ".i32");
        return switch (extend) {
            .none => raw,
            .zext => self.builder.cast(.zext, raw, self.ctx.int(64), name),
            .sext => self.builder.cast(.sext, raw, self.ctx.int(64), name),
        };
    }

    fn loadBase(self: *Assembler, base: *Value, offset: *Value, comptime ty: Type.Kind, comptime name: [:0]const u8) *Value {
        const gep = self.builder.gep(.inbounds, self.ctx.int(64), base, &.{offset}, name ++ ".ptr");
        const value = self.builder.load(self.ctx.int(64), gep, if (ty == .integer) name else name ++ ".i64");
        c.LLVMSetAlignment(@ptrCast(value), @alignOf(u64));
        return switch (ty) {
            .integer => value,
            .double => self.builder.cast(.bitcast, value, self.ctx.float(.double), name),
            .pointer => self.builder.cast(.inttoptr, value, self.ctx.ptr(address_space), name),
            else => unreachable,
        };
    }

    fn load(self: *Assembler, offset: *Value, comptime ty: Type.Kind, comptime name: [:0]const u8) *Value {
        const fp = self.param(.fp);
        return self.loadBase(fp, offset, ty, name);
    }

    fn storeBase(self: *Assembler, base: *Value, offset: *Value, value: *Value, comptime ty: Type.Kind, comptime name: [:0]const u8) void {
        const gep = self.builder.gep(.inbounds, self.ctx.int(64), base, &.{offset}, name ++ ".ptr");
        const value_int = switch (ty) {
            .integer => value,
            .double => self.builder.cast(.bitcast, value, self.ctx.int(64), name ++ ".int"),
            .pointer => self.builder.cast(.ptrtoint, value, self.ctx.int(64), name ++ ".int"),
            else => unreachable,
        };
        const store_inst = self.builder.store(value_int, gep);
        c.LLVMSetAlignment(@ptrCast(store_inst), @alignOf(u64));
    }

    fn store(self: *Assembler, offset: *Value, value: *Value, comptime ty: Type.Kind, comptime name: [:0]const u8) void {
        const fp = self.param(.fp);
        self.storeBase(fp, offset, value, ty, name);
    }
};

test "read ir" {
    var assembler = try Assembler.init("interpreter");
    const function_type = assembler.ctx.function(
        assembler.ctx.void(),
        &.{assembler.ctx.ptr(Assembler.address_space)}, // ip
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
            \\define void @read_none(ptr %0) {
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
            \\define void @read_zext(ptr %0) {
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
            \\define void @read_sext(ptr %0) {
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
            \\define void @load_base(ptr %0, ptr %1) {
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
            \\define void @load_fp(ptr %0, ptr %1) {
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
            \\define void @load_float(ptr %0, ptr %1) {
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
            \\define void @load_ptr(ptr %0, ptr %1) {
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
            \\define void @store_base(ptr %0, ptr %1) {
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
            \\define void @store_fp(ptr %0, ptr %1) {
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
            \\define void @store_float(ptr %0, ptr %1) {
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
            \\define void @store_ptr(ptr %0, ptr %1) {
            \\entry:
            \\  %store.ptr = getelementptr inbounds i64, ptr %1, i32 2
            \\  store i64 0, ptr %store.ptr, align 8
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

    const assembler = try Assembler.init("interpreter");
    // assembler.module.printToStderr();
    _ = args;
    try assembler.assemble();
    // try assembler.finalize(args[1]);
}
