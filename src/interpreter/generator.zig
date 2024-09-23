const std = @import("std");
const c = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/Error.h");
    @cInclude("llvm-c/Analysis.h");
    @cInclude("llvm-c/BitWriter.h");
    @cInclude("llvm-c/TargetMachine.h");
    @cInclude("llvm-c/Target.h");
});

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
        // c.LLVMSetUnnamedAddress(handler, c.LLVMLocalUnnamedAddr);

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
        self.declareJumpTable(2);
        try self.generateLdHandler();
        try self.generateIaddHandler();
        self.defineJumpTable();
    }

    fn generateLdHandler(self: *Generator) !void {
        const handler = try self.addHandler("ld", 2);
        const entry = c.LLVMAppendBasicBlock(handler, "entry");
        c.LLVMPositionBuilderAtEnd(self.builder, entry);

        const immediate = self.readImmediate(2, "imm");
        const dst = self.readRegister(1, "dst.reg");
        self.storeStack(dst, immediate, "dst");

        self.tailCallHandler(3);
        _ = c.LLVMBuildRetVoid(self.builder);
    }

    fn generateIaddHandler(self: *Generator) !void {
        const handler = try self.addHandler("iadd", 4);
        const entry = c.LLVMAppendBasicBlock(handler, "entry");
        c.LLVMPositionBuilderAtEnd(self.builder, entry);

        _ = self.readImmediate(2, "imm");
        _ = c.LLVMBuildRetVoid(self.builder);
    }

    fn readImmediate(self: *Generator, offset: usize, comptime name: [:0]const u8) Value {
        const insert_block = c.LLVMGetInsertBlock(self.builder);
        const function = c.LLVMGetBasicBlockParent(insert_block);
        const ip = c.LLVMGetParam(function, ip_param_index);

        const offset_value = c.LLVMConstInt(self.usize_type, @intCast(offset), @intFromBool(true));
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
        const offset_value = c.LLVMConstInt(self.usize_type, @intCast(offset), @intFromBool(true));
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
        return value;
    }

    fn storeStack(self: *Generator, register: Value, value: Value, comptime name: [:0]const u8) void {
        const insert_block = c.LLVMGetInsertBlock(self.builder);
        const function = c.LLVMGetBasicBlockParent(insert_block);
        const fp = c.LLVMGetParam(function, fp_param_index);

        // load data from stack using the register offset
        const stack_gep = c.LLVMBuildInBoundsGEP2(self.builder, self.usize_type, fp, @constCast(&register), 1, name ++ ".ptr");
        _ = c.LLVMBuildStore(self.builder, value, stack_gep);
    }

    fn declareJumpTable(self: *Generator, count: u32) void {
        const jump_table_type = c.LLVMArrayType2(self.ptr_type, count);
        const jump_table = c.LLVMAddGlobal(self.module, jump_table_type, "jump_table");
        c.LLVMSetLinkage(jump_table, c.LLVMPrivateLinkage);
        c.LLVMSetUnnamedAddress(jump_table, c.LLVMLocalUnnamedAddr);
    }

    fn defineJumpTable(self: *Generator) void {
        const jump_table = c.LLVMGetNamedGlobal(self.module, "jump_table");
        const handlers: []const Value = &.{
            c.LLVMGetNamedFunction(self.module, "ld"),
            c.LLVMGetNamedFunction(self.module, "iadd"),
        };
        const pointers = c.LLVMConstArray2(self.ptr_type, @constCast(handlers.ptr), @intCast(handlers.len));
        c.LLVMSetInitializer(jump_table, pointers);
    }

    fn tailCallHandler(self: *Generator, offset: u32) void {
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
