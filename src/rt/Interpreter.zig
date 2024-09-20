const std = @import("std");
const Assembler = @import("../bc/Assembler.zig");
const Bytecode = @import("../bc/Bytecode.zig");
const InternPool = @import("../InternPool.zig");
const IrGen = @import("../ir/IrGen.zig");
const Ir = @import("../ir/Ir.zig");
const render = @import("../render.zig");
const String = @import("string.zig").String;
const Object = @import("object.zig").Object;
const ShapePool = @import("ShapePool.zig");
const ConstantPool = @import("ConstantPool.zig");
const builtins = @import("builtins.zig");

const Allocator = std.mem.Allocator;
const Word = Bytecode.Word;
const Opcode = Bytecode.Opcode;
const asBytes = std.mem.asBytes;
const Shape = ShapePool.Shape;
// pub const GlobalMap = std.AutoHashMap(InternPool.Index, i64);
const FunctionInfo = InternPool.FunctionInfo;

pub const Slot = extern union {
    int: i64,
    float: f64,
    ptr: *anyopaque,
};

pub const ContextFrame = extern struct {
    pba: *const Allocator,
    global_object: *Object,
    shape_pool: *ShapePool,
    constant_pool: *ConstantPool,
    ic_vector: [*]u32,

    comptime {
        for (std.meta.fields(ContextFrame)) |field| {
            std.debug.assert(@sizeOf(field.type) == @sizeOf(*anyopaque));
        }
    }
};

pub const CallFrame = struct {
    // pointer to the start of the bytecode array for this function
    code: [*]const Word,
    // return program counter at time of call (points to instruction after the call)
    pc: u64,
    // frame pointer for the value stack
    fp: u64,
    // register count for the current value stack "frame"
    register_count: u64,
    // register to place the return value after the call
    return_register: u64,

    comptime {
        for (std.meta.fields(ContextFrame)) |field| {
            std.debug.assert(@sizeOf(field.type) == @sizeOf(*anyopaque));
        }
    }
};

const Handler = *const fn (
    in_pc: usize,
    code: [*]const Word,
    fp: u64,
    stack: [*]Slot,
) void;
const jump_table: [std.meta.tags(Opcode).len]Handler = .{
    ld, // ld
    ldi, // ldi
    ldg, // ld_global
    stg, // st_global
    unary(.mov), // mov
    unary(.itof), // itof
    unary(.ftoi), // ftoi
    unary(.ineg), // ineg
    unary(.fneg), // fneg
    unary(.binv), // binv
    unary(.lnot), // lnot
    binary(.iadd), // iadd
    binary(.fadd), // fadd
    binary(.isub), // isub
    binary(.fsub), // fsub
    binary(.imul), // imul
    binary(.fmul), // fmul
    binary(.idiv), // idiv
    binary(.fdiv), // fdiv
    binary(.imod), // imod
    binary(.fmod), // fmod
    binary(.ipow), // ipow
    binary(.fpow), // fpow
    binary(.bor), // bor
    binary(.band), // band
    binary(.bxor), // bxor
    binary(.sll), // sll
    binary(.sra), // sra
    binary(.ieq), // ieq
    binary(.ine), // ine
    binary(.ilt), // ilt
    binary(.flt), // flt
    binary(.igt), // igt
    binary(.fgt), // fgt
    binary(.ile), // ile
    binary(.fle), // fle
    binary(.ige), // ige
    binary(.fge), // fge
    call, // call
    pint, // pint
    pfloat, // pfloat
    pbool, // pbool
    pstr, // pstr
    strlen, // strlen
    strcat, // strcat
    strrep, // strrep
    branch, // branch
    jump, // jump
    ret, // ret
    exit, // exit
};

pub fn entry(pc: usize, code: [*]const Word, fp: u64, stack: [*]Slot) void {
    next(pc, code, fp, stack);
}

inline fn next(pc: usize, code: [*]const Word, fp: u64, stack: [*]Slot) void {
    const opcode = code[pc].opcode;
    const handler = jump_table[@intFromEnum(opcode)];
    // std.debug.print("{s}\n", .{@tagName(opcode)});
    @call(.always_tail, handler, .{ pc, code, fp, stack });
}

fn slot(stack: [*]Slot, fp: u64, register: Bytecode.Register) *Slot {
    const temp: i128 = @intCast(fp);
    const pos: u64 = @intCast(temp + register);
    return &stack[pos];
}

fn contextFrameFieldType(comptime field_name: []const u8) type {
    inline for (std.meta.fields(ContextFrame)) |field| {
        if (std.mem.eql(u8, field.name, field_name)) return field.type;
    }

    unreachable;
}

fn contextFieldPtr(
    stack: [*]Slot,
    comptime field_name: []const u8,
) contextFrameFieldType(field_name) {
    inline for (std.meta.fields(ContextFrame), 0..) |field, i| {
        if (std.mem.eql(u8, field.name, field_name)) {
            return @ptrCast(@alignCast(stack[i].ptr));
        }
    }

    unreachable;
}

fn ld(pc: usize, code: [*]const Word, fp: u64, stack: [*]Slot) void {
    const dst = code[pc + 1].register;
    const imm = code[pc + 2].imm;
    slot(stack, fp, dst).int = imm;
    next(pc + 3, code, fp, stack);
}

fn ldi(pc: usize, code: [*]const Word, fp: u64, stack: [*]Slot) void {
    const dst = code[pc + 1].register;
    const index = code[pc + 2].count;

    const constant_array: [*]*anyopaque = @ptrFromInt(code[2].imm | (@as(u64, code[3].imm) << 32));
    slot(stack, fp, dst).ptr = constant_array[index];
    next(pc + 3, code, fp, stack);
}

fn ldg(pc: usize, code: [*]const Word, fp: u64, stack: [*]Slot) void {
    const dst = code[pc + 1].register;
    const ip = code[pc + 2].ip;
    const ic = code[pc + 3].count;

    const global_object = contextFieldPtr(stack, "global_object");
    const shape_pool = contextFieldPtr(stack, "shape_pool");
    const ic_vector = contextFieldPtr(stack, "ic_vector");
    if (ic_vector[ic] == std.math.maxInt(u32)) {
        ic_vector[ic] = shape_pool.attributeIndex(global_object.shape, ip).?; // TODO: what if nonexistant
    }
    const index = ic_vector[ic];
    slot(stack, fp, dst).* = @bitCast(global_object.attributes.items[index]);
    next(pc + 4, code, fp, stack);
}

fn stg(pc: usize, code: [*]const Word, fp: u64, stack: [*]Slot) void {
    const op = code[pc + 1].register;
    const ip = code[pc + 2].ip;
    const global_object = contextFieldPtr(stack, "global_object");
    const shape_pool = contextFieldPtr(stack, "shape_pool");
    const pba = contextFieldPtr(stack, "pba");

    // TODO: merge types
    const src = slot(stack, fp, op);
    if (shape_pool.attributeIndex(global_object.shape, ip)) |index| {
        global_object.attributes.items[index] = @bitCast(src.*);
        next(pc + 3, code, fp, stack);
    }

    global_object.shape = shape_pool.transition(global_object.shape, ip) catch unreachable;
    global_object.attributes.append(pba.*, src.int) catch unreachable;
    next(pc + 3, code, fp, stack);
}

inline fn unary(comptime opcode: Opcode) Handler {
    return struct {
        pub fn unary(pc: usize, code: [*]const Word, fp: u64, stack: [*]Slot) void {
            const dst = code[pc + 1].register;
            const src = code[pc + 2].register;

            const op = slot(stack, fp, src);
            slot(stack, fp, dst).* = switch (opcode) {
                .mov => op.*,
                .itof => .{ .float = @floatFromInt(op.int) },
                .ftoi => .{ .int = @intFromFloat(op.float) },
                .ineg => .{ .int = -op.int },
                .fneg => .{ .float = -op.float },
                .binv => .{ .int = ~op.int },
                .lnot => .{ .int = @intFromBool(op.int == 0) },
                else => unreachable,
            };

            next(pc + 3, code, fp, stack);
        }
    }.unary;
}

inline fn binary(comptime opcode: Opcode) Handler {
    return struct {
        pub fn binary(pc: usize, code: [*]const Word, fp: u64, stack: [*]Slot) void {
            const dst = code[pc + 1].register;
            const src1 = code[pc + 2].register;
            const src2 = code[pc + 3].register;

            const op1 = slot(stack, fp, src1);
            const op2 = slot(stack, fp, src2);
            slot(stack, fp, dst).* = switch (opcode) {
                .iadd => .{ .int = op1.int + op2.int },
                .fadd => .{ .float = op1.float + op2.float },
                .isub => .{ .int = op1.int - op2.int },
                .fsub => .{ .float = op1.float - op2.float },
                .imul => .{ .int = op1.int * op2.int },
                .fmul => .{ .float = op1.float * op2.float },
                .idiv => .{ .int = @divFloor(op1.int, op2.int) },
                .fdiv => .{ .float = op1.float / op2.float },
                .imod => .{ .int = @mod(op1.int, op2.int) },
                .fmod => .{ .float = @mod(op1.float, op2.float) },
                .ipow => .{ .int = std.math.pow(i64, op1.int, op2.int) },
                .fpow => .{ .float = std.math.pow(f64, op1.float, op2.float) },
                .bor => .{ .int = op1.int | op2.int },
                .band => .{ .int = op1.int & op2.int },
                .bxor => .{ .int = op1.int ^ op2.int },
                .sll => .{ .int = op1.int << @truncate(@as(u64, @bitCast(op2.int))) },
                .sra => .{ .int = op1.int >> @truncate(@as(u64, @bitCast(op2.int))) },
                .ieq => .{ .int = @intFromBool(op1.int == op2.int) },
                .ine => .{ .int = @intFromBool(op1.int != op2.int) },
                .ilt => .{ .int = @intFromBool(op1.int < op2.int) },
                .flt => .{ .int = @intFromBool(op1.float < op2.float) },
                .igt => .{ .int = @intFromBool(op1.int > op2.int) },
                .fgt => .{ .int = @intFromBool(op1.float > op2.float) },
                .ile => .{ .int = @intFromBool(op1.int <= op2.int) },
                .fle => .{ .int = @intFromBool(op1.float <= op2.float) },
                .ige => .{ .int = @intFromBool(op1.int >= op2.int) },
                .fge => .{ .int = @intFromBool(op1.float >= op2.float) },
                else => unreachable,
            };

            next(pc + 4, code, fp, stack);
        }
    }.binary;
}

fn strlen(pc: usize, code: [*]const Word, fp: u64, stack: [*]Slot) void {
    const dst = code[pc + 1].register;
    const src = code[pc + 2].register;
    var pool: *InternPool = undefined;
    pool = @ptrFromInt(code[0].imm | (@as(u64, code[1].imm) << 32));

    const str: *const String = @ptrCast(@alignCast(slot(stack, fp, src).ptr));
    slot(stack, fp, dst).int = @intCast(str.len);

    next(pc + 3, code, fp, stack);
}

fn strcat(pc: usize, code: [*]const Word, fp: u64, stack: [*]Slot) void {
    const dst = code[pc + 1].register;
    const src1 = code[pc + 2].register;
    const src2 = code[pc + 3].register;
    const pba = contextFieldPtr(stack, "pba");

    const a: *const String = @ptrCast(@alignCast(slot(stack, fp, src1).ptr));
    const b: *const String = @ptrCast(@alignCast(slot(stack, fp, src2).ptr));
    const str = String.catenate(pba.*, a, b) catch unreachable;
    slot(stack, fp, dst).ptr = @constCast(str);

    next(pc + 4, code, fp, stack);
}

fn strrep(pc: usize, code: [*]const Word, fp: u64, stack: [*]Slot) void {
    const dst = code[pc + 1].register;
    const src1 = code[pc + 2].register;
    const src2 = code[pc + 3].register;
    const pba = contextFieldPtr(stack, "pba");

    const template: *const String = @ptrCast(@alignCast(slot(stack, fp, src1).ptr));
    const count: u64 = @intCast(slot(stack, fp, src2).int);
    const str = String.repeat(pba.*, template, count) catch unreachable;
    slot(stack, fp, dst).ptr = @constCast(str);

    next(pc + 4, code, fp, stack);
}

fn branch(pc: usize, code: [*]const Word, fp: u64, stack: [*]Slot) void {
    const condition = code[pc + 1].register;
    if (slot(stack, fp, condition).int == 1) {
        const target = code[pc + 2].target;
        next(target, code, fp, stack);
    }
    next(pc + 3, code, fp, stack);
}

fn jump(pc: usize, code: [*]const Word, fp: u64, stack: [*]Slot) void {
    const target = code[pc + 1].target;
    next(target, code, fp, stack);
}

fn call(pc: usize, code: [*]const Word, fp: u64, stack: [*]Slot) void {
    const return_register = code[pc + 2].register;
    const argument_count = code[pc + 3].count;

    // stack[fp - 1] is a pointer to the call frame, which is a separate
    // object (also allocated contiguously from a separate buffer)
    const caller_frame: *CallFrame = @ptrCast(@alignCast(slot(stack, fp, -1).ptr));

    // push function arguments to the value stack
    var sp = fp + caller_frame.register_count;
    var i: u32 = argument_count;
    while (i > 0) {
        i -= 1;
        slot(stack, sp, 0).int = slot(stack, fp, code[pc + 4 + i].register).int;
        sp += 1;
    }

    // std.debug.print("last arg at: {}\n", .{sp});
    // store the return program counter and frame pointer
    caller_frame.pc = pc + 4 + argument_count;
    caller_frame.fp = fp;
    caller_frame.return_register = @intCast(return_register);

    // call invoke without advancing the pc, which will load the callee and figure
    // out how to run it
    @call(.always_tail, invoke, .{ pc, code, fp, stack });
}

fn invoke(pc: usize, code: [*]const Word, fp: u64, stack: [*]Slot) void {
    // what we're actually calling
    const callee = code[pc + 1].register;
    // the function info that the callee points to (conatins ir and bytecode information)
    const fi: *FunctionInfo = @ptrCast(@alignCast(slot(stack, fp, callee).ptr));
    // the runtime needs the intern pool to query function information during compiling
    const intern_pool: *InternPool = @ptrFromInt(code[0].imm | (@as(u64, code[1].imm) << 32));
    // for memory savings, all functions run by this interpreter thread share the
    // constant pool, which is used by the assembler to store compile time constants
    const constant_pool = contextFieldPtr(stack, "constant_pool");

    // calculate pointer to the caller stack frame so we can call
    const caller_frame: [*]CallFrame = @ptrCast(@alignCast(slot(stack, fp, -1).ptr));

    // ask the runtime to lazily compile our function
    const bc = builtins.lazyCompileFunction(intern_pool, constant_pool, fi) catch unreachable;

    // now that we have our bytecode, setup a call frame for the callee and trampoline into it
    const callee_frame = &caller_frame[1];
    callee_frame.code = bc.code.ptr;
    callee_frame.register_count = bc.register_count;

    // and push our callee frame pointer to the stack
    const argument_count = code[pc + 3].count;
    const sp = fp + caller_frame[0].register_count + argument_count + 1;
    slot(stack, sp, -1).ptr = callee_frame;

    next(bc.entry_pc, callee_frame.code, sp, stack);
}

fn ret(pc: usize, code: [*]const Word, fp: u64, stack: [*]Slot) void {
    const src = code[pc + 1].register;

    // load the callee's frame to calculate the caller frame
    const callee_frame: [*]CallFrame = @ptrCast(@alignCast(slot(stack, fp, -1).ptr));
    const caller_frame = &(callee_frame - 1)[0];

    // and save the return value
    slot(stack, caller_frame.fp, @intCast(caller_frame.return_register)).int = slot(stack, fp, src).int;

    next(caller_frame.pc, caller_frame.code, caller_frame.fp, stack);
}

fn exit(pc: usize, code: [*]const Word, fp: u64, stack: [*]Slot) void {
    _ = pc;
    _ = code;
    _ = fp;
    _ = stack;

    // std.debug.print("interpreter exit\n", .{});
    // std.debug.print("stack:\n", .{});
    // for (fp..sp) |i| std.debug.print("x{} = {}\n", .{ i - fp, stack[i].int });
    // std.debug.print("\n", .{});
    // while (true) {}
}

fn pint(pc: usize, code: [*]const Word, fp: u64, stack: [*]Slot) void {
    const src = code[pc + 1].register;
    const int = slot(stack, fp, src).int;
    std.debug.print("{}\n", .{int});

    next(pc + 2, code, fp, stack);
}

fn pfloat(pc: usize, code: [*]const Word, fp: u64, stack: [*]Slot) void {
    const src = code[pc + 1].register;
    const float = slot(stack, fp, src).float;
    std.debug.print("{}\n", .{float});

    next(pc + 2, code, fp, stack);
}

fn pbool(pc: usize, code: [*]const Word, fp: u64, stack: [*]Slot) void {
    const src = code[pc + 1].register;
    const int = slot(stack, fp, src).int;
    std.debug.print("{s}\n", .{if (int == 1) "True" else "False"});

    next(pc + 2, code, fp, stack);
}

fn pstr(pc: usize, code: [*]const Word, fp: u64, stack: [*]Slot) void {
    const src = code[pc + 1].register;

    const str: *const String = @ptrCast(@alignCast(slot(stack, fp, src).ptr));
    std.debug.print("{s}\n", .{str.bytes()});

    next(pc + 2, code, fp, stack);
}

fn trap(pc: usize, code: [*]const Word, fp: u64, stack: [*]Slot) void {
    // _ = fp;
    // _ = stack;

    std.debug.print("trap at {s}\n", .{@tagName(code[pc].opcode)});
    std.debug.print("stack: ", .{});
    // for (0..4) |i| std.debug.print("r{} = {}\n", .{ i, stack[i].int });
    var i: i32 = -1;
    while (i < 4) : (i += 1) std.debug.print("r{} = {}\n", .{ i, slot(stack, fp, i).int });
    std.debug.print("\n", .{});
    // while (true) {}
}
