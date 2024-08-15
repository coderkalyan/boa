const std = @import("std");
const InternPool = @import("../InternPool.zig");
const Ir = @import("../ir/Ir.zig");
const Bytecode = @import("Bytecode.zig");

const Allocator = std.mem.Allocator;
const asBytes = std.mem.asBytes;
const Opcode = Bytecode.Opcode;
const Inst = Ir.Inst;

const Assembler = @This();

gpa: Allocator,
pool: *InternPool,
ir: *const Ir,
code: std.ArrayListUnmanaged(u8),

// fn add(self: *Assembler, inst: Inst) !void {
//     switch (inst) {
//
//     }
// }

fn addImplicit(self: *Assembler, opcode: Opcode) !void {
    try self.code.append(self.gpa, @intFromEnum(opcode));
}

fn addOperand(self: *Assembler, opcode: Opcode, operand: u64) !void {
    var bytes = [_]u8{0} ** 10;
    var count: usize = 0;

    // TODO: this is endian specific
    if (operand <= std.math.maxInt(u8)) {
        bytes[0] = @intFromEnum(opcode);
        bytes[1] = @truncate(operand);

        count = 2;
    } else if (operand <= std.math.maxInt(u16)) {
        bytes[0] = @intFromEnum(Opcode.wide);
        bytes[1] = @intFromEnum(opcode);
        const trunc: u16 = @truncate(operand);
        @memcpy(bytes[2..4], asBytes(&trunc));

        count = 4;
    } else if (operand <= std.math.maxInt(u32)) {
        bytes[0] = @intFromEnum(Opcode.dwide);
        bytes[1] = @intFromEnum(opcode);
        const trunc: u32 = @truncate(operand);
        @memcpy(bytes[2..6], asBytes(&trunc));

        count = 6;
    } else {
        bytes[0] = @intFromEnum(Opcode.qwide);
        bytes[1] = @intFromEnum(opcode);
        @memcpy(bytes[2..10], asBytes(&operand));

        count = 10;
    }

    try self.code.appendSlice(self.gpa, bytes[0..count]);
}

fn addImmediate(self: *Assembler, opcode: Opcode, ty: type, imm: ty) !void {
    const size = switch (ty) {
        u8 => 1,
        bool => 1,
        u16 => 2,
        u32 => 4,
        u64 => 8,
        f64 => 8,
        else => unreachable,
    };

    const width_byte: usize = @intFromBool(size > 0);
    try self.code.ensureUnusedCapacity(self.gpa, 1 + width_byte + size);
    self.code.appendAssumeCapacity(@intFromEnum(opcode));
    switch (size) {
        1 => {},
        2 => self.code.appendAssumeCapacity(@intFromEnum(Opcode.wide)),
        4 => self.code.appendAssumeCapacity(@intFromEnum(Opcode.dwide)),
        8 => self.code.appendAssumeCapacity(@intFromEnum(Opcode.qwide)),
        else => unreachable,
    }

    self.code.appendSliceAssumeCapacity(asBytes(&imm));
}

pub fn assemble(gpa: Allocator, pool: *InternPool, ir: *const Ir) !Bytecode {
    var assembler: Assembler = .{
        .gpa = gpa,
        .pool = pool,
        .ir = ir,
        .code = .{},
    };

    const block = ir.extraData(Inst.ExtraSlice, ir.block);
    const insts = ir.extraSlice(block);
    for (insts) |inst| {
        try assembler.generate(@enumFromInt(inst));
    }

    return .{
        .ir = ir,
        .code = try assembler.code.toOwnedSlice(gpa),
    };
}

fn generate(self: *Assembler, inst: Ir.Index) !void {
    const ir = self.ir;

    const index = @intFromEnum(inst);
    // const payload = ir.insts.items(.payload)[index];
    switch (ir.insts.items(.tag)[index]) {
        .constant => try self.constant(inst),
        else => {},
    }
}

fn constant(self: *Assembler, inst: Ir.Index) !void {
    const ip = self.ir.instPayload(inst).ip;
    const tv = self.pool.get(ip).tv;
    switch (tv.ty) {
        .nonetype => unreachable, // TODO: what should this emit?
        .int => try self.addOperand(.ld, tv.val.int),
        // .float => try self.addImmediate(.ld, f64, tv.val.float),
        .bool => try self.addImmediate(.ld, bool, tv.val.bool),
        else => {},
    }
}
