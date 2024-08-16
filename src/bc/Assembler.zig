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
arena: Allocator,
pool: *InternPool,
ir: *const Ir,
code: std.ArrayListUnmanaged(u8),
ra: RegisterAllocator,

const RegisterAllocator = struct {
    locals: []Local,
    temporaries: u32,
    max_temporaries: u32,

    const Local = struct {
        inst: Ir.Index,
        used: bool,
    };

    pub fn init(arena: Allocator, reserved: u32) !RegisterAllocator {
        const ra: RegisterAllocator = .{
            .locals = try arena.alloc(Local, reserved),
            .temporaries = 0,
            .max_temporaries = 0,
        };

        for (ra.locals) |*local| local.used = false;
        return ra;
    }

    pub fn allocLocal(ra: *const RegisterAllocator, inst: Ir.Index) void {
        for (ra.locals) |*local| {
            if (!local.used) {
                local.used = true;
                local.inst = inst;
                break;
            }
        }
    }

    pub fn getLocal(ra: *const RegisterAllocator, inst: Ir.Index) u32 {
        for (ra.locals, 0..) |local, i| {
            if (local.used and local.inst == inst) return @intCast(i);
        }

        unreachable;
    }

    pub fn freeLocal(ra: *const RegisterAllocator, inst: Ir.Index) void {
        // this works because both local registers and the locals slice
        // are zero indexed
        const index = ra.getLocal(inst);
        ra.locals[index].used = false;
    }

    pub fn allocTemporary(ra: *RegisterAllocator) u32 {
        ra.temporaries += 1;
        ra.max_temporaries = @max(ra.max_temporaries, ra.temporaries);
        return ra.max_locals + ra.temporaries - 1;
    }

    pub fn freeTemporary(ra: *RegisterAllocator) void {
        ra.temporaries -= 1;
    }
};

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
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    const block = ir.extraData(Inst.ExtraSlice, ir.block);
    const insts = ir.extraSlice(block);

    // figure out how many "fixed" registers we need to allocate
    // for locals
    var max: u32 = 0;
    var used: u32 = 0;
    for (insts) |inst| {
        switch (ir.insts.items(.tag)[inst]) {
            .alloc => {
                used += 1;
                max = @max(max, used);
            },
            .dealloc => used -= 1,
            else => {},
        }
    }

    var assembler: Assembler = .{
        .gpa = gpa,
        .arena = arena.allocator(),
        .pool = pool,
        .ir = ir,
        .code = .{},
        .ra = try RegisterAllocator.init(arena.allocator(), max),
    };

    // we know how many locals we'll have at any time, so
    // reserve space ahead of time
    // try assembler.map.ensureUnusedCapacity(assembler.arena, assembler.locals.reserved);

    // now generate the code
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
    switch (ir.insts.items(.tag)[index]) {
        .constant => try self.constant(inst),
        .alloc => try self.alloc(inst),
        .load => try self.load(inst),
        .store => try self.store(inst),
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

    const reg = self.ra.allocTemporary();
    try self.addOperand(.str, reg);
}

fn alloc(self: *Assembler, inst: Ir.Index) !void {
    self.ra.allocLocal(inst);
}

fn dealloc(self: *Assembler, inst: Ir.Index) !void {
    const unary = self.ir.instPayload(inst).unary;
    self.ra.freeLocal(unary);
}

fn load(self: *Assembler, inst: Ir.Index) !void {
    const unary = self.ir.instPayload(inst).unary;
    const src = self.ra.getLocal(unary);
    try self.addOperand(.ldr, src);
    const dest = self.ra.allocTemporary();
    try self.addOperand(.str, dest);
}

fn store(self: *Assembler, inst: Ir.Index) !void {
    const binary = self.ir.instPayload(inst).binary;
    const dest = self.ra.getLocal(binary.l);
    try self.addOperand(.str, dest);
}
