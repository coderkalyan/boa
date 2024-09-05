// Currently, this file doesn't perform any type of "intelligent"
// register allocation like linear scan. It is instead an interface
// for making a stack machine look like a register machine, and perform
// better in practice by pre-computing the stack pointer offsets and
// providing an implicit accumulator register, which is just a pointer
// to the top of the stack frame. Therefore it is basically a wrapper around
// integer addition.
const std = @import("std");
const Ir = @import("../ir/Ir.zig");

const RegisterAllocator = @This();
const Allocator = std.mem.Allocator;

// variable registers
locals: []Slot,
// the number of temporary registers currently in use by the assembler
tlive: u32,
// the maximum number of temporaries needed for the function associated
// with this register allocator instance - used to determine the size
// of the stack frame
tmax: u32,

const Slot = struct {
    inst: Ir.Index,
    live: bool,
};

pub fn init(arena: Allocator, vmax: u32) !RegisterAllocator {
    const locals = try arena.alloc(Slot, vmax);
    errdefer arena.free(locals);
    for (locals) |*slot| slot.live = false;

    return .{
        .locals = locals,
        .tlive = 0,
        .tmax = 0,
    };
}

pub fn alloc(ra: *RegisterAllocator, inst: Ir.Index) u32 {
    for (ra.locals, 0..) |*slot, i| {
        if (!slot.live) {
            slot.live = true;
            slot.inst = inst;
            return @intCast(i);
        }
    }

    unreachable;
}

pub fn local(ra: *RegisterAllocator, inst: Ir.Index) u32 {
    for (ra.locals, 0..) |*slot, i| {
        if (slot.live and slot.inst == inst) return @intCast(i);
    }

    unreachable;
}

pub fn dealloc(ra: *RegisterAllocator, inst: Ir.Index) u32 {
    const index = ra.local(inst);
    ra.slots[index].live = false;
}

pub inline fn push(ra: *RegisterAllocator) u32 {
    ra.tlive += 1;
    ra.tmax = @max(ra.tmax, ra.tlive);
    const vmax: u32 = @intCast(ra.locals.len);
    return vmax + ra.tlive - 1;
}

pub inline fn pop(ra: *RegisterAllocator) void {
    std.debug.assert(ra.tlive >= 1);
    ra.tlive -= 1;
}

// signals that the assembler wants to re-use the accumulator
// if the accumulator is different than what it was previously,
// we signal to the assembler to generate a str
// pub fn use(ra: *RegisterAllocator) void {
//
// }
// pub inline fn current(ra: *const RegisterAllocator) u32 {
//     std.debug.assert(ra.tlive >= 1);
//     const vmax: u32 = @intCast(ra.locals.len);
//     return vmax + ra.tlive - 1;
// }
//
// pub inline fn skip(ra: *const RegisterAllocator) u32 {
//     std.debug.assert(ra.tlive >= 2);
//     const vmax: u32 = @intCast(ra.locals.len);
//     return vmax + ra.tlive - 2;
// }

pub inline fn final(ra: *RegisterAllocator) u32 {
    const vmax: u32 = @intCast(ra.locals.len);
    return vmax + ra.tmax;
}
