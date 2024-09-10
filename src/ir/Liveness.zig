const std = @import("std");
const Ir = @import("Ir.zig");

const Allocator = std.mem.Allocator;
const Liveness = @This();

dead: []const u8,
special: std.AutoHashMapUnmanaged(Ir.Index, ExtraIndex),
extra: []const u32,

pub const ExtraIndex = enum(u32) { _ };

pub fn deadBits(liveness: *const Liveness, inst: Ir.Index) u4 {
    const i = @intFromEnum(inst);
    const elem = liveness.dead[i / 2];

    if (i % 2 == 0) {
        return @truncate(elem);
    } else {
        return @truncate(elem >> 4);
    }
}

const Analysis = struct {
    gpa: Allocator,
    dead: []u8,
    live_set: std.AutoHashMapUnmanaged(Ir.Index, void),

    pub fn init(gpa: Allocator, inst_count: u32) !Analysis {
        const dead = try gpa.alloc(u8, inst_count);
        errdefer gpa.free(dead);

        @memset(dead, 0);
        return .{
            .gpa = gpa,
            .dead = dead,
            .live_set = .{},
        };
    }

    pub fn deinit(self: *Analysis) void {
        self.gpa.free(self.dead);
    }
};

pub fn analyze(gpa: Allocator, temp_ir: *const Ir) !Liveness {
    var arena_allocator = std.heap.ArenaAllocator.init(gpa);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    const insts = temp_ir.insts;
    var analysis = try Analysis.init(gpa, @intCast(insts.len));
    errdefer analysis.deinit();

    // TODO: we need to consider instructions in the right order,
    // rather than just blindly iterating through the temp_ir
    // but to hack things for now, pretend phi_entry_body_bodys
    // never die
    for (0..insts.len) |i| {
        const inst: Ir.Index = @enumFromInt(i);
        const tag = temp_ir.instTag(inst);
        if (tag == .phi_entry_body_body) try analysis.live_set.put(arena, inst, {});
    }

    var i: u32 = @intCast(insts.len);
    while (i > 0) {
        i -= 1;
        const inst: Ir.Index = @enumFromInt(i);
        var bits: u4 = 0x0;

        // instruction is unused, since it is defined here
        // but we haven't seen it used below (moving backwards)
        if (!analysis.live_set.contains(inst)) {
            bits |= 0x8;
        } else {
            _ = analysis.live_set.remove(inst);
        }

        // based on the number of operands the instruction has,
        // check each of them against the live set, and if they
        // don't exist, add them and mark the "dead" bit
        const tag = temp_ir.instTag(inst);
        const payload = temp_ir.instPayload(inst);
        switch (tag) {
            .constant => {},
            // TODO: think about how to handle these
            // .alloc, .dealloc => {},
            .itof,
            .ftoi,
            .neg,
            .binv,
            .lnot,
            // .load,
            .ret,
            => if (!analysis.live_set.contains(payload.unary)) {
                bits |= 0x1;
                try analysis.live_set.put(arena, payload.unary, {});
            },
            .add,
            .sub,
            .mul,
            .div,
            .mod,
            .pow,
            .bor,
            .band,
            .bxor,
            .sll,
            .sra,
            .lor,
            .land,
            .eq,
            .ne,
            .lt,
            .gt,
            .le,
            .ge,
            .phi_if_else,
            .phi_entry_if,
            .phi_entry_else,
            .phi_entry_body_body,
            .phi_entry_body_exit,
            => {
                if (!analysis.live_set.contains(payload.binary.l)) {
                    bits |= 0x1;
                    try analysis.live_set.put(arena, payload.binary.l, {});
                }

                if (!analysis.live_set.contains(payload.binary.r)) {
                    bits |= 0x2;
                    try analysis.live_set.put(arena, payload.binary.r, {});
                }
            },
            // TODO: is this correct?
            // .store => if (!analysis.live_set.contains(payload.binary.r)) {
            //     bits |= 0x2;
            //     try analysis.live_set.put(arena, payload.binary.r, {});
            // },
            // TODO: what to do here?
            .if_else => if (!analysis.live_set.contains(payload.op_extra.op)) {
                bits |= 0x1;
                try analysis.live_set.put(arena, payload.op_extra.op, {});
            },
            // TODO: what to do here?
            .loop => if (!analysis.live_set.contains(payload.op_extra.op)) {
                bits |= 0x1;
                try analysis.live_set.put(arena, payload.op_extra.op, {});
            },
        }

        const elem: u8 = if (i % 2 == 0) bits else @as(u8, bits) << 4;
        analysis.dead[i / 2] |= elem;
    }

    return .{
        .dead = analysis.dead,
        .special = .{},
        .extra = &.{},
    };
}

// pub fn addExtra(ig: *IrGen, extra: anytype) !Ir.ExtraIndex {
//     const len: u32 = @intCast(ig.extra.items.len);
//     const fields = std.meta.fields(@TypeOf(extra));
//     try ig.extra.ensureUnusedCapacity(ig.gpa, fields.len);
//     inline for (fields) |field| {
//         switch (field.type) {
//             inline else => {
//                 const num: u32 = @intFromEnum(@field(extra, field.name));
//                 ig.extra.appendAssumeCapacity(@bitCast(num));
//             },
//         }
//     }
//     return @enumFromInt(len);
// }
