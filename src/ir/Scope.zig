const std = @import("std");
const Ast = @import("../Ast.zig");
const Ir = @import("Ir.zig");
const IrGen = @import("IrGen.zig");
const InternPool = @import("../InternPool.zig");

const Allocator = std.mem.Allocator;
const Node = Ast.Node;
const Inst = Ir.Inst;
const Scope = @This();

tag: Tag,

pub fn cast(base: *Scope, comptime T: type) ?*T {
    return @alignCast(@fieldParentPtr("base", base));
}

pub fn parent(base: *Scope) ?*Scope {
    return switch (base.tag) {
        .module => null,
        .function => base.cast(Function).?.parent,
        .block => base.cast(Block).?.parent,
        // .local_type => base.cast(LocalType).?.parent,
    };
}

const Tag = enum {
    module,
    function,
    block,
    // local_type,
};

pub const Module = struct {
    const base_tag: Tag = .module;
    base: Scope = .{ .tag = base_tag },
};

// functions actually generate a scope to store local variables
pub const Function = struct {
    const base_tag: Tag = .function;
    base: Scope = .{ .tag = base_tag },

    parent: *Scope,
    var_table: std.AutoHashMapUnmanaged(InternPool.Index, u32),
    var_slots: std.MultiArrayList(Slot),

    const Slot = struct {
        // the type of a slot can be changed (demoted) during the first step
        // of IrGen analysis, to increasingly generic types as all possible
        // code paths are analyzed
        ty: InternPool.Index,
        // when a variable is first defined, it's liveness is false, and its
        // type is unknown. when it is definitively assigned to, its liveness
        // is set to true, and the type is unioned with the value being assigned
        // when it is undefined and first defined in some cases, the type is
        // a union of undef and the value type
        //
        // that is, a variable currently holds a value if its stack slot is `live`
        // *and* its current type is not set to undef. for undef-union types, this
        // check is unfortunately done at runtime, similar to safe optionals - except
        // that unlike an optional, which is a nonetype-union, variable access of
        // an undef-union tagged as undef causes a panic (stack trace)
        // live: bool,
    };

    pub fn init(s: *Scope) Function {
        return .{
            .parent = s,
            .var_table = .{},
            .var_slots = .{},
        };
    }

    pub fn declare(self: *Function, arena: Allocator, ident: InternPool.Index) !u32 {
        std.debug.assert(!self.var_table.contains(ident));

        const top: u32 = @intCast(self.var_slots.len);
        try self.var_slots.ensureUnusedCapacity(arena, 1);
        try self.var_table.ensureUnusedCapacity(arena, 1);
        self.var_slots.appendAssumeCapacity(.{ .ty = undefined });
        self.var_table.putAssumeCapacity(ident, top);
        return top;
    }
};

// blocks are purely organizational structures for maintaining a list
// of instructions that execute within an ast's scope block
pub const Block = struct {
    const base_tag: Tag = .block;
    base: Scope = .{ .tag = base_tag },

    parent: *Scope,
    ig: *IrGen,
    vars: std.AutoHashMapUnmanaged(InternPool.Index, Ir.Index),

    pub fn init(ig: *IrGen, s: *Scope) Block {
        return .{
            .parent = s,
            .ig = ig,
            .vars = .{},
        };
    }

    pub fn put(b: *Block, ident: InternPool.Index, inst: Ir.Index) !void {
        try b.vars.put(b.ig.arena, ident, inst);
    }

    // variables that are defined in both a and b will be hoisted up
    // into self with a phi, overwriting any existing definition in self
    // variables that are defined in a ^ b will be hoisted up into
    // self with phi or undef-phi, depending on if they exist in self or not
    // variables not defined in either a or b, but existing in self, will
    // be untouched
    pub fn hoistMerge(
        self: *Block,
        a: *const Block,
        a_bindex: Ir.BlockIndex,
        b: *const Block,
        b_bindex: Ir.BlockIndex,
        self_bindex: ?Ir.BlockIndex,
    ) !void {
        const ig = self.ig;

        var it = a.vars.iterator();
        while (it.next()) |entry| {
            const ident = entry.key_ptr.*;
            const a_inst = entry.value_ptr.*;
            const a_ty = ig.getTempIr().typeOf(a_inst);

            if (b.vars.get(ident)) |b_inst| {
                const b_ty = ig.getTempIr().typeOf(b_inst);
                // TODO: support type merging
                std.debug.assert(a_ty == b_ty);
                const phi = try ig.current_builder.phi(a_ty, a_inst, a_bindex, b_inst, b_bindex);
                try self.put(ident, phi);
            } else if (self_bindex != null and self.vars.contains(ident)) {
                const self_inst = self.vars.get(ident).?;
                const self_ty = ig.getTempIr().typeOf(self_inst);
                // TODO: support type merging
                std.debug.assert(a_ty == self_ty);
                const phi = try ig.current_builder.phi(a_ty, a_inst, a_bindex, self_inst, self_bindex.?);
                try self.put(ident, phi);
            } else unreachable; // TODO: undef-phi
        }

        it = b.vars.iterator();
        while (it.next()) |entry| {
            const ident = entry.key_ptr.*;
            const b_inst = entry.value_ptr.*;
            const b_ty = ig.getTempIr().typeOf(b_inst);

            // we already merge these
            if (a.vars.contains(ident)) continue;
            if (self_bindex != null and self.vars.contains(ident)) {
                const self_inst = self.vars.get(ident).?;
                const self_ty = ig.getTempIr().typeOf(self_inst);
                // TODO: support type merging
                std.debug.assert(b_ty == self_ty);
                const phi = try ig.current_builder.phi(b_ty, b_inst, b_bindex, self_inst, self_bindex.?);
                try self.put(ident, phi);
            } else unreachable; // TODO: undef-phi
        }
    }
};

// pub const LocalType = struct {
//     const base_tag: Tag = .local_type;
//     base: Scope = .{ .tag = base_tag },
//
//     parent: *Scope,
//     ident: InternPool.StringIndex,
//     inst: Ir.Index,
//
//     pub fn init(s: *Scope, ident: InternPool.StringIndex, inst: Ir.Index) @This() {
//         return .{
//             .parent = s,
//             .ident = ident,
//             .inst = inst,
//         };
//     }
// };

// tries to find an identifier in the current scope, *expecting* to find it
// to be used in an expression to load from or assign to
pub fn resolveIdent(inner: *Scope, ident: InternPool.Index) ?*Scope {
    var s: *Scope = inner;

    while (true) {
        switch (s.tag) {
            .module => break,
            .function => {
                const function = s.cast(Function).?;
                // if (function.var_table.contains(ident)) return s;

                s = function.parent;
            },
            .block => {
                const block = s.cast(Block).?;
                if (block.vars.contains(ident)) return s;

                s = block.parent;
            },
        }
    }

    return null;
}

pub fn declScope(inner: *Scope) *Scope {
    var s: *Scope = inner;

    while (true) {
        switch (s.tag) {
            .module, .function => return s,
            .block => {
                const block = s.cast(Block).?;
                s = block.parent;
            },
        }
    }
}

test "local var" {
    var pool = try InternPool.init(std.testing.allocator);
    defer pool.deinit();

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var module: Scope.Module = .{};
    var function = Scope.Function.init(&module.base);

    const apple = try pool.put(.{ .str = "apple" });
    const banana = try pool.put(.{ .str = "banana" });
    const cherry = try pool.put(.{ .str = "cherry" });

    // TODO: bring this back once scoping rules are finalized
    try std.testing.expectEqual(function.base.resolveIdent(apple), null);
    try std.testing.expectEqual(function.base.resolveIdent(banana), null);
    try std.testing.expectEqual(function.base.resolveIdent(cherry), null);
    //
    // try std.testing.expectEqual(0, try function.declare(arena.allocator(), apple));
    // try std.testing.expectEqual(1, try function.declare(arena.allocator(), banana));
    // try std.testing.expectEqual(2, try function.declare(arena.allocator(), cherry));
    //
    // try std.testing.expectEqual(&function.base, function.base.resolveIdent(apple));
    // try std.testing.expectEqual(&function.base, function.base.resolveIdent(banana));
    // try std.testing.expectEqual(&function.base, function.base.resolveIdent(cherry));
    // try std.testing.expectEqual(3, function.var_slots.len);
    //
    // try std.testing.expectEqual(0, function.var_table.get(apple));
    // try std.testing.expectEqual(1, function.var_table.get(banana));
    // try std.testing.expectEqual(2, function.var_table.get(cherry));
}
