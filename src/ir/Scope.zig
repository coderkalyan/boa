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

// in python, most blocks don't actually create new scopes - a variable
// defined for the first time in an if statement is available outside the
// if block
//
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
        live: bool,
    };

    pub fn init(s: *Scope) Function {
        return .{
            .parent = s,
            .var_table = .{},
            .var_slots = .{},
        };
    }

    pub fn reserveSlot(self: *Function, arena: Allocator, ident: InternPool.Index) !u32 {
        std.debug.assert(!self.var_table.contains(ident));

        const top: u32 = @intCast(self.var_slots.len);
        try self.var_slots.ensureUnusedCapacity(arena, 1);
        try self.var_table.ensureUnusedCapacity(arena, 1);
        self.var_slots.appendAssumeCapacity(.{ .ty = undefined, .live = false });
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
    tree: *const Ast,
    // list of instruction indices (in order) that represent a block body
    insts: std.ArrayListUnmanaged(Ir.Index),

    pub fn init(ig: *IrGen, s: *Scope) Block {
        return .{
            .parent = s,
            .ig = ig,
            .tree = ig.tree,
            .insts = .{},
        };
    }

    pub fn deinit(b: *Block) void {
        b.insts.deinit(b.ig.arena);
    }

    pub inline fn addUnlinked(b: *Block, inst: Inst) !Ir.Index {
        return b.ig.add(inst);
    }

    pub fn linkInst(b: *Block, inst: Ir.Index) !void {
        return b.insts.append(b.ig.arena, inst);
    }

    pub fn add(b: *Block, inst: Inst) !Ir.Index {
        const index = try b.addUnlinked(inst);
        try b.linkInst(index);

        return index;
    }

    pub fn addBlock(b: *Block, inner: *Block) !Ir.ExtraIndex {
        const ig = b.ig;

        const scratch_top = ig.scratch.items.len;
        defer ig.scratch.shrinkRetainingCapacity(scratch_top);
        try ig.scratch.ensureUnusedCapacity(ig.arena, inner.insts.items.len);

        for (inner.insts.items) |inst| {
            ig.scratch.appendAssumeCapacity(@intFromEnum(inst));
        }
        const insts = ig.scratch.items[scratch_top..];
        const pl = try ig.addSlice(insts);

        return pl;
    }

    // pub fn addBranchDouble(b: *Block, cond: Ir.Index, exec_true: Ir.Index, exec_false: Ir.Index, node: Node.Index) !Ir.Index {
    //     const pl = try b.ig.addExtra(Inst.BranchDouble{
    //         .exec_true = exec_true,
    //         .exec_false = exec_false,
    //     });
    //     return b.add(.{
    //         .data = .{ .branch_double = .{ .cond = cond, .pl = pl } },
    //         .loc = .{ .node = node },
    //     });
    // }
    //
    // pub fn addLoopWhile(b: *Block, cond: Ir.Index, body: Ir.Index, node: Node.Index) !Ir.Index {
    //     return b.add(.{
    //         .data = .{ .loop_while = .{ .cond = cond, .body = body } },
    //         .loc = .{ .node = node },
    //     });
    // }
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
// this assumes that the identifier is not incorrectly shadowed, as it will
// only find the most local use of the identifier
pub fn resolveIdent(inner: *Scope, ident: InternPool.Index) ?*Scope {
    var s: *Scope = inner;

    while (true) {
        switch (s.tag) {
            .module => break,
            .function => {
                const function = s.cast(Function).?;
                if (function.var_table.contains(ident)) return s;

                s = function.parent;
            },
            .block => {
                const block = s.cast(Block).?;
                s = block.parent;
            },
        }
    }

    return null;
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

    try std.testing.expectEqual(function.base.resolveIdent(apple), null);
    try std.testing.expectEqual(function.base.resolveIdent(banana), null);
    try std.testing.expectEqual(function.base.resolveIdent(cherry), null);

    try std.testing.expectEqual(0, try function.reserveSlot(arena.allocator(), apple));
    try std.testing.expectEqual(1, try function.reserveSlot(arena.allocator(), banana));
    try std.testing.expectEqual(2, try function.reserveSlot(arena.allocator(), cherry));

    try std.testing.expectEqual(&function.base, function.base.resolveIdent(apple));
    try std.testing.expectEqual(&function.base, function.base.resolveIdent(banana));
    try std.testing.expectEqual(&function.base, function.base.resolveIdent(cherry));
    try std.testing.expectEqual(3, function.var_slots.len);

    try std.testing.expectEqual(0, function.var_table.get(apple));
    try std.testing.expectEqual(1, function.var_table.get(banana));
    try std.testing.expectEqual(2, function.var_table.get(cherry));
}
