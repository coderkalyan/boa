const std = @import("std");
const Object = @import("../rt/object.zig").Object;
const Allocator = std.mem.Allocator;

pub const BuiltinIndex = enum(u32) {
    push_args,
    eval_callable,
    trap,
    attr_index_or_panic,
    attr_index_or_insert,
    load_index,
    store_index,
    pint,
};

pub const Context = struct {
    pba: Allocator,
    global_object: *Object,
};
