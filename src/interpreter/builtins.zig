const std = @import("std");

pub const BuiltinIndex = enum(u32) {
    push_args,
    eval_callable,
    trap,
    // attr_index,
};
