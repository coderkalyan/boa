const std = @import("std");

pub const BuiltinIndex = enum(u32) {
    push_args,
};

pub fn builtinName(comptime index: BuiltinIndex) [:0]const u8 {
    switch (index) {
        .push_args => "pushArgs",
    }
}
