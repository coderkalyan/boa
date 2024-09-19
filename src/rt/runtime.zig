const std = @import("std");
const IrGen = @import("../ir/IrGen.zig");
const Assembler = @import("../bc/Assembler.zig");
const InternPool = @import("../InternPool.zig");
const String = @import("string.zig").String;

const Allocator = std.mem.Allocator;
const FunctionInfo = InternPool.FunctionInfo;
