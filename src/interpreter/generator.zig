const std = @import("std");
const c = @cImport({
    @cInclude("llvm-c/Core.h");
});

pub fn main() !void {
    const module = c.LLVMModuleCreateWithName("interpreter");
    _ = module;
}
