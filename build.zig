const std = @import("std");

const Compile = std.Build.Step.Compile;

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    const build_runtime = buildRuntime(b);
    const runtime = build_runtime.lib;
    const runtime_test = build_runtime.lib_test;
    const run_runtime_tests = b.addRunArtifact(runtime_test);

    b.installArtifact(runtime);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_runtime_tests.step);
    // const target = b.standardTargetOptions(.{});
    // const optimize = b.standardOptimizeOption(.{});
    //
    // // The interpreter is generated at build time using a Zig program that
    // // uses LLVM to emit efficient bytecode handlers.
    // const vm_assembler = b.addExecutable(.{
    //     .name = "assembler",
    //     .root_source_file = b.path("vm/assembler.zig"),
    //     .target = target,
    //     .optimize = optimize,
    // });
    // vm_assembler.linkLibC();
    // vm_assembler.linkSystemLibrary("LLVM-18");
    //
    // const vm_assembler_unit_tests = b.addTest(.{
    //     .root_source_file = b.path("vm/assembler.zig"),
    //     .target = target,
    //     .optimize = optimize,
    // });
    // vm_assembler_unit_tests.linkLibC();
    // vm_assembler_unit_tests.linkSystemLibrary("LLVM-18");
    //
    // const run_vm_assembler_unit_tests = b.addRunArtifact(vm_assembler_unit_tests);
    //
    // // Run the assembler (as part of the main build process) and capture its output LLVM bitcode.
    // const run_assembler = b.addRunArtifact(vm_assembler);
    // const interpreter_bc = run_assembler.addOutputFileArg("interpreter.bc");
    //
    // // Run the bitcode through the standard optimizer.
    // // TODO: disabled until we use custom optimization passes, since this was
    // // generating poor code with -O3 (worse than non-optimized in some cases)
    // // const opt_run = b.addSystemCommand(&.{"opt"});
    // // opt_run.addFileArg(interpreter_bc);
    // // opt_run.addArg("-o");
    // // const opt_bc = opt_run.addOutputFileArg("interpreter-opt.bc");
    //
    // // Compile the bitcode into assembly for the target platform.
    // const llc_run = b.addSystemCommand(&.{"llc"});
    // // llc_run.addFileArg(opt_bc);
    // llc_run.addFileArg(interpreter_bc);
    // llc_run.addArg("-o");
    // const interpreter_s = llc_run.addOutputFileArg("interpreter.s");
    //
    // const vm_unit_tests = b.addTest(.{
    //     .root_source_file = b.path("vm/test.zig"),
    //     .target = target,
    //     .optimize = optimize,
    // });
    //
    // // Add the generated interpreter to the executable source.
    // vm_unit_tests.addAssemblyFile(interpreter_s);
    //
    // const run_vm_unit_tests = b.addRunArtifact(vm_unit_tests);
    //
    // const exe = b.addExecutable(.{
    //     .name = "boa",
    //     .root_source_file = b.path("src/main.zig"),
    //     .target = target,
    //     .optimize = optimize,
    // });
    //
    // // Add the generated interpreter to the executable source.
    // exe.addAssemblyFile(interpreter_s);
    //
    // // This declares intent for the executable to be installed into the
    // // standard location when the user invokes the "install" step (the default
    // // step when running `zig build`).
    // b.installArtifact(exe);
    //
    // // This *creates* a Run step in the build graph, to be executed when another
    // // step is evaluated that depends on it. The next line below will establish
    // // such a dependency.
    // const run_cmd = b.addRunArtifact(exe);
    //
    // // By making the run step depend on the install step, it will be run from the
    // // installation directory rather than directly from within the cache directory.
    // // This is not necessary, however, if the application depends on other installed
    // // files, this ensures they will be present and in the expected location.
    // run_cmd.step.dependOn(b.getInstallStep());
    //
    // // This allows the user to pass arguments to the application in the build
    // // command itself, like this: `zig build run -- arg1 arg2 etc`
    // if (b.args) |args| {
    //     run_cmd.addArgs(args);
    // }
    //
    // // This creates a build step. It will be visible in the `zig build --help` menu,
    // // and can be selected like this: `zig build run`
    // // This will evaluate the `run` step rather than the default, which is "install".
    // const run_step = b.step("run", "Run the app");
    // run_step.dependOn(&run_cmd.step);
    //
    // const exe_unit_tests = b.addTest(.{
    //     .root_source_file = b.path("src/root.zig"),
    //     .target = target,
    //     .optimize = optimize,
    // });
    //
    // const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);
    //
    // // Similar to creating the run step earlier, this exposes a `test` step to
    // // the `zig build --help` menu, providing a way for the user to request
    // // running the unit tests.
    // const test_step = b.step("test", "Run unit tests");
    // test_step.dependOn(&run_exe_unit_tests.step);
    // test_step.dependOn(&run_vm_assembler_unit_tests.step);
    // test_step.dependOn(&run_vm_unit_tests.step);
    //
    // const vm_benchmark = b.addExecutable(.{
    //     .name = "vm",
    //     .root_source_file = b.path("vm/test.zig"),
    //     .target = target,
    //     .optimize = optimize,
    // });
    //
    // // Add the generated interpreter to the executable source.
    // vm_benchmark.addAssemblyFile(interpreter_s);
    //
    // const run_vm_benchmark = b.addRunArtifact(vm_benchmark);
    // const run_vm_step = b.step("run-vm", "Run vm benchmark");
    // run_vm_step.dependOn(&run_vm_benchmark.step);
}

fn buildRuntime(b: *std.Build) struct { lib: *Compile, lib_test: *Compile } {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // the runtime contains all structures needed to support interpreted and compiled
    // code, including memory management, strings and objects/shapes, and builtin functions
    // for io, math, etc. It however cannot compile code, libcore does that.
    const runtime = b.addStaticLibrary(.{
        .name = "runtime",
        .root_source_file = b.path("rt/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const runtime_test = b.addTest(.{
        .name = "runtime",
        .root_source_file = b.path("rt/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    return .{ .lib = runtime, .lib_test = runtime_test };
}
