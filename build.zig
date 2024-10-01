const std = @import("std");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // The interpreter is generated at build time using a Zig program that
    // uses LLVM to emit efficient bytecode handlers.
    // const generator = b.addExecutable(.{
    //     .name = "interpreter-generator",
    //     .root_source_file = b.path("src/interpreter/generator.zig"),
    //     .target = target,
    //     .optimize = optimize,
    // });

    // The generator depends on libc and LLVM.
    // generator.linkLibC();
    // generator.linkSystemLibrary("LLVM-18");

    // Run the generator and capture its output LLVM bitcode.
    // const run_generator_user = b.addRunArtifact(generator);
    // if (b.args) |args| {
    //     run_generator_user.addArgs(args);
    // }

    // Add a user facing step to run only the generator.
    // const run_generator_step = b.step("run-generator", "Run the interpreter generator");
    // run_generator_step.dependOn(&run_generator_user.step);

    // Test the generator.
    // const generator_tests = b.addTest(.{
    //     .root_source_file = b.path("src/interpreter/generator.zig"),
    //     .target = target,
    //     .optimize = optimize,
    // });

    // generator_tests.linkLibC();
    // generator_tests.linkSystemLibrary("LLVM-18");
    // const run_generator_tests = b.addRunArtifact(generator_tests);

    // Add a user facing step to test the generator.
    // const test_generator_step = b.step("test-generator", "Run interpreter generator unit tests");
    // test_generator_step.dependOn(&run_generator_tests.step);

    // Run the generator (as part of the main build process) and capture its output LLVM bitcode.
    // const run_generator = b.addRunArtifact(generator);
    // const interpreter_bc = run_generator.addOutputFileArg("interpreter.bc");

    // Run the bitcode through the standard optimizer.
    // const opt_run = b.addSystemCommand(&.{"opt"});
    // opt_run.addFileArg(interpreter_bc);
    // opt_run.addArg("-o");
    // const opt_bc = opt_run.addOutputFileArg("interpreter-opt.bc");

    // Compile the bitcode into assembly for the target platform.
    // const llc_run = b.addSystemCommand(&.{"llc"});
    // llc_run.addFileArg(opt_bc);
    // llc_run.addArg("-o");
    // const interpreter_s = llc_run.addOutputFileArg("interpreter.s");

    const exe = b.addExecutable(.{
        .name = "boa",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Add the generated interpreter to the executable source.
    // exe.addAssemblyFile(interpreter_s);

    // This declares intent for the executable to be installed into the
    // standard location when the user invokes the "install" step (the default
    // step when running `zig build`).
    b.installArtifact(exe);

    // This *creates* a Run step in the build graph, to be executed when another
    // step is evaluated that depends on it. The next line below will establish
    // such a dependency.
    const run_cmd = b.addRunArtifact(exe);

    // By making the run step depend on the install step, it will be run from the
    // installation directory rather than directly from within the cache directory.
    // This is not necessary, however, if the application depends on other installed
    // files, this ensures they will be present and in the expected location.
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // This creates a build step. It will be visible in the `zig build --help` menu,
    // and can be selected like this: `zig build run`
    // This will evaluate the `run` step rather than the default, which is "install".
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    // The interpreter is generated at build time using a Zig program that
    // uses LLVM to emit efficient bytecode handlers.
    const vm_assembler = b.addExecutable(.{
        .name = "assembler",
        .root_source_file = b.path("vm/assembler.zig"),
        .target = target,
        .optimize = optimize,
    });
    vm_assembler.linkLibC();
    vm_assembler.linkSystemLibrary("LLVM-18");

    const vm_assembler_unit_tests = b.addTest(.{
        .root_source_file = b.path("vm/assembler.zig"),
        .target = target,
        .optimize = optimize,
    });
    vm_assembler_unit_tests.linkLibC();
    vm_assembler_unit_tests.linkSystemLibrary("LLVM-18");

    const run_vm_assembler_unit_tests = b.addRunArtifact(vm_assembler_unit_tests);

    // Run the assembler (as part of the main build process) and capture its output LLVM bitcode.
    const run_assembler = b.addRunArtifact(vm_assembler);
    const interpreter_bc = run_assembler.addOutputFileArg("interpreter.bc");

    // Run the bitcode through the standard optimizer.
    const opt_run = b.addSystemCommand(&.{"opt"});
    opt_run.addFileArg(interpreter_bc);
    opt_run.addArg("-o");
    const opt_bc = opt_run.addOutputFileArg("interpreter-opt.bc");

    // Compile the bitcode into assembly for the target platform.
    const llc_run = b.addSystemCommand(&.{"llc"});
    llc_run.addFileArg(opt_bc);
    llc_run.addArg("-o");
    const interpreter_s = llc_run.addOutputFileArg("interpreter.s");

    const vm_unit_tests = b.addTest(.{
        .root_source_file = b.path("vm/test.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Add the generated interpreter to the executable source.
    vm_unit_tests.addAssemblyFile(interpreter_s);

    const run_vm_unit_tests = b.addRunArtifact(vm_unit_tests);

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);
    test_step.dependOn(&run_vm_assembler_unit_tests.step);
    test_step.dependOn(&run_vm_unit_tests.step);
}
