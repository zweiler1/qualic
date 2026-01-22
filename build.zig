const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const version: []const u8 = @import("build.zig.zon").version;

    const imports = b.addOptions();
    imports.addOption([]const u8, "VERSION", version);
    const mod = imports.createModule();

    const exe = b.addExecutable(.{
        .name = "qualic",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{.{ .name = "version", .module = mod }},
        }),
        .use_llvm = true,
        .use_lld = true,
    });
    b.installArtifact(exe);

    const clap = b.dependency("clap", .{});
    exe.root_module.addImport("clap", clap.module("clap"));

    const run_step = b.step("run", "Run the app");
    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
}
