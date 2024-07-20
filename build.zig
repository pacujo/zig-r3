const std = @import("std");

pub fn build(b: *std.Build) void {
    const root_source_file = b.path("src/r3.zig");
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    _ = b.addModule("r3", .{
        .root_source_file = root_source_file,
        .target = target,
        .optimize = optimize,
    });

    const autodoc = b.addObject(.{
        .name = "r3",
        .root_source_file = root_source_file,
        .target = target,
        .optimize = .Debug,
    });
    const install_docs = b.addInstallDirectory(.{
        .source_dir = autodoc.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "doc",
    });
    const docs_step = b.step("docs", "Build and install documentation");
    docs_step.dependOn(&install_docs.step);
}
