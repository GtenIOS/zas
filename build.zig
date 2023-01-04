const std = @import("std");

const pkgs = struct {
    const fileio = std.build.Pkg{
        .name = "fileio",
        .source = .{ .path = "libs/fileio/fileio.zig" },
        .dependencies = &[_]std.build.Pkg{},
    };
    const common = std.build.Pkg{
        .name = "common",
        .source = .{ .path = "libs/common/common.zig" },
        .dependencies = &[_]std.build.Pkg{},
    };
    const lexer = std.build.Pkg{
        .name = "lexer",
        .source = .{ .path = "libs/lexer/lexer.zig" },
        .dependencies = &[_]std.build.Pkg{
            common,
        },
    };
    const encx64 = std.build.Pkg{
        .name = "encx64",
        .source = .{ .path = "libs/encx64/encx64.zig" },
        .dependencies = &[_]std.build.Pkg{
            common,
        },
    };
    const elf = std.build.Pkg{
        .name = "elf",
        .source = .{ .path = "libs/out/elf/elf.zig" },
        .dependencies = &[_]std.build.Pkg{
            common,
            fileio,
        },
    };
    const macho = std.build.Pkg{
        .name = "macho",
        .source = .{ .path = "libs/out/macho/macho.zig" },
        .dependencies = &[_]std.build.Pkg{
            common,
            fileio,
        },
    };
    const out = std.build.Pkg{
        .name = "out",
        .source = .{ .path = "libs/out/out.zig" },
        .dependencies = &[_]std.build.Pkg{
            elf,
            macho,
        },
    };
};

pub fn build(b: *std.build.Builder) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("zas", "src/main.zig");
    exe.addPackage(pkgs.fileio);
    exe.addPackage(pkgs.common);
    exe.addPackage(pkgs.lexer);
    exe.addPackage(pkgs.encx64);
    exe.addPackage(pkgs.out);
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_tests = b.addTest("src/main.zig");
    exe_tests.setTarget(target);
    exe_tests.setBuildMode(mode);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&exe_tests.step);

    // encx64 tests
    const enc_tests = b.addTest("libs/encx64/encx64.zig");
    enc_tests.addPackage(pkgs.common);
    enc_tests.setTarget(target);
    enc_tests.setBuildMode(mode);

    test_step.dependOn(&enc_tests.step);
}
