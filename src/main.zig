const std = @import("std");
const os = std.os;
const mem = std.mem;
const fileio = @import("fileio");
const X64Parser = @import("./frontend/x64/parser.zig").Parser;
const Symtab = @import("./frontend/x64/symtab.zig").Symtab;
const out = @import("out");
const Reltab = @import("./frontend/x64/reltab.zig").Reltab;
const builtin = @import("builtin");

pub fn usage() !void {
    _ = try std.io.getStdOut().write("usage: zas [-f [macho|elf]] <input-file>\n");
}

var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
const allocator = gpa.allocator();
const ExeType = enum {
    macho,
    elf,
};

fn handleArgs(args: [][*:0]u8, input_file_name: *[]const u8, exe_type: *ExeType) !void {
    if (args.len > 3) {
        _ = try std.io.getStdErr().write("Excessive options\n");
        return error.TooManyOptions;
    }

    const arg0 = mem.sliceTo(args[0], 0);
    if (std.mem.eql(u8, arg0, "-f")) {
        if (args.len < 2) {
            _ = try std.io.getStdErr().write("Missing executable type after '-f'\n",);
            return error.MissingExecutableType;
        }

        const arg1 = mem.sliceTo(args[1], 0);
        if (std.mem.eql(u8, arg1, "macho")) {
            exe_type.* = .macho;
        }
        else if (std.mem.eql(u8, arg1, "elf")) {
            exe_type.* = .elf;
        }
        else {
            try std.io.getStdErr().writer().print("Invalid executable type: {s}\n", .{arg1});
            return error.InvalidExecutable;
        }

        if (args.len < 3) {
            _ = try std.io.getStdErr().write("Missing input file path\n");
            return error.MissingInputFilePath;
        }
        const arg2 = mem.sliceTo(args[2], 0);
        input_file_name.* = mem.sliceTo(arg2, 0);
    }
    else {
        if (arg0[0] == '-') {
            try std.io.getStdErr().writer().print("Invalid option: {s}\n", .{arg0});
            return error.InvalidOption;
        }

        input_file_name.* = arg0;

        if (args.len > 1) {
            const arg1 = mem.sliceTo(args[1], 0);
            if (std.mem.eql(u8, arg0, "-f")) {
                if (args.len < 3) {
                    _ = try std.io.getStdErr().write("Missing executable type after '-f'\n",);
                    return error.MissingExecutableType;
                }
                const arg2 = mem.sliceTo(args[2], 0);
                if (std.mem.eql(u8, arg2, "macho")) {
                    exe_type.* = .macho;
                }
                else if (std.mem.eql(u8, arg2, "elf")) {
                    exe_type.* = .elf;
                }
                else {
                    try std.io.getStdErr().writer().print("Invalid executable type: {s}\n", .{arg2});
                    return error.InvalidExecutable;
                }
            }
            else {
                try std.io.getStdErr().writer().print("Invalid option: {s}\n", .{arg1});
                return error.InvalidOption;
            }
        }
        else {
            exe_type.* = if (builtin.target.os.tag.isDarwin()) .macho else .elf;
        }
    }
}

pub fn main() anyerror!void {
    // Parse args
    if (std.os.argv.len < 2) {
        try usage();
        return error.InSufficientArguments;
    }

    // Input file
    const args: [][*:0]u8 = os.argv[1..os.argv.len];
    var input_file_name: []const u8 = undefined;
    var exe_type: ExeType = undefined;
    handleArgs(args, &input_file_name, &exe_type) catch {
        try usage();
        return error.InvalidArguments;
    };

    // Allocator
    defer {
        _ = gpa.deinit();
    }

    var lines = std.ArrayList([]const u8).init(allocator);
    defer lines.deinit();
    try fileio.readFileToLines(allocator, input_file_name, &lines);
    defer {
        for (lines.items) |line| {
            allocator.free(line);
        }
    }

    var parser = X64Parser.init(allocator, .Bits64, lines) catch |err| {
        std.log.err("Could not instantiate parser!", .{});
        return err;
    };
    defer parser.deinit();

    parser.parse() catch |err| {
        std.log.err("Could not parse the file `{s}`", .{input_file_name});
        return err;
    };

    if (parser.errors > 0) {
        std.log.err("Could not assemble the file `{s}` due to previous {d} error(s)", .{ input_file_name, parser.errors });
    }

    // Linux
    switch (exe_type) {
        .elf => try out.elf.Elf.genPieExe64(allocator, parser.sections.items, if (Reltab.relocations) |relocs| relocs.items else null, "a.out"),
        .macho => try out.macho.Macho.genPieExe64(allocator, parser.sections.items, if (Reltab.relocations) |relocs| relocs.items else null, "a.out"),
    }
}
