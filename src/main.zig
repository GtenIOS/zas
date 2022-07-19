const std = @import("std");
const os = std.os;
const mem = std.mem;
const fileio = @import("fileio");
const X64Parser = @import("./backend/x64/parser.zig").Parser;
const Symtab = @import("./backend/x64/symtab.zig").Symtab;
const out = @import("out");
const Reltab = @import("./backend/x64/reltab.zig").Reltab;

pub fn usage() !void {
    _ = try std.io.getStdOut().write("usage: parser input-file\n");
}

var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
const allocator = gpa.allocator();
pub fn main() anyerror!void {
    // Parse args
    if (std.os.argv.len < 2) {
        try usage();
        return error.InSufficientArguments;
    }

    // Input file
    const args: [][*:0]u8 = os.argv[1..os.argv.len];
    const input_file_name: []const u8 = mem.sliceTo(args[0], 0);

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
    // try out.elf.Elf.genPieExe64(allocator, parser.sections.items, if (Reltab.relocations) |relocs| relocs.items else null, "a.out");
    
    // Darwin, i.e. Mac
    try out.macho.Macho.genPieExe64(allocator, parser.sections.items, if (Reltab.relocations) |relocs| relocs.items else null, "a.out");
}
