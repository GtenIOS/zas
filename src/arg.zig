const std = @import("std");
const ExeType = @import("out").ExeType;

const ArgHyphn = struct {
    opt: u8,
    val: ?[]const u8,
};
const Arg = union(enum) {
    hyphn: ArgHyphn,
    raw: []const u8,
};

fn parseArgs(args: [][*:0]u8, allocator: std.mem.Allocator) !std.ArrayList(Arg) {
    if (args.len == 0) @panic("Can't parse empty argumenst");

    var parsed_args: std.ArrayList(Arg) = try std.ArrayList(Arg).initCapacity(allocator, args.len);
    errdefer parsed_args.deinit();
    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        if (args[i][0] == '-') { // Positional
            if (args[i][1] == '\x00') {
                _ = try std.io.getStdErr().write("Missing option sepecifier after '-'\n");
                return error.MissingOptionSpecifier;
            }
            const opt: u8 = args[i][1];
            const opt_val: ?[]const u8 = blk: {
                if (args.len > i + 1) {
                    i += 1;
                    break :blk std.mem.sliceTo(args[i], 0);
                } else break :blk null;
            };
            try parsed_args.append(Arg{ .hyphn = ArgHyphn{ .opt = opt, .val = opt_val } });
        } else {
            try parsed_args.append(Arg{ .raw = std.mem.sliceTo(args[i], 0) });
        }
    }

    return parsed_args;
}

pub fn handleArgs(args: [][*:0]u8, input_file_name: *[]const u8, exe_type: *ExeType, allocator: std.mem.Allocator) !void {
    if (args.len > 3) {
        _ = try std.io.getStdErr().write("Excessive options\n");
        return error.TooManyOptions;
    }

    const parsed_args = try parseArgs(args, allocator);
    defer parsed_args.deinit();
    var found_input_filename_opt: bool = false;
    var found_exe_opt: bool = false;
    for (parsed_args.items) |arg| {
        switch (arg) {
            .hyphn => |hy_arg| {
                if (hy_arg.opt != 'f') {
                    try std.io.getStdErr().writer().print("Invalid option: -{c}\n", .{hy_arg.opt});
                    return error.InvalidOption;
                }

                if (hy_arg.val) |opt_val| {
                    found_exe_opt = true;
                    if (std.mem.eql(u8, opt_val, "macho")) {
                        exe_type.* = .macho;
                    } else if (std.mem.eql(u8, opt_val, "elf")) {
                        exe_type.* = .elf;
                    } else {
                        try std.io.getStdErr().writer().print("Unrecognized executable file type: {s}\n", .{opt_val});
                        return error.UnrecognizedExeType;
                    }
                } else {
                    _ = try std.io.getStdErr().write("Missing executable type\n");
                    return error.MissingExeType;
                }
            },
            .raw => |raw_arg| {
                found_input_filename_opt = true;
                input_file_name.* = raw_arg;
            },
        }
    }

    if (!found_input_filename_opt) {
        _ = try std.io.getStdErr().write("Missing input file path\n");
        return error.MissingInputFilePath;
    }

    if (!found_exe_opt) {
        exe_type.* = ExeType.default();
    }
}


