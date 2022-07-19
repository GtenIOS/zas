const std = @import("std");
pub const Keyword = enum {
    Db,
    Dw,
    Dd,
    Dq,
    Resb,
    Resw,
    Resd,
    Resq,
    Equ,

    const Kywd = struct {
        name: []const u8,
        kywd: Keyword,
    };
    const kywds = &[_]Kywd{
        .{ .name = "db", .kywd = .Db },
        .{ .name = "dw", .kywd = .Dw },
        .{ .name = "dd", .kywd = .Dd },
        .{ .name = "dq", .kywd = .Dq },
        .{ .name = "resb", .kywd = .Resb },
        .{ .name = "resw", .kywd = .Resw },
        .{ .name = "resd", .kywd = .Resd },
        .{ .name = "resq", .kywd = .Resq },
        .{ .name = "equ", .kywd = .Equ },
    };

    pub fn isKeyword(name: []const u8) ?Keyword {
        for (Keyword.kywds) |kywd| {
            if (std.mem.eql(u8, kywd.name, name)) return kywd.kywd;
        }

        return null;
    }
};
