const std = @import("std");
pub const Directive = enum {
    Bits, // 32, 64 (default)
    Section, // text, data, rodata, bss
    Global,
    Extern,
    Include,
    Times,

    const Dirct = struct {
        name: []const u8,
        dir: Directive,
    };
    const dircts = &[_]Dirct{
        .{ .name = "bits", .dir = .Bits },
        .{ .name = "section", .dir = .Section },
        .{ .name = "global", .dir = .Global },
        .{ .name = "extern", .dir = .Extern },
        .{ .name = "include", .dir = .Include },
        .{ .name = "times", .dir = .Times },
    };

    pub fn isDirective(name: []const u8) ?Directive {
        for (Directive.dircts) |dirct| {
            if (std.mem.eql(u8, dirct.name, name)) return dirct.dir;
        }

        return null;
    }
};
