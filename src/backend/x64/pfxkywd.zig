const std = @import("std");
const Prefix = @import("encx64").pfx.Prefix;
pub const PrefixKeyword = enum {
    Lock,
    Repe,
    Repz,
    Repne,
    Repnz,
    Rep,
    
    const PfxKywd = struct {
        name: []const u8,
        pfx_kywd: PrefixKeyword,
        pfx: Prefix,
    };
    const pfx_kywds = &[_]PfxKywd{
        .{ .name = "lock", .pfx_kywd = .Lock, .pfx = .Lock },
        .{ .name = "repe", .pfx_kywd = .Repe, .pfx = .Repez },
        .{ .name = "repz", .pfx_kywd = .Repz, .pfx = .Repez },
        .{ .name = "repne", .pfx_kywd = .Repne, .pfx = .Repnez },
        .{ .name = "repnz", .pfx_kywd = .Repnz, .pfx = .Repnez },
        .{ .name = "rep", .pfx_kywd = .Rep, .pfx = .Repez },
    };
    
    pub fn isPrefix(name: []const u8) ?Prefix {
        for (pfx_kywds) |pfx_kywd| {
            if (std.mem.eql(u8, pfx_kywd.name, name)) return pfx_kywd.pfx;
        }
        return null;
    }
};
