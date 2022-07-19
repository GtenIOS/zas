const std = @import("std");
pub const SizeKeyword = enum {
    Byte,
    Word,
    Dword,
    Qword,
    Pword,
    const Self = @This();
    const SizeKywd = struct {
        name: []const u8,
        kywd: SizeKeyword,
    };
    const kywds = &[_]SizeKywd{
        .{ .name = "byte", .kywd = .Byte },
        .{ .name = "word", .kywd = .Word },
        .{ .name = "dword", .kywd = .Dword },
        .{ .name = "qword", .kywd = .Qword },
        .{ .name = "pword", .kywd = .Pword },
    };
    
    pub fn isSizeKeyword(name: []const u8) ?SizeKeyword {
        for (kywds) |kywd| if (std.mem.eql(u8, kywd.name, name)) return kywd.kywd;
        
        return null;
    }
    
    pub inline fn size(self: Self) u16 {
        switch (self) {
            .Byte => return 8,
            .Word => return 16,
            .Dword => return 32,
            .Qword => return 64,
            .Pword => return 80,
        }
    }
};