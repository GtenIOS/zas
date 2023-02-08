const std = @import("std");
const Relocation = @import("common").reloc.Relocation;
const RelocType = @import("common").reloc.RelocType;
const Section = @import("common").section.Section;

pub const Reltab = struct {
    pub var relocations: ?std.ArrayList(Relocation) = null;

    pub fn add(allocator: std.mem.Allocator, loc: u32, ofst_in_sec: u32, sec: *const Section, addr_size: u4, reloc_type: RelocType) !*Relocation {
        if (relocations == null) relocations = std.ArrayList(Relocation).init(allocator);

        try relocations.?.append(.{ .loc = loc, .ofst_in_sec = ofst_in_sec, .sec = sec, .size = addr_size, .type = reloc_type });
        return &relocations.?.items[relocations.?.items.len - 1];
    }

    pub fn description() void {
        if (relocations) |relocs| {
            for (relocs.items) |reloc| std.log.info("loc: {d}, ofst_in_sec: {d}, sec: {s}, addr_size: {d}, type: {d}", .{ reloc.loc, reloc.ofst_in_sec, reloc.sec.name, reloc.size, @enumToInt(reloc.type) });
        }
    }

    pub fn deinit() void {
        if (relocations) |relocs| relocs.deinit();
    }
};
