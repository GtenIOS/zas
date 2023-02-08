const std = @import("std");
const Symbol = @import("common").symbol.Symbol;
const Section = @import("common").section.Section;
const Relocation = @import("common").reloc.Relocation;
const Token = @import("lexer").Token;

pub const LineInfo = struct {
    idx: u32,
    tokens: std.ArrayList(Token),
    start_ofst: u32,
    instr_size: ?u32,
    sym: ?*Symbol,
    reloc: ?*Relocation = null,
    times: u32 = 1,
    pub var total_count: u32 = 0;
    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, idx: u32, tokens: []const Token, start_ofst: u32, instr_size: ?u32, sym: ?*Symbol, times: u32) !Self {
        var toks = std.ArrayList(Token).init(allocator);
        try toks.appendSlice(tokens);
        total_count += 1;
        return Self{ .idx = idx, .tokens = toks, .start_ofst = start_ofst, .instr_size = instr_size, .sym = sym, .times = times };
    }

    pub fn deinit(self: *Self) void {
        self.tokens.deinit();
    }
};

pub const UnknownSym = struct {
    name: []const u8,
    used_at_lines: std.ArrayList(usize), // We only support forward reference in text section
    lines: std.ArrayList(LineInfo),
    resolved: bool = false,
    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, name: []const u8) Self {
        var lines = std.ArrayList(usize).init(allocator);
        var lines_info = std.ArrayList(LineInfo).init(allocator);
        return Self{ .name = name, .used_at_lines = lines, .lines = lines_info };
    }

    pub fn addLine(self: *Self, line: usize, line_info: LineInfo) !void {
        try self.used_at_lines.append(line);
        try self.lines.append(line_info);
    }

    pub fn setResolved(self: *Self) void {
        self.resolved = true;
    }

    pub fn deinit(self: *Self) void {
        self.used_at_lines.deinit();
        for (self.lines.items) |*line_info| line_info.deinit();
        self.lines.deinit();
    }
};

pub const UnknownSymInfo = struct {
    unknown_sym: *UnknownSym,
    idx: usize,
};

pub const Symtab = struct {
    var symbols: ?std.ArrayList(Symbol) = null;
    var unknown_syms: ?std.ArrayList(UnknownSym) = null;

    pub inline fn find(name: []const u8) ?*Symbol {
        for (symbols.?.items) |*symbol| {
            if (std.mem.eql(u8, name, symbol.name)) return symbol;
        }

        return null;
    }

    pub fn init(allocator: std.mem.Allocator, name: []const u8, global: bool, should_init: bool, section: *const Section) !*Symbol {
        symbols = std.ArrayList(Symbol).init(allocator);
        try symbols.?.append(.{ .idx = @intCast(u32, symbols.?.items.len), .name = name, .global = global, .did_init = should_init, .section = section, .ofst_in_sec = section.size() });
        return &symbols.?.items[symbols.?.items.len - 1];
    }

    pub fn add(allocator: std.mem.Allocator, name: []const u8, global: bool, should_init: bool, section: *Section) !*Symbol {
        if (symbols == null) {
            const symbol = try init(allocator, name, global, should_init, section);
            try section.addSymbol(allocator, symbol);
            return symbol;
        }

        if (find(name)) |sym| {
            if (sym.did_init)
                return error.SymbolExists;

            sym.ofst_in_sec = section.size();
            sym.did_init = true;
            return sym;
        } else {
            const symbol = Symbol{ .idx = @intCast(u32, symbols.?.items.len), .name = name, .global = global, .did_init = should_init, .section = section, .ofst_in_sec = section.size() };
            try symbols.?.append(symbol);
            try section.addSymbol(allocator, &symbols.?.items[symbols.?.items.len - 1]);
            return &symbols.?.items[symbols.?.items.len - 1];
        }
    }

    pub fn incrementOffsets(allocator: std.mem.Allocator, diff: u32, start_idx: u32, start_ofst: u32, section: *Section) !std.ArrayList(UnknownSymInfo) {
        var unknown_sym_infos = std.ArrayList(UnknownSymInfo).init(allocator);
        if (section.symbols) |syms| {
            for (syms.items) |sym| {
                if (sym.idx > start_idx and sym.ofst_in_sec >= start_ofst) {
                    sym.ofst_in_sec += diff;
                    if (sym.unknown_idx) |unknwn_idx| try unknown_sym_infos.append(UnknownSymInfo{ .unknown_sym = &unknown_syms.?.items[unknwn_idx], .idx = unknwn_idx });
                }
            }
        }

        return unknown_sym_infos;
    }

    pub fn total() usize {
        return if (symbols) |symbs| symbs.items.len else 0;
    }

    pub inline fn findUnknown(name: []const u8) ?UnknownSymInfo {
        if (unknown_syms) |unknwn_syms| {
            for (unknwn_syms.items) |*symbol, i| {
                if (std.mem.eql(u8, name, symbol.name)) return UnknownSymInfo{ .unknown_sym = symbol, .idx = i };
            }
        }

        return null;
    }

    pub fn addUnknown(allocator: std.mem.Allocator, name: []const u8) !*UnknownSym {
        if (unknown_syms == null) unknown_syms = std.ArrayList(UnknownSym).init(allocator);

        try unknown_syms.?.append(UnknownSym.init(
            allocator,
            name,
        ));
        return &unknown_syms.?.items[unknown_syms.?.items.len - 1];
    }

    pub fn incrementLineInfoOffsets(diff: u32, start_idx: u32, start_ofst: u32) void {
        if (unknown_syms) |unknwn_syms| {
            for (unknwn_syms.items) |*unknwn_sym| {
                for (unknwn_sym.lines.items) |*line| {
                    if (line.start_ofst >= start_ofst and line.idx > start_idx) line.start_ofst += diff;
                }
            }
        }
    }

    pub fn hasUnresolvedSym() bool {
        if (unknown_syms) |unknwn_syms| {
            for (unknwn_syms.items) |sym| if (!sym.resolved) return true;

            return false;
        } else return false;
    }

    pub fn removeUnknown(unknown_sym_info: UnknownSymInfo) !void {
        if (unknown_syms) |*unknwn_syms| {
            if (unknwn_syms.items.len <= unknown_sym_info.idx) return error.IndexOutOfBound;
            _ = unknwn_syms.swapRemove(unknown_sym_info.idx);
        }
    }

    pub fn description() void {
        if (symbols) |syms| {
            for (syms.items) |sym| std.log.info("{s} ofst_in_sec: {d} sec: {s}", .{ sym.name, sym.ofst_in_sec, sym.section.name });
        }
    }

    pub fn deinit() void {
        if (symbols) |*symbs| symbs.deinit();

        if (unknown_syms) |*unknwn_syms| {
            for (unknwn_syms.items) |*sym| sym.deinit();
            unknwn_syms.deinit();
        }
    }
};
