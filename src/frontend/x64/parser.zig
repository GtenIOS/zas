const std = @import("std");
const lexer = @import("lexer");
const Lexer = lexer.Lexer;
const Token = lexer.Token;
const TokenKind = lexer.TokenKind;
const encx64 = @import("encx64");
const instr = encx64.instr;
const Register = encx64.reg.Register;
const MOffset = encx64.mem.MOffset;
const Rel = encx64.mem.Rel;
const Memory = encx64.mem.Memory;
const Operand = encx64.opnd.Operand;
const Prefix = encx64.pfx.Prefix;
const Displacement = encx64.mem.Displacement;
const Imm = @import("common").imm.Imm;
const Directive = @import("dirct.zig").Directive;
const Keyword = @import("kywd.zig").Keyword;
const PrefixKeyword = @import("pfxkywd.zig").PrefixKeyword;
const SizeKeyword = @import("sizekywd.zig").SizeKeyword;
const Symtab = @import("symtab.zig").Symtab;
const Reltab = @import("reltab.zig").Reltab;
const RelocType = @import("common").reloc.RelocType;
const UnknownSym = @import("symtab.zig").UnknownSym;
const UnknownSymInfo = @import("symtab.zig").UnknownSymInfo;
const LineInfo = @import("symtab.zig").LineInfo;
const Symbol = @import("common").symbol.Symbol;
const Section = @import("common").section.Section;
const SectionType = @import("common").section.SectionType;
const OperatingMode = @import("common").OperatingMode;
const Rpn = @import("rpn.zig").Rpn;
const Oper = @import("oper.zig").Oper;
const Ast = @import("ast.zig").Ast;

pub const Parser = struct {
    allocator: std.mem.Allocator,
    lexer: Lexer,
    op_mode: OperatingMode,
    sections: std.ArrayList(Section),
    curr_sec: *Section,
    curr_sym: ?*Symbol = null,
    line: usize = 1,
    peek_token: ?Token = null,
    allocd_names: ?std.ArrayList([]const u8) = null,
    line_tokens: std.ArrayList(Token),
    explicit_tokens: ?[]const Token = null,
    explicit_tokens_visited: usize = undefined,
    errors: usize = 0,
    times: u32 = 1,
    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, op_mode: OperatingMode, lines: std.ArrayList([]const u8)) !Self {
        // Init sections list with `.text` section as default
        // Note: First section in the sections array list should always be the text section
        //       The rest of the program also assumes the same
        var sections = std.ArrayList(Section).init(allocator);
        const sec_text = Section.initText(0);
        try sections.append(sec_text);
        var line_tokens = std.ArrayList(Token).init(allocator);
        return Self{ .allocator = allocator, .lexer = Lexer.init(lines), .op_mode = op_mode, .sections = sections, .curr_sec = &sections.items[0], .line_tokens = line_tokens };
    }

    inline fn findSection(self: Self, name: []const u8) ?*Section {
        for (self.sections.items) |*sec| {
            // Ignore the `.` at the beginning
            if (std.mem.eql(u8, sec.name[1..], name)) return sec;
        }

        return null;
    }

    inline fn addSection(self: *Self, sec: Section) !*Section {
        try self.sections.append(sec);
        return &self.sections.items[self.sections.items.len - 1];
    }

    inline fn changeSection(self: *Self, sec: *Section) void {
        self.curr_sec = sec;

        // Clear current symbol pointer
        self.curr_sym = null;
    }

    inline fn addAllocdName(self: *Self, name: []const u8) !void {
        if (self.allocd_names) |*allocd_names| {
            try allocd_names.append(name);
        } else {
            self.allocd_names = std.ArrayList([]const u8).init(self.allocator);
            try self.allocd_names.?.append(name);
        }
    }

    const NextTokenError = error{
        OutOfMemory,
    };
    fn nextToken(self: *Self) NextTokenError!?Token {
        return if (self.peek_token) |token| blk: {
            self.peek_token = null;
            break :blk token;
        } else if (self.explicit_tokens) |tokens| blk: {
            const next_tok: ?Token = if (self.explicit_tokens_visited >= tokens.len) null else tokens[self.explicit_tokens_visited];
            self.explicit_tokens_visited += 1;
            break :blk next_tok;
        } else blk: {
            const token = self.lexer.fetchNextToken();
            if (token) |tok| {
                if (tok.line != self.line) self.line_tokens.clearRetainingCapacity();
                try self.line_tokens.append(tok);
            }
            break :blk token;
        };
    }

    const AcceptError = error{
        MissingToken,
        InvalidToken,
    } || NextTokenError;
    fn accept(self: *Self, tok_type: TokenKind) AcceptError!Token {
        if (try self.nextToken()) |token| {
            if (tok_type != token.type) {
                self.peek_token = token;
                return error.InvalidToken;
            }

            return token;
        } else return error.MissingToken;
    }

    fn handleGlobal(self: *Self) !void {
        const id_tok = try self.accept(TokenKind.id);
        const name = try id_tok.val.strVal();
        _ = try Symtab.add(self.allocator, name, true, false, self.curr_sec);
    }

    fn handleSection(self: *Self) !void {
        _ = try self.accept(TokenKind.dot);
        const id_tok = try self.accept(TokenKind.id);
        const name = try id_tok.val.strVal();
        if (self.findSection(name)) |sec| {
            if (self.curr_sec.idx != sec.idx) self.changeSection(sec);
        } else {
            const sec = try Section.init(name, @intCast(u8, self.sections.items.len));
            self.changeSection(try self.addSection(sec));
        }
    }

    fn handleTimes(self: *Self) !void {
        // We only accept integer literal as `times` suffix at this moment
        // TODO: - Accept const expression instead
        const tok_num = try self.accept(TokenKind.num);
        const num_val = try tok_num.val.numVal();
        self.times = num_val.toImm().toImm32();
    }

    fn parseDirective(self: *Self, dirct: Directive) !void {
        switch (dirct) {
            .Bits => std.log.info("Found directive: bits", .{}),
            .Section => return self.handleSection(),
            .Global => return self.handleGlobal(),
            .Extern => std.log.info("Found directive: extern", .{}),
            .Include => std.log.info("Found directive: include", .{}),
            .Times => return self.handleTimes(),
        }
    }

    fn parseData(self: *Self, size: u4) !void {
        var bytes = std.ArrayList(u8).init(self.allocator);
        defer bytes.deinit();

        while (try self.nextToken()) |token| {
            const tok_bytes = try token.bytesValue(self.*.allocator);
            defer tok_bytes.deinit();
            try bytes.appendSlice(tok_bytes.items);

            // data are comma separated and ends with new line
            _ = self.accept(TokenKind.com) catch {
                _ = self.accept(TokenKind.nl) catch |err| {
                    if (err == error.MissingToken) {
                        break;
                    }
                    return err;
                };
                break;
            };
        }

        if (bytes.items.len == 0) return error.MissingData;

        // Align according to given data size (in bytes)
        var bytes_to_align = @mod(bytes.items.len, size);
        if (bytes_to_align > 0) {
            try bytes.appendNTimes(0, bytes_to_align);
        }

        try self.curr_sec.appendSlice(self.allocator, bytes.items);
    }

    fn parseReserveData(self: *Self, size: u4) !void {
        if (self.curr_sym) |curr_sym| {
            if (curr_sym.section.*.type != .Bss) return error.UseOfResKeywordInInvalidSection;
        }
        else {
            return error.InvalidUseOfResKeyword;
        }
        const tok = try self.accept(TokenKind.num);
        const num_val = try (try tok.numVal()).toISize();
        if (num_val <= 0) return error.InvalidReserveSize;
        const res_size = @intCast(usize, num_val) * size;
        self.curr_sym.?.*.res_size = res_size;
        self.curr_sec.addResSize(res_size);
    }

    fn parseConstExpr(self: *Self) !Ast {
        var rpn = Rpn.init(self.allocator);
        defer rpn.deinit();

        while (try self.nextToken()) |token| {
            switch (token.type) {
                .dol => try rpn.handleImm(Imm{ .imm32 = self.curr_sec.size() }),
                .num, .chr => {
                    switch (try token.val.numVal()) {
                        .int => |ival| try rpn.handleImm(Imm.fromISize(ival)),
                        .float => |fval| try rpn.handleImm(Imm.fromFloat(fval)),
                    }
                },
                .id => {
                    const name = try token.val.strVal();
                    if (self.curr_sec.findSymbol(name)) |symbol| {
                        // try rpn.handleSym(symbol.*);
                        try rpn.handleImm(Imm{ .imm32 = symbol.ofst_in_sec });
                    } else {
                        return error.UnknownSymbol;
                    }
                },
                .nl => break,
                else => {
                    if (token.type.isArithOperator()) {
                        try rpn.handleOper(try Oper.from(token.type));
                    } else {
                        return error.InvalidConstExpression;
                    }
                },
            }
        }

        return rpn.genTree(self.allocator);
    }

    // `Equ`
    fn parseConstData(self: *Self) !void {
        var ast = try self.parseConstExpr();
        defer Ast.deinit();
        const imm = try ast.resolveConst();
        if (self.curr_sym) |sym| {
            sym.imm = imm;
        } else {
            const bytes_array = try imm.encode(self.*.allocator);
            defer bytes_array.deinit();
            try self.curr_sec.data.?.appendSlice(bytes_array.items);
        }
    }

    fn parseKeyword(self: *Self, kywd: Keyword) !void {
        switch (kywd) {
            .Db => return self.parseData(1),
            .Dw => return self.parseData(2),
            .Dd => return self.parseData(4),
            .Dq => return self.parseData(8),
            .Resb => return self.parseReserveData(1),
            .Resw => return self.parseReserveData(2),
            .Resd => return self.parseReserveData(4),
            .Resq => return self.parseReserveData(8),
            .Equ => return try self.parseConstData(),
        }
    }

    const ResolveLineError = error{
        OutOfMemory,
        NotAString,
    } || ParseInstrError;
    fn resolveLine(self: *Self, line: LineInfo) ResolveLineError!?ParseInstrInfo {
        self.explicit_tokens = line.tokens.items;
        self.explicit_tokens_visited = 0;
        defer self.explicit_tokens = null;

        const mnem_tok = try self.accept(.id);
        return try self.parseInstr(try mnem_tok.val.strVal());
    }

    const ResolveUnknownSymbolError = error{
        CouldNotResolveUnknownSymbol,
    } || ResolveLineError;
    fn resolveUnknownSymbol(self: *Self, unknwn_sym_info: UnknownSymInfo) ResolveUnknownSymbolError!void {
        // Backup and change current parser context
        const curr_sym = self.curr_sym;
        const curr_sec = self.curr_sec;
        self.curr_sec = &self.sections.items[0];
        const peek_tok = self.peek_token;
        self.peek_token = null;
        defer {
            // Restore original parser context
            self.curr_sym = curr_sym;
            self.curr_sec = curr_sec;
            self.peek_token = peek_tok;
        }

        // Instruction resolve loop
        for (unknwn_sym_info.unknown_sym.lines.items) |*line_info, i| {
            self.curr_sym = line_info.sym;
            var resolve_attempts: u2 = 2;
            resolve_loop: while (resolve_attempts > 0) : (resolve_attempts -= 1) {
                const resolve_instr_info = try self.resolveLine(line_info.*);
                const instr_info = resolve_instr_info orelse break;
                defer instr_info.instr_bytes.deinit();
                const old_instr_size = line_info.instr_size;
                const new_instr_size = @intCast(u32, instr_info.instr_bytes.items.len * line_info.times);
                const text_sec: *Section = &self.sections.items[0]; // .text section is always the first section
                try text_sec.replaceRange(self.allocator, line_info.start_ofst, old_instr_size orelse 0, instr_info.instr_bytes.items, line_info.times);

                // Updated relocatable if any
                if (line_info.reloc) |reloc| {
                    reloc.loc = line_info.start_ofst + new_instr_size - instr_info.reloc_size.?;
                    reloc.ofst_in_sec = instr_info.reloc_sym.?.*.ofst_in_sec;
                    reloc.sec = instr_info.reloc_sym.?.*.section;
                    reloc.size = instr_info.reloc_size.?;
                }
                // Found relocatable, add it in the line info to be used on possible next pass of the resolve loop
                else if (instr_info.reloc_sym) |rel_sym| {
                    line_info.reloc = try Reltab.add(self.allocator, line_info.start_ofst + new_instr_size - instr_info.reloc_size.?, rel_sym.ofst_in_sec, rel_sym.section, instr_info.reloc_size.?, instr_info.reloc_type.?);
                }

                if (old_instr_size == new_instr_size) break; // Instruction size remains same, no need for further loop pass

                unknwn_sym_info.unknown_sym.lines.items[i].instr_size = new_instr_size;

                const diff = new_instr_size - (old_instr_size orelse 0);
                std.log.info("instr size diff {d}", .{diff});
                Symtab.incrementLineInfoOffsets(diff, line_info.idx, line_info.start_ofst);

                // Increment offsets of known symbols occuring after current offset by the instr bytes size difference
                var unknown_syms = try Symtab.incrementOffsets(self.allocator, diff, line_info.sym.?.idx, line_info.start_ofst, self.curr_sec);
                defer unknown_syms.deinit();
                for (unknown_syms.items) |unknwn_sym| {
                    try self.resolveUnknownSymbol(unknwn_sym);
                    // No need to continue the loop anymore
                    break :resolve_loop;
                }
            }

            if (resolve_attempts == 0) return error.CouldNotResolveUnknownSymbol;
        }

        // Mark unknown symbol as resolved
        unknwn_sym_info.unknown_sym.setResolved();
    }

    fn resolveMatchingUnknown(self: *Self, sym: *Symbol) !void {
        if (Symtab.findUnknown(sym.name)) |unknwn_sym| {
            // Resolve unknown symbol
            sym.unknown_idx = unknwn_sym.idx;

            try self.resolveUnknownSymbol(unknwn_sym);
        }
    }

    fn parseLabel(self: *Self, name: []const u8) !void {
        self.curr_sym = try Symtab.add(self.allocator, name, false, true, self.curr_sec);

        const id_tok = self.accept(TokenKind.id) catch {
            // Check if the symbol was used before it's declaration,
            // If it was, re assemble all the lines where it was used
            try self.resolveMatchingUnknown(self.curr_sym.?);
            return;
        };

        if (Keyword.isKeyword(try id_tok.val.strVal())) |kywd| {
            try self.parseKeyword(kywd);
        }

        // Check if the symbol was used before declaration,
        // If it was, re assemble all the lines where it was used
        try self.resolveMatchingUnknown(self.curr_sym.?);
    }

    fn localLabelName(self: Self, name: []const u8) ![]u8 {
        if (self.curr_sym) |sym| {
            return std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ sym.name, name });
        } else {
            return std.fmt.allocPrint(self.allocator, ".{s}", .{name});
        }
    }

    fn parseLocalLabel(self: *Self) !void {
        const id_tok = try self.accept(TokenKind.id);
        _ = try self.accept(TokenKind.col);
        const name = try id_tok.val.strVal();
        const full_name = try self.localLabelName(name);
        try self.addAllocdName(full_name);
        self.curr_sym = try Symtab.add(self.allocator, full_name, false, true, self.curr_sec);

        // Check if the symbol was used before declaration,
        // If it was, re assemble all the lines where it was used
        try self.resolveMatchingUnknown(self.curr_sym.?);
    }

    const ParserOperand = struct {
        oper: Operand,
        is_frwd_ref: bool = false,
        unknwn_sym: ?*UnknownSym = null,
        sym: ?*Symbol = null,
    };

    const Operands = struct {
        count: u3 = 0,
        opers: [4]ParserOperand = [4]ParserOperand{ undefined, undefined, undefined, undefined },

        inline fn curr_oper(self: Operands) !*ParserOperand {
            return if (self.count > 4) return error.TooManyOperands else return &self.opers[self.count];
        }

        inline fn setOper(self: *Operands, oper: ParserOperand) error{TooManyOperands}!void {
            if (self.count > 3) return error.TooManyOperands;

            self.opers[self.count] = oper;
            self.count += 1;
        }

        inline fn toOpers(self: Operands) []Operand {
            // Static variable to hold opers
            // Otherwise, the release build would crash
            const Opers = struct {
                var opers: [4]Operand = undefined;
            };
            Opers.opers = [4]Operand{ self.opers[0].oper, self.opers[1].oper, self.opers[2].oper, self.opers[3].oper };
            return if (self.count == 0) &[_]Operand{} else Opers.opers[0..self.count];
        }

        inline fn hasUnknownSym(self: Operands) ?*UnknownSym {
            for (self.opers[0..self.count]) |oper| {
                if (oper.unknwn_sym) |unknwn_sym| return unknwn_sym;
            }

            return null;
        }

        inline fn relocSym(self: *Operands) ?*ParserOperand {
            for (self.opers) |*oper| {
                // Relative offset is not considered as relocatable
                if (!oper.oper.isRel() and oper.sym != null and oper.sym.?.imm == null) return oper;
            }

            return null;
        }

        const RelocInfoTuple = std.meta.Tuple(&[_]type{ u4, RelocType });
        inline fn relocOperInfo(self: *Operands) ?RelocInfoTuple {
            for (self.opers) |oper| {
                if (oper.oper.isRel() or oper.oper.isImm()) {
                    std.log.info("Size in bits: {d}", .{oper.oper.size()});
                    return RelocInfoTuple{ @intCast(u4, oper.oper.size() / 8), if (oper.oper.isImm()) RelocType.Abs else RelocType.Rela }; // Convert operand size from bits to bytes
                } else if (oper.oper.isMemory() and oper.sym != null and oper.sym.?.imm == null) return RelocInfoTuple{ 4, RelocType.Rela };
            }

            return null;
        }
    };

    inline fn parseOperand(self: *Self, token: Token, size_ovrd: ?SizeKeyword) !ParserOperand {
        switch (token.type) {
            .id => {
                const str_val = try token.val.strVal();
                if (Register.isReg(str_val)) |reg| {
                    return ParserOperand{ .oper = Operand{ .reg = reg } };
                }

                if (Symtab.find(str_val)) |sym| {
                    if (sym.imm) |imm| {
                        const imm_oper: Imm = if (size_ovrd) |sz_ovrd| try imm.toImm(sz_ovrd.size()) else imm;
                        return ParserOperand{ .oper = Operand{ .imm = imm_oper }, .sym = sym };
                    } else {
                        if (sym.section.type != .Text) {
                            return ParserOperand{ .oper = Operand{ .imm = .{ .imm = 0 } }, .sym = sym };
                        }
                        const imm = Imm.fromUSize(@intCast(usize, self.sections.items[0].size() - sym.ofst_in_sec));
                        const rel: Rel = try Rel.from(imm);
                        const rel_oper: Rel = if (size_ovrd) |sz_ovrd| try rel.toRel(sz_ovrd.size()) else rel;
                        return ParserOperand{ .oper = Operand{ .rel = rel_oper }, .sym = sym };
                    }
                } else { // Maybe a forward reference
                    const unknwn_sym = if (Symtab.findUnknown(str_val)) |unknwn_info| unknwn_info.unknown_sym else try Symtab.addUnknown(self.allocator, str_val);
                    return ParserOperand{ .oper = Operand{ .imm = .{ .imm8 = 0 } }, .is_frwd_ref = true, .unknwn_sym = unknwn_sym };
                }

                return error.InvalidOperand;
            },
            .num, .chr => {
                const num_val = try token.val.numVal();
                const imm_oper = if (size_ovrd) |sz_ovrd| try num_val.toImm().toImm(sz_ovrd.size()) else num_val.toImm();
                return ParserOperand{ .oper = Operand{ .imm = imm_oper } };
            },
            .dot => {
                const id_tok = try self.accept(TokenKind.id);
                const name = try id_tok.val.strVal();
                const full_name = try self.localLabelName(name);
                try self.addAllocdName(full_name);
                if (Symtab.find(full_name)) |sym| {
                    if (sym.imm) |imm| {
                        const imm_oper: Imm = if (size_ovrd) |sz_ovrd| try imm.toImm(sz_ovrd.size()) else imm;
                        return ParserOperand{ .oper = Operand{ .imm = imm_oper }, .sym = sym };
                    } else {
                        if (sym.section.type != .Text) {
                            return ParserOperand{ .oper = Operand{ .imm = .{ .imm = 0 } }, .sym = sym };
                        }
                        const imm = Imm.fromUSize(@intCast(usize, self.sections.items[0].size() - sym.ofst_in_sec));
                        const rel: Rel = try Rel.from(imm);
                        const rel_oper: Rel = if (size_ovrd) |sz_ovrd| try rel.toRel(sz_ovrd.size()) else rel;
                        return ParserOperand{ .oper = Operand{ .rel = rel_oper }, .sym = sym };
                    }
                } else { // Maybe a forward reference
                    const unknwn_sym = if (Symtab.findUnknown(full_name)) |unknwn_info| unknwn_info.unknown_sym else try Symtab.addUnknown(self.allocator, full_name);
                    return ParserOperand{ .oper = Operand{ .imm = .{ .imm8 = 0 } }, .is_frwd_ref = true, .unknwn_sym = unknwn_sym };
                }
            },
            else => return error.InvalidOperand,
        }
    }

    const ParseMemoryOperandError = error{
        InvalidMemoryOperand,
        InvalidRegisterUsedWithMemOffset,
        TooManyRegOperands,
        TooManyDisplacements,
        NotAStringValue,
        InvalidOperand,
        ValueNotCastableToNumber,
        NumberTooBig,
        InvalidScalerWithMemOffset,
        InvalidScalerOperand,
        InvalidSignedToken,
        InvalidScaleValue,
        VariableSizedRegisterOperands,
        InvalidRegisterSize,
        InvalidDisplacementSize,
        InvalidImmediateSize,
        InvalidMemorySize,
        InvalidRelativeOffsetSize,
        InvalidMOffsetSize,
        SizeTooBigForRelativeOffset,
    } || AcceptError;
    inline fn parseMemoryOperand(self: *Self, size_ovrd: ?SizeKeyword) ParseMemoryOperandError!ParserOperand {
        var scale: ?u4 = null;
        var displacement: ?Imm = null;
        var expecting_scale: bool = false;
        var registers: [2]Register = undefined;
        var registers_count: u8 = 0;
        var index_idx: u2 = 0;
        var base_idx: u2 = 0;
        var expecting_negative_displacement = false;
        var is_frwd_ref: bool = false;
        var found_non_const_sym: bool = false;
        var sym: ?*Symbol = null;
        var unknwn_sym: ?*UnknownSym = null;
        while (try self.nextToken()) |token| {
            if (token.type == TokenKind.rbrk) break;

            const operand = try self.parseOperand(token, size_ovrd);
            if (!found_non_const_sym and ((operand.sym != null and operand.sym.?.imm == null) or operand.is_frwd_ref)) found_non_const_sym = true;
            if (operand.unknwn_sym) |oper_unknwn_sym| {
                if (sym != null or unknwn_sym != null) return error.InvalidMemoryOperand;
                unknwn_sym = oper_unknwn_sym;
            }
            if (operand.sym) |oper_sym| {
                if (unknwn_sym != null or sym != null) return error.InvalidMemoryOperand;
                sym = oper_sym;
            }

            if (!found_non_const_sym) {
                switch (operand.oper) {
                    .reg => |reg| {
                        if (found_non_const_sym) return error.InvalidRegisterUsedWithMemOffset;
                        if (registers_count > 1) return error.TooManyRegOperands;

                        if (registers_count > 0 and scale != null) {
                            base_idx = 1;
                        } else if (registers_count > 0 and scale == null) {
                            index_idx = 1;
                        }

                        registers[registers_count] = reg;
                        registers_count += 1;
                    },
                    .imm => |imm| {
                        if (expecting_scale) {
                            scale = @intCast(u4, imm.toImm8());
                            expecting_scale = false;
                        } else {
                            if (displacement != null) return error.TooManyDisplacements;
                            displacement = if (!expecting_negative_displacement) imm else blk: {
                                expecting_negative_displacement = false;
                                break :blk try imm.toNegative();
                            };
                            is_frwd_ref = operand.is_frwd_ref;
                        }
                    },
                    else => {
                        if (operand.sym != null or operand.unknwn_sym != null) continue;
                        return error.InvalidMemoryOperand;
                    },
                }
            }
            _ = self.accept(TokenKind.pls) catch {
                _ = self.accept(TokenKind.min) catch {
                    _ = self.accept(TokenKind.mul) catch {
                        _ = try self.accept(TokenKind.rbrk);
                        break;
                    };
                    if (found_non_const_sym) return error.InvalidScalerWithMemOffset;
                    if (scale != null) return error.InvalidMemoryOperand;
                    if (registers_count == 0) return error.InvalidScalerOperand;

                    index_idx = if (registers_count == 2) 1 else 0;
                    expecting_scale = true;
                    continue;
                };

                // Only accept negative displacement
                if (displacement != null or expecting_scale == true) return error.InvalidSignedToken;
                expecting_negative_displacement = true;
            };
        }

        // Validate SIB
        if (registers_count == 0 and displacement == null and !found_non_const_sym) return error.InvalidMemoryOperand;

        if (registers_count == 2 and scale == null) {
            scale = 1;
        } else if (scale) |*s| {
            // Scale value should one of 1, 2, 4, 8
            if (s.* == 1 or s.* == 2 or s.* == 4 or s.* == 8) {
                s.* = blk: {
                    if (s.* == 1) break :blk 0 else if (s.* == 2) break :blk 1 else if (s.* == 4) break :blk 2 else break :blk 3;
                };
            } else {
                return error.InvalidScaleValue;
            }
        }

        // Mulitple registers should have the same size
        if (registers_count == 2 and (registers[0].size() != registers[1].size())) return error.VariableSizedRegisterOperands;

        var base: ?Register = null;
        var index: ?Register = null;
        if (registers_count > 0) {
            if (registers_count == 2) {
                base = registers[base_idx];
                index = registers[index_idx];
            } else if (scale != null) {
                index = registers[0];
            } else {
                base = registers[0];
            }

            switch (registers[0].size()) {
                8 => return ParserOperand{ .oper = Operand{ .mem = .{ .mem8 = .{ .base = base, .index = index, .scale = if (scale) |s| @intCast(u2, s) else null, .displacement = try Displacement.from(displacement) } } }, .is_frwd_ref = is_frwd_ref, .sym = sym, .unknwn_sym = unknwn_sym },
                16 => return ParserOperand{ .oper = Operand{ .mem = .{ .mem16 = .{ .base = base, .index = index, .scale = if (scale) |s| @intCast(u2, s) else null, .displacement = try Displacement.from(displacement) } } }, .is_frwd_ref = is_frwd_ref, .sym = sym, .unknwn_sym = unknwn_sym },
                32 => return ParserOperand{ .oper = Operand{ .mem = .{ .mem32 = .{ .base = base, .index = index, .scale = if (scale) |s| @intCast(u2, s) else null, .displacement = try Displacement.from(displacement) } } }, .is_frwd_ref = is_frwd_ref, .sym = sym, .unknwn_sym = unknwn_sym },
                64 => return ParserOperand{ .oper = Operand{ .mem = .{ .mem64 = .{ .base = base, .index = index, .scale = if (scale) |s| @intCast(u2, s) else null, .displacement = try Displacement.from(displacement) } } }, .is_frwd_ref = is_frwd_ref, .sym = sym, .unknwn_sym = unknwn_sym },
                else => return error.InvalidRegisterSize,
            }
        } else if (displacement) |disp| {
            const mofst: MOffset = if (size_ovrd) |sz_ovrd| try MOffset.from(disp).toMOfst(sz_ovrd.size()) else MOffset.from(disp);
            return ParserOperand{ .oper = Operand{ .mofst = mofst }, .is_frwd_ref = is_frwd_ref, .sym = sym, .unknwn_sym = unknwn_sym };
        } else {
            // If the symbol is a constant value, it will be handled as `displacement`
            // hence, we assume that the symbol is a non constant at this point
            if (sym) |symbol| {
                var mem_oper = Memory{ .mem = .{ .base = Register{ .rip = 64 }, .index = null, .scale = null, .displacement = Displacement{ .disp32 = 0 } } };
                if (size_ovrd) |sz_ovrd| mem_oper = try mem_oper.toMem(sz_ovrd.size());
                return ParserOperand{ .oper = Operand{ .mem = mem_oper }, .is_frwd_ref = is_frwd_ref, .sym = symbol };
            } else {
                return ParserOperand{ .oper = Operand{ .mofst = .{ .mofst = 0 } }, .is_frwd_ref = is_frwd_ref, .unknwn_sym = unknwn_sym };
            }
        }
    }

    const ParseOperandsError = error{
        TooManyOperands,
    } || ParseMemoryOperandError;
    inline fn parseOperands(self: *Self) ParseOperandsError!Operands {
        var opers = Operands{};
        while (try self.nextToken()) |tok| {
            var token = tok;
            // Is it a no operand instruction?
            if (token.type.isNl()) break;

            // First check for size override
            var size_ovrd: ?SizeKeyword = null;
            if (token.type == TokenKind.id) {
                const str_val = try token.val.strVal();
                if (SizeKeyword.isSizeKeyword(str_val)) |sz_kywd| {
                    size_ovrd = sz_kywd;
                    if (try self.nextToken()) |next_tok| {
                        token = next_tok;
                    } else {
                        return error.InvalidOperand;
                    }
                }
            }

            // Parse an operand here
            if (token.type == TokenKind.lbrk) {
                try opers.setOper(try self.parseMemoryOperand(size_ovrd));
            } else {
                try opers.setOper(try self.parseOperand(token, size_ovrd));
            }
            _ = self.accept(TokenKind.com) catch {
                if (try self.nextToken()) |tok_peek| {
                    self.peek_token = tok_peek;
                    _ = self.accept(TokenKind.nl) catch |err| {
                        if (err == error.MissingToken) {
                            break;
                        }
                        return err;
                    };
                }
                break;
            };
        }

        return opers;
    }

    inline fn resolveUnsizedOperandsIfAny(opers: *Operands, temp_opers: []const Operand) !void {
        for (opers.opers) |*oper, i| {
            if (oper.oper.size() == 0) {
                // Found unsized variant
                switch (oper.oper) {
                    .imm => |imm| oper.*.oper = Operand{ .imm = try imm.toImm(temp_opers[i].size()) },
                    .mem => |mem| oper.*.oper = Operand{ .mem = try mem.toMem(temp_opers[i].size()) },
                    .mofst => |mofst| oper.*.oper = Operand{ .mofst = try mofst.toMOfst(temp_opers[i].size()) },
                    .rel => |rel| oper.*.oper = Operand{ .rel = try rel.toRel(temp_opers[i].size()) },
                    else => {},
                }
            }
        }
    }

    const ParseInstrInfo = struct {
        instr_bytes: std.ArrayList(u8),
        reloc_sym: ?*Symbol,
        reloc_size: ?u4,
        reloc_type: ?RelocType,
    };
    const ParseInstrError = error{
        MissingRexPrefix,
        MissingPrefixes,
        InvalidMemoryReference,
        OperandNotAMemory,
        InvalidMemorySize,
        OperandNotARegister,
        NoMatchingInstructionFound,
        InvalidTemplateTypeEncoding,
        MissingRexByte,
        MissingModRmByte,
        MustBeResolvedToASizedVariant,
        FoundMultipleVariantOfTheInstruction,
        NeedsExplicitSizeDerivative,
        PrefixNotAllowed,
        InvalidRegisterEncoding,
    } || ParseOperandsError;
    fn parseInstr(self: *Self, id: []const u8) ParseInstrError!?ParseInstrInfo {
        var prefix: ?Prefix = null;
        var mnem: []const u8 = id;
        if (PrefixKeyword.isPrefix(id)) |pfx| {
            prefix = pfx;
            const id_tok = try self.accept(.id);
            mnem = try id_tok.val.strVal();
        }

        var operands: Operands = try self.parseOperands();

        if (operands.hasUnknownSym()) |unknwn_sym| {
            try unknwn_sym.addLine(self.line, try LineInfo.init(self.allocator, LineInfo.total_count, self.line_tokens.items, self.curr_sec.size(), null, self.curr_sym, self.times));
            return null;
        } else {
            var opers: []Operand = operands.toOpers();
            std.log.info("mnem: {s}", .{mnem});
            for (opers) |oper| std.log.info("{s}{d}", .{ oper.typeStr(), oper.size() });
            if (try instr.findInstr(mnem, opers, self.op_mode)) |instruction| {
                std.log.info("found instr: {s}", .{mnem});
                for (instruction.opcode()) |op_co| std.log.info("0x{x}", .{op_co});
                try resolveUnsizedOperandsIfAny(&operands, instruction.operands());
                opers = operands.toOpers();

                var reloc_sym: ?*Symbol = null;
                var reloc_size: ?u4 = null;
                var reloc_type: ?RelocType = null;
                if (operands.relocSym()) |oper| {
                    reloc_sym = oper.sym;
                    if (operands.relocOperInfo()) |reloc_info| {
                        reloc_size = reloc_info[0];
                        reloc_type = reloc_info[1];
                    }
                }

                const instr_bytes = try instruction.encode(self.allocator, opers, self.op_mode, prefix);
                return ParseInstrInfo{ .instr_bytes = instr_bytes, .reloc_sym = reloc_sym, .reloc_size = reloc_size, .reloc_type = reloc_type };
            } else {
                return error.NoMatchingInstructionFound;
            }
        }
    }

    fn parseId(self: *Self, token: Token) !void {
        const str_val = try token.val.strVal();

        // A Keyword, i.e. db, dw, dd, dq, resb, resw, resd, resq, equ?
        if (Keyword.isKeyword(str_val)) |kywd| {
            return self.parseKeyword(kywd);
        }

        // A Directive, i.e. global, include, extern, bits, section?
        if (Directive.isDirective(str_val)) |drct| {
            return self.parseDirective(drct);
        }

        // A symbol?
        if (try self.nextToken()) |tok| {
            if (tok.type.isColon()) {
                return self.parseLabel(str_val);
            } else {
                self.peek_token = tok;
            }
        }

        if (self.curr_sec.type != .Text) return error.InstructionInNonExecutableSection;

        // An instruction?
        const instr_info = try self.parseInstr(str_val);
        if (instr_info) |info| {
            defer info.instr_bytes.deinit();
            if (self.times > 1) {
                try self.curr_sec.appendSliceNTimes(self.allocator, info.instr_bytes.items, self.times);
                self.times = 1;
            } else {
                try self.curr_sec.appendSlice(self.allocator, info.instr_bytes.items);
            }

            // Add relocation
            if (info.reloc_sym) |reloc| {
                _ = try Reltab.add(self.allocator, self.curr_sec.size() - info.reloc_size.?, reloc.ofst_in_sec, reloc.section, info.reloc_size.?, info.reloc_type.?);
            }
        } else if (self.times > 1) self.times = 1;
    }

    pub fn parse(self: *Self) !void {
        while (try self.nextToken()) |token| {
            self.line = token.line;
            switch (token.type) {
                .id => self.parseId(token) catch |err| {
                    self.lexer.skipLine();
                    std.log.err("Could not parse line {d}: {s} \n\tdue to: {}", .{ self.line, self.lexer.lines.items[self.line - 1], err });
                    self.errors += 1;
                    if (self.errors >= 20) return error.TooManyErrors;
                },
                .dot => self.parseLocalLabel() catch |err| {
                    self.lexer.skipLine();
                    std.log.err("Could not parse local label on line {d}: {s} \n\tdue to: {}", .{ self.line, self.lexer.lines.items[self.line - 1], err });
                    self.errors += 1;
                    if (self.errors >= 20) return error.TooManyErrors;
                },
                .nl => {
                    if (self.times > 1) self.times = 1;
                },
                else => return error.UnrecognisedToken,
            }
        }

        Reltab.description();
        Symtab.description();
        if (Symtab.hasUnresolvedSym()) return error.ContainsUnknownSymbols;
    }

    pub fn deinit(self: *Self) void {
        // Sections
        for (self.sections.items) |*sec| sec.deinit();
        self.sections.deinit();

        // Symbols
        Symtab.deinit();

        // Allocated local label names
        if (self.allocd_names) |*allocd_names| {
            for (allocd_names.items) |name| {
                self.allocator.free(name);
            }
            allocd_names.deinit();
        }

        // Line tokens
        self.line_tokens.deinit();

        // Relocations
        Reltab.deinit();
    }
};
