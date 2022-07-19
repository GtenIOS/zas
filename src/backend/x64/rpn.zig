const std = @import("std");
const Imm = @import("common").imm.Imm;
const Symbol = @import("common").symbol.Symbol;
const Oper = @import("oper.zig").Oper;
const ast = @import("ast.zig");
const Ast = ast.Ast;
const AstNode = ast.Node;
const Opnd = union(enum) {
    imm: Imm,
    sym: Symbol,
    const Self = @This();

    pub inline fn getAstNode(self: Self, tree: *Ast) !*AstNode {
        switch (self) {
            .imm => |imm| return tree.genImm(imm),
            .sym => |sym| return tree.genSym(sym),
        }
    }
};

const Node = union(enum) {
    oper: Oper,
    opnd: Opnd,
    const Self = @This();
    pub inline fn isOper(self: Self) ?Oper {
        switch (self) {
            .oper => |oper| return oper,
            else => return null,
        }
    }

    pub inline fn getOperand(self: Self) !Opnd {
        switch (self) {
            .opnd => |opnd| return opnd,
            else => return error.NodeNotAnOperand,
        }
    }
};

pub const Rpn = struct {
    outq: std.ArrayList(Node),
    oprq: std.ArrayList(Oper),
    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        const outq = std.ArrayList(Node).init(allocator);
        const oprq = std.ArrayList(Oper).init(allocator);
        return Self{ .outq = outq, .oprq = oprq };
    }

    pub inline fn handleImm(self: *Self, imm: Imm) !void {
        try self.outq.append(.{ .opnd = .{ .imm = imm } });
    }

    pub inline fn handleSym(self: *Self, sym: Symbol) !void {
        try self.outq.append(.{ .opnd = .{ .sym = sym } });
    }

    pub inline fn handleOper(self: *Self, oper: Oper) !void {
        var size = self.oprq.items.len;
        if (size == 0) {
            try self.oprq.append(oper);
            return;
        } else {
            while (size > 0) : (size -= 1) {
                const stack_oper = self.oprq.items[size - 1];
                if (oper.precedence() < stack_oper.precedence() or (oper.precedence() == stack_oper.precedence() and oper.associativity() == .Right)) {
                    try self.outq.append(Node{ .oper = stack_oper });
                } else {
                    try self.oprq.append(oper);
                    break;
                }
            }
        }
    }

    pub inline fn popAllOperators(self: *Self) !void {
        var size = self.oprq.items.len;
        while (size > 0) : (size -= 1) {
            try self.outq.append(Node{ .oper = self.oprq.pop() });
        }
    }

    pub inline fn genOperNode(self: *Self, tree: *Ast, oper: Oper) error{ InvalidExpression, NodeNotAnOperand, OutOfMemory }!*AstNode {
        var left: *AstNode = undefined;
        if (self.outq.popOrNull()) |node| {
            if (node.isOper()) |op| {
                left = try self.genOperNode(tree, op);
            } else {
                const opnd = try node.getOperand();
                left = try opnd.getAstNode(tree);
            }
        } else return error.InvalidExpression;

        var right: *AstNode = undefined;
        if (self.outq.popOrNull()) |node| {
            if (node.isOper()) |op| {
                right = try self.genOperNode(tree, op);
            } else {
                const opnd = try node.getOperand();
                right = try opnd.getAstNode(tree);
            }
        } else return error.InvalidExpression;
        return tree.genBinary(oper, left, right);
    }

    pub inline fn genTree(self: *Self, allocator: std.mem.Allocator) !Ast {
        try self.popAllOperators();

        var tree = Ast.init(allocator);
        errdefer Ast.deinit();
        if (self.outq.popOrNull()) |node| {
            if (node.isOper()) |oper| {
                const ast_node = try self.genOperNode(&tree, oper);
                tree.root = ast_node;
                return tree;
            }

            const opnd = try node.getOperand();
            switch (opnd) {
                .imm => |imm| tree.root = try tree.genImm(imm),
                .sym => |sym| tree.root = try tree.genSym(sym),
            }
        }

        return tree;
    }

    pub fn deinit(self: *Self) void {
        self.outq.deinit();
        self.oprq.deinit();
    }
};
