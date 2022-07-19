const std = @import("std");
const Imm = @import("common").imm.Imm;
const Symbol = @import("common").symbol.Symbol;
const Oper = @import("oper.zig").Oper;

pub const NodeOper = struct {
    oper: Oper,
    left: *Node,
    right: *Node,
};

pub const Node = union(enum) {
    imm: Imm,
    sym: Symbol,
    oper: NodeOper,
    const Self = @This();

    pub inline fn isOper(self: Self) bool {
        switch (self) {
            .oper => return true,
            else => return false,
        }
    }

    pub inline fn operNode(self: *Self) !NodeOper {
        switch (self.*) {
            .oper => |oper| return oper,
            else => return error.NodeNotAnOper,
        }
    }

    pub inline fn isImm(self: Self) bool {
        switch (self) {
            .imm => |imm| return imm,
            else => return null,
        }
    }

    pub inline fn getImm(self: Self) ?Imm {
        switch (self) {
            .imm => |imm| return imm,
            else => return null,
        }
    }
};

pub const Ast = struct {
    root: ?*Node,
    allocator: std.mem.Allocator,
    var nodes: ?std.ArrayList(Node) = null;
    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return Self{ .root = null, .allocator = allocator };
    }

    pub fn genImm(self: *Self, imm: Imm) !*Node {
        if (nodes == null) {
            nodes = std.ArrayList(Node).init(self.allocator);
        }

        const node = Node{ .imm = imm };
        try nodes.?.append(node);
        return &nodes.?.items[nodes.?.items.len - 1];
    }

    pub fn genSym(self: *Self, sym: Symbol) !*Node {
        if (nodes == null) {
            nodes = std.ArrayList(Node).init(self.allocator);
        }

        const node = Node{ .sym = sym };
        try nodes.?.append(node);
        return &nodes.?.items[nodes.?.items.len - 1];
    }

    pub fn genBinary(self: *Self, oper: Oper, node1: *Node, node2: *Node) !*Node {
        if (nodes == null) {
            nodes = std.ArrayList(Node).init(self.allocator);
        }
        const oper_node = Node{ .oper = .{ .oper = oper, .left = node1, .right = node2 } };
        try nodes.?.append(oper_node);

        return &nodes.?.items[nodes.?.items.len - 1];
    }

    fn resolveOper(node: *Node) error{ NodeNotAnOper, CouldNotResolveTreeToConst, UnexpectedRemainder, DivisionByZero, Overflow, InvalidImmediateSize }!void {
        const oper_node = try node.operNode();
        if (oper_node.left.isOper()) try resolveOper(oper_node.left);
        const left = if (oper_node.left.getImm()) |imm| imm else return error.CouldNotResolveTreeToConst;
        if (oper_node.right.isOper()) try resolveOper(oper_node.right);
        const right = if (oper_node.right.getImm()) |imm| imm else return error.CouldNotResolveTreeToConst;

        const size: u8 = @maximum(left.size(), right.size());
        const left64: u64 = left.toImm64();
        const right64: u64 = right.toImm64();

        const result: u64 = blk: {
            switch (oper_node.oper) {
                .Div => break :blk try std.math.divExact(u64, right64, left64),
                .Mul => break :blk try std.math.mul(u64, right64, left64),
                .Mod => break :blk try std.math.mod(u64, right64, left64),
                .Add => break :blk try std.math.add(u64, right64, left64),
                .Sub => break :blk right64 -% left64,
            }
        };

        const imm_val: Imm = blk: {
            switch (size) {
                8 => break :blk Imm{ .imm8 = @intCast(u8, result & 0x00000000000000ff) },
                16 => break :blk Imm{ .imm16 = @intCast(u16, result & 0x000000000000ffff) },
                32 => break :blk Imm{ .imm32 = @intCast(u32, result & 0x00000000ffffffff) },
                64 => break :blk Imm{ .imm64 = result },
                else => return error.InvalidImmediateSize,
            }
        };
        node.* = Node{ .imm = imm_val };
    }

    pub fn resolveConst(self: *Self) !Imm {
        if (self.root) |root_node| {
            switch (root_node.*) {
                .imm => |imm| return imm,
                .sym => |sym| return Imm{ .imm32 = sym.ofst_in_sec },
                .oper => {
                    try resolveOper(root_node);
                    if (root_node.getImm()) |imm| return imm else return error.CouldNotResolveTreeToConst;
                },
            }
        } else {
            return error.MissingRootNode;
        }
    }

    pub fn deinit() void {
        if (nodes) |nds|
            nds.deinit();
    }
};
