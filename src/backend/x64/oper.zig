const TokenKind = @import("lexer").TokenKind;
pub const Oper = enum {
    Div,
    Mul,
    Mod,
    Add,
    Sub,
    const Self = @This();

    pub const Assoc = enum {
        Left,
        Right,
    };

    pub fn from(token_type: TokenKind) !Self {
        switch (token_type) {
            .div => return .Div,
            .mul => return .Mul,
            .mod => return .Mod,
            .pls => return .Add,
            .min => return .Sub,
            else => return error.TokenNotAnOperator,
        }
    }

    pub inline fn precedence(self: Self) u8 {
        switch (self) {
            .Div => return 1 << 7,
            .Mul => return 1 << 7,
            .Mod => return 1 << 7,
            .Add => return 1 << 6,
            .Sub => return 1 << 6,
        }
    }

    pub inline fn associativity(self: Self) Assoc {
        _ = self;
        return .Left;
    }
};
