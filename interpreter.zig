const std = @import("std");

const TokenType = enum {
    PLUS,
    MINUS,
    DIV,
    MUL,

    LPAREN,
    RPAREN,

    NUMBER,

    ILLEGAL,
    EOF,
};

const Token = struct {
    type: TokenType,
    literal: []const u8,
};

const LanguageError = error {
    SyntaxError,
    IllegalCharacterError,
    ZeroDivisionError
};

fn isWhitespace(c: u8) bool {
    return c == ' ' or c == '\n' or c == '\r' or c == '\t';
}

fn isOperator(c: u8) bool {
    return c == '-' or c == '+' or c == '*' or c == '/';
}

fn isNumber(c: u8) bool {
    return c >= '0' and c <= '9';
}

const Lexer = struct {
    input: []const u8,
    current_position: usize,
    next_position: usize,
    current_char: u8,
    
    const Self = @This();

    pub fn init(input: []const u8) Self {
        var lexer: Lexer = .{
            .input =  input,
            .current_position = 0,
            .next_position = 0,
            .current_char = 0,
        };
        lexer.readChar();
        return lexer;
    }

    pub fn nextToken(self: *Lexer) LanguageError!Token {
        self.skipWhitespace();
        return switch (self.current_char) {
            '0' ... '9' => {
                const parsedToken = self.peakNumber();
                return Token {.type = TokenType.NUMBER, .literal = parsedToken  };
            },
            '-', '+', '*', '/', '(', ')' => {
                var tokenType: TokenType = undefined; var literal: []const u8 = undefined;
                if (self.current_char == '-') {
                    tokenType = TokenType.MINUS; literal = "-";
                } else if (self.current_char == '+') {
                    tokenType = TokenType.PLUS; literal = "+";
                } else if (self.current_char == '/') {
                    tokenType = TokenType.DIV; literal = "/";
                } else if (self.current_char == '*')  {
                    tokenType = TokenType.MUL; literal = "*";
                } else if(self.current_char == '(') {
                    tokenType = TokenType.LPAREN; literal = "(";
                } else if (self.current_char == ')') {
                    tokenType = TokenType.RPAREN; literal = ")";
                }

                const token = Token {.type= tokenType, .literal = literal  };
                self.readChar();
                return token;
            },
            else => {
                if (self.current_char == 0) {
                    return Token {.type = TokenType.EOF, .literal = "End Of Line" };
                }
                return LanguageError.IllegalCharacterError;
            }
        };
    }

    fn peakNumber(self: *Lexer) []const u8 {
        const start = self.current_position;
        while (isNumber(self.current_char)) {
            self.readChar();
        }
        return self.input[start..self.current_position];
    }

    fn skipWhitespace(self: *Lexer) void {
        while (isWhitespace(self.current_char)) {
            self.readChar();
        }
    }

    fn readChar(self: * Lexer) void {
        if (self.next_position >= self.input.len) {
            self.current_char = 0;
        } else  {
            self.current_char = self.input[self.next_position];
        }

        self.current_position = self.next_position;
        self.next_position += 1;
    }
};

const Node = struct {
    token: Token,
    left_node: ?*Node,
    right_node: ?*Node,
    
    const Self = @This();

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        if (self.*.left_node) |ln| ln.deinit(allocator);
        if (self.*.right_node) |rn| rn.deinit(allocator);
        allocator.destroy(self);
    }
};

fn isOperatorToken(token: Token) bool {
    return switch(token.type) {
        .PLUS, .MINUS, .MUL, .DIV => true,
        else => false,
    };
}

fn tokenChecker(current_token: Token, next_token: Token, prev_token: ?Token) LanguageError!void {
    switch(current_token.type) {
        .NUMBER => return numberTokenChecker(next_token, prev_token),
        .PLUS, .MINUS, .MUL, .DIV => return operatorTokenChecker(next_token, prev_token),
        .LPAREN => return leftParenthesisTokenChecker(next_token, prev_token),
        .RPAREN => return rightParenthesisTokenChecker(next_token, prev_token),
        else => return,

    }
}

fn numberTokenChecker(next_token: Token, prev_token: ?Token) LanguageError!void {
    if (prev_token) |ptoken| {
        if (ptoken.type == TokenType.RPAREN and next_token.type == TokenType.LPAREN) {
            return LanguageError.SyntaxError;
        }
        if (ptoken.type == TokenType.LPAREN and next_token.type == TokenType.EOF) {
            return LanguageError.SyntaxError;
        }
        if (ptoken.type == TokenType.RPAREN and next_token.type == TokenType.EOF) {
            return LanguageError.SyntaxError;
        }
    } else {
        if (next_token.type == TokenType.RPAREN or next_token.type == TokenType.LPAREN) {
            return LanguageError.SyntaxError;
        }
    }
}

fn operatorTokenChecker(next_token: Token, prev_token: ?Token) LanguageError!void {
    if (prev_token) |ptoken| {
        if (next_token.type == TokenType.EOF) {
            return LanguageError.SyntaxError;
        }
        if (ptoken.type == TokenType.LPAREN) {
            return LanguageError.SyntaxError;
        }
        if (isOperatorToken(next_token)) {
            return LanguageError.SyntaxError;
        }
        if (next_token.type == TokenType.RPAREN) {
            return LanguageError.SyntaxError;
        }
    } else {
        return LanguageError.SyntaxError;
    }
}

fn leftParenthesisTokenChecker(next_token: Token, prev_token: ?Token) LanguageError!void {
    if (prev_token) |ptoken| {
        if (ptoken.type == TokenType.RPAREN or next_token.type == TokenType.RPAREN) {
            return LanguageError.SyntaxError;
        }
        if(next_token.type == TokenType.EOF) {
            return LanguageError.SyntaxError;
        }
        if (ptoken.type == TokenType.NUMBER) {
            return LanguageError.SyntaxError;
        }
        if (isOperatorToken(next_token)) {
            return LanguageError.SyntaxError;
        }
    } else {
        if (isOperatorToken(next_token)) {
            return LanguageError.SyntaxError;
        }
        if (next_token.type == TokenType.RPAREN) {
            return LanguageError.SyntaxError;
        }
    }
}

fn rightParenthesisTokenChecker(next_token: Token, prev_token: ?Token) LanguageError!void {
    if (prev_token) |ptoken| {
        if (ptoken.type == TokenType.LPAREN or next_token.type == TokenType.LPAREN) {
            return LanguageError.SyntaxError;
        }
        if (next_token.type == TokenType.NUMBER) {
            return LanguageError.SyntaxError;
        }
        if (isOperatorToken(ptoken)) {
            return LanguageError.SyntaxError;
        }
    } else {
        return LanguageError.SyntaxError;
    }
}

const Parser = struct {
    allocator: std.mem.Allocator,
    lexer: Lexer,
    operandsStack: std.ArrayList(*Node) = .empty,
    operatorsStack: std.ArrayList(Token) = .empty,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, input: []const u8) Self {
        return .{
            .allocator = allocator,
            .lexer = Lexer.init(input),
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.operandsStack.items) |operand| {
            operand.deinit(self.allocator);
        }

        self.operandsStack.deinit(self.allocator);
        self.operatorsStack.deinit(self.allocator);
    }

    pub fn parse(self: *Self) !*Node {
        var current_token = try self.lexer.nextToken();
        var next_token = try self.lexer.nextToken();
        var prev_token: ?Token = null;

        while (current_token.type != TokenType.EOF) {
            try tokenChecker(current_token, next_token, prev_token);
            switch (current_token.type) {
                .NUMBER => {
                    const node = try self.makeNode(current_token, null, null);
                    try self.operandsStack.append(self.allocator, node);
                },
                .LPAREN => {
                    try self.operatorsStack.append(self.allocator, current_token);
                },
                .RPAREN => {
                    if (self.operatorsLen() == 0) {
                        return LanguageError.SyntaxError;
                    }
                    var index: usize = self.operatorsLen() - 1;
                    while (self.operatorsStack.items[index].type != TokenType.LPAREN and index > 0) {
                        index -= 1;
                    }

                    if (index == 0 and self.operatorsStack.items[index].type != TokenType.LPAREN) {
                        return LanguageError.SyntaxError;
                    }

                    _ = self.operatorsStack.orderedRemove(index);
                    try self.appendNode();
                },
                .DIV, .MUL, .PLUS, .MINUS => {
                    while (self.operatorsLen() > 0 and getTokenWeight(self.operatorsStack.getLast()) >= getTokenWeight(current_token)) {
                        try self.appendNode();
                    }

                    try self.operatorsStack.append(self.allocator, current_token);
                },
                else => {},
            }

            prev_token = current_token;
            current_token = next_token;
            next_token = try self.lexer.nextToken();
        }

        while (self.operatorsLen() > 0) {
            try self.appendNode();
        }

        return self.operandsStack.pop().?;
    }

    fn appendNode(self: *Self) !void {
        const op = self.operatorsStack.pop();
        if (op.?.type == TokenType.LPAREN) {
            return LanguageError.SyntaxError;
        }

        const right_node = self.operandsStack.pop();
        const left_node = self.operandsStack.pop();

        const newNode = try self.makeNode(op.?, left_node, right_node);
        try self.operandsStack.append(self.allocator, newNode);
    }

    fn operatorsLen(self: *Self) usize {
        return self.operatorsStack.items.len;
    }

    fn makeNode(self: *Self, token: Token, left_node: ?*Node, right_node: ?*Node) !*Node {
        const node = try self.allocator.create(Node);
        node.* = .{
            .token = token,
            .left_node = left_node,
            .right_node = right_node,
        };
        return node;
    }

    fn getTokenWeight(token: Token) u8 {
        switch (token.type) {
            .DIV, .MUL => return 2,
            .MINUS, .PLUS => return 1,
            else => return 0,
        }
    }
};

fn parseToI32(s: []const u8) !i32 {
    return std.fmt.parseInt(i32, s, 10);
}

fn recursiveEval(allocator: std.mem.Allocator, ast:  * Node) !i32  {
    if (ast.*.left_node == null and ast.*.right_node == null and ast.*.token.type == TokenType.NUMBER) {
        return parseToI32(ast.token.literal);
    }

    const ln = try recursiveEval(allocator, ast.*.left_node.?);
    const rn = try recursiveEval(allocator, ast.*.right_node.?);
    const current_token_type = ast.*.token.type;

    return switch (current_token_type) {
        .MUL => return ln * rn,
        .PLUS => return ln + rn,
        .MINUS => return ln - rn,
        else => {
            if (rn == 0) return LanguageError.ZeroDivisionError;
            return @divFloor(ln, rn);
        },
    };
}

fn eval(allocator: std.mem.Allocator, i: []const u8) !i32 {
    var parser = Parser.init(allocator, i);
    defer parser.deinit();
    const ast = try parser.parse();
    defer ast.deinit(allocator);
    return try recursiveEval(allocator, ast);
}


const _allocator = std.heap.wasm_allocator;

export fn js_eval(ptr: [*]const u8, len: usize) i32 {
    const input = ptr[0..len];
    return eval(_allocator, input) catch 0;
}
