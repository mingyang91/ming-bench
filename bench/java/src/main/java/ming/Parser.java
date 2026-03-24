package ming;

import java.util.ArrayList;
import java.util.List;

final class Parser {
    private final String input;
    private int index;
    private int line = 1;
    private int column = 1;

    Parser(String input) {
        this.input = input == null ? "" : input;
    }

    List<Expr> parseProgram() throws EvalError {
        List<Expr> expressions = new ArrayList<>();
        skipIgnored();
        while (!isAtEnd()) {
            expressions.add(parseExpr());
            skipIgnored();
        }
        if (expressions.isEmpty()) {
            throw new EvalError("empty program");
        }
        return List.copyOf(expressions);
    }

    private Expr parseExpr() throws EvalError {
        skipIgnored();
        SourcePos pos = currentPos();
        if (isAtEnd()) {
            throw EvalError.syntax(pos, "unexpected end of input");
        }

        return switch (peek()) {
            case '(' -> parseList();
            case '"' -> parseString();
            case '#' -> parseBoolean();
            default -> parseAtom();
        };
    }

    private Expr parseList() throws EvalError {
        SourcePos pos = currentPos();
        advance();

        List<Expr> elements = new ArrayList<>();
        skipIgnored();
        while (!isAtEnd() && peek() != ')') {
            elements.add(parseExpr());
            skipIgnored();
        }

        if (isAtEnd()) {
            throw EvalError.syntax(pos, "unterminated list");
        }

        advance();
        return new Expr.ListExpr(List.copyOf(elements), pos);
    }

    private Expr parseString() throws EvalError {
        SourcePos pos = currentPos();
        advance();

        StringBuilder builder = new StringBuilder();
        while (!isAtEnd()) {
            char ch = advance();
            if (ch == '"') {
                return new Expr.StringExpr(builder.toString(), pos);
            }
            if (ch == '\\') {
                if (isAtEnd()) {
                    throw EvalError.syntax(pos, "unterminated string");
                }
                char escaped = advance();
                switch (escaped) {
                    case '"', '\\' -> builder.append(escaped);
                    case 'n' -> builder.append('\n');
                    case 'r' -> builder.append('\r');
                    case 't' -> builder.append('\t');
                    default -> throw EvalError.syntax(currentPos(), "unsupported escape sequence \\" + escaped);
                }
            } else {
                builder.append(ch);
            }
        }

        throw EvalError.syntax(pos, "unterminated string");
    }

    private Expr parseBoolean() throws EvalError {
        SourcePos pos = currentPos();
        if (matchesLiteral("#t")) {
            return new Expr.BooleanExpr(true, pos);
        }
        if (matchesLiteral("#f")) {
            return new Expr.BooleanExpr(false, pos);
        }
        throw EvalError.syntax(pos, "invalid boolean literal");
    }

    private Expr parseAtom() throws EvalError {
        SourcePos pos = currentPos();
        StringBuilder builder = new StringBuilder();
        while (!isAtEnd() && !isDelimiter(peek())) {
            builder.append(advance());
        }

        String token = builder.toString();
        if (token.isEmpty()) {
            throw EvalError.syntax(pos, "unexpected token");
        }

        if (isIntegerToken(token)) {
            try {
                return new Expr.IntegerExpr(Long.parseLong(token), pos);
            } catch (NumberFormatException e) {
                throw EvalError.syntax(pos, "invalid integer literal: " + token);
            }
        }

        return new Expr.SymbolExpr(token, pos);
    }

    private boolean matchesLiteral(String literal) {
        if (!input.startsWith(literal, index)) {
            return false;
        }
        int end = index + literal.length();
        if (end < input.length() && !isDelimiter(input.charAt(end))) {
            return false;
        }
        for (int i = 0; i < literal.length(); i++) {
            advance();
        }
        return true;
    }

    private boolean isIntegerToken(String token) {
        if (token.isEmpty()) {
            return false;
        }
        int start = 0;
        char first = token.charAt(0);
        if (first == '+' || first == '-') {
            if (token.length() == 1) {
                return false;
            }
            start = 1;
        }
        for (int i = start; i < token.length(); i++) {
            if (!Character.isDigit(token.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    private void skipIgnored() {
        while (!isAtEnd()) {
            char ch = peek();
            if (Character.isWhitespace(ch)) {
                advance();
                continue;
            }
            if (ch == ';') {
                while (!isAtEnd() && peek() != '\n') {
                    advance();
                }
                continue;
            }
            break;
        }
    }

    private boolean isDelimiter(char ch) {
        return Character.isWhitespace(ch) || ch == '(' || ch == ')' || ch == ';';
    }

    private boolean isAtEnd() {
        return index >= input.length();
    }

    private char peek() {
        return input.charAt(index);
    }

    private char advance() {
        char ch = input.charAt(index++);
        if (ch == '\n') {
            line++;
            column = 1;
        } else {
            column++;
        }
        return ch;
    }

    private SourcePos currentPos() {
        return new SourcePos(line, column);
    }
}
