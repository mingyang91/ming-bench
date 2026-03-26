package ming;

import java.util.ArrayList;
import java.util.List;

final class Parser {
    private final String input;
    private int index;
    private int line = 1;
    private int column = 1;

    Parser(String input) {
        this.input = input;
    }

    List<Expr> parseProgram() throws EvalError {
        List<Expr> expressions = new ArrayList<>();
        skipTrivia();
        while (!isAtEnd()) {
            expressions.add(parseExpression());
            skipTrivia();
        }
        return expressions;
    }

    private Expr parseExpression() throws EvalError {
        skipTrivia();
        if (isAtEnd()) {
            throw error("unexpected end of input");
        }

        int startLine = line;
        int startColumn = column;
        char current = currentChar();
        if (current == '(') {
            return parseList(startLine, startColumn);
        }
        if (current == '\'') {
            advance();
            Expr quoted = parseExpression();
            return new ListExpr(
                    List.of(new SymbolExpr("quote", startLine, startColumn), quoted),
                    startLine,
                    startColumn);
        }
        if (current == '"') {
            return parseString(startLine, startColumn);
        }
        if (current == ')') {
            throw error("unexpected ')'");
        }
        return parseAtom(startLine, startColumn);
    }

    private Expr parseList(int startLine, int startColumn) throws EvalError {
        consume('(');
        List<Expr> elements = new ArrayList<>();
        skipTrivia();
        while (!isAtEnd() && currentChar() != ')') {
            elements.add(parseExpression());
            skipTrivia();
        }

        if (isAtEnd()) {
            throw error("unterminated list");
        }

        consume(')');
        return new ListExpr(List.copyOf(elements), startLine, startColumn);
    }

    private Expr parseString(int startLine, int startColumn) throws EvalError {
        consume('"');
        StringBuilder builder = new StringBuilder();
        while (!isAtEnd()) {
            char current = advance();
            if (current == '"') {
                return new StringExpr(builder.toString(), startLine, startColumn);
            }
            if (current == '\\') {
                if (isAtEnd()) {
                    throw error("unterminated string escape");
                }
                builder.append(unescape(advance()));
                continue;
            }
            builder.append(current);
        }
        throw error("unterminated string literal");
    }

    private char unescape(char escaped) {
        return switch (escaped) {
            case 'n' -> '\n';
            case 'r' -> '\r';
            case 't' -> '\t';
            case '"' -> '"';
            case '\\' -> '\\';
            default -> escaped;
        };
    }

    private Expr parseAtom(int startLine, int startColumn) throws EvalError {
        int start = index;
        while (!isAtEnd() && !isDelimiter(currentChar())) {
            advance();
        }
        String token = input.substring(start, index);
        return parseAtomToken(token, startLine, startColumn);
    }

    private Expr parseAtomToken(String token, int startLine, int startColumn)
            throws EvalError {
        return switch (token) {
            case "#t" -> new BoolExpr(true, startLine, startColumn);
            case "#f" -> new BoolExpr(false, startLine, startColumn);
            default -> parseCharNumberOrSymbol(token, startLine, startColumn);
        };
    }

    private Expr parseCharNumberOrSymbol(String token, int startLine, int startColumn)
            throws EvalError {
        if (token.startsWith("#\\")) {
            return parseCharToken(token, startLine, startColumn);
        }
        return parseNumberOrSymbol(token, startLine, startColumn);
    }

    private Expr parseCharToken(String token, int startLine, int startColumn)
            throws EvalError {
        String value = token.substring(2);
        return switch (value) {
            case "space" -> new CharExpr(' ', startLine, startColumn);
            case "newline" -> new CharExpr('\n', startLine, startColumn);
            default -> {
                if (value.length() == 1) {
                    yield new CharExpr(value.charAt(0), startLine, startColumn);
                }
                throw new EvalError("invalid character literal", startLine, startColumn);
            }
        };
    }

    private Expr parseNumberOrSymbol(String token, int startLine, int startColumn) {
        if (isIntegerToken(token)) {
            return new IntExpr(Long.parseLong(token), startLine, startColumn);
        }
        return new SymbolExpr(token, startLine, startColumn);
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

    private void skipTrivia() {
        while (!isAtEnd()) {
            char current = currentChar();
            if (Character.isWhitespace(current)) {
                advance();
                continue;
            }
            if (current == ';') {
                skipComment();
                continue;
            }
            return;
        }
    }

    private void skipComment() {
        while (!isAtEnd() && currentChar() != '\n') {
            advance();
        }
    }

    private boolean isDelimiter(char ch) {
        return Character.isWhitespace(ch) || ch == '(' || ch == ')' || ch == ';'
                || ch == '\'';
    }

    private void consume(char expected) throws EvalError {
        if (isAtEnd() || currentChar() != expected) {
            throw error("expected '" + expected + "'");
        }
        advance();
    }

    private char currentChar() {
        return input.charAt(index);
    }

    private boolean isAtEnd() {
        return index >= input.length();
    }

    private char advance() {
        char current = input.charAt(index);
        index++;
        if (current == '\n') {
            line++;
            column = 1;
        } else {
            column++;
        }
        return current;
    }

    private EvalError error(String message) {
        return new EvalError(message, line, column);
    }
}
