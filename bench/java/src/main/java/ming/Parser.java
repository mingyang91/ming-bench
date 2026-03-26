package ming;

import java.util.ArrayList;
import java.util.List;

final class Parser {
    private final String input;
    private int index;
    private int line;
    private int column;

    Parser(String input) {
        this.input = input;
        this.index = 0;
        this.line = 1;
        this.column = 1;
    }

    List<Expr> parseProgram() throws EvalError {
        List<Expr> expressions = new ArrayList<>();
        skipIgnored();
        while (!isAtEnd()) {
            expressions.add(parseExpr());
            skipIgnored();
        }
        return List.copyOf(expressions);
    }

    private Expr parseExpr() throws EvalError {
        skipIgnored();
        if (isAtEnd()) {
            throw error("unexpected end of input");
        }

        char ch = peek();
        return switch (ch) {
            case '(' -> parseList();
            case '"' -> parseString();
            case '\'' -> parseQuote();
            case ')' -> throw error("unexpected ')'");
            default -> parseAtom();
        };
    }

    private Expr parseQuote() throws EvalError {
        int startLine = line;
        int startColumn = column;
        advance();
        return new ListExpr(
                List.of(new SymbolExpr("quote", startLine, startColumn), parseExpr()),
                startLine,
                startColumn);
    }

    private Expr parseList() throws EvalError {
        int startLine = line;
        int startColumn = column;
        advance();
        List<Expr> elements = new ArrayList<>();
        skipIgnored();

        while (!isAtEnd() && peek() != ')') {
            elements.add(parseExpr());
            skipIgnored();
        }

        if (isAtEnd()) {
            throw error("unterminated list");
        }

        advance();
        return new ListExpr(List.copyOf(elements), startLine, startColumn);
    }

    private Expr parseString() throws EvalError {
        int startLine = line;
        int startColumn = column;
        advance();
        StringBuilder builder = new StringBuilder();

        while (!isAtEnd()) {
            char ch = advance();
            if (ch == '"') {
                return new StringExpr(builder.toString(), startLine, startColumn);
            }

            if (ch == '\\') {
                if (isAtEnd()) {
                    throw error("unterminated string literal");
                }

                char escaped = advance();
                switch (escaped) {
                    case 'n' -> builder.append('\n');
                    case 'r' -> builder.append('\r');
                    case 't' -> builder.append('\t');
                    case '"' -> builder.append('"');
                    case '\\' -> builder.append('\\');
                    default -> builder.append(escaped);
                }
            } else {
                builder.append(ch);
            }
        }

        throw error("unterminated string literal");
    }

    private Expr parseAtom() throws EvalError {
        int startLine = line;
        int startColumn = column;
        String token = readToken();
        if (token.isEmpty()) {
            throw error("expected expression");
        }

        return switch (token) {
            case "#t" -> new BoolExpr(true, startLine, startColumn);
            case "#f" -> new BoolExpr(false, startLine, startColumn);
            default -> token.startsWith("#\\")
                    ? parseCharLiteral(token, startLine, startColumn)
                    : parseNumberOrSymbol(token, startLine, startColumn);
        };
    }

    private Expr parseNumberOrSymbol(String token, int startLine, int startColumn) throws EvalError {
        if (Numbers.isIntegerToken(token)) {
            try {
                return new IntExpr(Long.parseLong(token), startLine, startColumn);
            } catch (NumberFormatException e) {
                throw new EvalError("invalid integer literal: " + token, startLine, startColumn);
            }
        }
        if (Numbers.looksLikeNumberLiteral(token)) {
            return new NumberExpr(token, startLine, startColumn);
        }
        return new SymbolExpr(token, startLine, startColumn);
    }

    private Expr parseCharLiteral(String token, int startLine, int startColumn) throws EvalError {
        String value = token.substring(2);
        if (value.isEmpty()) {
            throw new EvalError("invalid character literal", startLine, startColumn);
        }

        char ch = switch (value) {
            case "space" -> ' ';
            case "newline" -> '\n';
            default -> {
                if (value.length() != 1) {
                    throw new EvalError("invalid character literal", startLine, startColumn);
                }
                yield value.charAt(0);
            }
        };

        return new CharExpr(ch, startLine, startColumn);
    }

    private String readToken() {
        StringBuilder builder = new StringBuilder();
        while (!isAtEnd()) {
            char ch = peek();
            if (Character.isWhitespace(ch) || ch == '(' || ch == ')' || ch == ';') {
                break;
            }
            builder.append(advance());
        }
        return builder.toString();
    }

    private void skipIgnored() {
        while (!isAtEnd()) {
            char ch = peek();
            if (Character.isWhitespace(ch)) {
                advance();
                continue;
            }
            if (ch == ';') {
                skipComment();
                continue;
            }
            break;
        }
    }

    private void skipComment() {
        while (!isAtEnd() && peek() != '\n') {
            advance();
        }
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

    private EvalError error(String message) {
        return new EvalError(message, line, column);
    }
}
