package ming;

import java.math.BigInteger;
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
            expressions.add(parseExpression());
            skipIgnored();
        }
        return List.copyOf(expressions);
    }

    private Expr parseExpression() throws EvalError {
        skipIgnored();
        if (isAtEnd()) {
            throw new EvalError("unexpected end of input");
        }

        SourceLoc loc = currentLoc();
        char ch = peek();
        if (ch == '\'') {
            return parseQuoteShorthand();
        }
        if (ch == '(') {
            return parseList();
        }
        if (ch == ')') {
            throw SchemeErrors.at(loc, "unexpected ')'");
        }
        if (ch == '"') {
            return parseString();
        }
        return parseAtom();
    }

    private Expr parseList() throws EvalError {
        SourceLoc start = currentLoc();
        advance();

        List<Expr> elements = new ArrayList<>();
        skipIgnored();
        while (!isAtEnd() && peek() != ')') {
            elements.add(parseExpression());
            skipIgnored();
        }

        if (isAtEnd()) {
            throw SchemeErrors.at(start, "unterminated list");
        }
        advance();
        return new ListExpr(start, List.copyOf(elements));
    }

    private Expr parseQuoteShorthand() throws EvalError {
        SourceLoc start = currentLoc();
        advance();
        Expr quotedExpr = parseExpression();
        return new ListExpr(start, List.of(new SymbolExpr(start, "quote"), quotedExpr));
    }

    private Expr parseString() throws EvalError {
        SourceLoc start = currentLoc();
        advance();

        StringBuilder builder = new StringBuilder();
        while (!isAtEnd()) {
            char ch = advance();
            if (ch == '"') {
                return new StringExpr(start, builder.toString());
            }
            if (ch == '\\') {
                if (isAtEnd()) {
                    throw SchemeErrors.at(start, "unterminated string literal");
                }
                builder.append(parseEscape(advance()));
            } else {
                builder.append(ch);
            }
        }

        throw SchemeErrors.at(start, "unterminated string literal");
    }

    private char parseEscape(char escaped) {
        return switch (escaped) {
            case 'n' -> '\n';
            case 'r' -> '\r';
            case 't' -> '\t';
            case '"' -> '"';
            case '\\' -> '\\';
            default -> escaped;
        };
    }

    private Expr parseAtom() throws EvalError {
        SourceLoc start = currentLoc();
        StringBuilder builder = new StringBuilder();
        while (!isAtEnd() && !isDelimiter(peek())) {
            builder.append(advance());
        }

        String token = builder.toString();
        if ("#t".equals(token)) {
            return new BooleanExpr(start, true);
        }
        if ("#f".equals(token)) {
            return new BooleanExpr(start, false);
        }
        if (token.startsWith("#\\")) {
            return new CharExpr(start, parseCharacterLiteral(token, start));
        }
        if (Rational.isIntegerToken(token)) {
            return new NumberExpr(start, Rational.integer(new BigInteger(token)));
        }
        return new SymbolExpr(start, token);
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
            return;
        }
    }

    private void skipComment() {
        while (!isAtEnd() && peek() != '\n') {
            advance();
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
        char ch = input.charAt(index);
        index++;
        if (ch == '\n') {
            line++;
            column = 1;
        } else {
            column++;
        }
        return ch;
    }

    private SourceLoc currentLoc() {
        return new SourceLoc(line, column);
    }

    private int parseCharacterLiteral(String token, SourceLoc loc) throws EvalError {
        String literal = token.substring(2);
        return switch (literal) {
            case "space" -> ' ';
            case "newline" -> '\n';
            default -> {
                if (literal.codePointCount(0, literal.length()) != 1) {
                    throw SchemeErrors.at(loc, "invalid character literal");
                }
                yield literal.codePointAt(0);
            }
        };
    }
}
