package ming;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

sealed interface Expr permits IntExpr, RationalExpr, InexactExpr,
        BoolExpr, StringExpr, CharExpr, SymbolExpr, ListExpr {
    SourcePos position();
}

record IntExpr(int value, SourcePos position) implements Expr {
}

record RationalExpr(BigInteger numerator, BigInteger denominator, SourcePos position)
        implements Expr {
}

record InexactExpr(double value, SourcePos position) implements Expr {
}

record BoolExpr(boolean value, SourcePos position) implements Expr {
}

record StringExpr(String value, SourcePos position) implements Expr {
}

record CharExpr(char value, SourcePos position) implements Expr {
}

record SymbolExpr(String name, SourcePos position) implements Expr {
}

record ListExpr(List<Expr> elements, SourcePos position) implements Expr {
}

record LetBinding(String name, Expr valueExpr) {
}

record ParameterSpec(List<String> requiredParameters, String restParameter) {
    ParameterSpec {
        requiredParameters = List.copyOf(requiredParameters);
    }
}

record SourcePos(int line, int column) {
}

final class Parser {
    private final String input;
    private int index;
    private int line = 1;
    private int column = 1;

    Parser(String input) {
        this.input = input;
    }

    boolean hasMore() {
        skipWhitespace();
        return index < input.length();
    }

    Expr parseExpr() throws EvalError {
        skipWhitespace();
        if (index >= input.length()) {
            throw errorAtCurrent("unexpected end of input");
        }

        SourcePos position = currentPosition();
        char ch = input.charAt(index);
        return switch (ch) {
            case '(' -> parseList(position);
            case '\'' -> parseQuoted(position);
            case '"' -> parseString(position);
            case ')' -> throw errorAt(position, "unexpected ')'");
            default -> {
                if (ch == '#' && index + 1 < input.length() && input.charAt(index + 1) == '\\') {
                    yield parseCharLiteral(position);
                }
                yield parseAtom(position);
            }
        };
    }

    private Expr parseQuoted(SourcePos position) throws EvalError {
        advance();
        return new ListExpr(List.of(new SymbolExpr("quote", position), parseExpr()), position);
    }

    private Expr parseList(SourcePos position) throws EvalError {
        advance();
        List<Expr> elements = new ArrayList<>();

        while (true) {
            skipWhitespace();
            if (index >= input.length()) {
                throw errorAt(position, "unterminated list");
            }
            if (input.charAt(index) == ')') {
                advance();
                return new ListExpr(elements, position);
            }
            elements.add(parseExpr());
        }
    }

    private Expr parseString(SourcePos position) throws EvalError {
        advance();
        StringBuilder builder = new StringBuilder();

        while (index < input.length()) {
            char ch = readChar();
            if (ch == '"') {
                return new StringExpr(builder.toString(), position);
            }
            if (ch == '\\') {
                builder.append(parseEscape(position));
                continue;
            }
            builder.append(ch);
        }

        throw errorAt(position, "unterminated string");
    }

    private Expr parseCharLiteral(SourcePos position) throws EvalError {
        advance();
        advance();

        if (index >= input.length()) {
            throw errorAt(position, "invalid character literal");
        }

        char next = input.charAt(index);
        if (isTokenDelimiter(next)) {
            advance();
            return new CharExpr(next, position);
        }

        int start = index;
        while (index < input.length() && !isTokenDelimiter(input.charAt(index))) {
            advance();
        }

        String literal = input.substring(start, index);
        return switch (literal) {
            case "space" -> new CharExpr(' ', position);
            case "newline" -> new CharExpr('\n', position);
            default -> {
                if (literal.length() == 1) {
                    yield new CharExpr(literal.charAt(0), position);
                }
                throw errorAt(position, "invalid character literal");
            }
        };
    }

    private char parseEscape(SourcePos position) throws EvalError {
        if (index >= input.length()) {
            throw errorAt(position, "unterminated string escape");
        }

        char ch = readChar();
        return switch (ch) {
            case '\\' -> '\\';
            case '"' -> '"';
            case 'n' -> '\n';
            case 't' -> '\t';
            case 'r' -> '\r';
            default -> ch;
        };
    }

    private Expr parseAtom(SourcePos position) throws EvalError {
        int start = index;
        while (index < input.length()) {
            char ch = input.charAt(index);
            if (isTokenDelimiter(ch)) {
                break;
            }
            advance();
        }

        String token = input.substring(start, index);
        if (token.equals("#t")) {
            return new BoolExpr(true, position);
        }
        if (token.equals("#f")) {
            return new BoolExpr(false, position);
        }

        ParsedNumber parsedNumber;
        try {
            parsedNumber = NumericSupport.parseLiteral(token);
        } catch (IllegalArgumentException error) {
            throw errorAt(position, error.getMessage());
        }

        if (parsedNumber instanceof ParsedInteger parsedInteger) {
            return new IntExpr(parsedInteger.value(), position);
        }
        if (parsedNumber instanceof ParsedRational parsedRational) {
            return new RationalExpr(parsedRational.numerator(), parsedRational.denominator(),
                    position);
        }
        if (parsedNumber instanceof ParsedInexact parsedInexact) {
            return new InexactExpr(parsedInexact.value(), position);
        }
        return new SymbolExpr(token, position);
    }

    private boolean isTokenDelimiter(char ch) {
        return Character.isWhitespace(ch) || ch == '(' || ch == ')' || ch == ';';
    }

    private void skipWhitespace() {
        while (index < input.length()) {
            char ch = input.charAt(index);
            if (Character.isWhitespace(ch)) {
                advance();
                continue;
            }
            if (ch == ';') {
                advance();
                while (index < input.length() && input.charAt(index) != '\n') {
                    advance();
                }
                continue;
            }
            break;
        }
    }

    private SourcePos currentPosition() {
        return new SourcePos(line, column);
    }

    private EvalError errorAtCurrent(String message) {
        return errorAt(currentPosition(), message);
    }

    private EvalError errorAt(SourcePos position, String message) {
        return new EvalError(message, position.line(), position.column());
    }

    private char readChar() {
        char ch = input.charAt(index);
        advance();
        return ch;
    }

    private void advance() {
        char ch = input.charAt(index++);
        if (ch == '\n') {
            line++;
            column = 1;
            return;
        }
        column++;
    }
}
