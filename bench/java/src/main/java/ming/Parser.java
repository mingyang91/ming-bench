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
            expressions.add(parseExpr());
            skipIgnored();
        }
        return expressions;
    }

    private Expr parseExpr() throws EvalError {
        skipIgnored();
        if (isAtEnd()) {
            throw error("unexpected end of input", currentPos());
        }

        char current = peek();
        if (current == '(') {
            return parseList();
        }
        if (current == '\'') {
            return parseQuote();
        }
        if (current == '"') {
            return parseString();
        }
        if (current == '#') {
            return parseHashLiteral();
        }
        if (current == ')') {
            throw error("unexpected ')'", currentPos());
        }
        if (isNumberStart()) {
            return parseNumber();
        }
        return parseSymbol();
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
            throw error("unterminated list", pos);
        }

        advance();
        return new ListExpr(elements, pos);
    }

    private Expr parseQuote() throws EvalError {
        SourcePos pos = currentPos();
        advance();

        List<Expr> elements = new ArrayList<>(2);
        elements.add(new SymbolExpr("quote", pos));
        elements.add(parseExpr());
        return new ListExpr(elements, pos);
    }

    private Expr parseString() throws EvalError {
        SourcePos pos = currentPos();
        advance();

        StringBuilder builder = new StringBuilder();
        while (!isAtEnd()) {
            char current = advance();
            if (current == '"') {
                return new StringExpr(builder.toString(), pos);
            }
            if (current == '\\') {
                if (isAtEnd()) {
                    throw error("unterminated string escape", pos);
                }
                builder.append(readEscape(pos));
            } else {
                builder.append(current);
            }
        }

        throw error("unterminated string", pos);
    }

    private char readEscape(SourcePos pos) throws EvalError {
        char escaped = advance();
        return switch (escaped) {
            case 'n' -> '\n';
            case 'r' -> '\r';
            case 't' -> '\t';
            case '\\' -> '\\';
            case '"' -> '"';
            default -> throw error("unsupported string escape: \\" + escaped, pos);
        };
    }

    private Expr parseHashLiteral() throws EvalError {
        SourcePos pos = currentPos();
        advance();
        if (isAtEnd()) {
            throw error("incomplete hash literal", pos);
        }

        char value = advance();
        return switch (value) {
            case 't' -> new BoolExpr(true, pos);
            case 'f' -> new BoolExpr(false, pos);
            case '\\' -> parseCharacterLiteral(pos);
            default -> throw error("unknown hash literal '#" + value + "'", pos);
        };
    }

    private Expr parseCharacterLiteral(SourcePos pos) throws EvalError {
        if (isAtEnd()) {
            throw error("incomplete character literal", pos);
        }

        int start = index;
        while (!isAtEnd() && !isDelimiter(peek())) {
            advance();
        }

        String token = input.substring(start, index);
        if (token.isEmpty()) {
            throw error("incomplete character literal", pos);
        }
        return switch (token) {
            case "space" -> new CharExpr(' ', pos);
            case "newline" -> new CharExpr('\n', pos);
            default -> {
                if (token.length() != 1) {
                    throw error("unknown character literal '#\\" + token + "'", pos);
                }
                yield new CharExpr(token.charAt(0), pos);
            }
        };
    }

    private Expr parseNumber() throws EvalError {
        SourcePos pos = currentPos();
        int start = index;
        while (!isAtEnd() && !isDelimiter(peek())) {
            advance();
        }
        String token = input.substring(start, index);
        try {
            return new NumberExpr(SchemeNumber.parseLiteral(token), pos);
        } catch (NumberFormatException error) {
            throw error("invalid number: " + token, pos);
        }
    }

    private Expr parseSymbol() {
        SourcePos pos = currentPos();
        int start = index;
        while (!isAtEnd() && !isDelimiter(peek())) {
            advance();
        }
        return new SymbolExpr(input.substring(start, index), pos);
    }

    private boolean isNumberStart() {
        if (isAtEnd()) {
            return false;
        }

        char current = peek();
        if (Character.isDigit(current)) {
            return true;
        }
        if ((current == '+' || current == '-') && index + 1 < input.length()) {
            return Character.isDigit(input.charAt(index + 1));
        }
        return false;
    }

    private void skipIgnored() {
        while (!isAtEnd()) {
            char current = peek();
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
        while (!isAtEnd() && peek() != '\n') {
            advance();
        }
    }

    private boolean isDelimiter(char value) {
        return Character.isWhitespace(value) || value == '(' || value == ')'
                || value == '"' || value == ';';
    }

    private boolean isAtEnd() {
        return index >= input.length();
    }

    private char peek() {
        return input.charAt(index);
    }

    private char advance() {
        char value = input.charAt(index);
        index++;
        if (value == '\n') {
            line++;
            column = 1;
        } else {
            column++;
        }
        return value;
    }

    private SourcePos currentPos() {
        return new SourcePos(line, column);
    }

    private EvalError error(String message, SourcePos pos) {
        return new EvalError(message, pos.line(), pos.column());
    }
}
