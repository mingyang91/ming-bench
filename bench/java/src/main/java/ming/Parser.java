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

    List<Evaluator.Expr> parseProgram() throws EvalError {
        List<Evaluator.Expr> expressions = new ArrayList<>();
        skipIgnored();
        while (!isAtEnd()) {
            expressions.add(parseExpr());
            skipIgnored();
        }
        return expressions;
    }

    private Evaluator.Expr parseExpr() throws EvalError {
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

    private Evaluator.Expr parseList() throws EvalError {
        Evaluator.SourcePos pos = currentPos();
        advance();

        List<Evaluator.Expr> elements = new ArrayList<>();
        skipIgnored();
        while (!isAtEnd() && peek() != ')') {
            elements.add(parseExpr());
            skipIgnored();
        }

        if (isAtEnd()) {
            throw error("unterminated list", pos);
        }

        advance();
        return new Evaluator.ListExpr(elements, pos);
    }

    private Evaluator.Expr parseQuote() throws EvalError {
        Evaluator.SourcePos pos = currentPos();
        advance();

        List<Evaluator.Expr> elements = new ArrayList<>(2);
        elements.add(new Evaluator.SymbolExpr("quote", pos));
        elements.add(parseExpr());
        return new Evaluator.ListExpr(elements, pos);
    }

    private Evaluator.Expr parseString() throws EvalError {
        Evaluator.SourcePos pos = currentPos();
        advance();

        StringBuilder builder = new StringBuilder();
        while (!isAtEnd()) {
            char current = advance();
            if (current == '"') {
                return new Evaluator.StringExpr(builder.toString(), pos);
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

    private char readEscape(Evaluator.SourcePos pos) throws EvalError {
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

    private Evaluator.Expr parseHashLiteral() throws EvalError {
        Evaluator.SourcePos pos = currentPos();
        advance();
        if (isAtEnd()) {
            throw error("incomplete hash literal", pos);
        }

        char value = advance();
        return switch (value) {
            case 't' -> new Evaluator.BoolExpr(true, pos);
            case 'f' -> new Evaluator.BoolExpr(false, pos);
            case '\\' -> parseCharacterLiteral(pos);
            default -> throw error("unknown hash literal '#" + value + "'", pos);
        };
    }

    private Evaluator.Expr parseCharacterLiteral(Evaluator.SourcePos pos) throws EvalError {
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
            case "space" -> new Evaluator.CharExpr(' ', pos);
            case "newline" -> new Evaluator.CharExpr('\n', pos);
            default -> {
                if (token.length() != 1) {
                    throw error("unknown character literal '#\\" + token + "'", pos);
                }
                yield new Evaluator.CharExpr(token.charAt(0), pos);
            }
        };
    }

    private Evaluator.Expr parseNumber() {
        Evaluator.SourcePos pos = currentPos();
        int start = index;
        if (peek() == '+' || peek() == '-') {
            advance();
        }
        while (!isAtEnd() && Character.isDigit(peek())) {
            advance();
        }
        return new Evaluator.IntExpr(new BigInteger(input.substring(start, index)), pos);
    }

    private Evaluator.Expr parseSymbol() {
        Evaluator.SourcePos pos = currentPos();
        int start = index;
        while (!isAtEnd() && !isDelimiter(peek())) {
            advance();
        }
        return new Evaluator.SymbolExpr(input.substring(start, index), pos);
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

    private Evaluator.SourcePos currentPos() {
        return new Evaluator.SourcePos(line, column);
    }

    private EvalError error(String message, Evaluator.SourcePos pos) {
        return new EvalError(message, pos.line(), pos.column());
    }
}
