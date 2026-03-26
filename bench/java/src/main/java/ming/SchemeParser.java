package ming;

import java.util.ArrayList;
import java.util.List;

final class SchemeParser {
    private final String input;
    private int index;

    SchemeParser(String input) {
        this.input = input;
    }

    List<SchemeExpression> parseProgram() throws EvalError {
        List<SchemeExpression> expressions = new ArrayList<>();
        skipIgnorable();
        while (!isAtEnd()) {
            expressions.add(parseExpression());
            skipIgnorable();
        }
        return expressions;
    }

    private SchemeExpression parseExpression() throws EvalError {
        skipIgnorable();
        if (isAtEnd()) {
            throw new EvalError("unexpected end of input");
        }

        char current = input.charAt(index);
        if (current == '(') {
            return parseList();
        }
        if (current == ')') {
            throw new EvalError("unexpected ')'");
        }
        if (current == '"') {
            return new LiteralExpression(new StringValue(parseStringLiteral()));
        }
        if (current == '\'') {
            index++;
            return new ListExpression(List.of(new SymbolExpression("quote"), parseExpression()));
        }

        String token = readToken();
        if ("#t".equals(token)) {
            return new LiteralExpression(BoolValue.TRUE);
        }
        if ("#f".equals(token)) {
            return new LiteralExpression(BoolValue.FALSE);
        }
        if (isIntegerToken(token)) {
            return new LiteralExpression(new IntValue(parseInteger(token)));
        }
        return new SymbolExpression(token);
    }

    private ListExpression parseList() throws EvalError {
        index++;
        List<SchemeExpression> elements = new ArrayList<>();
        skipIgnorable();

        while (!isAtEnd() && input.charAt(index) != ')') {
            elements.add(parseExpression());
            skipIgnorable();
        }

        if (isAtEnd()) {
            throw new EvalError("unterminated list");
        }

        index++;
        return new ListExpression(List.copyOf(elements));
    }

    private String parseStringLiteral() throws EvalError {
        index++;
        StringBuilder builder = new StringBuilder();

        while (!isAtEnd()) {
            char current = input.charAt(index++);
            if (current == '"') {
                return builder.toString();
            }
            if (current == '\\') {
                builder.append(parseEscape());
            } else {
                builder.append(current);
            }
        }

        throw new EvalError("unterminated string");
    }

    private char parseEscape() throws EvalError {
        if (isAtEnd()) {
            throw new EvalError("unterminated string escape");
        }

        char escaped = input.charAt(index++);
        return switch (escaped) {
            case 'n' -> '\n';
            case 'r' -> '\r';
            case 't' -> '\t';
            case '"' -> '"';
            case '\\' -> '\\';
            default -> escaped;
        };
    }

    private void skipIgnorable() {
        while (!isAtEnd()) {
            char current = input.charAt(index);
            if (Character.isWhitespace(current)) {
                index++;
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
        while (!isAtEnd() && input.charAt(index) != '\n') {
            index++;
        }
        if (!isAtEnd()) {
            index++;
        }
    }

    private String readToken() {
        int start = index;
        while (!isAtEnd() && !isDelimiter(input.charAt(index))) {
            index++;
        }
        return input.substring(start, index);
    }

    private boolean isDelimiter(char current) {
        return Character.isWhitespace(current)
                || current == '('
                || current == ')'
                || current == ';';
    }

    private boolean isAtEnd() {
        return index >= input.length();
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

        for (int current = start; current < token.length(); current++) {
            if (!Character.isDigit(token.charAt(current))) {
                return false;
            }
        }
        return true;
    }

    private long parseInteger(String token) throws EvalError {
        try {
            return Long.parseLong(token);
        } catch (NumberFormatException error) {
            throw new EvalError("invalid integer: " + token);
        }
    }
}
