package ming;

import java.util.ArrayList;
import java.util.List;

final class SchemeParser {
    private final String input;
    private int index;
    private int line = 1;
    private int column = 1;

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
            throw new EvalError(currentPosition(), "unexpected end of input");
        }

        SourcePosition position = currentPosition();
        char current = currentChar();
        if (current == '(') {
            return parseList(position);
        }
        if (current == ')') {
            throw new EvalError(position, "unexpected ')'");
        }
        if (current == '"') {
            return new LiteralExpression(new StringValue(parseStringLiteral(position)), position);
        }
        if (current == '\'') {
            advance();
            return new ListExpression(
                    List.of(new SymbolExpression("quote", position), parseExpression()),
                    position
            );
        }

        String token = readToken();
        if ("#t".equals(token)) {
            return new LiteralExpression(BoolValue.TRUE, position);
        }
        if ("#f".equals(token)) {
            return new LiteralExpression(BoolValue.FALSE, position);
        }
        if (token.startsWith("#\\")) {
            return new LiteralExpression(new CharValue(parseCharacterLiteral(token, position)), position);
        }
        SchemeValue numericLiteral = Numbers.parseNumber(token, position);
        if (numericLiteral != null) {
            return new LiteralExpression(numericLiteral, position);
        }
        return new SymbolExpression(token, position);
    }

    private ListExpression parseList(SourcePosition startPosition) throws EvalError {
        advance();
        List<SchemeExpression> elements = new ArrayList<>();
        skipIgnorable();

        while (!isAtEnd() && currentChar() != ')') {
            elements.add(parseExpression());
            skipIgnorable();
        }

        if (isAtEnd()) {
            throw new EvalError(startPosition, "unterminated list");
        }

        advance();
        return new ListExpression(List.copyOf(elements), startPosition);
    }

    private String parseStringLiteral(SourcePosition startPosition) throws EvalError {
        advance();
        StringBuilder builder = new StringBuilder();

        while (!isAtEnd()) {
            char current = advance();
            if (current == '"') {
                return builder.toString();
            }
            if (current == '\\') {
                builder.append(parseEscape());
            } else {
                builder.append(current);
            }
        }

        throw new EvalError(startPosition, "unterminated string");
    }

    private char parseEscape() throws EvalError {
        if (isAtEnd()) {
            throw new EvalError(currentPosition(), "unterminated string escape");
        }

        char escaped = advance();
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
        if (!isAtEnd()) {
            advance();
        }
    }

    private String readToken() {
        int start = index;
        while (!isAtEnd() && !isDelimiter(currentChar())) {
            advance();
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

    private char currentChar() {
        return input.charAt(index);
    }

    private SourcePosition currentPosition() {
        return new SourcePosition(line, column);
    }

    private char advance() {
        char current = input.charAt(index++);
        if (current == '\n') {
            line++;
            column = 1;
        } else {
            column++;
        }
        return current;
    }
    private char parseCharacterLiteral(String token, SourcePosition position) throws EvalError {
        String character = token.substring(2);
        if (character.isEmpty()) {
            throw new EvalError(position, "invalid character literal: " + token);
        }
        return switch (character) {
            case "space" -> ' ';
            case "newline" -> '\n';
            default -> {
                if (character.length() == 1) {
                    yield character.charAt(0);
                }
                throw new EvalError(position, "invalid character literal: " + token);
            }
        };
    }
}
