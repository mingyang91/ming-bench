package ming;

import java.util.ArrayList;
import java.util.List;

public class Tokenizer {
    public enum TokenType {
        LPAREN, RPAREN, SYMBOL, INTEGER, BOOLEAN, STRING, EOF
    }

    public record Token(TokenType type, String value, int pos) {}

    private final String input;
    private int pos;

    public Tokenizer(String input) {
        this.input = input;
        this.pos = 0;
    }

    public List<Token> tokenize() throws EvalError {
        List<Token> tokens = new ArrayList<>();
        while (pos < input.length()) {
            skipWhitespaceAndComments();
            if (pos >= input.length()) break;

            char c = input.charAt(pos);
            if (c == '(') {
                tokens.add(new Token(TokenType.LPAREN, "(", pos));
                pos++;
            } else if (c == ')') {
                tokens.add(new Token(TokenType.RPAREN, ")", pos));
                pos++;
            } else if (c == '"') {
                tokens.add(readString());
            } else if (c == '#') {
                tokens.add(readHash());
            } else {
                tokens.add(readAtom());
            }
        }
        tokens.add(new Token(TokenType.EOF, "", pos));
        return tokens;
    }

    private void skipWhitespaceAndComments() {
        while (pos < input.length()) {
            char c = input.charAt(pos);
            if (Character.isWhitespace(c)) {
                pos++;
            } else if (c == ';') {
                while (pos < input.length() && input.charAt(pos) != '\n') pos++;
            } else {
                break;
            }
        }
    }

    private Token readString() throws EvalError {
        int start = pos;
        pos++; // skip opening quote
        StringBuilder sb = new StringBuilder();
        while (pos < input.length() && input.charAt(pos) != '"') {
            if (input.charAt(pos) == '\\') {
                pos++;
                if (pos >= input.length()) throw new EvalError("Unterminated string");
                char esc = input.charAt(pos);
                switch (esc) {
                    case 'n' -> sb.append('\n');
                    case 't' -> sb.append('\t');
                    case '\\' -> sb.append('\\');
                    case '"' -> sb.append('"');
                    default -> { sb.append('\\'); sb.append(esc); }
                }
            } else {
                sb.append(input.charAt(pos));
            }
            pos++;
        }
        if (pos >= input.length()) throw new EvalError("Unterminated string");
        pos++; // skip closing quote
        return new Token(TokenType.STRING, sb.toString(), start);
    }

    private Token readHash() throws EvalError {
        int start = pos;
        pos++; // skip #
        if (pos >= input.length()) throw new EvalError("Unexpected end after #");
        char c = input.charAt(pos);
        if (c == 't') {
            pos++;
            // Check it's not part of a longer symbol like #true
            if (pos < input.length() && !isDelimiter(input.charAt(pos))) {
                // read rest
                StringBuilder sb = new StringBuilder("#t");
                while (pos < input.length() && !isDelimiter(input.charAt(pos))) {
                    sb.append(input.charAt(pos));
                    pos++;
                }
                if (sb.toString().equals("#true")) return new Token(TokenType.BOOLEAN, "true", start);
                throw new EvalError("Unknown literal: " + sb);
            }
            return new Token(TokenType.BOOLEAN, "true", start);
        } else if (c == 'f') {
            pos++;
            if (pos < input.length() && !isDelimiter(input.charAt(pos))) {
                StringBuilder sb = new StringBuilder("#f");
                while (pos < input.length() && !isDelimiter(input.charAt(pos))) {
                    sb.append(input.charAt(pos));
                    pos++;
                }
                if (sb.toString().equals("#false")) return new Token(TokenType.BOOLEAN, "false", start);
                throw new EvalError("Unknown literal: " + sb);
            }
            return new Token(TokenType.BOOLEAN, "false", start);
        }
        throw new EvalError("Unknown # literal at position " + start);
    }

    private Token readAtom() {
        int start = pos;
        StringBuilder sb = new StringBuilder();
        while (pos < input.length() && !isDelimiter(input.charAt(pos))) {
            sb.append(input.charAt(pos));
            pos++;
        }
        String val = sb.toString();
        // Check if integer
        try {
            Long.parseLong(val);
            return new Token(TokenType.INTEGER, val, start);
        } catch (NumberFormatException e) {
            return new Token(TokenType.SYMBOL, val, start);
        }
    }

    private boolean isDelimiter(char c) {
        return Character.isWhitespace(c) || c == '(' || c == ')' || c == '"' || c == ';';
    }
}
