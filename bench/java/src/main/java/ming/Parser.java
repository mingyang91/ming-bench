package ming;

import java.util.ArrayList;
import java.util.List;

public class Parser {
    private final String input;
    private int pos;

    public Parser(String input) {
        this.input = input;
        this.pos = 0;
    }

    public List<SchemeValue> parseAll() throws EvalError {
        List<SchemeValue> exprs = new ArrayList<>();
        while (true) {
            skipWhitespace();
            if (pos >= input.length()) break;
            exprs.add(parseExpr());
        }
        return exprs;
    }

    private SchemeValue parseExpr() throws EvalError {
        skipWhitespace();
        if (pos >= input.length()) throw new EvalError("Unexpected end of input");

        char c = input.charAt(pos);

        if (c == '(') {
            return parseList();
        } else if (c == '"') {
            return parseString();
        } else if (c == '#') {
            return parseHash();
        } else {
            return parseAtom();
        }
    }

    private SchemeValue parseList() throws EvalError {
        pos++; // skip '('
        List<SchemeValue> elements = new ArrayList<>();
        while (true) {
            skipWhitespace();
            if (pos >= input.length()) throw new EvalError("Unexpected end of input: unclosed parenthesis");
            if (input.charAt(pos) == ')') {
                pos++;
                return new SchemeValue.ListVal(elements);
            }
            elements.add(parseExpr());
        }
    }

    private SchemeValue parseString() throws EvalError {
        pos++; // skip opening quote
        var sb = new StringBuilder();
        while (pos < input.length()) {
            char c = input.charAt(pos);
            if (c == '\\') {
                pos++;
                if (pos >= input.length()) throw new EvalError("Unexpected end of string");
                char esc = input.charAt(pos);
                switch (esc) {
                    case 'n' -> sb.append('\n');
                    case 't' -> sb.append('\t');
                    case '\\' -> sb.append('\\');
                    case '"' -> sb.append('"');
                    default -> { sb.append('\\'); sb.append(esc); }
                }
            } else if (c == '"') {
                pos++;
                return new SchemeValue.StringVal(sb.toString());
            } else {
                sb.append(c);
            }
            pos++;
        }
        throw new EvalError("Unterminated string");
    }

    private SchemeValue parseHash() throws EvalError {
        if (pos + 1 >= input.length()) throw new EvalError("Unexpected end of input after #");
        char next = input.charAt(pos + 1);
        if (next == 't') {
            pos += 2;
            return new SchemeValue.BoolVal(true);
        } else if (next == 'f') {
            pos += 2;
            return new SchemeValue.BoolVal(false);
        }
        throw new EvalError("Unknown hash literal: #" + next);
    }

    private SchemeValue parseAtom() throws EvalError {
        int start = pos;
        while (pos < input.length() && !isDelimiter(input.charAt(pos))) {
            pos++;
        }
        String token = input.substring(start, pos);
        if (token.isEmpty()) throw new EvalError("Empty token");

        // Try integer
        try {
            long val = Long.parseLong(token);
            return new SchemeValue.IntVal(val);
        } catch (NumberFormatException ignored) {}

        return new SchemeValue.SymbolVal(token);
    }

    private void skipWhitespace() {
        while (pos < input.length()) {
            char c = input.charAt(pos);
            if (c == ';') {
                // Skip line comment
                while (pos < input.length() && input.charAt(pos) != '\n') pos++;
            } else if (Character.isWhitespace(c)) {
                pos++;
            } else {
                break;
            }
        }
    }

    private boolean isDelimiter(char c) {
        return Character.isWhitespace(c) || c == '(' || c == ')' || c == '"' || c == ';';
    }
}
