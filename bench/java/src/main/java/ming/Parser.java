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
        var exprs = new ArrayList<SchemeValue>();
        while (true) {
            skipWhitespaceAndComments();
            if (pos >= input.length()) break;
            exprs.add(parseExpr());
        }
        return exprs;
    }

    private SchemeValue parseExpr() throws EvalError {
        skipWhitespaceAndComments();
        if (pos >= input.length()) throw new EvalError("unexpected end of input");

        char c = input.charAt(pos);

        if (c == '(') {
            return parseList();
        } else if (c == '"') {
            return parseString();
        } else if (c == '#') {
            return parseHash();
        } else if (c == '\'') {
            pos++;
            var quoted = parseExpr();
            var elems = new ArrayList<SchemeValue>();
            elems.add(new SchemeValue.SymbolVal("quote"));
            elems.add(quoted);
            return new SchemeValue.ListVal(elems);
        } else {
            return parseAtom();
        }
    }

    private SchemeValue parseList() throws EvalError {
        pos++; // skip '('
        var elems = new ArrayList<SchemeValue>();
        while (true) {
            skipWhitespaceAndComments();
            if (pos >= input.length()) throw new EvalError("unterminated list");
            if (input.charAt(pos) == ')') {
                pos++;
                return new SchemeValue.ListVal(elems);
            }
            elems.add(parseExpr());
        }
    }

    private SchemeValue parseString() throws EvalError {
        pos++; // skip opening quote
        var sb = new StringBuilder();
        while (pos < input.length()) {
            char c = input.charAt(pos);
            if (c == '\\') {
                pos++;
                if (pos >= input.length()) throw new EvalError("unterminated string");
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
        throw new EvalError("unterminated string");
    }

    private SchemeValue parseHash() throws EvalError {
        if (pos + 1 >= input.length()) throw new EvalError("unexpected #");
        char next = input.charAt(pos + 1);
        if (next == 't') {
            pos += 2;
            // Make sure it's not part of a longer token
            if (pos < input.length() && isSymbolChar(input.charAt(pos))) {
                throw new EvalError("unexpected character after #t");
            }
            return new SchemeValue.BoolVal(true);
        } else if (next == 'f') {
            pos += 2;
            if (pos < input.length() && isSymbolChar(input.charAt(pos))) {
                throw new EvalError("unexpected character after #f");
            }
            return new SchemeValue.BoolVal(false);
        }
        throw new EvalError("unexpected #" + next);
    }

    private SchemeValue parseAtom() throws EvalError {
        int start = pos;
        while (pos < input.length() && isSymbolChar(input.charAt(pos))) {
            pos++;
        }
        String token = input.substring(start, pos);
        if (token.isEmpty()) throw new EvalError("unexpected character: " + input.charAt(pos));

        // Try integer
        try {
            return new SchemeValue.IntVal(Long.parseLong(token));
        } catch (NumberFormatException ignored) {}

        return new SchemeValue.SymbolVal(token);
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

    private boolean isSymbolChar(char c) {
        return !Character.isWhitespace(c) && c != '(' && c != ')' && c != '"' && c != ';';
    }
}
