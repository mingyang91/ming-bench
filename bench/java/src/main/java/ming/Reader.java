package ming;

import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;

public class Reader {
    private final String input;
    private int pos;
    private final IdentityHashMap<SchemeValue, SourcePos> positions = new IdentityHashMap<>();

    public Reader(String input) {
        this.input = input;
        this.pos = 0;
    }

    public IdentityHashMap<SchemeValue, SourcePos> getPositions() {
        return positions;
    }

    public List<SchemeValue> readAll() throws EvalError {
        var exprs = new ArrayList<SchemeValue>();
        while (true) {
            skipWhitespace();
            if (pos >= input.length()) break;
            exprs.add(readExpr());
        }
        return exprs;
    }

    private SourcePos offsetToPos(int offset) {
        int line = 1, col = 1;
        for (int i = 0; i < offset && i < input.length(); i++) {
            if (input.charAt(i) == '\n') { line++; col = 1; }
            else col++;
        }
        return new SourcePos(line, col);
    }

    private SchemeValue readExpr() throws EvalError {
        skipWhitespace();
        if (pos >= input.length()) throw new EvalError("unexpected end of input");

        int startOffset = pos;
        char c = input.charAt(pos);
        SchemeValue result;

        if (c == '(') {
            result = readList();
        } else if (c == '"') {
            result = readString();
        } else if (c == '#') {
            result = readHash();
        } else if (c == '\'') {
            pos++;
            var quoted = readExpr();
            result = new SchemeValue.ListVal(List.of(new SchemeValue.SymbolVal("quote"), quoted));
        } else {
            result = readAtom();
        }
        positions.put(result, offsetToPos(startOffset));
        return result;
    }

    private SchemeValue readList() throws EvalError {
        pos++; // skip '('
        var elements = new ArrayList<SchemeValue>();
        while (true) {
            skipWhitespace();
            if (pos >= input.length()) throw new EvalError("unexpected end of input: unclosed list");
            if (input.charAt(pos) == ')') {
                pos++;
                return new SchemeValue.ListVal(elements);
            }
            elements.add(readExpr());
        }
    }

    private SchemeValue readString() throws EvalError {
        pos++; // skip opening "
        var sb = new StringBuilder();
        while (pos < input.length()) {
            char c = input.charAt(pos);
            if (c == '\\') {
                pos++;
                if (pos >= input.length()) throw new EvalError("unexpected end of string");
                char esc = input.charAt(pos);
                switch (esc) {
                    case 'n' -> sb.append('\n');
                    case 't' -> sb.append('\t');
                    case '\\' -> sb.append('\\');
                    case '"' -> sb.append('"');
                    default -> { sb.append('\\'); sb.append(esc); }
                }
                pos++;
            } else if (c == '"') {
                pos++;
                return new SchemeValue.StringVal(sb.toString());
            } else {
                sb.append(c);
                pos++;
            }
        }
        throw new EvalError("unterminated string");
    }

    private SchemeValue readHash() throws EvalError {
        if (pos + 1 >= input.length()) throw new EvalError("unexpected end of input after #");
        char next = input.charAt(pos + 1);
        if (next == 't') {
            pos += 2;
            return new SchemeValue.BoolVal(true);
        } else if (next == 'f') {
            pos += 2;
            return new SchemeValue.BoolVal(false);
        }
        throw new EvalError("unknown hash literal: #" + next);
    }

    private SchemeValue readAtom() throws EvalError {
        int start = pos;
        while (pos < input.length()) {
            char c = input.charAt(pos);
            if (c == '(' || c == ')' || c == '"' || Character.isWhitespace(c) || c == ';') break;
            pos++;
        }
        String token = input.substring(start, pos);
        if (token.isEmpty()) throw new EvalError("unexpected character: " + input.charAt(pos));

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
                while (pos < input.length() && input.charAt(pos) != '\n') pos++;
            } else if (Character.isWhitespace(c)) {
                pos++;
            } else {
                break;
            }
        }
    }
}
