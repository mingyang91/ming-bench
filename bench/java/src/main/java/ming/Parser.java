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
            int[] lc = lineCol(pos);
            pos++;
            var quoted = parseExpr();
            var elems = new ArrayList<SchemeValue>();
            elems.add(new SchemeValue.SymbolVal("quote", lc[0], lc[1]));
            elems.add(quoted);
            return new SchemeValue.ListVal(elems, lc[0], lc[1]);
        } else {
            return parseAtom();
        }
    }

    private SchemeValue parseList() throws EvalError {
        int[] lc = lineCol(pos);
        pos++; // skip '('
        var elems = new ArrayList<SchemeValue>();
        while (true) {
            skipWhitespaceAndComments();
            if (pos >= input.length()) throw new EvalError("unterminated list");
            if (input.charAt(pos) == ')') {
                pos++;
                return new SchemeValue.ListVal(elems, lc[0], lc[1]);
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
                return new SchemeValue.StringVal(sb.toString(), true);
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
        if (next == '\\') {
            pos += 2; // skip #\
            if (pos >= input.length()) throw new EvalError("unexpected end of character literal");
            // Named characters
            if (pos + 5 <= input.length() && input.substring(pos, pos + 5).equals("space") &&
                (pos + 5 >= input.length() || !isSymbolChar(input.charAt(pos + 5)))) {
                pos += 5;
                return new SchemeValue.CharVal(' ');
            }
            if (pos + 7 <= input.length() && input.substring(pos, pos + 7).equals("newline") &&
                (pos + 7 >= input.length() || !isSymbolChar(input.charAt(pos + 7)))) {
                pos += 7;
                return new SchemeValue.CharVal('\n');
            }
            if (pos + 3 <= input.length() && input.substring(pos, pos + 3).equals("tab") &&
                (pos + 3 >= input.length() || !isSymbolChar(input.charAt(pos + 3)))) {
                pos += 3;
                return new SchemeValue.CharVal('\t');
            }
            char ch = input.charAt(pos);
            pos++;
            return new SchemeValue.CharVal(ch);
        }
        if (next == '(') {
            pos++; // skip #
            // parse as list then convert to vector literal form: (vector ...)
            var list = parseList();
            if (!(list instanceof SchemeValue.ListVal lv))
                throw new EvalError("unexpected #(");
            var elems = new ArrayList<SchemeValue>();
            elems.add(new SchemeValue.SymbolVal("vector"));
            elems.addAll(lv.elements());
            return new SchemeValue.ListVal(elems);
        }
        throw new EvalError("unexpected #" + next);
    }

    private SchemeValue parseAtom() throws EvalError {
        int[] lc = lineCol(pos);
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

        return new SchemeValue.SymbolVal(token, lc[0], lc[1]);
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

    private int[] lineCol(int position) {
        int line = 1, col = 1;
        for (int i = 0; i < position && i < input.length(); i++) {
            if (input.charAt(i) == '\n') {
                line++;
                col = 1;
            } else {
                col++;
            }
        }
        return new int[]{line, col};
    }
}
