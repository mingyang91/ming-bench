package ming;

import java.util.ArrayList;
import java.util.List;

class Parser {
    private final String input;
    private int pos;

    private Parser(String input) {
        this.input = input;
        this.pos = 0;
    }

    static List<Object> parse(String input) throws EvalError {
        Parser p = new Parser(input);
        List<Object> exprs = new ArrayList<>();
        while (true) {
            p.skipWhitespace();
            if (p.pos >= p.input.length()) break;
            exprs.add(p.readExpr());
        }
        return exprs;
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

    private Object readExpr() throws EvalError {
        skipWhitespace();
        if (pos >= input.length()) throw new EvalError("unexpected end of input");
        char c = input.charAt(pos);

        if (c == '(') return readList();
        if (c == '"') return readString();
        if (c == '#') return readHash();
        if (c == '\'') {
            pos++;
            Object quoted = readExpr();
            List<Object> q = new ArrayList<>();
            q.add("quote");
            q.add(quoted);
            return q;
        }
        return readAtom();
    }

    private List<Object> readList() throws EvalError {
        pos++; // skip '('
        List<Object> list = new ArrayList<>();
        while (true) {
            skipWhitespace();
            if (pos >= input.length()) throw new EvalError("unterminated list");
            if (input.charAt(pos) == ')') {
                pos++;
                return list;
            }
            list.add(readExpr());
        }
    }

    private String readString() throws EvalError {
        pos++; // skip opening "
        StringBuilder sb = new StringBuilder();
        sb.append('"');
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
                pos++;
            } else if (c == '"') {
                pos++;
                sb.append('"');
                return sb.toString();
            } else {
                sb.append(c);
                pos++;
            }
        }
        throw new EvalError("unterminated string");
    }

    private Object readHash() throws EvalError {
        pos++; // skip '#'
        if (pos >= input.length()) throw new EvalError("unexpected end after #");
        char c = input.charAt(pos);
        if (c == 't') { pos++; return Boolean.TRUE; }
        if (c == 'f') { pos++; return Boolean.FALSE; }
        throw new EvalError("unknown # literal: #" + c);
    }

    private Object readAtom() {
        int start = pos;
        while (pos < input.length()) {
            char c = input.charAt(pos);
            if (Character.isWhitespace(c) || c == '(' || c == ')' || c == '"' || c == ';') break;
            pos++;
        }
        String token = input.substring(start, pos);
        try {
            return Long.parseLong(token);
        } catch (NumberFormatException e) {
            return token;
        }
    }
}
