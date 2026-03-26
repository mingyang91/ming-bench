package ming;

import java.util.ArrayList;
import java.util.List;

class Parser {
    private final String input;
    private int pos;
    private int line;
    private int col;

    private Parser(String input) {
        this.input = input;
        this.pos = 0;
        this.line = 1;
        this.col = 1;
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

    private void advance() {
        if (pos < input.length()) {
            if (input.charAt(pos) == '\n') {
                line++;
                col = 1;
            } else {
                col++;
            }
            pos++;
        }
    }

    private void skipWhitespace() {
        while (pos < input.length()) {
            char c = input.charAt(pos);
            if (c == ';') {
                while (pos < input.length() && input.charAt(pos) != '\n') advance();
            } else if (Character.isWhitespace(c)) {
                advance();
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
            int qLine = line, qCol = col;
            advance();
            Object quoted = readExpr();
            SourceList q = new SourceList(qLine, qCol);
            q.add("quote");
            q.add(quoted);
            return q;
        }
        if (c == '`') {
            int qLine = line, qCol = col;
            advance();
            Object quoted = readExpr();
            SourceList q = new SourceList(qLine, qCol);
            q.add("quasiquote");
            q.add(quoted);
            return q;
        }
        if (c == ',') {
            int qLine = line, qCol = col;
            advance();
            if (pos < input.length() && input.charAt(pos) == '@') {
                advance();
                Object expr = readExpr();
                SourceList q = new SourceList(qLine, qCol);
                q.add("unquote-splicing");
                q.add(expr);
                return q;
            }
            Object expr = readExpr();
            SourceList q = new SourceList(qLine, qCol);
            q.add("unquote");
            q.add(expr);
            return q;
        }
        return readAtom();
    }

    private Object readList() throws EvalError {
        int startLine = line, startCol = col;
        advance(); // skip '('
        SourceList list = new SourceList(startLine, startCol);
        while (true) {
            skipWhitespace();
            if (pos >= input.length()) throw new EvalError("unterminated list");
            if (input.charAt(pos) == ')') {
                advance();
                return list;
            }
            // Check for dotted pair notation: (a b . c)
            if (input.charAt(pos) == '.' && pos + 1 < input.length()) {
                char next = input.charAt(pos + 1);
                if (next == ' ' || next == '\t' || next == '\n' || next == '\r' || next == ')') {
                    advance(); // skip '.'
                    skipWhitespace();
                    Object cdr = readExpr();
                    skipWhitespace();
                    if (pos >= input.length() || input.charAt(pos) != ')')
                        throw new EvalError("expected ) after dotted pair at " + line + ":" + col +
                            (pos < input.length() ? " got '" + input.charAt(pos) + "'" : " got EOF"));
                    advance(); // skip ')'
                    // Build Pair chain: (a b . c) => Pair(a, Pair(b, c))
                    Object result = cdr;
                    for (int i = list.size() - 1; i >= 0; i--) {
                        result = new Pair(list.get(i), result);
                    }
                    return result;
                }
            }
            list.add(readExpr());
        }
    }

    private String readString() throws EvalError {
        advance(); // skip opening "
        StringBuilder sb = new StringBuilder();
        sb.append('"');
        while (pos < input.length()) {
            char c = input.charAt(pos);
            if (c == '\\') {
                advance();
                if (pos >= input.length()) throw new EvalError("unterminated string");
                char esc = input.charAt(pos);
                switch (esc) {
                    case 'n' -> sb.append('\n');
                    case 't' -> sb.append('\t');
                    case '\\' -> sb.append('\\');
                    case '"' -> sb.append('"');
                    default -> { sb.append('\\'); sb.append(esc); }
                }
                advance();
            } else if (c == '"') {
                advance();
                sb.append('"');
                return sb.toString();
            } else {
                sb.append(c);
                advance();
            }
        }
        throw new EvalError("unterminated string");
    }

    private Object readHash() throws EvalError {
        int hLine = line, hCol = col;
        advance(); // skip '#'
        if (pos >= input.length()) throw new EvalError("unexpected end after #");
        char c = input.charAt(pos);
        if (c == 't') { advance(); return Boolean.TRUE; }
        if (c == 'f') { advance(); return Boolean.FALSE; }
        if (c == '\'') {
            advance(); // skip '
            Object inner = readExpr();
            SourceList s = new SourceList(hLine, hCol);
            s.add("syntax");
            s.add(inner);
            return s;
        }
        if (c == '\\') {
            advance(); // skip '\'
            if (pos >= input.length()) throw new EvalError("unexpected end after #\\");
            // Check for named characters
            int start = pos;
            if (Character.isLetter(input.charAt(pos))) {
                while (pos < input.length() && Character.isLetter(input.charAt(pos))) advance();
                String name = input.substring(start, pos);
                if (name.length() == 1) return name.charAt(0);
                return switch (name) {
                    case "space" -> ' ';
                    case "newline" -> '\n';
                    case "tab" -> '\t';
                    default -> throw new EvalError("unknown character name: " + name);
                };
            }
            char ch = input.charAt(pos);
            advance();
            return ch;
        }
        if (c == '(') {
            // Vector literal #(...)
            advance(); // skip '('
            List<Object> elems = new java.util.ArrayList<>();
            skipWhitespace();
            while (pos < input.length() && input.charAt(pos) != ')') {
                elems.add(readExpr());
                skipWhitespace();
            }
            if (pos >= input.length()) throw new EvalError("unterminated vector literal");
            advance(); // skip ')'
            return new SchemeVector(elems.toArray());
        }
        throw new EvalError("unknown # literal: #" + c);
    }

    private Object readAtom() {
        int start = pos;
        while (pos < input.length()) {
            char c = input.charAt(pos);
            if (Character.isWhitespace(c) || c == '(' || c == ')' || c == '"' || c == ';') break;
            advance();
        }
        String token = input.substring(start, pos);
        try {
            return Long.parseLong(token);
        } catch (NumberFormatException e) {
            // Try rational n/d (but not symbols like "let/cc")
            int slash = token.indexOf('/');
            if (slash > 0 && slash < token.length() - 1) {
                try {
                    long num = Long.parseLong(token.substring(0, slash));
                    long den = Long.parseLong(token.substring(slash + 1));
                    if (den != 0) return new Rational(num, den).simplify();
                } catch (NumberFormatException ignored) {}
            }
            // Try floating point
            try {
                return Double.parseDouble(token);
            } catch (NumberFormatException e2) {
                return token;
            }
        }
    }
}
