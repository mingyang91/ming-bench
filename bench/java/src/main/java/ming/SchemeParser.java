package ming;

import static ming.Numbers.parseNumber;

import java.util.ArrayList;
import java.util.List;

class SchemeParser {

    record DottedTail(Object expr) {}

    record Pos(int line, int col) {
        String fmt() { return line + ":" + col; }
    }

    record Token(Object value, int line, int col) {}

    static class Located {
        final Object expr;
        final int line;
        final int col;
        Located(Object expr, int line, int col) {
            this.expr = expr;
            this.line = line;
            this.col = col;
        }
    }

    @SuppressWarnings("checkstyle:MethodLength")
    static List<Token> tokenize(String input) throws EvalError {
        List<Token> tokens = new ArrayList<>();
        int i = 0;
        int len = input.length();
        int line = 1, col = 1;
        while (i < len) {
            char c = input.charAt(i);
            if (Character.isWhitespace(c)) {
                if (c == '\n') { line++; col = 1; } else { col++; }
                i++;
            } else if (c == ';') {
                while (i < len && input.charAt(i) != '\n') { i++; col++; }
            } else if (c == '(') {
                tokens.add(new Token("(", line, col));
                i++; col++;
            } else if (c == ')') {
                tokens.add(new Token(")", line, col));
                i++; col++;
            } else if (c == '\'') {
                tokens.add(new Token("'", line, col));
                i++; col++;
            } else if (c == '`') {
                tokens.add(new Token("`", line, col));
                i++; col++;
            } else if (c == ',') {
                if (i + 1 < len && input.charAt(i + 1) == '@') {
                    tokens.add(new Token(",@", line, col));
                    i += 2; col += 2;
                } else {
                    tokens.add(new Token(",", line, col));
                    i++; col++;
                }
            } else if (c == '"') {
                int startLine = line, startCol = col;
                StringBuilder sb = new StringBuilder();
                i++; col++;
                while (i < len && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\') {
                        i++; col++;
                        if (i < len) {
                            char esc = input.charAt(i);
                            switch (esc) {
                                case 'n' -> sb.append('\n');
                                case 't' -> sb.append('\t');
                                case '\\' -> sb.append('\\');
                                case '"' -> sb.append('"');
                                default -> { sb.append('\\'); sb.append(esc); }
                            }
                        }
                    } else {
                        if (input.charAt(i) == '\n') { line++; col = 0; }
                        sb.append(input.charAt(i));
                    }
                    i++; col++;
                }
                if (i < len) { i++; col++; }
                tokens.add(new Token(new SchemeString(sb.toString(), true), startLine, startCol));
            } else if (c == '#') {
                int startCol = col;
                if (i + 1 < len) {
                    char next = input.charAt(i + 1);
                    if (next == 't') {
                        tokens.add(new Token(Boolean.TRUE, line, startCol));
                        i += 2; col += 2;
                    } else if (next == 'f') {
                        tokens.add(new Token(Boolean.FALSE, line, startCol));
                        i += 2; col += 2;
                    } else if (next == '\\') {
                        i += 2; col += 2;
                        if (i >= len) throw new EvalError("unexpected end after #\\ at " + line + ":" + startCol);
                        StringBuilder charName = new StringBuilder();
                        while (i < len && !Character.isWhitespace(input.charAt(i))
                                && input.charAt(i) != '(' && input.charAt(i) != ')'
                                && input.charAt(i) != '"' && input.charAt(i) != ';') {
                            charName.append(input.charAt(i));
                            i++; col++;
                        }
                        String cn = charName.toString();
                        char ch;
                        if (cn.length() == 1) {
                            ch = cn.charAt(0);
                        } else if (cn.equals("space")) {
                            ch = ' ';
                        } else if (cn.equals("newline")) {
                            ch = '\n';
                        } else if (cn.equals("tab")) {
                            ch = '\t';
                        } else {
                            throw new EvalError("unknown character name: " + cn + " at " + line + ":" + startCol);
                        }
                        tokens.add(new Token(new SchemeChar(ch), line, startCol));
                    } else if (next == '\'') {
                        tokens.add(new Token("#'", line, startCol));
                        i += 2; col += 2;
                    } else if (next == '(') {
                        tokens.add(new Token("#(", line, startCol));
                        i += 2; col += 2;
                    } else {
                        throw new EvalError("unexpected token: #" + next + " at " + line + ":" + startCol);
                    }
                } else {
                    throw new EvalError("unexpected end after # at " + line + ":" + startCol);
                }
            } else {
                int startCol = col;
                StringBuilder sb = new StringBuilder();
                while (i < len && !Character.isWhitespace(input.charAt(i))
                        && input.charAt(i) != '(' && input.charAt(i) != ')'
                        && input.charAt(i) != '"' && input.charAt(i) != ';'
                        && input.charAt(i) != '\'') {
                    sb.append(input.charAt(i));
                    i++; col++;
                }
                String tok = sb.toString();
                Object parsed = parseNumber(tok);
                if (parsed != null) {
                    tokens.add(new Token(parsed, line, startCol));
                } else {
                    tokens.add(new Token(tok, line, startCol));
                }
            }
        }
        return tokens;
    }

    static Object parse(List<Token> tokens, int[] pos) throws EvalError {
        if (pos[0] >= tokens.size()) {
            throw new EvalError("unexpected end of input");
        }
        Token tok = tokens.get(pos[0]);
        Object token = tok.value();
        int tLine = tok.line(), tCol = tok.col();
        if (token.equals("'")) {
            pos[0]++;
            Object datum = parse(tokens, pos);
            List<Object> quoted = new ArrayList<>();
            quoted.add("quote");
            quoted.add(datum instanceof Located loc ? loc.expr : datum);
            return new Located(quoted, tLine, tCol);
        }
        if (token.equals("`")) {
            pos[0]++;
            Object datum = parse(tokens, pos);
            List<Object> qq = new ArrayList<>();
            qq.add("quasiquote");
            qq.add(datum instanceof Located loc ? loc.expr : datum);
            return new Located(qq, tLine, tCol);
        }
        if (token.equals(",")) {
            pos[0]++;
            Object datum = parse(tokens, pos);
            List<Object> uq = new ArrayList<>();
            uq.add("unquote");
            uq.add(datum instanceof Located loc ? loc.expr : datum);
            return new Located(uq, tLine, tCol);
        }
        if (token.equals(",@")) {
            pos[0]++;
            Object datum = parse(tokens, pos);
            List<Object> uqs = new ArrayList<>();
            uqs.add("unquote-splicing");
            uqs.add(datum instanceof Located loc ? loc.expr : datum);
            return new Located(uqs, tLine, tCol);
        }
        if (token.equals("#'")) {
            pos[0]++;
            Object datum = parse(tokens, pos);
            List<Object> syntaxForm = new ArrayList<>();
            syntaxForm.add("syntax");
            syntaxForm.add(datum instanceof Located loc ? loc.expr : datum);
            return new Located(syntaxForm, tLine, tCol);
        }
        if (token.equals("#(")) {
            pos[0]++;
            List<Object> elems = new ArrayList<>();
            elems.add(new Located("vector", tLine, tCol));
            while (pos[0] < tokens.size() && !tokens.get(pos[0]).value().equals(")")) {
                elems.add(parse(tokens, pos));
            }
            if (pos[0] >= tokens.size()) {
                throw new EvalError("missing closing paren for vector at " + tLine + ":" + tCol);
            }
            pos[0]++;
            return new Located(elems, tLine, tCol);
        }
        if (token.equals("(")) {
            pos[0]++;
            List<Object> list = new ArrayList<>();
            boolean dotted = false;
            while (pos[0] < tokens.size() && !tokens.get(pos[0]).value().equals(")")) {
                // Check for dot notation: (a b . c)
                if (pos[0] < tokens.size() && tokens.get(pos[0]).value() instanceof String s && s.equals(".")
                        && !list.isEmpty()) {
                    int savedPos = pos[0];
                    pos[0]++; // skip the dot
                    if (pos[0] < tokens.size() && !tokens.get(pos[0]).value().equals(")")) {
                        Object tail = parse(tokens, pos);
                        // Verify: next token must be ) for valid dotted pair
                        if (pos[0] < tokens.size() && tokens.get(pos[0]).value().equals(")")) {
                            list.add(new DottedTail(tail));
                            dotted = true;
                            break;
                        } else {
                            // Not a valid dotted pair (more than one expr after dot)
                            // Restore and parse normally: add dot as symbol, and the parsed tail too
                            pos[0] = savedPos;
                            list.add(parse(tokens, pos)); // parse the "." as a symbol
                        }
                    } else {
                        // Not a dotted pair (nothing after dot before close), parse dot as symbol
                        pos[0] = savedPos;
                        list.add(parse(tokens, pos));
                    }
                } else {
                    list.add(parse(tokens, pos));
                }
            }
            if (pos[0] >= tokens.size()) {
                throw new EvalError("missing closing paren at " + tLine + ":" + tCol);
            }
            pos[0]++;
            return new Located(list, tLine, tCol);
        } else if (token.equals(")")) {
            throw new EvalError("unexpected ) at " + tLine + ":" + tCol);
        } else {
            pos[0]++;
            return new Located(token, tLine, tCol);
        }
    }
}
