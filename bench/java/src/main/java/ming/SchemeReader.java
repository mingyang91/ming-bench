package ming;

import java.util.ArrayList;
import java.util.List;

/**
 * Tokenizer and parser for Scheme source text.
 * Converts raw input strings into S-expression trees.
 */
class SchemeReader {

    record Token(String value, int line, int col) {}

    record Located(Object expr, int line, int col) {}

    static Object unwrap(Object o) {
        return o instanceof Located loc ? loc.expr() : o;
    }

    @SuppressWarnings("unchecked")
    static Object deepUnwrap(Object o) {
        o = unwrap(o);
        if (o instanceof List<?> list) {
            List<Object> result = new ArrayList<>(list.size());
            for (Object elem : list) result.add(deepUnwrap(elem));
            return result;
        }
        return o;
    }

    List<Token> tokenize(String input) throws EvalError {
        List<Token> tokens = new ArrayList<>();
        int i = 0;
        int len = input.length();
        int line = 1;
        int col = 1;
        while (i < len) {
            char c = input.charAt(i);
            if (c == '\n') {
                line++;
                col = 1;
                i++;
            } else if (Character.isWhitespace(c)) {
                col++;
                i++;
            } else if (c == ';') {
                while (i < len && input.charAt(i) != '\n') {
                    i++;
                    col++;
                }
            } else if (c == '\'') {
                tokens.add(new Token("'", line, col));
                i++;
                col++;
            } else if (c == '`') {
                tokens.add(new Token("`", line, col));
                i++;
                col++;
            } else if (c == ',') {
                if (i + 1 < len && input.charAt(i + 1) == '@') {
                    tokens.add(new Token(",@", line, col));
                    i += 2;
                    col += 2;
                } else {
                    tokens.add(new Token(",", line, col));
                    i++;
                    col++;
                }
            } else if (c == '(') {
                tokens.add(new Token("(", line, col));
                i++;
                col++;
            } else if (c == ')') {
                tokens.add(new Token(")", line, col));
                i++;
                col++;
            } else if (c == '"') {
                int startLine = line;
                int startCol = col;
                StringBuilder sb = new StringBuilder();
                sb.append('"');
                i++;
                col++;
                while (i < len && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\') {
                        sb.append(input.charAt(i));
                        i++;
                        col++;
                        if (i < len) {
                            sb.append(input.charAt(i));
                            i++;
                            col++;
                        }
                    } else {
                        if (input.charAt(i) == '\n') {
                            line++;
                            col = 1;
                        } else {
                            col++;
                        }
                        sb.append(input.charAt(i));
                        i++;
                    }
                }
                if (i < len) {
                    sb.append('"');
                    i++;
                    col++;
                }
                tokens.add(new Token(sb.toString(), startLine, startCol));
            } else if (c == '#') {
                int startCol = col;
                if (i + 1 < len) {
                    char next = input.charAt(i + 1);
                    if (next == 't') {
                        tokens.add(new Token("#t", line, startCol));
                        i += 2;
                        col += 2;
                    } else if (next == 'f') {
                        tokens.add(new Token("#f", line, startCol));
                        i += 2;
                        col += 2;
                    } else if (next == '\\') {
                        if (i + 2 < len) {
                            int charStart = i + 2;
                            int charEnd = charStart;
                            while (charEnd < len && !Character.isWhitespace(input.charAt(charEnd))
                                    && input.charAt(charEnd) != ')' && input.charAt(charEnd) != '('
                                    && input.charAt(charEnd) != '"' && input.charAt(charEnd) != ';') {
                                charEnd++;
                            }
                            String charName = input.substring(charStart, charEnd);
                            String tok = "#\\" + charName;
                            tokens.add(new Token(tok, line, startCol));
                            int tokLen = tok.length();
                            i += tokLen;
                            col += tokLen;
                        } else {
                            tokens.add(new Token("#\\", line, startCol));
                            i += 2;
                            col += 2;
                        }
                    } else if (next == '(') {
                        tokens.add(new Token("#(", line, startCol));
                        i += 2;
                        col += 2;
                    } else if (next == '\'') {
                        tokens.add(new Token("#'", line, startCol));
                        i += 2;
                        col += 2;
                    } else {
                        String sym = readSymbol(input, i);
                        tokens.add(new Token(sym, line, startCol));
                        i += sym.length();
                        col += sym.length();
                    }
                } else {
                    tokens.add(new Token("#", line, startCol));
                    i++;
                    col++;
                }
            } else {
                int startCol = col;
                String sym = readSymbol(input, i);
                tokens.add(new Token(sym, line, startCol));
                i += sym.length();
                col += sym.length();
            }
        }
        return tokens;
    }

    private String readSymbol(String input, int start) {
        int i = start;
        int len = input.length();
        while (i < len) {
            char c = input.charAt(i);
            if (Character.isWhitespace(c) || c == '(' || c == ')' || c == '"' || c == ';' || c == '\'') {
                break;
            }
            i++;
        }
        return input.substring(start, i);
    }

    Object parse(List<Token> tokens, int[] pos) throws EvalError {
        if (pos[0] >= tokens.size()) {
            throw new EvalError("unexpected end of input");
        }
        Token token = tokens.get(pos[0]);
        pos[0]++;

        if (token.value().equals("'")) {
            Object quoted = parse(tokens, pos);
            List<Object> quoteExpr = new ArrayList<>();
            quoteExpr.add(new SchemeSymbol("quote"));
            quoteExpr.add(quoted);
            return new Located(quoteExpr, token.line(), token.col());
        }

        if (token.value().equals("`")) {
            Object body = parse(tokens, pos);
            List<Object> expr = new ArrayList<>();
            expr.add(new SchemeSymbol("quasiquote"));
            expr.add(body);
            return new Located(expr, token.line(), token.col());
        }

        if (token.value().equals(",")) {
            Object body = parse(tokens, pos);
            List<Object> expr = new ArrayList<>();
            expr.add(new SchemeSymbol("unquote"));
            expr.add(body);
            return new Located(expr, token.line(), token.col());
        }

        if (token.value().equals(",@")) {
            Object body = parse(tokens, pos);
            List<Object> expr = new ArrayList<>();
            expr.add(new SchemeSymbol("unquote-splicing"));
            expr.add(body);
            return new Located(expr, token.line(), token.col());
        }

        if (token.value().equals("#'")) {
            Object syntaxed = parse(tokens, pos);
            List<Object> syntaxExpr = new ArrayList<>();
            syntaxExpr.add(new SchemeSymbol("syntax"));
            syntaxExpr.add(syntaxed);
            return new Located(syntaxExpr, token.line(), token.col());
        }

        if (token.value().equals("#(")) {
            List<Object> elems = new ArrayList<>();
            while (pos[0] < tokens.size() && !tokens.get(pos[0]).value().equals(")")) {
                elems.add(parse(tokens, pos));
            }
            if (pos[0] >= tokens.size()) throw new EvalError("missing closing parenthesis");
            pos[0]++;
            List<Object> vecForm = new ArrayList<>();
            vecForm.add(new SchemeSymbol("vector"));
            vecForm.addAll(elems);
            return new Located(vecForm, token.line(), token.col());
        }

        if (token.value().equals("(")) {
            List<Object> list = new ArrayList<>();
            while (pos[0] < tokens.size() && !tokens.get(pos[0]).value().equals(")")) {
                list.add(parse(tokens, pos));
            }
            if (pos[0] >= tokens.size()) {
                throw new EvalError("missing closing parenthesis");
            }
            pos[0]++;
            return new Located(list, token.line(), token.col());
        } else if (token.value().equals(")")) {
            throw new EvalError("unexpected )");
        } else {
            return new Located(parseAtom(token.value()), token.line(), token.col());
        }
    }

    private Object parseAtom(String token) {
        if (token.equals("#t")) {
            return Boolean.TRUE;
        }
        if (token.equals("#f")) {
            return Boolean.FALSE;
        }
        if (token.startsWith("#\\")) {
            String charName = token.substring(2);
            if (charName.equals("space")) return new SchemeChar(' ');
            if (charName.equals("newline")) return new SchemeChar('\n');
            if (charName.equals("tab")) return new SchemeChar('\t');
            if (charName.length() == 1) return new SchemeChar(charName.charAt(0));
            return new SchemeChar(charName.charAt(0));
        }
        if (token.startsWith("\"") && token.endsWith("\"")) {
            return token;
        }
        try {
            return Long.parseLong(token);
        } catch (NumberFormatException e) {}
        int slash = token.indexOf('/');
        if (slash > 0 && slash < token.length() - 1) {
            try {
                long num = Long.parseLong(token.substring(0, slash));
                long den = Long.parseLong(token.substring(slash + 1));
                return SchemeRational.make(num, den);
            } catch (NumberFormatException e) {}
        }
        try {
            return Double.parseDouble(token);
        } catch (NumberFormatException e) {}
        return new SchemeSymbol(token);
    }
}
