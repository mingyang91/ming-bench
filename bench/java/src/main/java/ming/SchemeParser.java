package ming;

import java.util.ArrayList;
import java.util.List;

final class SchemeParser {

    private int pos;

    List<Object> parse(String input) throws EvalError {
        List<Token> tokens = tokenize(input);
        pos = 0;
        List<Object> exprs = new ArrayList<>();
        while (pos < tokens.size()) exprs.add(parseExpr(tokens));
        return exprs;
    }

    // ---- Token ----
    private static final class Token {
        final String value;
        final int line;
        final int col;
        Token(String value, int line, int col) { this.value = value; this.line = line; this.col = col; }
    }

    // ---- Tokenizer ----
    private List<Token> tokenize(String input) {
        List<Token> tokens = new ArrayList<>();
        int i = 0, len = input.length(), line = 1, col = 1;
        while (i < len) {
            char c = input.charAt(i);
            if (c == '\n') { i++; line++; col = 1; continue; }
            if (Character.isWhitespace(c)) { i++; col++; continue; }
            if (c == ';') { while (i < len && input.charAt(i) != '\n') { i++; col++; } continue; }
            int startLine = line, startCol = col;
            if (c == '#' && i + 1 < len && input.charAt(i + 1) == '\'') {
                tokens.add(new Token("#'", startLine, startCol)); i += 2; col += 2; continue;
            }
            if (c == '#' && i + 1 < len && input.charAt(i + 1) == '(') {
                tokens.add(new Token("#(", startLine, startCol)); i += 2; col += 2; continue;
            }
            if (c == '(') { tokens.add(new Token("(", startLine, startCol)); i++; col++; continue; }
            if (c == ')') { tokens.add(new Token(")", startLine, startCol)); i++; col++; continue; }
            if (c == '\'') { tokens.add(new Token("'", startLine, startCol)); i++; col++; continue; }
            if (c == '`') { tokens.add(new Token("`", startLine, startCol)); i++; col++; continue; }
            if (c == ',') {
                if (i + 1 < len && input.charAt(i + 1) == '@') {
                    tokens.add(new Token(",@", startLine, startCol)); i += 2; col += 2;
                } else {
                    tokens.add(new Token(",", startLine, startCol)); i++; col++;
                }
                continue;
            }
            if (c == '"') {
                StringBuilder sb = new StringBuilder();
                sb.append('"'); i++; col++;
                while (i < len && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\') {
                        sb.append(input.charAt(i)); i++; col++;
                        if (i < len) { sb.append(input.charAt(i)); i++; col++; }
                    } else {
                        if (input.charAt(i) == '\n') { line++; col = 1; } else { col++; }
                        sb.append(input.charAt(i)); i++;
                    }
                }
                if (i < len) { sb.append('"'); i++; col++; }
                tokens.add(new Token(sb.toString(), startLine, startCol));
                continue;
            }
            StringBuilder sb = new StringBuilder();
            while (i < len) {
                char ch = input.charAt(i);
                if (Character.isWhitespace(ch) || ch == '(' || ch == ')' || ch == '"' || ch == ';') break;
                sb.append(ch); i++; col++;
            }
            tokens.add(new Token(sb.toString(), startLine, startCol));
        }
        return tokens;
    }

    // ---- Parser ----
    private Object parseExpr(List<Token> tokens) throws EvalError {
        if (pos >= tokens.size()) throw new EvalError("unexpected end of input");
        Token token = tokens.get(pos++);
        if (token.value.equals("#(")) {
            List<Object> elems = new ArrayList<>();
            while (pos < tokens.size() && !tokens.get(pos).value.equals(")")) elems.add(parseExpr(tokens));
            if (pos >= tokens.size()) throw new EvalError("missing closing parenthesis");
            pos++;
            List<Object> list = new ArrayList<>();
            list.add("vector");
            list.addAll(elems);
            return new Evaluator.Located(list, token.line, token.col);
        }
        if (token.value.equals("(")) {
            List<Object> elems = new ArrayList<>();
            Object dotTail = null;
            boolean dotted = false;
            while (pos < tokens.size() && !tokens.get(pos).value.equals(")")) {
                if (pos < tokens.size() && tokens.get(pos).value.equals(".")
                    && pos + 1 < tokens.size() && !tokens.get(pos + 1).value.equals(")")) {
                    int savedPos = pos;
                    pos++;
                    Object tail = parseExpr(tokens);
                    if (pos < tokens.size() && tokens.get(pos).value.equals(")")) {
                        dotTail = tail;
                        dotted = true;
                        break;
                    } else {
                        pos = savedPos;
                        elems.add(parseExpr(tokens));
                    }
                } else {
                    elems.add(parseExpr(tokens));
                }
            }
            if (pos >= tokens.size()) throw new EvalError("missing closing parenthesis");
            pos++;
            if (dotted) {
                Object result = dotTail;
                for (int i = elems.size() - 1; i >= 0; i--) {
                    result = new Evaluator.Pair(elems.get(i), result);
                }
                return new Evaluator.Located(result, token.line, token.col);
            }
            return new Evaluator.Located(elems, token.line, token.col);
        }
        if (token.value.equals(")")) throw new EvalError("unexpected )");
        if (token.value.equals("'")) {
            Object quoted = parseExpr(tokens);
            List<Object> q = new ArrayList<>();
            q.add("quote"); q.add(quoted);
            return new Evaluator.Located(q, token.line, token.col);
        }
        if (token.value.equals("#'")) {
            Object syntaxed = parseExpr(tokens);
            List<Object> q = new ArrayList<>();
            q.add("syntax"); q.add(syntaxed);
            return new Evaluator.Located(q, token.line, token.col);
        }
        if (token.value.equals("`")) {
            Object expr = parseExpr(tokens);
            List<Object> q = new ArrayList<>();
            q.add("quasiquote"); q.add(expr);
            return new Evaluator.Located(q, token.line, token.col);
        }
        if (token.value.equals(",")) {
            Object expr = parseExpr(tokens);
            List<Object> q = new ArrayList<>();
            q.add("unquote"); q.add(expr);
            return new Evaluator.Located(q, token.line, token.col);
        }
        if (token.value.equals(",@")) {
            Object expr = parseExpr(tokens);
            List<Object> q = new ArrayList<>();
            q.add("unquote-splicing"); q.add(expr);
            return new Evaluator.Located(q, token.line, token.col);
        }
        Object atom = parseAtom(token.value);
        return new Evaluator.Located(atom, token.line, token.col);
    }

    private Object parseAtom(String token) {
        if (token.equals("#t")) return Boolean.TRUE;
        if (token.equals("#f")) return Boolean.FALSE;
        if (token.startsWith("#\\")) {
            String charName = token.substring(2);
            if (charName.equals("space")) return new Evaluator.SchemeChar(' ');
            if (charName.equals("newline")) return new Evaluator.SchemeChar('\n');
            if (charName.equals("tab")) return new Evaluator.SchemeChar('\t');
            if (charName.length() == 1) return new Evaluator.SchemeChar(charName.charAt(0));
            throw new RuntimeException("unknown character literal: " + token);
        }
        if (token.startsWith("\"") && token.endsWith("\"")) {
            String s = token.substring(1, token.length() - 1);
            s = s.replace("\\n", "\n").replace("\\t", "\t").replace("\\\\", "\\").replace("\\\"", "\"");
            return new Evaluator.SchemeString(s);
        }
        try { return Long.parseLong(token); } catch (NumberFormatException ignored) {}
        int slashIdx = token.indexOf('/');
        if (slashIdx > 0 && slashIdx < token.length() - 1) {
            try {
                long numer = Long.parseLong(token.substring(0, slashIdx));
                long denom = Long.parseLong(token.substring(slashIdx + 1));
                if (denom != 0) return Evaluator.makeRational(numer, denom);
            } catch (NumberFormatException ignored) {}
        }
        try { return Double.parseDouble(token); } catch (NumberFormatException ignored) {}
        return token;
    }
}
