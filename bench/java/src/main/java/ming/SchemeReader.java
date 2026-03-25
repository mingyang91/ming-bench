package ming;

import java.util.ArrayList;
import java.util.List;


/**
 * Tokenizer and parser for Scheme source text.
 */
final class SchemeReader {

    private int lineNum;
    private int colNum;

    private Pos posAt(int line, int col) { return new Pos(line, col); }

    private boolean isDelimiter(char c) {
        return Character.isWhitespace(c) || c == '(' || c == ')' || c == '"' || c == ';' || c == '\'';
    }

    List<Token> tokenize(String input) throws EvalError {
        List<Token> tokens = new ArrayList<>();
        int i = 0;
        lineNum = 1;
        colNum = 1;
        while (i < input.length()) {
            char c = input.charAt(i);
            if (c == '\n') {
                lineNum++;
                colNum = 1;
                i++;
            } else if (Character.isWhitespace(c)) {
                colNum++;
                i++;
            } else if (c == ';') {
                while (i < input.length() && input.charAt(i) != '\n') { i++; colNum++; }
            } else if (c == '\'') {
                tokens.add(new Token("'", posAt(lineNum, colNum)));
                i++; colNum++;
            } else if (c == '(') {
                tokens.add(new Token("(", posAt(lineNum, colNum)));
                i++; colNum++;
            } else if (c == ')') {
                tokens.add(new Token(")", posAt(lineNum, colNum)));
                i++; colNum++;
            } else if (c == '"') {
                i = tokenizeString(input, i, tokens);
            } else if (c == '#') {
                i = tokenizeHash(input, i, tokens);
            } else {
                Pos tokPos = posAt(lineNum, colNum);
                StringBuilder sb = new StringBuilder();
                while (i < input.length()) {
                    char ch = input.charAt(i);
                    if (Character.isWhitespace(ch) || ch == '(' || ch == ')' || ch == '"' || ch == ';' || ch == '\'') break;
                    sb.append(ch);
                    i++; colNum++;
                }
                String tok = sb.toString();
                Object parsed = parseNumber(tok);
                if (parsed != null) {
                    tokens.add(new Token(parsed, tokPos));
                } else {
                    tokens.add(new Token(tok, tokPos));
                }
            }
        }
        return tokens;
    }

    private int tokenizeString(String input, int i, List<Token> tokens) throws EvalError {
        Pos strPos = posAt(lineNum, colNum);
        StringBuilder sb = new StringBuilder();
        i++; colNum++;
        while (i < input.length() && input.charAt(i) != '"') {
            if (input.charAt(i) == '\\') {
                i++; colNum++;
                if (i < input.length()) {
                    switch (input.charAt(i)) {
                        case 'n' -> sb.append('\n');
                        case 't' -> sb.append('\t');
                        case '\\' -> sb.append('\\');
                        case '"' -> sb.append('"');
                        default -> { sb.append('\\'); sb.append(input.charAt(i)); }
                    }
                }
            } else {
                sb.append(input.charAt(i));
            }
            if (input.charAt(i) == '\n') { lineNum++; colNum = 1; } else { colNum++; }
            i++;
        }
        if (i < input.length()) { i++; colNum++; }
        tokens.add(new Token(new SchemeString(sb.toString(), true), strPos));
        return i;
    }

    private int tokenizeHash(String input, int i, List<Token> tokens) throws EvalError {
        Pos hPos = posAt(lineNum, colNum);
        if (i + 1 < input.length()) {
            char next = input.charAt(i + 1);
            if (next == 't') {
                if (i + 2 >= input.length() || isDelimiter(input.charAt(i + 2))) {
                    tokens.add(new Token(Boolean.TRUE, hPos));
                    i += 2; colNum += 2;
                } else {
                    throw new EvalError("unexpected #" + next + " at " + hPos);
                }
            } else if (next == 'f') {
                if (i + 2 >= input.length() || isDelimiter(input.charAt(i + 2))) {
                    tokens.add(new Token(Boolean.FALSE, hPos));
                    i += 2; colNum += 2;
                } else {
                    throw new EvalError("unexpected #" + next + " at " + hPos);
                }
            } else if (next == '\\') {
                i += 2; colNum += 2;
                if (i >= input.length()) throw new EvalError("unexpected end after #\\ at " + hPos);
                if (i + 4 < input.length() && input.substring(i, i + 5).equals("space") &&
                        (i + 5 >= input.length() || isDelimiter(input.charAt(i + 5)))) {
                    tokens.add(new Token(new SchemeChar(' '), hPos));
                    i += 5; colNum += 5;
                } else if (i + 6 < input.length() && input.substring(i, i + 7).equals("newline") &&
                        (i + 7 >= input.length() || isDelimiter(input.charAt(i + 7)))) {
                    tokens.add(new Token(new SchemeChar('\n'), hPos));
                    i += 7; colNum += 7;
                } else if (i + 2 < input.length() && input.substring(i, i + 3).equals("tab") &&
                        (i + 3 >= input.length() || isDelimiter(input.charAt(i + 3)))) {
                    tokens.add(new Token(new SchemeChar('\t'), hPos));
                    i += 3; colNum += 3;
                } else {
                    tokens.add(new Token(new SchemeChar(input.charAt(i)), hPos));
                    i++; colNum++;
                }
            } else if (next == '\'') {
                tokens.add(new Token("#'", hPos));
                i += 2; colNum += 2;
            } else {
                throw new EvalError("unexpected #" + next + " at " + hPos);
            }
        } else {
            throw new EvalError("unexpected end after # at " + hPos);
        }
        return i;
    }

    Object parseNumber(String tok) {
        try { return Long.parseLong(tok); } catch (NumberFormatException ignored) {}
        int slash = tok.indexOf('/');
        if (slash > 0 && slash < tok.length() - 1) {
            try {
                long num = Long.parseLong(tok.substring(0, slash));
                long den = Long.parseLong(tok.substring(slash + 1));
                if (den == 0) return null;
                return new SchemeRational(num, den);
            } catch (NumberFormatException ignored) {}
        }
        try {
            double d = Double.parseDouble(tok);
            if (!Double.isInfinite(d) && !Double.isNaN(d)) return d;
        } catch (NumberFormatException ignored) {}
        return null;
    }

    Object parse(List<Token> tokens, int[] pos) throws EvalError {
        if (pos[0] >= tokens.size()) {
            throw new EvalError("unexpected end of input");
        }
        Token token = tokens.get(pos[0]);
        if ("#'".equals(token.value())) {
            pos[0]++;
            Object inner = parse(tokens, pos);
            SExpr syntaxExpr = new SExpr(token.pos());
            syntaxExpr.add("syntax");
            syntaxExpr.add(inner);
            return syntaxExpr;
        }
        if ("'".equals(token.value())) {
            pos[0]++;
            Object quoted = parse(tokens, pos);
            SExpr quoteExpr = new SExpr(token.pos());
            quoteExpr.add("quote");
            quoteExpr.add(quoted);
            return quoteExpr;
        }
        if ("(".equals(token.value())) {
            pos[0]++;
            SExpr list = new SExpr(token.pos());
            while (pos[0] < tokens.size() && !")".equals(tokens.get(pos[0]).value())) {
                list.add(parse(tokens, pos));
            }
            if (pos[0] >= tokens.size()) {
                throw new EvalError("missing closing paren at " + token.pos());
            }
            pos[0]++;
            return list;
        } else if (")".equals(token.value())) {
            throw new EvalError("unexpected ) at " + token.pos());
        } else {
            pos[0]++;
            return token;
        }
    }
}
