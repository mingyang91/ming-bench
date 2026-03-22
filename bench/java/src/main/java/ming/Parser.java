package ming;

import java.util.ArrayList;
import java.util.List;

public class Parser {
    private final String input;
    private int pos;
    private int line;
    private int col;

    public Parser(String input) {
        this.input = input;
        this.pos = 0;
        this.line = 1;
        this.col = 1;
    }

    public List<SchemeValue> parseAll() throws EvalError {
        var exprs = new ArrayList<SchemeValue>();
        skipWhitespace();
        while (pos < input.length()) {
            exprs.add(parseExpr());
            skipWhitespace();
        }
        if (exprs.isEmpty()) {
            throw new EvalError("empty input");
        }
        return exprs;
    }

    private char advance() {
        char c = input.charAt(pos);
        pos++;
        if (c == '\n') {
            line++;
            col = 1;
        } else {
            col++;
        }
        return c;
    }

    private SourcePos currentPos() {
        return new SourcePos(line, col);
    }

    private SchemeValue parseExpr() throws EvalError {
        skipWhitespace();
        if (pos >= input.length()) {
            throw new EvalError("unexpected end of input");
        }
        char c = input.charAt(pos);
        if (c == '(') {
            return parseList();
        } else if (c == '"') {
            return parseString();
        } else if (c == '#') {
            return parseHash();
        } else if (c == '\'') {
            SourcePos sp = currentPos();
            advance();
            var quoted = parseExpr();
            return new SchemeValue.ListVal(List.of(new SchemeValue.SymbolVal("quote"), quoted)).withPos(sp);
        } else {
            return parseAtom();
        }
    }

    private SchemeValue parseList() throws EvalError {
        SourcePos sp = currentPos();
        advance(); // skip '('
        var elements = new ArrayList<SchemeValue>();
        skipWhitespace();
        while (pos < input.length() && input.charAt(pos) != ')') {
            elements.add(parseExpr());
            skipWhitespace();
        }
        if (pos >= input.length()) {
            throw new EvalError("unmatched '(' at " + sp);
        }
        advance(); // skip ')'
        return new SchemeValue.ListVal(elements).withPos(sp);
    }

    private SchemeValue parseString() throws EvalError {
        SourcePos sp = currentPos();
        advance(); // skip opening '"'
        var sb = new StringBuilder();
        while (pos < input.length() && input.charAt(pos) != '"') {
            if (input.charAt(pos) == '\\') {
                advance();
                if (pos >= input.length()) throw new EvalError("unterminated string at " + sp);
                char esc = input.charAt(pos);
                switch (esc) {
                    case 'n' -> sb.append('\n');
                    case 't' -> sb.append('\t');
                    case '\\' -> sb.append('\\');
                    case '"' -> sb.append('"');
                    default -> { sb.append('\\'); sb.append(esc); }
                }
            } else {
                sb.append(input.charAt(pos));
            }
            advance();
        }
        if (pos >= input.length()) {
            throw new EvalError("unterminated string at " + sp);
        }
        advance(); // skip closing '"'
        return new SchemeValue.StringVal(sb.toString()).withPos(sp);
    }

    private SchemeValue parseHash() throws EvalError {
        SourcePos sp = currentPos();
        advance(); // skip '#'
        if (pos >= input.length()) throw new EvalError("unexpected end after # at " + sp);
        char c = input.charAt(pos);
        if (c == 't') {
            advance();
            return new SchemeValue.BoolVal(true).withPos(sp);
        } else if (c == 'f') {
            advance();
            return new SchemeValue.BoolVal(false).withPos(sp);
        } else {
            throw new EvalError("unknown hash literal: #" + c + " at " + sp);
        }
    }

    private SchemeValue parseAtom() throws EvalError {
        SourcePos sp = currentPos();
        int start = pos;
        while (pos < input.length() && !isDelimiter(input.charAt(pos))) {
            advance();
        }
        String token = input.substring(start, pos);
        if (token.isEmpty()) {
            throw new EvalError("unexpected character: " + input.charAt(pos) + " at " + sp);
        }
        // Try parsing as integer
        try {
            long val = Long.parseLong(token);
            return new SchemeValue.IntVal(val).withPos(sp);
        } catch (NumberFormatException e) {
            return new SchemeValue.SymbolVal(token).withPos(sp);
        }
    }

    private void skipWhitespace() {
        while (pos < input.length()) {
            char c = input.charAt(pos);
            if (c == ';') {
                // skip line comment
                while (pos < input.length() && input.charAt(pos) != '\n') advance();
            } else if (Character.isWhitespace(c)) {
                advance();
            } else {
                break;
            }
        }
    }

    private boolean isDelimiter(char c) {
        return Character.isWhitespace(c) || c == '(' || c == ')' || c == '"' || c == ';';
    }
}
