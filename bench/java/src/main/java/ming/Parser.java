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

    private SourcePos currentSourcePos() {
        int line = 1, col = 1;
        for (int i = 0; i < pos; i++) {
            if (input.charAt(i) == '\n') { line++; col = 1; }
            else col++;
        }
        return new SourcePos(line, col);
    }

    public List<SchemeValue> parseAll() throws EvalError {
        List<SchemeValue> exprs = new ArrayList<>();
        skipWhitespace();
        while (pos < input.length()) {
            exprs.add(parseExpr());
            skipWhitespace();
        }
        return exprs;
    }

    private SchemeValue parseExpr() throws EvalError {
        skipWhitespace();
        if (pos >= input.length()) throw new EvalError("unexpected end of input");

        char c = input.charAt(pos);

        if (c == '\'') {
            SourcePos sp = currentSourcePos();
            pos++; // skip quote char
            SchemeValue quoted = parseExpr();
            return new SchemeValue.ListVal(List.of(new SchemeValue.SymbolVal("quote", sp), quoted), sp);
        } else if (c == '(') {
            return parseList();
        } else if (c == '"') {
            return parseString();
        } else if (c == '#') {
            return parseHash();
        } else {
            return parseAtom();
        }
    }

    private SchemeValue parseList() throws EvalError {
        SourcePos sp = currentSourcePos();
        pos++; // skip '('
        List<SchemeValue> elements = new ArrayList<>();
        skipWhitespace();
        while (pos < input.length() && input.charAt(pos) != ')') {
            elements.add(parseExpr());
            skipWhitespace();
        }
        if (pos >= input.length()) throw new EvalError("unterminated list");
        pos++; // skip ')'
        return new SchemeValue.ListVal(elements, sp);
    }

    private SchemeValue parseString() throws EvalError {
        SourcePos sp = currentSourcePos();
        pos++; // skip opening '"'
        var sb = new StringBuilder();
        while (pos < input.length() && input.charAt(pos) != '"') {
            if (input.charAt(pos) == '\\') {
                pos++;
                if (pos >= input.length()) throw new EvalError("unterminated string");
                char esc = input.charAt(pos);
                switch (esc) {
                    case 'n' -> sb.append('\n');
                    case 't' -> sb.append('\t');
                    case '"' -> sb.append('"');
                    case '\\' -> sb.append('\\');
                    default -> { sb.append('\\'); sb.append(esc); }
                }
            } else {
                sb.append(input.charAt(pos));
            }
            pos++;
        }
        if (pos >= input.length()) throw new EvalError("unterminated string");
        pos++; // skip closing '"'
        return new SchemeValue.StringVal(sb.toString(), sp);
    }

    private SchemeValue parseHash() throws EvalError {
        SourcePos sp = currentSourcePos();
        pos++; // skip '#'
        if (pos >= input.length()) throw new EvalError("unexpected end after #");
        char c = input.charAt(pos);
        pos++;
        return switch (c) {
            case 't' -> new SchemeValue.BoolVal(true, sp);
            case 'f' -> new SchemeValue.BoolVal(false, sp);
            default -> throw new EvalError("unknown hash literal: #" + c);
        };
    }

    private SchemeValue parseAtom() throws EvalError {
        SourcePos sp = currentSourcePos();
        int start = pos;
        while (pos < input.length() && !isDelimiter(input.charAt(pos))) {
            pos++;
        }
        String token = input.substring(start, pos);
        if (token.isEmpty()) throw new EvalError("unexpected character: " + input.charAt(pos));

        // Try integer
        try {
            return new SchemeValue.IntVal(Long.parseLong(token), sp);
        } catch (NumberFormatException ignored) {}

        // Otherwise it's a symbol
        return new SchemeValue.SymbolVal(token, sp);
    }

    private void skipWhitespace() {
        while (pos < input.length()) {
            char c = input.charAt(pos);
            if (c == ';') {
                // Skip line comment
                while (pos < input.length() && input.charAt(pos) != '\n') pos++;
            } else if (Character.isWhitespace(c)) {
                pos++;
            } else {
                break;
            }
        }
    }

    private boolean isDelimiter(char c) {
        return Character.isWhitespace(c) || c == '(' || c == ')' || c == '"' || c == ';' || c == '\'';
    }
}
