package ming;

import java.util.ArrayList;
import java.util.List;

public class Evaluator {

    public String evalStr(String input) throws EvalError {
        List<Object> exprs = parse(input);
        Object result = null;
        for (Object expr : exprs) {
            result = eval(expr);
        }
        return schemeToString(result);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        String result = evalStr(input);
        return new EvalResult(result, "");
    }

    // --- Parser ---

    private int pos;
    private String src;

    private List<Object> parse(String input) throws EvalError {
        this.src = input;
        this.pos = 0;
        List<Object> exprs = new ArrayList<>();
        while (true) {
            skipWhitespace();
            if (pos >= src.length()) break;
            exprs.add(readExpr());
        }
        return exprs;
    }

    private void skipWhitespace() {
        while (pos < src.length()) {
            char c = src.charAt(pos);
            if (c == ';') {
                while (pos < src.length() && src.charAt(pos) != '\n') pos++;
            } else if (Character.isWhitespace(c)) {
                pos++;
            } else {
                break;
            }
        }
    }

    private Object readExpr() throws EvalError {
        skipWhitespace();
        if (pos >= src.length()) throw new EvalError("unexpected end of input");
        char c = src.charAt(pos);
        if (c == '(') {
            return readList();
        } else if (c == '"') {
            return readString();
        } else if (c == '#') {
            return readHash();
        } else {
            return readAtom();
        }
    }

    private SchemeList readList() throws EvalError {
        pos++; // skip '('
        List<Object> elems = new ArrayList<>();
        while (true) {
            skipWhitespace();
            if (pos >= src.length()) throw new EvalError("unexpected end of input");
            if (src.charAt(pos) == ')') {
                pos++;
                return new SchemeList(elems);
            }
            elems.add(readExpr());
        }
    }

    private SchemeString readString() throws EvalError {
        pos++; // skip opening "
        StringBuilder sb = new StringBuilder();
        while (pos < src.length()) {
            char c = src.charAt(pos);
            if (c == '\\') {
                pos++;
                if (pos >= src.length()) throw new EvalError("unexpected end of string");
                char esc = src.charAt(pos);
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
                return new SchemeString(sb.toString());
            } else {
                sb.append(c);
                pos++;
            }
        }
        throw new EvalError("unterminated string");
    }

    private Object readHash() throws EvalError {
        pos++; // skip '#'
        if (pos >= src.length()) throw new EvalError("unexpected end of input after #");
        char c = src.charAt(pos);
        if (c == 't') {
            pos++;
            return Boolean.TRUE;
        } else if (c == 'f') {
            pos++;
            return Boolean.FALSE;
        }
        throw new EvalError("unknown hash literal: #" + c);
    }

    private Object readAtom() throws EvalError {
        int start = pos;
        while (pos < src.length()) {
            char c = src.charAt(pos);
            if (Character.isWhitespace(c) || c == '(' || c == ')' || c == '"' || c == ';') break;
            pos++;
        }
        String token = src.substring(start, pos);
        if (token.isEmpty()) throw new EvalError("empty token");
        // Try integer
        try {
            return Long.parseLong(token);
        } catch (NumberFormatException e) {
            // symbol
            return new SchemeSymbol(token);
        }
    }

    // --- Eval ---

    private Object eval(Object expr) throws EvalError {
        if (expr instanceof Long || expr instanceof Boolean || expr instanceof SchemeString) {
            return expr;
        }
        if (expr instanceof SchemeSymbol) {
            throw new EvalError("unbound variable: " + ((SchemeSymbol) expr).name);
        }
        if (expr instanceof SchemeList list) {
            if (list.elems.isEmpty()) {
                throw new EvalError("empty application");
            }
            Object first = list.elems.get(0);
            if (first instanceof SchemeSymbol sym) {
                switch (sym.name) {
                    case "and" -> { return evalAnd(list.elems); }
                    case "or" -> { return evalOr(list.elems); }
                }
                // Builtin function call
                List<Object> args = new ArrayList<>();
                for (int i = 1; i < list.elems.size(); i++) {
                    args.add(eval(list.elems.get(i)));
                }
                return applyBuiltin(sym.name, args);
            }
            throw new EvalError("not a procedure: " + schemeToString(first));
        }
        throw new EvalError("cannot eval: " + expr);
    }

    private Object evalAnd(List<Object> elems) throws EvalError {
        Object result = Boolean.TRUE;
        for (int i = 1; i < elems.size(); i++) {
            result = eval(elems.get(i));
            if (!isTruthy(result)) return result;
        }
        return result;
    }

    private Object evalOr(List<Object> elems) throws EvalError {
        Object result = Boolean.FALSE;
        for (int i = 1; i < elems.size(); i++) {
            result = eval(elems.get(i));
            if (isTruthy(result)) return result;
        }
        return result;
    }

    private boolean isTruthy(Object val) {
        return !(val instanceof Boolean b && !b);
    }

    private Object applyBuiltin(String name, List<Object> args) throws EvalError {
        switch (name) {
            case "+" -> {
                long sum = 0;
                for (Object a : args) sum += requireLong(a, "+");
                return sum;
            }
            case "-" -> {
                if (args.isEmpty()) throw new EvalError("-: need at least 1 argument");
                if (args.size() == 1) return -requireLong(args.get(0), "-");
                long result = requireLong(args.get(0), "-");
                for (int i = 1; i < args.size(); i++) result -= requireLong(args.get(i), "-");
                return result;
            }
            case "*" -> {
                long product = 1;
                for (Object a : args) product *= requireLong(a, "*");
                return product;
            }
            case "/" -> {
                if (args.isEmpty()) throw new EvalError("/: need at least 1 argument");
                long result = requireLong(args.get(0), "/");
                for (int i = 1; i < args.size(); i++) {
                    long divisor = requireLong(args.get(i), "/");
                    if (divisor == 0) throw new EvalError("division by zero");
                    result /= divisor;
                }
                return result;
            }
            case "<" -> {
                requireArgCount(name, args, 2);
                return requireLong(args.get(0), "<") < requireLong(args.get(1), "<");
            }
            case ">" -> {
                requireArgCount(name, args, 2);
                return requireLong(args.get(0), ">") > requireLong(args.get(1), ">");
            }
            case "=" -> {
                requireArgCount(name, args, 2);
                return requireLong(args.get(0), "=") == requireLong(args.get(1), "=");
            }
            case "<=" -> {
                requireArgCount(name, args, 2);
                return requireLong(args.get(0), "<=") <= requireLong(args.get(1), "<=");
            }
            case "not" -> {
                requireArgCount(name, args, 1);
                return !isTruthy(args.get(0));
            }
            default -> throw new EvalError("unbound variable: " + name);
        }
    }

    private long requireLong(Object val, String op) throws EvalError {
        if (val instanceof Long l) return l;
        throw new EvalError(op + ": not a number: " + schemeToString(val));
    }

    private void requireArgCount(String name, List<Object> args, int expected) throws EvalError {
        if (args.size() != expected)
            throw new EvalError(name + ": expected " + expected + " arguments, got " + args.size());
    }

    // --- Output ---

    private String schemeToString(Object val) {
        if (val == null) return "void";
        if (val instanceof Long l) return l.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value + "\"";
        if (val instanceof SchemeSymbol s) return s.name;
        if (val instanceof SchemeList list) {
            StringBuilder sb = new StringBuilder("(");
            for (int i = 0; i < list.elems.size(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(schemeToString(list.elems.get(i)));
            }
            sb.append(")");
            return sb.toString();
        }
        return val.toString();
    }

    // --- Data types ---

    record SchemeSymbol(String name) {}
    record SchemeString(String value) {}
    record SchemeList(List<Object> elems) {}
}
