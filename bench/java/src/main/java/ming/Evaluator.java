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

    // ---- Representation ----
    // Integer  -> Long
    // Boolean  -> Boolean
    // String   -> SchemeString (wrapper to distinguish from symbols)
    // Symbol   -> String
    // List     -> List<Object>  (empty list = empty ArrayList)

    static final class SchemeString {
        final String value;
        SchemeString(String value) { this.value = value; }
    }

    // ---- Tokenizer ----

    private List<String> tokenize(String input) {
        List<String> tokens = new ArrayList<>();
        int i = 0;
        int len = input.length();
        while (i < len) {
            char c = input.charAt(i);
            if (Character.isWhitespace(c)) { i++; continue; }
            if (c == ';') { // line comment
                while (i < len && input.charAt(i) != '\n') i++;
                continue;
            }
            if (c == '(') { tokens.add("("); i++; continue; }
            if (c == ')') { tokens.add(")"); i++; continue; }
            if (c == '\'') { tokens.add("'"); i++; continue; }
            if (c == '"') {
                StringBuilder sb = new StringBuilder();
                sb.append('"');
                i++;
                while (i < len && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\') {
                        sb.append(input.charAt(i));
                        i++;
                        if (i < len) { sb.append(input.charAt(i)); i++; }
                    } else {
                        sb.append(input.charAt(i));
                        i++;
                    }
                }
                if (i < len) { sb.append('"'); i++; }
                tokens.add(sb.toString());
                continue;
            }
            // atom
            StringBuilder sb = new StringBuilder();
            while (i < len) {
                char ch = input.charAt(i);
                if (Character.isWhitespace(ch) || ch == '(' || ch == ')' || ch == '"' || ch == ';') break;
                sb.append(ch);
                i++;
            }
            tokens.add(sb.toString());
        }
        return tokens;
    }

    // ---- Parser ----

    private int pos;

    private List<Object> parse(String input) throws EvalError {
        List<String> tokens = tokenize(input);
        pos = 0;
        List<Object> exprs = new ArrayList<>();
        while (pos < tokens.size()) {
            exprs.add(parseExpr(tokens));
        }
        return exprs;
    }

    private Object parseExpr(List<String> tokens) throws EvalError {
        if (pos >= tokens.size()) throw new EvalError("unexpected end of input");
        String token = tokens.get(pos++);

        if (token.equals("(")) {
            List<Object> list = new ArrayList<>();
            while (pos < tokens.size() && !tokens.get(pos).equals(")")) {
                list.add(parseExpr(tokens));
            }
            if (pos >= tokens.size()) throw new EvalError("missing closing parenthesis");
            pos++; // skip )
            return list;
        }
        if (token.equals(")")) throw new EvalError("unexpected )");
        if (token.equals("'")) {
            Object quoted = parseExpr(tokens);
            List<Object> q = new ArrayList<>();
            q.add("quote");
            q.add(quoted);
            return q;
        }
        return parseAtom(token);
    }

    private Object parseAtom(String token) {
        if (token.equals("#t")) return Boolean.TRUE;
        if (token.equals("#f")) return Boolean.FALSE;
        if (token.startsWith("\"") && token.endsWith("\"")) {
            String s = token.substring(1, token.length() - 1);
            s = s.replace("\\n", "\n").replace("\\t", "\t").replace("\\\\", "\\").replace("\\\"", "\"");
            return new SchemeString(s);
        }
        try { return Long.parseLong(token); } catch (NumberFormatException ignored) {}
        return token; // symbol
    }

    // ---- Evaluator ----

    private static final java.util.Set<String> BUILTINS = java.util.Set.of(
        "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not"
    );

    @SuppressWarnings("unchecked")
    private Object eval(Object expr) throws EvalError {
        if (expr instanceof Long || expr instanceof Boolean || expr instanceof SchemeString) {
            return expr;
        }
        if (expr instanceof String sym) {
            throw new EvalError("unbound variable: " + sym);
        }
        if (expr instanceof List<?> list) {
            if (list.isEmpty()) throw new EvalError("empty application");
            Object head = list.get(0);

            // Special forms
            if (head instanceof String op) {
                switch (op) {
                    case "and" -> { return evalAnd((List<Object>) expr); }
                    case "or" -> { return evalOr((List<Object>) expr); }
                }
                // Builtin function call
                if (BUILTINS.contains(op)) {
                    List<Object> args = new ArrayList<>();
                    for (int i = 1; i < list.size(); i++) {
                        args.add(eval(list.get(i)));
                    }
                    return applyBuiltin(op, args);
                }
            }

            // General function application
            Object proc = eval(head);
            if (proc instanceof String name) {
                List<Object> args = new ArrayList<>();
                for (int i = 1; i < list.size(); i++) {
                    args.add(eval(list.get(i)));
                }
                return applyBuiltin(name, args);
            }
            throw new EvalError("not a procedure: " + schemeToString(proc));
        }
        throw new EvalError("unknown expression type");
    }

    private Object evalAnd(List<Object> expr) throws EvalError {
        Object result = Boolean.TRUE;
        for (int i = 1; i < expr.size(); i++) {
            result = eval(expr.get(i));
            if (isFalse(result)) return result;
        }
        return result;
    }

    private Object evalOr(List<Object> expr) throws EvalError {
        Object result = Boolean.FALSE;
        for (int i = 1; i < expr.size(); i++) {
            result = eval(expr.get(i));
            if (!isFalse(result)) return result;
        }
        return result;
    }

    private boolean isFalse(Object val) {
        return val instanceof Boolean b && !b;
    }

    private Object applyBuiltin(String name, List<Object> args) throws EvalError {
        return switch (name) {
            case "+" -> {
                long sum = 0;
                for (Object a : args) sum += requireLong(a, "+");
                yield sum;
            }
            case "-" -> {
                if (args.isEmpty()) throw new EvalError("-: need at least 1 argument");
                if (args.size() == 1) yield -requireLong(args.get(0), "-");
                long result = requireLong(args.get(0), "-");
                for (int i = 1; i < args.size(); i++) result -= requireLong(args.get(i), "-");
                yield result;
            }
            case "*" -> {
                long product = 1;
                for (Object a : args) product *= requireLong(a, "*");
                yield product;
            }
            case "/" -> {
                if (args.isEmpty()) throw new EvalError("/: need at least 1 argument");
                long result = requireLong(args.get(0), "/");
                for (int i = 1; i < args.size(); i++) {
                    long divisor = requireLong(args.get(i), "/");
                    if (divisor == 0) throw new EvalError("division by zero");
                    result /= divisor;
                }
                yield result;
            }
            case "<" -> {
                requireArgCount(args, 2, "<");
                yield requireLong(args.get(0), "<") < requireLong(args.get(1), "<");
            }
            case ">" -> {
                requireArgCount(args, 2, ">");
                yield requireLong(args.get(0), ">") > requireLong(args.get(1), ">");
            }
            case "=" -> {
                requireArgCount(args, 2, "=");
                yield requireLong(args.get(0), "=") == requireLong(args.get(1), "=");
            }
            case "<=" -> {
                requireArgCount(args, 2, "<=");
                yield requireLong(args.get(0), "<=") <= requireLong(args.get(1), "<=");
            }
            case ">=" -> {
                requireArgCount(args, 2, ">=");
                yield requireLong(args.get(0), ">=") >= requireLong(args.get(1), ">=");
            }
            case "not" -> {
                requireArgCount(args, 1, "not");
                yield isFalse(args.get(0));
            }
            default -> throw new EvalError("unbound variable: " + name);
        };
    }

    private long requireLong(Object val, String context) throws EvalError {
        if (val instanceof Long l) return l;
        throw new EvalError(context + ": expected number, got " + schemeToString(val));
    }

    private void requireArgCount(List<Object> args, int expected, String name) throws EvalError {
        if (args.size() != expected) {
            throw new EvalError(name + ": expected " + expected + " arguments, got " + args.size());
        }
    }

    // ---- Output formatting ----

    @SuppressWarnings("unchecked")
    private String schemeToString(Object val) {
        if (val == null) return "()";
        if (val instanceof Long l) return l.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value + "\"";
        if (val instanceof String s) return s;
        if (val instanceof List<?> list) {
            StringBuilder sb = new StringBuilder("(");
            for (int i = 0; i < list.size(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(schemeToString(list.get(i)));
            }
            sb.append(")");
            return sb.toString();
        }
        return val.toString();
    }
}
