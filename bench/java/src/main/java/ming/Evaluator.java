package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Evaluator {

    private final Env globalEnv = new Env(null);

    public Evaluator() {
        // Register builtins as procedures in the global environment
        for (String name : new String[]{"+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not"}) {
            globalEnv.define(name, new BuiltinProc(name));
        }
    }

    public String evalStr(String input) throws EvalError {
        List<Object> exprs = parse(input);
        Object result = null;
        for (Object expr : exprs) {
            result = eval(expr, globalEnv);
        }
        return schemeToString(result);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        String result = evalStr(input);
        return new EvalResult(result, "");
    }

    // ---- Representation ----
    static final class SchemeString {
        final String value;
        SchemeString(String value) { this.value = value; }
    }

    // ---- Environment ----
    static class Env {
        private final Map<String, Object> bindings = new HashMap<>();
        private final Env parent;

        Env(Env parent) { this.parent = parent; }

        void define(String name, Object value) { bindings.put(name, value); }

        Object lookup(String name) throws EvalError {
            if (bindings.containsKey(name)) return bindings.get(name);
            if (parent != null) return parent.lookup(name);
            throw new EvalError("unbound variable: " + name);
        }
    }

    // ---- Procedure types ----
    static final class BuiltinProc {
        final String name;
        BuiltinProc(String name) { this.name = name; }
    }

    static final class Lambda {
        final List<String> params;
        final List<Object> body;
        final Env closureEnv;
        Lambda(List<String> params, List<Object> body, Env closureEnv) {
            this.params = params;
            this.body = body;
            this.closureEnv = closureEnv;
        }
    }

    // ---- Tokenizer ----

    private List<String> tokenize(String input) {
        List<String> tokens = new ArrayList<>();
        int i = 0;
        int len = input.length();
        while (i < len) {
            char c = input.charAt(i);
            if (Character.isWhitespace(c)) { i++; continue; }
            if (c == ';') {
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
                        sb.append(input.charAt(i)); i++;
                        if (i < len) { sb.append(input.charAt(i)); i++; }
                    } else {
                        sb.append(input.charAt(i)); i++;
                    }
                }
                if (i < len) { sb.append('"'); i++; }
                tokens.add(sb.toString());
                continue;
            }
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
            pos++;
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

    @SuppressWarnings("unchecked")
    private Object eval(Object expr, Env env) throws EvalError {
        if (expr instanceof Long || expr instanceof Boolean || expr instanceof SchemeString) {
            return expr;
        }
        if (expr instanceof String sym) {
            return env.lookup(sym);
        }
        if (expr instanceof List<?> list) {
            if (list.isEmpty()) throw new EvalError("empty application");
            Object head = list.get(0);

            if (head instanceof String op) {
                switch (op) {
                    case "quote" -> {
                        if (list.size() != 2) throw new EvalError("quote: expected 1 argument");
                        return list.get(1);
                    }
                    case "if" -> {
                        if (list.size() < 3 || list.size() > 4)
                            throw new EvalError("if: expected 2 or 3 arguments");
                        Object cond = eval(list.get(1), env);
                        if (!isFalse(cond)) {
                            return eval(list.get(2), env);
                        } else if (list.size() == 4) {
                            return eval(list.get(3), env);
                        }
                        return null; // void
                    }
                    case "define" -> {
                        if (list.size() < 3) throw new EvalError("define: bad syntax");
                        Object target = list.get(1);
                        if (target instanceof String name) {
                            Object val = eval(list.get(2), env);
                            env.define(name, val);
                            return null;
                        }
                        if (target instanceof List<?> sig) {
                            // (define (f params...) body...)
                            if (sig.isEmpty()) throw new EvalError("define: bad syntax");
                            String name = (String) sig.get(0);
                            List<String> params = new ArrayList<>();
                            for (int i = 1; i < sig.size(); i++) {
                                params.add((String) sig.get(i));
                            }
                            List<Object> body = new ArrayList<>();
                            for (int i = 2; i < list.size(); i++) {
                                body.add(list.get(i));
                            }
                            env.define(name, new Lambda(params, body, env));
                            return null;
                        }
                        throw new EvalError("define: bad syntax");
                    }
                    case "lambda" -> {
                        if (list.size() < 3) throw new EvalError("lambda: bad syntax");
                        List<?> paramList = (List<?>) list.get(1);
                        List<String> params = new ArrayList<>();
                        for (Object p : paramList) params.add((String) p);
                        List<Object> body = new ArrayList<>();
                        for (int i = 2; i < list.size(); i++) body.add(list.get(i));
                        return new Lambda(params, body, env);
                    }
                    case "and" -> { return evalAnd((List<Object>) expr, env); }
                    case "or" -> { return evalOr((List<Object>) expr, env); }
                }
            }

            // General application
            Object proc = eval(head, env);
            List<Object> args = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) {
                args.add(eval(list.get(i), env));
            }
            return apply(proc, args);
        }
        throw new EvalError("unknown expression type");
    }

    private Object apply(Object proc, List<Object> args) throws EvalError {
        if (proc instanceof BuiltinProc bp) {
            return applyBuiltin(bp.name, args);
        }
        if (proc instanceof Lambda lam) {
            if (args.size() != lam.params.size()) {
                throw new EvalError("wrong number of arguments: expected " + lam.params.size() + ", got " + args.size());
            }
            Env callEnv = new Env(lam.closureEnv);
            for (int i = 0; i < lam.params.size(); i++) {
                callEnv.define(lam.params.get(i), args.get(i));
            }
            Object result = null;
            for (Object bodyExpr : lam.body) {
                result = eval(bodyExpr, callEnv);
            }
            return result;
        }
        throw new EvalError("not a procedure: " + schemeToString(proc));
    }

    private Object evalAnd(List<Object> expr, Env env) throws EvalError {
        Object result = Boolean.TRUE;
        for (int i = 1; i < expr.size(); i++) {
            result = eval(expr.get(i), env);
            if (isFalse(result)) return result;
        }
        return result;
    }

    private Object evalOr(List<Object> expr, Env env) throws EvalError {
        Object result = Boolean.FALSE;
        for (int i = 1; i < expr.size(); i++) {
            result = eval(expr.get(i), env);
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
