package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Evaluator {

    private static final Object VOID = new Object() {
        @Override public String toString() { return "#<void>"; }
    };

    public String evalStr(String input) throws EvalError {
        List<Object> tokens = tokenize(input);
        int[] pos = {0};
        Env env = createGlobalEnv();
        Object lastResult = null;
        while (pos[0] < tokens.size()) {
            Object expr = parse(tokens, pos);
            lastResult = eval(expr, env);
        }
        if (lastResult == null) {
            throw new EvalError("no expression");
        }
        if (lastResult == VOID) {
            throw new EvalError("no expression");
        }
        return schemeToString(lastResult);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        throw new EvalError("not implemented");
    }

    private Env createGlobalEnv() {
        return new Env(null);
    }

    // --- Environment ---

    private static class Env {
        final Env parent;
        final Map<String, Object> bindings = new HashMap<>();

        Env(Env parent) {
            this.parent = parent;
        }

        Object lookup(String name) throws EvalError {
            if (bindings.containsKey(name)) return bindings.get(name);
            if (parent != null) return parent.lookup(name);
            throw new EvalError("unbound variable: " + name);
        }

        void define(String name, Object value) {
            bindings.put(name, value);
        }
    }

    // --- Lambda ---

    private static class Lambda {
        final List<String> params;
        final Object body;
        final Env closure;

        Lambda(List<String> params, Object body, Env closure) {
            this.params = params;
            this.body = body;
            this.closure = closure;
        }
    }

    // --- Tokenizer ---

    private List<Object> tokenize(String input) throws EvalError {
        List<Object> tokens = new ArrayList<>();
        int i = 0;
        int len = input.length();
        while (i < len) {
            char c = input.charAt(i);
            if (Character.isWhitespace(c)) {
                i++;
            } else if (c == ';') {
                while (i < len && input.charAt(i) != '\n') i++;
            } else if (c == '(') {
                tokens.add("(");
                i++;
            } else if (c == ')') {
                tokens.add(")");
                i++;
            } else if (c == '\'') {
                tokens.add("(");
                tokens.add("quote");
                i++;
                // The next token(s) will be parsed, then we close
                // We need to handle this at parse level instead
                // Revert: use a special marker
                tokens.remove(tokens.size() - 1);
                tokens.remove(tokens.size() - 1);
                tokens.add("'");
                // Actually let's handle quote expansion in the tokenizer properly
                // We'll handle it in the parser instead
            } else if (c == '"') {
                StringBuilder sb = new StringBuilder();
                i++; // skip opening quote
                while (i < len && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\') {
                        i++;
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
                        sb.append(input.charAt(i));
                    }
                    i++;
                }
                if (i >= len) throw new EvalError("unterminated string");
                i++; // skip closing quote
                tokens.add(new SchemeString(sb.toString()));
            } else if (c == '#') {
                if (i + 1 < len) {
                    char next = input.charAt(i + 1);
                    if (next == 't') {
                        tokens.add(Boolean.TRUE);
                        i += 2;
                    } else if (next == 'f') {
                        tokens.add(Boolean.FALSE);
                        i += 2;
                    } else {
                        throw new EvalError("unknown token: #" + next);
                    }
                } else {
                    throw new EvalError("unexpected end of input after #");
                }
            } else {
                // symbol or number
                StringBuilder sb = new StringBuilder();
                while (i < len && !Character.isWhitespace(input.charAt(i))
                        && input.charAt(i) != '(' && input.charAt(i) != ')'
                        && input.charAt(i) != '"' && input.charAt(i) != ';') {
                    sb.append(input.charAt(i));
                    i++;
                }
                String tok = sb.toString();
                // Try parsing as integer
                try {
                    tokens.add(Long.parseLong(tok));
                } catch (NumberFormatException e) {
                    tokens.add(tok); // symbol
                }
            }
        }
        return tokens;
    }

    // --- Parser ---

    private Object parse(List<Object> tokens, int[] pos) throws EvalError {
        if (pos[0] >= tokens.size()) {
            throw new EvalError("unexpected end of input");
        }
        Object tok = tokens.get(pos[0]);
        pos[0]++;

        if (tok.equals("'")) {
            // Quote shorthand: 'x -> (quote x)
            Object quoted = parse(tokens, pos);
            List<Object> quoteExpr = new ArrayList<>();
            quoteExpr.add("quote");
            quoteExpr.add(quoted);
            return quoteExpr;
        }

        if (tok.equals("(")) {
            List<Object> list = new ArrayList<>();
            while (pos[0] < tokens.size() && !tokens.get(pos[0]).equals(")")) {
                list.add(parse(tokens, pos));
            }
            if (pos[0] >= tokens.size()) {
                throw new EvalError("missing closing parenthesis");
            }
            pos[0]++; // skip ')'
            return list;
        } else if (tok.equals(")")) {
            throw new EvalError("unexpected )");
        } else {
            return tok;
        }
    }

    // --- Evaluator ---

    @SuppressWarnings("unchecked")
    private Object eval(Object expr, Env env) throws EvalError {
        if (expr instanceof Long || expr instanceof Boolean || expr instanceof SchemeString) {
            return expr;
        }
        if (expr instanceof String sym) {
            if (isPrimitive(sym)) {
                return sym; // primitives are self-evaluating for now
            }
            return env.lookup(sym);
        }
        if (expr instanceof List<?> list) {
            if (list.isEmpty()) {
                throw new EvalError("empty application");
            }
            Object head = list.get(0);

            // Special forms
            if (head instanceof String s) {
                switch (s) {
                    case "define" -> {
                        if (list.size() < 3) throw new EvalError("define: bad syntax");
                        Object target = list.get(1);
                        if (target instanceof String name) {
                            // (define x expr)
                            Object val = eval(list.get(2), env);
                            env.define(name, val);
                            return VOID;
                        } else if (target instanceof List<?> sig) {
                            // (define (f params...) body)
                            if (sig.isEmpty() || !(sig.get(0) instanceof String fname)) {
                                throw new EvalError("define: bad syntax");
                            }
                            List<String> params = new ArrayList<>();
                            for (int i = 1; i < sig.size(); i++) {
                                if (!(sig.get(i) instanceof String p)) {
                                    throw new EvalError("define: parameter must be a symbol");
                                }
                                params.add(p);
                            }
                            // body: if multiple exprs, wrap in begin
                            Object body;
                            if (list.size() == 3) {
                                body = list.get(2);
                            } else {
                                List<Object> beginBody = new ArrayList<>();
                                beginBody.add("begin");
                                for (int i = 2; i < list.size(); i++) {
                                    beginBody.add(list.get(i));
                                }
                                body = beginBody;
                            }
                            Lambda lambda = new Lambda(params, body, env);
                            env.define(fname, lambda);
                            return VOID;
                        }
                        throw new EvalError("define: bad syntax");
                    }
                    case "if" -> {
                        if (list.size() < 3 || list.size() > 4) {
                            throw new EvalError("if: bad syntax");
                        }
                        Object cond = eval(list.get(1), env);
                        if (!isFalse(cond)) {
                            return eval(list.get(2), env);
                        } else if (list.size() == 4) {
                            return eval(list.get(3), env);
                        }
                        return VOID;
                    }
                    case "quote" -> {
                        if (list.size() != 2) throw new EvalError("quote: expected 1 argument");
                        return list.get(1);
                    }
                    case "lambda" -> {
                        if (list.size() < 3) throw new EvalError("lambda: bad syntax");
                        Object paramsExpr = list.get(1);
                        if (!(paramsExpr instanceof List<?> paramList)) {
                            throw new EvalError("lambda: parameters must be a list");
                        }
                        List<String> params = new ArrayList<>();
                        for (Object p : paramList) {
                            if (!(p instanceof String ps)) {
                                throw new EvalError("lambda: parameter must be a symbol");
                            }
                            params.add(ps);
                        }
                        Object body;
                        if (list.size() == 3) {
                            body = list.get(2);
                        } else {
                            List<Object> beginBody = new ArrayList<>();
                            beginBody.add("begin");
                            for (int i = 2; i < list.size(); i++) {
                                beginBody.add(list.get(i));
                            }
                            body = beginBody;
                        }
                        return new Lambda(params, body, env);
                    }
                    case "and" -> {
                        Object result = Boolean.TRUE;
                        for (int i = 1; i < list.size(); i++) {
                            result = eval(list.get(i), env);
                            if (isFalse(result)) return result;
                        }
                        return result;
                    }
                    case "or" -> {
                        Object result = Boolean.FALSE;
                        for (int i = 1; i < list.size(); i++) {
                            result = eval(list.get(i), env);
                            if (!isFalse(result)) return result;
                        }
                        return result;
                    }
                    case "not" -> {
                        if (list.size() != 2) throw new EvalError("not: expected 1 argument");
                        Object val = eval(list.get(1), env);
                        return isFalse(val) ? Boolean.TRUE : Boolean.FALSE;
                    }
                }
            }

            // Procedure call
            Object proc = eval(head, env);
            List<Object> args = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) {
                args.add(eval(list.get(i), env));
            }

            if (proc instanceof Lambda lambda) {
                if (args.size() != lambda.params.size()) {
                    throw new EvalError("wrong number of arguments: expected " + lambda.params.size() + ", got " + args.size());
                }
                Env callEnv = new Env(lambda.closure);
                for (int i = 0; i < lambda.params.size(); i++) {
                    callEnv.define(lambda.params.get(i), args.get(i));
                }
                return eval(lambda.body, callEnv);
            }

            if (proc instanceof String p && isPrimitive(p)) {
                return applyPrimitive(p, args);
            }

            throw new EvalError("not a procedure: " + schemeToString(proc));
        }
        throw new EvalError("cannot evaluate: " + expr);
    }

    private static final java.util.Set<String> PRIMITIVES = java.util.Set.of(
            "+", "-", "*", "/", "<", ">", "=", "<="
    );

    private boolean isPrimitive(String name) {
        return PRIMITIVES.contains(name);
    }

    private Object applyPrimitive(String proc, List<Object> args) throws EvalError {
        return switch (proc) {
            case "+" -> {
                long sum = 0;
                for (Object a : args) sum += requireLong(a, "+");
                yield sum;
            }
            case "-" -> {
                if (args.isEmpty()) throw new EvalError("-: expected at least 1 argument");
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
                if (args.isEmpty()) throw new EvalError("/: expected at least 1 argument");
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
            default -> throw new EvalError("unbound variable: " + proc);
        };
    }

    private boolean isFalse(Object val) {
        return val instanceof Boolean b && !b;
    }

    private long requireLong(Object val, String context) throws EvalError {
        if (val instanceof Long l) return l;
        throw new EvalError(context + ": expected number, got " + schemeToString(val));
    }

    private void requireArgCount(List<Object> args, int expected, String context) throws EvalError {
        if (args.size() != expected) {
            throw new EvalError(context + ": expected " + expected + " arguments, got " + args.size());
        }
    }

    // --- Output formatting ---

    @SuppressWarnings("unchecked")
    private String schemeToString(Object val) {
        if (val instanceof Long l) return l.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value() + "\"";
        if (val instanceof List<?> list) {
            StringBuilder sb = new StringBuilder("(");
            for (int i = 0; i < list.size(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(schemeToString(list.get(i)));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof Lambda) return "#<procedure>";
        if (val == VOID) return "#<void>";
        return val.toString();
    }

    // Internal type for Scheme strings (to distinguish from symbols which are Java Strings)
    record SchemeString(String value) {}
}
