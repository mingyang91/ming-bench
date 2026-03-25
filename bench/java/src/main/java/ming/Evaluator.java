package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Evaluator {

    // --- Environment ---

    private static class Env {
        final Map<String, Object> bindings = new HashMap<>();
        final Env parent;

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

    // --- Lambda (closure) ---

    private record Lambda(List<String> params, List<Object> body, Env closureEnv) {}

    // Sentinel for void (define returns this)
    private static final Object VOID = new Object() {
        @Override public String toString() { return "#<void>"; }
    };

    // --- Builtin procedure ---

    @FunctionalInterface
    private interface BuiltinProc {
        Object apply(List<Object> args) throws EvalError;
    }

    private final Env globalEnv = new Env(null);

    public Evaluator() {
        // Register builtins
        globalEnv.define("+", (BuiltinProc) args -> {
            long result = 0;
            for (Object a : args) result += asLong(a);
            return result;
        });
        globalEnv.define("-", (BuiltinProc) args -> {
            if (args.isEmpty()) throw new EvalError("- requires at least 1 argument");
            if (args.size() == 1) return -asLong(args.get(0));
            long result = asLong(args.get(0));
            for (int i = 1; i < args.size(); i++) result -= asLong(args.get(i));
            return result;
        });
        globalEnv.define("*", (BuiltinProc) args -> {
            long result = 1;
            for (Object a : args) result *= asLong(a);
            return result;
        });
        globalEnv.define("/", (BuiltinProc) args -> {
            if (args.size() < 2) throw new EvalError("/ requires at least 2 arguments");
            long result = asLong(args.get(0));
            for (int i = 1; i < args.size(); i++) {
                long d = asLong(args.get(i));
                if (d == 0) throw new EvalError("division by zero");
                result /= d;
            }
            return result;
        });
        globalEnv.define("<", (BuiltinProc) args -> asLong(args.get(0)) < asLong(args.get(1)));
        globalEnv.define(">", (BuiltinProc) args -> asLong(args.get(0)) > asLong(args.get(1)));
        globalEnv.define("=", (BuiltinProc) args -> asLong(args.get(0)) == asLong(args.get(1)));
        globalEnv.define("<=", (BuiltinProc) args -> asLong(args.get(0)) <= asLong(args.get(1)));
        globalEnv.define(">=", (BuiltinProc) args -> asLong(args.get(0)) >= asLong(args.get(1)));
        globalEnv.define("not", (BuiltinProc) args -> isFalse(args.get(0)));
    }

    public String evalStr(String input) throws EvalError {
        List<Object> tokens = tokenize(input);
        int[] pos = {0};
        Object lastResult = null;
        while (pos[0] < tokens.size()) {
            Object expr = parse(tokens, pos);
            lastResult = eval(expr, globalEnv);
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
            } else if (c == '\'') {
                tokens.add("'");
                i++;
            } else if (c == '(') {
                tokens.add("(");
                i++;
            } else if (c == ')') {
                tokens.add(")");
                i++;
            } else if (c == '"') {
                StringBuilder sb = new StringBuilder();
                i++;
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
                i++;
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
                        throw new EvalError("unexpected token: #" + next);
                    }
                } else {
                    throw new EvalError("unexpected end after #");
                }
            } else {
                StringBuilder sb = new StringBuilder();
                while (i < len && !Character.isWhitespace(input.charAt(i))
                        && input.charAt(i) != '(' && input.charAt(i) != ')'
                        && input.charAt(i) != '"' && input.charAt(i) != ';'
                        && input.charAt(i) != '\'') {
                    sb.append(input.charAt(i));
                    i++;
                }
                String tok = sb.toString();
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
        Object token = tokens.get(pos[0]);
        if ("'".equals(token)) {
            pos[0]++;
            Object quoted = parse(tokens, pos);
            List<Object> quoteExpr = new ArrayList<>();
            quoteExpr.add("quote");
            quoteExpr.add(quoted);
            return quoteExpr;
        }
        if ("(".equals(token)) {
            pos[0]++;
            List<Object> list = new ArrayList<>();
            while (pos[0] < tokens.size() && !")".equals(tokens.get(pos[0]))) {
                list.add(parse(tokens, pos));
            }
            if (pos[0] >= tokens.size()) {
                throw new EvalError("missing closing parenthesis");
            }
            pos[0]++;
            return list;
        } else if (")".equals(token)) {
            throw new EvalError("unexpected )");
        } else {
            pos[0]++;
            return token;
        }
    }

    // --- Evaluator ---

    @SuppressWarnings("unchecked")
    private Object eval(Object expr, Env env) throws EvalError {
        if (expr instanceof Long || expr instanceof Boolean || expr instanceof SchemeString) {
            return expr;
        }
        if (expr instanceof String sym) {
            return env.lookup(sym);
        }
        if (expr instanceof List<?> rawList) {
            List<Object> list = (List<Object>) rawList;
            if (list.isEmpty()) {
                throw new EvalError("empty application");
            }
            Object head = list.get(0);

            // Special forms
            if (head instanceof String op) {
                switch (op) {
                    case "quote" -> {
                        return list.get(1);
                    }
                    case "if" -> {
                        Object cond = eval(list.get(1), env);
                        if (!isFalse(cond)) {
                            return eval(list.get(2), env);
                        } else if (list.size() > 3) {
                            return eval(list.get(3), env);
                        }
                        return VOID;
                    }
                    case "define" -> {
                        Object target = list.get(1);
                        if (target instanceof String name) {
                            // (define x expr)
                            env.define(name, eval(list.get(2), env));
                        } else if (target instanceof List<?> sig) {
                            // (define (f params...) body...)
                            String name = (String) sig.get(0);
                            List<String> params = new ArrayList<>();
                            for (int i = 1; i < sig.size(); i++) {
                                params.add((String) sig.get(i));
                            }
                            List<Object> body = new ArrayList<>(list.subList(2, list.size()));
                            env.define(name, new Lambda(params, body, env));
                        }
                        return VOID;
                    }
                    case "lambda" -> {
                        List<?> paramList = (List<?>) list.get(1);
                        List<String> params = new ArrayList<>();
                        for (Object p : paramList) {
                            params.add((String) p);
                        }
                        List<Object> body = new ArrayList<>(list.subList(2, list.size()));
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
                }
            }

            // Function application
            Object proc = eval(head, env);
            List<Object> args = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) {
                args.add(eval(list.get(i), env));
            }

            if (proc instanceof BuiltinProc builtin) {
                return builtin.apply(args);
            }
            if (proc instanceof Lambda lambda) {
                if (args.size() != lambda.params.size()) {
                    throw new EvalError("wrong number of arguments: expected " + lambda.params.size() + ", got " + args.size());
                }
                Env callEnv = new Env(lambda.closureEnv);
                for (int i = 0; i < lambda.params.size(); i++) {
                    callEnv.define(lambda.params.get(i), args.get(i));
                }
                Object result = VOID;
                for (Object bodyExpr : lambda.body) {
                    result = eval(bodyExpr, callEnv);
                }
                return result;
            }
            throw new EvalError("cannot apply: " + schemeToString(proc));
        }
        throw new EvalError("unknown expression type");
    }

    private boolean isFalse(Object val) {
        return Boolean.FALSE.equals(val);
    }

    private long asLong(Object val) throws EvalError {
        if (val instanceof Long l) return l;
        throw new EvalError("expected number, got: " + schemeToString(val));
    }

    // --- Display ---

    @SuppressWarnings("unchecked")
    private String schemeToString(Object val) {
        if (val instanceof Long l) return l.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value() + "\"";
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
        if (val instanceof Lambda) return "#<procedure>";
        return val.toString();
    }

    // Internal wrapper to distinguish strings from symbols
    record SchemeString(String value) {}
}
