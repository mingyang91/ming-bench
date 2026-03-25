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

    // --- Scheme Pair (cons cell) ---

    private static class Pair {
        Object car;
        Object cdr;
        Pair(Object car, Object cdr) { this.car = car; this.cdr = cdr; }
    }

    // Sentinel for empty list '()
    private static final Object NIL = new Object() {
        @Override public String toString() { return "()"; }
    };

    // --- Builtin procedure ---

    @FunctionalInterface
    private interface BuiltinProc {
        Object apply(List<Object> args) throws EvalError;
    }

    private final Env globalEnv = new Env(null);

    public Evaluator() {
        // Arithmetic
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

        // Comparisons
        globalEnv.define("<", (BuiltinProc) args -> asLong(args.get(0)) < asLong(args.get(1)));
        globalEnv.define(">", (BuiltinProc) args -> asLong(args.get(0)) > asLong(args.get(1)));
        globalEnv.define("=", (BuiltinProc) args -> asLong(args.get(0)) == asLong(args.get(1)));
        globalEnv.define("<=", (BuiltinProc) args -> asLong(args.get(0)) <= asLong(args.get(1)));
        globalEnv.define(">=", (BuiltinProc) args -> asLong(args.get(0)) >= asLong(args.get(1)));
        globalEnv.define("not", (BuiltinProc) args -> isFalse(args.get(0)));

        // List operations
        globalEnv.define("cons", (BuiltinProc) args -> new Pair(args.get(0), args.get(1)));
        globalEnv.define("car", (BuiltinProc) args -> {
            if (args.get(0) instanceof Pair p) return p.car;
            throw new EvalError("car: not a pair");
        });
        globalEnv.define("cdr", (BuiltinProc) args -> {
            if (args.get(0) instanceof Pair p) return p.cdr;
            throw new EvalError("cdr: not a pair");
        });
        globalEnv.define("null?", (BuiltinProc) args -> args.get(0) == NIL);
        globalEnv.define("list", (BuiltinProc) args -> {
            Object result = NIL;
            for (int i = args.size() - 1; i >= 0; i--) {
                result = new Pair(args.get(i), result);
            }
            return result;
        });
        globalEnv.define("length", (BuiltinProc) args -> {
            long count = 0;
            Object curr = args.get(0);
            while (curr instanceof Pair p) {
                count++;
                curr = p.cdr;
            }
            return count;
        });
        globalEnv.define("append", (BuiltinProc) args -> {
            if (args.isEmpty()) return NIL;
            if (args.size() == 1) return args.get(0);
            // append last arg onto reversed-copy of earlier args
            Object result = args.get(args.size() - 1);
            for (int i = args.size() - 2; i >= 0; i--) {
                result = appendTwo(args.get(i), result);
            }
            return result;
        });

        // Type predicates
        globalEnv.define("boolean?", (BuiltinProc) args -> args.get(0) instanceof Boolean);
        globalEnv.define("number?", (BuiltinProc) args -> args.get(0) instanceof Long);
        globalEnv.define("pair?", (BuiltinProc) args -> args.get(0) instanceof Pair);
        globalEnv.define("string?", (BuiltinProc) args -> args.get(0) instanceof SchemeString);
        globalEnv.define("symbol?", (BuiltinProc) args -> args.get(0) instanceof String);
    }

    private Object appendTwo(Object a, Object b) {
        if (a == NIL) return b;
        if (a instanceof Pair p) {
            return new Pair(p.car, appendTwo(p.cdr, b));
        }
        return b; // shouldn't happen for proper lists
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

    // Convert parsed AST list to Scheme cons-cell list (for quote)
    private Object astToScheme(Object ast) {
        if (ast instanceof List<?> list) {
            Object result = NIL;
            for (int i = list.size() - 1; i >= 0; i--) {
                result = new Pair(astToScheme(list.get(i)), result);
            }
            return result;
        }
        return ast;
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
                        return astToScheme(list.get(1));
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
                            env.define(name, eval(list.get(2), env));
                        } else if (target instanceof List<?> sig) {
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
                    case "let" -> {
                        // Named let: (let name ((var init) ...) body...)
                        // Regular let: (let ((var init) ...) body...)
                        int offset;
                        String loopName = null;
                        if (list.get(1) instanceof String name) {
                            loopName = name;
                            offset = 2;
                        } else {
                            offset = 1;
                        }
                        List<?> bindings = (List<?>) list.get(offset);
                        List<Object> body = new ArrayList<>(list.subList(offset + 1, list.size()));

                        List<String> params = new ArrayList<>();
                        List<Object> inits = new ArrayList<>();
                        for (Object b : bindings) {
                            List<?> binding = (List<?>) b;
                            params.add((String) binding.get(0));
                            inits.add(binding.get(1));
                        }

                        if (loopName != null) {
                            // Named let: create a lambda and call it
                            Env letEnv = new Env(env);
                            Lambda loopLambda = new Lambda(params, body, letEnv);
                            letEnv.define(loopName, loopLambda);
                            // Evaluate inits in outer env
                            List<Object> args = new ArrayList<>();
                            for (Object init : inits) {
                                args.add(eval(init, env));
                            }
                            return applyLambda(loopLambda, args);
                        } else {
                            // Regular let
                            Env letEnv = new Env(env);
                            for (int i = 0; i < params.size(); i++) {
                                letEnv.define(params.get(i), eval(inits.get(i), env));
                            }
                            Object result = VOID;
                            for (Object bodyExpr : body) {
                                result = eval(bodyExpr, letEnv);
                            }
                            return result;
                        }
                    }
                    case "begin" -> {
                        Object result = VOID;
                        for (int i = 1; i < list.size(); i++) {
                            result = eval(list.get(i), env);
                        }
                        return result;
                    }
                    case "cond" -> {
                        for (int i = 1; i < list.size(); i++) {
                            List<Object> clause = (List<Object>) list.get(i);
                            Object test = clause.get(0);
                            if ("else".equals(test)) {
                                Object result = VOID;
                                for (int j = 1; j < clause.size(); j++) {
                                    result = eval(clause.get(j), env);
                                }
                                return result;
                            }
                            Object condVal = eval(test, env);
                            if (!isFalse(condVal)) {
                                if (clause.size() == 1) return condVal;
                                Object result = VOID;
                                for (int j = 1; j < clause.size(); j++) {
                                    result = eval(clause.get(j), env);
                                }
                                return result;
                            }
                        }
                        return VOID;
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
                return applyLambda(lambda, args);
            }
            throw new EvalError("cannot apply: " + schemeToString(proc));
        }
        throw new EvalError("unknown expression type");
    }

    private Object applyLambda(Lambda lambda, List<Object> args) throws EvalError {
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

    private boolean isFalse(Object val) {
        return Boolean.FALSE.equals(val);
    }

    private long asLong(Object val) throws EvalError {
        if (val instanceof Long l) return l;
        throw new EvalError("expected number, got: " + schemeToString(val));
    }

    // --- Display ---

    private String schemeToString(Object val) {
        if (val instanceof Long l) return l.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value() + "\"";
        if (val instanceof String s) return s;
        if (val == NIL) return "()";
        if (val instanceof Pair) {
            StringBuilder sb = new StringBuilder("(");
            Object curr = val;
            boolean first = true;
            while (curr instanceof Pair p) {
                if (!first) sb.append(" ");
                first = false;
                sb.append(schemeToString(p.car));
                curr = p.cdr;
            }
            if (curr != NIL) {
                sb.append(" . ");
                sb.append(schemeToString(curr));
            }
            sb.append(")");
            return sb.toString();
        }
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
