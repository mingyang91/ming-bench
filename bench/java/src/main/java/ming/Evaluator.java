package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Evaluator {

    record SchemeString(String value) {}
    record Pair(Object car, Object cdr) {}
    static final Object NIL = new Object() {
        @Override public String toString() { return "()"; }
    };
    record Lambda(List<String> params, List<Object> body, Env closure) {}
    record Builtin(String name) {}

    static class Env {
        final Map<String, Object> bindings = new HashMap<>();
        final Env parent;
        Env(Env parent) { this.parent = parent; }
        Object lookup(String name) throws EvalError {
            if (bindings.containsKey(name)) return bindings.get(name);
            if (parent != null) return parent.lookup(name);
            throw new EvalError("unbound variable: " + name);
        }
        void define(String name, Object val) { bindings.put(name, val); }
    }

    private static final String[] BUILTIN_NAMES = {
        "+", "-", "*", "/", "<", ">", "=", "<=", ">=",
        "cons", "car", "cdr", "null?", "list", "length", "append",
        "not",
        "string?", "number?", "boolean?", "pair?", "symbol?"
    };

    private final Env globalEnv;

    public Evaluator() {
        globalEnv = new Env(null);
        for (String name : BUILTIN_NAMES) {
            globalEnv.define(name, new Builtin(name));
        }
    }

    public String evalStr(String input) throws EvalError {
        List<Object> tokens = tokenize(input);
        int[] pos = {0};
        Object result = null;
        while (pos[0] < tokens.size()) {
            Object expr = parse(tokens, pos);
            result = eval(expr, globalEnv);
        }
        if (result == null) {
            throw new EvalError("no expression");
        }
        return schemeToString(result);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        throw new EvalError("not implemented");
    }

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
                tokens.add("'");
                i++;
            } else if (c == '#') {
                if (i + 1 < len && (input.charAt(i + 1) == 't' || input.charAt(i + 1) == 'f')) {
                    tokens.add(input.charAt(i + 1) == 't' ? Boolean.TRUE : Boolean.FALSE);
                    i += 2;
                } else {
                    throw new EvalError("unexpected character after #");
                }
            } else if (c == '"') {
                StringBuilder sb = new StringBuilder();
                i++;
                while (i < len && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\' && i + 1 < len) {
                        i++;
                        char esc = input.charAt(i);
                        switch (esc) {
                            case 'n' -> sb.append('\n');
                            case 't' -> sb.append('\t');
                            case '"' -> sb.append('"');
                            case '\\' -> sb.append('\\');
                            default -> sb.append(esc);
                        }
                    } else {
                        sb.append(input.charAt(i));
                    }
                    i++;
                }
                if (i >= len) throw new EvalError("unterminated string");
                i++;
                tokens.add(new SchemeString(sb.toString()));
            } else if (c == '-' && i + 1 < len && Character.isDigit(input.charAt(i + 1))
                    && (tokens.isEmpty() || tokens.getLast().equals("("))) {
                StringBuilder sb = new StringBuilder();
                sb.append('-');
                i++;
                while (i < len && Character.isDigit(input.charAt(i))) {
                    sb.append(input.charAt(i));
                    i++;
                }
                tokens.add(Long.parseLong(sb.toString()));
            } else {
                StringBuilder sb = new StringBuilder();
                while (i < len && !Character.isWhitespace(input.charAt(i))
                        && input.charAt(i) != '(' && input.charAt(i) != ')'
                        && input.charAt(i) != '"' && input.charAt(i) != ';') {
                    sb.append(input.charAt(i));
                    i++;
                }
                String tok = sb.toString();
                try {
                    tokens.add(Long.parseLong(tok));
                } catch (NumberFormatException e) {
                    tokens.add(tok);
                }
            }
        }
        return tokens;
    }

    private Object parse(List<Object> tokens, int[] pos) throws EvalError {
        if (pos[0] >= tokens.size()) {
            throw new EvalError("unexpected end of input");
        }
        Object token = tokens.get(pos[0]);
        if (token.equals("'")) {
            pos[0]++;
            Object datum = parse(tokens, pos);
            List<Object> quoted = new ArrayList<>();
            quoted.add("quote");
            quoted.add(datum);
            return quoted;
        }
        if (token.equals("(")) {
            pos[0]++;
            List<Object> list = new ArrayList<>();
            while (pos[0] < tokens.size() && !tokens.get(pos[0]).equals(")")) {
                list.add(parse(tokens, pos));
            }
            if (pos[0] >= tokens.size()) {
                throw new EvalError("missing closing parenthesis");
            }
            pos[0]++;
            return list;
        } else if (token.equals(")")) {
            throw new EvalError("unexpected )");
        } else {
            pos[0]++;
            return token;
        }
    }

    private Object eval(Object expr, Env env) throws EvalError {
        if (expr instanceof Long || expr instanceof Boolean || expr instanceof SchemeString) {
            return expr;
        }
        if (expr instanceof String symbol) {
            return env.lookup(symbol);
        }
        if (expr instanceof List<?> list) {
            if (list.isEmpty()) {
                throw new EvalError("empty application");
            }
            Object head = list.getFirst();
            if (head instanceof String s) {
                switch (s) {
                    case "define" -> {
                        if (list.size() < 3) throw new EvalError("define: bad syntax");
                        Object target = list.get(1);
                        if (target instanceof String name) {
                            Object val = eval(list.get(2), env);
                            env.define(name, val);
                            return val;
                        } else if (target instanceof List<?> sig) {
                            // (define (f params...) body...)
                            String name = (String) sig.getFirst();
                            List<String> params = new ArrayList<>();
                            for (int i = 1; i < sig.size(); i++) {
                                params.add((String) sig.get(i));
                            }
                            List<Object> body = new ArrayList<>();
                            for (int i = 2; i < list.size(); i++) {
                                body.add(list.get(i));
                            }
                            Lambda lambda = new Lambda(params, body, env);
                            env.define(name, lambda);
                            return lambda;
                        }
                        throw new EvalError("define: bad syntax");
                    }
                    case "if" -> {
                        if (list.size() < 3) throw new EvalError("if: bad syntax");
                        Object cond = eval(list.get(1), env);
                        if (!isFalse(cond)) {
                            return eval(list.get(2), env);
                        } else if (list.size() > 3) {
                            return eval(list.get(3), env);
                        }
                        return null; // unspecified
                    }
                    case "quote" -> {
                        if (list.size() != 2) throw new EvalError("quote: bad syntax");
                        return quote(list.get(1));
                    }
                    case "lambda" -> {
                        if (list.size() < 3) throw new EvalError("lambda: bad syntax");
                        Object paramSpec = list.get(1);
                        List<String> params = new ArrayList<>();
                        if (paramSpec instanceof List<?> plist) {
                            for (Object p : plist) params.add((String) p);
                        } else {
                            throw new EvalError("lambda: bad parameter list");
                        }
                        List<Object> body = new ArrayList<>();
                        for (int i = 2; i < list.size(); i++) {
                            body.add(list.get(i));
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
                    case "begin" -> {
                        Object result2 = null;
                        for (int i = 1; i < list.size(); i++) {
                            result2 = eval(list.get(i), env);
                        }
                        return result2;
                    }
                    case "let" -> {
                        if (list.size() < 3) throw new EvalError("let: bad syntax");
                        // Named let: (let name ((var init) ...) body ...)
                        if (list.get(1) instanceof String loopName) {
                            if (list.size() < 4) throw new EvalError("let: bad syntax");
                            List<?> bindingsList = (List<?>) list.get(2);
                            List<String> params = new ArrayList<>();
                            List<Object> inits = new ArrayList<>();
                            for (Object b : bindingsList) {
                                List<?> binding = (List<?>) b;
                                params.add((String) binding.get(0));
                                inits.add(eval(binding.get(1), env));
                            }
                            List<Object> body = new ArrayList<>();
                            for (int i = 3; i < list.size(); i++) body.add(list.get(i));
                            Env letEnv = new Env(env);
                            Lambda loopLambda = new Lambda(params, body, letEnv);
                            letEnv.define(loopName, loopLambda);
                            // Call with initial values
                            return apply(loopLambda, inits);
                        }
                        // Regular let
                        List<?> bindingsList2 = (List<?>) list.get(1);
                        Env letEnv = new Env(env);
                        for (Object b : bindingsList2) {
                            List<?> binding = (List<?>) b;
                            String name = (String) binding.get(0);
                            Object val = eval(binding.get(1), env);
                            letEnv.define(name, val);
                        }
                        Object result3 = null;
                        for (int i = 2; i < list.size(); i++) {
                            result3 = eval(list.get(i), letEnv);
                        }
                        return result3;
                    }
                    case "cond" -> {
                        for (int i = 1; i < list.size(); i++) {
                            List<?> clause = (List<?>) list.get(i);
                            Object test = clause.getFirst();
                            if (test instanceof String st && st.equals("else")) {
                                Object r = null;
                                for (int j = 1; j < clause.size(); j++) r = eval(clause.get(j), env);
                                return r;
                            }
                            Object testVal = eval(test, env);
                            if (!isFalse(testVal)) {
                                if (clause.size() == 1) return testVal;
                                Object r = null;
                                for (int j = 1; j < clause.size(); j++) r = eval(clause.get(j), env);
                                return r;
                            }
                        }
                        return null;
                    }
                }
            }
            // Function application
            Object proc = eval(head, env);
            List<Object> args = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) {
                args.add(eval(list.get(i), env));
            }
            return apply(proc, args);
        }
        throw new EvalError("cannot eval: " + expr);
    }

    private Object apply(Object proc, List<Object> args) throws EvalError {
        if (proc instanceof Lambda lambda) {
            if (args.size() != lambda.params().size()) {
                throw new EvalError("wrong number of arguments: expected " + lambda.params().size() + ", got " + args.size());
            }
            Env callEnv = new Env(lambda.closure());
            for (int i = 0; i < lambda.params().size(); i++) {
                callEnv.define(lambda.params().get(i), args.get(i));
            }
            Object result = null;
            for (Object bodyExpr : lambda.body()) {
                result = eval(bodyExpr, callEnv);
            }
            return result;
        }
        if (proc instanceof Builtin b) {
            return applyBuiltin(b.name(), args);
        }
        throw new EvalError("not a procedure: " + schemeToString(proc));
    }

    private Object quote(Object datum) {
        if (datum instanceof List<?> list) {
            Object result = NIL;
            for (int i = list.size() - 1; i >= 0; i--) {
                result = new Pair(quote(list.get(i)), result);
            }
            return result;
        }
        return datum;
    }

    private Object applyBuiltin(String op, List<Object> args) throws EvalError {
        return switch (op) {
            case "+" -> {
                long sum = 0;
                for (Object a : args) sum += asLong(a);
                yield sum;
            }
            case "-" -> {
                if (args.isEmpty()) throw new EvalError("-: need at least one argument");
                if (args.size() == 1) yield -asLong(args.getFirst());
                long r = asLong(args.getFirst());
                for (int i = 1; i < args.size(); i++) r -= asLong(args.get(i));
                yield r;
            }
            case "*" -> {
                long p = 1;
                for (Object a : args) p *= asLong(a);
                yield p;
            }
            case "/" -> {
                if (args.size() < 2) throw new EvalError("/: need at least two arguments");
                long r2 = asLong(args.getFirst());
                for (int i = 1; i < args.size(); i++) {
                    long d = asLong(args.get(i));
                    if (d == 0) throw new EvalError("division by zero");
                    r2 /= d;
                }
                yield r2;
            }
            case "<" -> {
                requireArgs(op, args, 2);
                yield asLong(args.get(0)) < asLong(args.get(1)) ? Boolean.TRUE : Boolean.FALSE;
            }
            case ">" -> {
                requireArgs(op, args, 2);
                yield asLong(args.get(0)) > asLong(args.get(1)) ? Boolean.TRUE : Boolean.FALSE;
            }
            case "=" -> {
                requireArgs(op, args, 2);
                yield asLong(args.get(0)) == asLong(args.get(1)) ? Boolean.TRUE : Boolean.FALSE;
            }
            case "<=" -> {
                requireArgs(op, args, 2);
                yield asLong(args.get(0)) <= asLong(args.get(1)) ? Boolean.TRUE : Boolean.FALSE;
            }
            case ">=" -> {
                requireArgs(op, args, 2);
                yield asLong(args.get(0)) >= asLong(args.get(1)) ? Boolean.TRUE : Boolean.FALSE;
            }
            case "cons" -> {
                requireArgs(op, args, 2);
                yield new Pair(args.get(0), args.get(1));
            }
            case "car" -> {
                requireArgs(op, args, 1);
                if (args.get(0) instanceof Pair p) yield p.car();
                throw new EvalError("car: not a pair");
            }
            case "cdr" -> {
                requireArgs(op, args, 1);
                if (args.get(0) instanceof Pair p) yield p.cdr();
                throw new EvalError("cdr: not a pair");
            }
            case "null?" -> {
                requireArgs(op, args, 1);
                yield args.get(0) == NIL ? Boolean.TRUE : Boolean.FALSE;
            }
            case "list" -> {
                Object result = NIL;
                for (int i = args.size() - 1; i >= 0; i--) {
                    result = new Pair(args.get(i), result);
                }
                yield result;
            }
            case "length" -> {
                Object obj = args.get(0);
                long len = 0;
                while (obj instanceof Pair p) {
                    len++;
                    obj = p.cdr();
                }
                if (obj != NIL) throw new EvalError("length: not a proper list");
                yield len;
            }
            case "append" -> {
                Object result = NIL;
                for (int i = args.size() - 1; i >= 0; i--) {
                    Object lst = args.get(i);
                    if (i == args.size() - 1) {
                        result = lst;
                    } else {
                        // Prepend elements of lst onto result
                        List<Object> elems = new ArrayList<>();
                        Object cur = lst;
                        while (cur instanceof Pair p) {
                            elems.add(p.car());
                            cur = p.cdr();
                        }
                        for (int j = elems.size() - 1; j >= 0; j--) {
                            result = new Pair(elems.get(j), result);
                        }
                    }
                }
                yield result;
            }
            case "not" -> {
                requireArgs(op, args, 1);
                yield isFalse(args.get(0)) ? Boolean.TRUE : Boolean.FALSE;
            }
            case "string?" -> {
                requireArgs(op, args, 1);
                yield args.get(0) instanceof SchemeString ? Boolean.TRUE : Boolean.FALSE;
            }
            case "number?" -> {
                requireArgs(op, args, 1);
                yield args.get(0) instanceof Long ? Boolean.TRUE : Boolean.FALSE;
            }
            case "boolean?" -> {
                requireArgs(op, args, 1);
                yield args.get(0) instanceof Boolean ? Boolean.TRUE : Boolean.FALSE;
            }
            case "pair?" -> {
                requireArgs(op, args, 1);
                yield args.get(0) instanceof Pair ? Boolean.TRUE : Boolean.FALSE;
            }
            case "symbol?" -> {
                requireArgs(op, args, 1);
                yield args.get(0) instanceof String ? Boolean.TRUE : Boolean.FALSE;
            }
            default -> throw new EvalError("unbound variable: " + op);
        };
    }

    private boolean isFalse(Object val) {
        return val instanceof Boolean b && !b;
    }

    private long asLong(Object val) throws EvalError {
        if (val instanceof Long l) return l;
        throw new EvalError("expected number, got: " + schemeToString(val));
    }

    private void requireArgs(String op, List<Object> args, int n) throws EvalError {
        if (args.size() != n)
            throw new EvalError(op + ": expected " + n + " arguments, got " + args.size());
    }

    private String schemeToString(Object val) {
        if (val instanceof Long l) return l.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value() + "\"";
        if (val == NIL) return "()";
        if (val instanceof Pair) {
            StringBuilder sb = new StringBuilder("(");
            Object cur = val;
            boolean first = true;
            while (cur instanceof Pair p) {
                if (!first) sb.append(" ");
                first = false;
                sb.append(schemeToString(p.car()));
                cur = p.cdr();
            }
            if (cur != NIL) {
                sb.append(" . ");
                sb.append(schemeToString(cur));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof Lambda) return "#<procedure>";
        return val.toString();
    }
}
