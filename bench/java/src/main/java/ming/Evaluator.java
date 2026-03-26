package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Evaluator {

    record SchemeString(String value) {}
    record SchemeList(List<Object> elements) {}
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
        "+", "-", "*", "/", "<", ">", "=", "<=", ">="
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
                    case "not" -> {
                        if (list.size() != 2) throw new EvalError("not: wrong argument count");
                        Object val = eval(list.get(1), env);
                        return isFalse(val) ? Boolean.TRUE : Boolean.FALSE;
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
            List<Object> quoted = new ArrayList<>();
            for (Object el : list) {
                quoted.add(quote(el));
            }
            return new SchemeList(quoted);
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
        if (val instanceof SchemeList sl) {
            StringBuilder sb = new StringBuilder("(");
            for (int i = 0; i < sl.elements().size(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(schemeToString(sl.elements().get(i)));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof Lambda) return "#<procedure>";
        return val.toString();
    }
}
