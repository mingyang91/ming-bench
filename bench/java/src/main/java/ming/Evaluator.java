package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Evaluator {

    // --- Sentinel for empty list ---
    private static final Object EMPTY_LIST = new Object() {
        @Override public String toString() { return "()"; }
    };

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

        void define(String name, Object val) {
            bindings.put(name, val);
        }
    }

    // --- Pair (cons cell) ---

    private static class Pair {
        Object car;
        Object cdr;
        Pair(Object car, Object cdr) {
            this.car = car;
            this.cdr = cdr;
        }
    }

    // --- Lambda (closure) ---

    private record Lambda(List<String> params, List<Object> body, Env closureEnv) {}

    // --- Builtin procedure ---

    @FunctionalInterface
    private interface Builtin {
        Object apply(List<Object> args) throws EvalError;
    }

    private record BuiltinProc(String name, Builtin fn) {}

    public String evalStr(String input) throws EvalError {
        List<Object> tokens = tokenize(input);
        int[] pos = {0};
        Object lastResult = null;
        Env env = createGlobalEnv();
        while (pos[0] < tokens.size()) {
            Object expr = parse(tokens, pos);
            lastResult = eval(expr, env);
        }
        if (lastResult == null) {
            throw new EvalError("no expression");
        }
        return schemeToString(lastResult);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        throw new EvalError("not implemented");
    }

    private Env createGlobalEnv() {
        Env env = new Env(null);
        // List operations
        env.define("cons", new BuiltinProc("cons", args -> {
            if (args.size() != 2) throw new EvalError("cons: expected 2 args");
            return new Pair(args.get(0), args.get(1));
        }));
        env.define("car", new BuiltinProc("car", args -> {
            if (args.size() != 1) throw new EvalError("car: expected 1 arg");
            if (!(args.get(0) instanceof Pair p)) throw new EvalError("car: not a pair");
            return p.car;
        }));
        env.define("cdr", new BuiltinProc("cdr", args -> {
            if (args.size() != 1) throw new EvalError("cdr: expected 1 arg");
            if (!(args.get(0) instanceof Pair p)) throw new EvalError("cdr: not a pair");
            return p.cdr;
        }));
        env.define("null?", new BuiltinProc("null?", args -> {
            if (args.size() != 1) throw new EvalError("null?: expected 1 arg");
            return args.get(0) == EMPTY_LIST ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("list", new BuiltinProc("list", args -> {
            Object result = EMPTY_LIST;
            for (int i = args.size() - 1; i >= 0; i--) {
                result = new Pair(args.get(i), result);
            }
            return result;
        }));
        env.define("length", new BuiltinProc("length", args -> {
            if (args.size() != 1) throw new EvalError("length: expected 1 arg");
            Object obj = args.get(0);
            long count = 0;
            while (obj instanceof Pair p) {
                count++;
                obj = p.cdr;
            }
            if (obj != EMPTY_LIST) throw new EvalError("length: not a proper list");
            return count;
        }));
        env.define("append", new BuiltinProc("append", args -> {
            if (args.size() == 0) return EMPTY_LIST;
            if (args.size() == 1) return args.get(0);
            // For 2 args: copy first list, attach second
            Object a = args.get(0);
            Object b = args.get(1);
            if (a == EMPTY_LIST) return b;
            List<Object> elems = new ArrayList<>();
            Object curr = a;
            while (curr instanceof Pair p) {
                elems.add(p.car);
                curr = p.cdr;
            }
            Object result = b;
            for (int i = elems.size() - 1; i >= 0; i--) {
                result = new Pair(elems.get(i), result);
            }
            return result;
        }));
        // Type predicates
        env.define("number?", new BuiltinProc("number?", args -> {
            if (args.size() != 1) throw new EvalError("number?: expected 1 arg");
            return args.get(0) instanceof Long ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("string?", new BuiltinProc("string?", args -> {
            if (args.size() != 1) throw new EvalError("string?: expected 1 arg");
            return args.get(0) instanceof SchemeString ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("boolean?", new BuiltinProc("boolean?", args -> {
            if (args.size() != 1) throw new EvalError("boolean?: expected 1 arg");
            return args.get(0) instanceof Boolean ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("pair?", new BuiltinProc("pair?", args -> {
            if (args.size() != 1) throw new EvalError("pair?: expected 1 arg");
            return args.get(0) instanceof Pair ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("symbol?", new BuiltinProc("symbol?", args -> {
            if (args.size() != 1) throw new EvalError("symbol?: expected 1 arg");
            return args.get(0) instanceof String ? Boolean.TRUE : Boolean.FALSE;
        }));
        // Arithmetic as builtins
        env.define("+", new BuiltinProc("+", args -> {
            long result = 0;
            for (Object a : args) result += asLong(a);
            return result;
        }));
        env.define("-", new BuiltinProc("-", args -> {
            if (args.isEmpty()) throw new EvalError("-: need at least 1 argument");
            if (args.size() == 1) return -asLong(args.get(0));
            long result = asLong(args.get(0));
            for (int i = 1; i < args.size(); i++) result -= asLong(args.get(i));
            return result;
        }));
        env.define("*", new BuiltinProc("*", args -> {
            long result = 1;
            for (Object a : args) result *= asLong(a);
            return result;
        }));
        env.define("/", new BuiltinProc("/", args -> {
            if (args.size() < 2) throw new EvalError("/: need at least 2 arguments");
            long result = asLong(args.get(0));
            for (int i = 1; i < args.size(); i++) {
                long divisor = asLong(args.get(i));
                if (divisor == 0) throw new EvalError("division by zero");
                result /= divisor;
            }
            return result;
        }));
        // Comparisons
        for (String op : new String[]{"<", ">", "=", "<=", ">="}) {
            env.define(op, new BuiltinProc(op, args -> {
                if (args.size() < 2) throw new EvalError(op + ": need at least 2 arguments");
                long prev = asLong(args.get(0));
                for (int i = 1; i < args.size(); i++) {
                    long curr = asLong(args.get(i));
                    boolean ok = switch (op) {
                        case "<" -> prev < curr;
                        case ">" -> prev > curr;
                        case "=" -> prev == curr;
                        case "<=" -> prev <= curr;
                        case ">=" -> prev >= curr;
                        default -> false;
                    };
                    if (!ok) return Boolean.FALSE;
                    prev = curr;
                }
                return Boolean.TRUE;
            }));
        }
        env.define("not", new BuiltinProc("not", args -> {
            if (args.size() != 1) throw new EvalError("not: expected 1 arg");
            return isTruthy(args.get(0)) ? Boolean.FALSE : Boolean.TRUE;
        }));
        return env;
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
                tokens.add("'");
                i++;
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
                if (i < len) i++; // skip closing quote
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
                // symbol or number
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
                throw new EvalError("missing closing paren");
            }
            pos[0]++; // skip )
            return list;
        } else if (token.equals(")")) {
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
        if (expr instanceof List<?> list) {
            if (list.isEmpty()) {
                throw new EvalError("empty application");
            }
            Object head = list.get(0);

            // Special forms
            if (head instanceof String op) {
                switch (op) {
                    case "quote" -> {
                        if (list.size() != 2) throw new EvalError("quote: expected 1 argument");
                        return quoteValue(list.get(1));
                    }
                    case "if" -> {
                        if (list.size() < 3 || list.size() > 4)
                            throw new EvalError("if: expected 2-3 arguments");
                        Object cond = eval(list.get(1), env);
                        if (isTruthy(cond)) {
                            return eval(list.get(2), env);
                        } else if (list.size() == 4) {
                            return eval(list.get(3), env);
                        } else {
                            return Boolean.FALSE;
                        }
                    }
                    case "define" -> {
                        if (list.size() < 3) throw new EvalError("define: too few arguments");
                        Object target = list.get(1);
                        if (target instanceof String name) {
                            Object val = eval(list.get(2), env);
                            env.define(name, val);
                            return val;
                        } else if (target instanceof List<?> sig) {
                            if (sig.isEmpty() || !(sig.get(0) instanceof String fname))
                                throw new EvalError("define: invalid function signature");
                            List<String> params = new ArrayList<>();
                            for (int i = 1; i < sig.size(); i++) {
                                if (!(sig.get(i) instanceof String pname))
                                    throw new EvalError("define: parameter must be symbol");
                                params.add(pname);
                            }
                            List<Object> body = new ArrayList<>(list.subList(2, list.size()));
                            Lambda lambda = new Lambda(params, body, env);
                            env.define(fname, lambda);
                            return lambda;
                        } else {
                            throw new EvalError("define: invalid syntax");
                        }
                    }
                    case "lambda" -> {
                        if (list.size() < 3) throw new EvalError("lambda: too few arguments");
                        Object paramSpec = list.get(1);
                        if (!(paramSpec instanceof List<?> paramList))
                            throw new EvalError("lambda: params must be a list");
                        List<String> params = new ArrayList<>();
                        for (Object p : paramList) {
                            if (!(p instanceof String pname))
                                throw new EvalError("lambda: parameter must be symbol");
                            params.add(pname);
                        }
                        List<Object> body = new ArrayList<>(list.subList(2, list.size()));
                        return new Lambda(params, body, env);
                    }
                    case "and" -> {
                        Object result = Boolean.TRUE;
                        for (int i = 1; i < list.size(); i++) {
                            result = eval(list.get(i), env);
                            if (!isTruthy(result)) return result;
                        }
                        return result;
                    }
                    case "or" -> {
                        Object result = Boolean.FALSE;
                        for (int i = 1; i < list.size(); i++) {
                            result = eval(list.get(i), env);
                            if (isTruthy(result)) return result;
                        }
                        return result;
                    }
                    case "begin" -> {
                        Object result = Boolean.FALSE;
                        for (int i = 1; i < list.size(); i++) {
                            result = eval(list.get(i), env);
                        }
                        return result;
                    }
                    case "cond" -> {
                        for (int i = 1; i < list.size(); i++) {
                            if (!(list.get(i) instanceof List<?> clause) || clause.isEmpty())
                                throw new EvalError("cond: invalid clause");
                            Object test = clause.get(0);
                            if (test instanceof String s && s.equals("else")) {
                                // else clause - eval body
                                Object result = Boolean.FALSE;
                                for (int j = 1; j < clause.size(); j++) {
                                    result = eval(clause.get(j), env);
                                }
                                return result;
                            }
                            Object testVal = eval(test, env);
                            if (isTruthy(testVal)) {
                                if (clause.size() == 1) return testVal;
                                Object result = testVal;
                                for (int j = 1; j < clause.size(); j++) {
                                    result = eval(clause.get(j), env);
                                }
                                return result;
                            }
                        }
                        return Boolean.FALSE; // no clause matched
                    }
                    case "let" -> {
                        // Check for named let: (let name ((var init) ...) body ...)
                        int bindingsIdx = 1;
                        String namedLetName = null;
                        if (list.get(1) instanceof String name) {
                            namedLetName = name;
                            bindingsIdx = 2;
                        }
                        if (!(list.get(bindingsIdx) instanceof List<?> bindingList))
                            throw new EvalError("let: bindings must be a list");
                        List<String> varNames = new ArrayList<>();
                        List<Object> initExprs = new ArrayList<>();
                        for (Object b : bindingList) {
                            if (!(b instanceof List<?> binding) || binding.size() != 2)
                                throw new EvalError("let: invalid binding");
                            if (!(binding.get(0) instanceof String vname))
                                throw new EvalError("let: binding name must be symbol");
                            varNames.add(vname);
                            initExprs.add(binding.get(1));
                        }
                        Env letEnv = new Env(env);
                        if (namedLetName != null) {
                            // Named let: create a lambda and bind it
                            List<Object> bodyExprs = new ArrayList<>(list.subList(bindingsIdx + 1, list.size()));
                            Lambda loopLambda = new Lambda(varNames, bodyExprs, letEnv);
                            letEnv.define(namedLetName, loopLambda);
                            // Evaluate init expressions in outer env, bind in letEnv
                            for (int i = 0; i < varNames.size(); i++) {
                                letEnv.define(varNames.get(i), eval(initExprs.get(i), env));
                            }
                            return evalBody(bodyExprs, letEnv);
                        } else {
                            // Regular let
                            for (int i = 0; i < varNames.size(); i++) {
                                letEnv.define(varNames.get(i), eval(initExprs.get(i), env));
                            }
                            return evalBody(new ArrayList<>(list.subList(bindingsIdx + 1, list.size())), letEnv);
                        }
                    }
                }
            }

            // Procedure application
            Object proc = eval(head, env);
            List<Object> args = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) {
                args.add(eval(list.get(i), env));
            }
            return applyProc(proc, args);
        }
        throw new EvalError("cannot evaluate: " + expr);
    }

    private Object applyProc(Object proc, List<Object> args) throws EvalError {
        if (proc instanceof Lambda lambda) {
            if (args.size() != lambda.params.size()) {
                throw new EvalError("wrong number of arguments: expected " +
                        lambda.params.size() + ", got " + args.size());
            }
            Env callEnv = new Env(lambda.closureEnv);
            for (int i = 0; i < lambda.params.size(); i++) {
                callEnv.define(lambda.params.get(i), args.get(i));
            }
            return evalBody(lambda.body, callEnv);
        }
        if (proc instanceof BuiltinProc bp) {
            return bp.fn.apply(args);
        }
        throw new EvalError("not a procedure: " + schemeToString(proc));
    }

    private Object evalBody(List<Object> body, Env env) throws EvalError {
        Object result = Boolean.FALSE;
        for (Object expr : body) {
            result = eval(expr, env);
        }
        return result;
    }

    private Object quoteValue(Object datum) {
        if (datum instanceof List<?> list) {
            Object result = EMPTY_LIST;
            for (int i = list.size() - 1; i >= 0; i--) {
                result = new Pair(quoteValue(list.get(i)), result);
            }
            return result;
        }
        return datum;
    }

    private boolean isTruthy(Object val) {
        return !(val instanceof Boolean b && !b);
    }

    private static long asLong(Object val) throws EvalError {
        if (val instanceof Long l) return l;
        throw new EvalError("expected number, got: " + val);
    }

    private String schemeToString(Object val) {
        if (val instanceof Long l) return l.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value() + "\"";
        if (val == EMPTY_LIST) return "()";
        if (val instanceof Pair p) {
            StringBuilder sb = new StringBuilder("(");
            sb.append(schemeToString(p.car));
            Object rest = p.cdr;
            while (rest instanceof Pair rp) {
                sb.append(" ");
                sb.append(schemeToString(rp.car));
                rest = rp.cdr;
            }
            if (rest != EMPTY_LIST) {
                sb.append(" . ");
                sb.append(schemeToString(rest));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof Lambda) return "#<procedure>";
        if (val instanceof BuiltinProc) return "#<procedure>";
        if (val instanceof String s) return s;
        return val.toString();
    }

    // Internal type to distinguish Scheme strings from symbols (Java Strings)
    record SchemeString(String value) {}
}
