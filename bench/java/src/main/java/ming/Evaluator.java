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

    // --- Source position ---

    private record Pos(int line, int col) {
        String fmt() { return line + ":" + col; }
    }

    // --- Token with position ---

    private record Token(Object value, int line, int col) {}

    // --- Located AST node ---

    private static class Located {
        final Object expr;
        final int line;
        final int col;
        Located(Object expr, int line, int col) {
            this.expr = expr;
            this.line = line;
            this.col = col;
        }
    }

    // --- Environment ---

    private static class Env {
        final Map<String, Object> bindings = new HashMap<>();
        final Env parent;

        Env(Env parent) {
            this.parent = parent;
        }

        Object lookup(String name, Pos pos) throws EvalError {
            if (bindings.containsKey(name)) return bindings.get(name);
            if (parent != null) return parent.lookup(name, pos);
            throw new EvalError("unbound variable: " + name + " at " + pos.fmt());
        }

        void define(String name, Object val) {
            bindings.put(name, val);
        }

        void set(String name, Object val, Pos pos) throws EvalError {
            if (bindings.containsKey(name)) {
                bindings.put(name, val);
                return;
            }
            if (parent != null) {
                parent.set(name, val, pos);
                return;
            }
            throw new EvalError("set!: unbound variable: " + name + " at " + pos.fmt());
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

    private record Lambda(List<String> params, String restParam, List<Object> body, Env closureEnv) {}

    // --- Builtin procedure ---

    @FunctionalInterface
    private interface Builtin {
        Object apply(List<Object> args) throws EvalError;
    }

    private record BuiltinProc(String name, Builtin fn) {}

    // Output buffer for display/write/newline
    private StringBuilder outputBuffer;

    public String evalStr(String input) throws EvalError {
        outputBuffer = new StringBuilder();
        List<Token> tokens = tokenize(input);
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
        outputBuffer = new StringBuilder();
        List<Token> tokens = tokenize(input);
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
        return new EvalResult(schemeToString(lastResult), outputBuffer.toString());
    }

    private Env createGlobalEnv() {
        Env env = new Env(null);
        registerListOps(env);
        registerTypePredicates(env);
        registerArithmetic(env);
        registerIOOps(env);
        registerStringOps(env);
        registerNumericUtils(env);
        registerListUtils(env);
        registerCharOps(env);
        registerStringComparisons(env);
        registerHigherOrder(env);
        return env;
    }

    private void registerListOps(Env env) {
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
    }

    private void registerTypePredicates(Env env) {
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
    }

    private void registerArithmetic(Env env) {
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
    }

    private void registerIOOps(Env env) {
        env.define("display", new BuiltinProc("display", args -> {
            if (args.size() != 1) throw new EvalError("display: expected 1 arg");
            outputBuffer.append(displayString(args.get(0)));
            return Boolean.FALSE;
        }));
        env.define("write", new BuiltinProc("write", args -> {
            if (args.size() != 1) throw new EvalError("write: expected 1 arg");
            outputBuffer.append(schemeToString(args.get(0)));
            return Boolean.FALSE;
        }));
        env.define("newline", new BuiltinProc("newline", args -> {
            if (args.size() != 0) throw new EvalError("newline: expected 0 args");
            outputBuffer.append("\n");
            return Boolean.FALSE;
        }));
    }

    private void registerStringOps(Env env) {
        env.define("string-append", new BuiltinProc("string-append", args -> {
            StringBuilder sb = new StringBuilder();
            for (Object a : args) {
                if (!(a instanceof SchemeString s)) throw new EvalError("string-append: expected string");
                sb.append(s.value());
            }
            return new SchemeString(sb.toString());
        }));
        env.define("string-length", new BuiltinProc("string-length", args -> {
            if (args.size() != 1) throw new EvalError("string-length: expected 1 arg");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-length: expected string");
            return (long) s.value().length();
        }));
        env.define("substring", new BuiltinProc("substring", args -> {
            if (args.size() < 2 || args.size() > 3) throw new EvalError("substring: expected 2-3 args");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("substring: expected string");
            int start = (int) asLong(args.get(1));
            int end = args.size() == 3 ? (int) asLong(args.get(2)) : s.value().length();
            return new SchemeString(s.value().substring(start, end));
        }));
        env.define("string->number", new BuiltinProc("string->number", args -> {
            if (args.size() != 1) throw new EvalError("string->number: expected 1 arg");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string->number: expected string");
            try {
                return Long.parseLong(s.value());
            } catch (NumberFormatException e) {
                return Boolean.FALSE;
            }
        }));
        env.define("number->string", new BuiltinProc("number->string", args -> {
            if (args.size() != 1) throw new EvalError("number->string: expected 1 arg");
            return new SchemeString(Long.toString(asLong(args.get(0))));
        }));
        env.define("symbol->string", new BuiltinProc("symbol->string", args -> {
            if (args.size() != 1) throw new EvalError("symbol->string: expected 1 arg");
            if (!(args.get(0) instanceof String s)) throw new EvalError("symbol->string: expected symbol");
            return new SchemeString(s);
        }));
        env.define("string->symbol", new BuiltinProc("string->symbol", args -> {
            if (args.size() != 1) throw new EvalError("string->symbol: expected 1 arg");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string->symbol: expected string");
            return s.value();
        }));
        env.define("string-ref", new BuiltinProc("string-ref", args -> {
            if (args.size() != 2) throw new EvalError("string-ref: expected 2 args");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-ref: expected string");
            int idx = (int) asLong(args.get(1));
            return new SchemeChar(s.value().charAt(idx));
        }));
        env.define("char?", new BuiltinProc("char?", args -> {
            if (args.size() != 1) throw new EvalError("char?: expected 1 arg");
            return args.get(0) instanceof SchemeChar ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("string-copy", new BuiltinProc("string-copy", args -> {
            if (args.size() != 1) throw new EvalError("string-copy: expected 1 arg");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-copy: expected string");
            return s.copy();
        }));
        env.define("string-set!", new BuiltinProc("string-set!", args -> {
            if (args.size() != 3) throw new EvalError("string-set!: expected 3 args");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-set!: expected string");
            int idx = (int) asLong(args.get(1));
            if (!(args.get(2) instanceof SchemeChar c)) throw new EvalError("string-set!: expected char");
            s.setChar(idx, c.value());
            return Boolean.FALSE;
        }));
    }

    private void registerNumericUtils(Env env) {
        env.define("abs", new BuiltinProc("abs", args -> {
            if (args.size() != 1) throw new EvalError("abs: expected 1 arg");
            return Math.abs(asLong(args.get(0)));
        }));
        env.define("modulo", new BuiltinProc("modulo", args -> {
            if (args.size() != 2) throw new EvalError("modulo: expected 2 args");
            long a = asLong(args.get(0)), b = asLong(args.get(1));
            return Math.floorMod(a, b);
        }));
        env.define("remainder", new BuiltinProc("remainder", args -> {
            if (args.size() != 2) throw new EvalError("remainder: expected 2 args");
            long a = asLong(args.get(0)), b = asLong(args.get(1));
            return a % b;
        }));
        env.define("quotient", new BuiltinProc("quotient", args -> {
            if (args.size() != 2) throw new EvalError("quotient: expected 2 args");
            long a = asLong(args.get(0)), b = asLong(args.get(1));
            return a / b;
        }));
        env.define("min", new BuiltinProc("min", args -> {
            if (args.isEmpty()) throw new EvalError("min: need at least 1 argument");
            long result = asLong(args.get(0));
            for (int i = 1; i < args.size(); i++) result = Math.min(result, asLong(args.get(i)));
            return result;
        }));
        env.define("max", new BuiltinProc("max", args -> {
            if (args.isEmpty()) throw new EvalError("max: need at least 1 argument");
            long result = asLong(args.get(0));
            for (int i = 1; i < args.size(); i++) result = Math.max(result, asLong(args.get(i)));
            return result;
        }));
        env.define("expt", new BuiltinProc("expt", args -> {
            if (args.size() != 2) throw new EvalError("expt: expected 2 args");
            long base = asLong(args.get(0)), exp = asLong(args.get(1));
            long result = 1;
            for (long i = 0; i < exp; i++) result *= base;
            return result;
        }));
        env.define("zero?", new BuiltinProc("zero?", args -> {
            if (args.size() != 1) throw new EvalError("zero?: expected 1 arg");
            return asLong(args.get(0)) == 0 ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("positive?", new BuiltinProc("positive?", args -> {
            if (args.size() != 1) throw new EvalError("positive?: expected 1 arg");
            return asLong(args.get(0)) > 0 ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("negative?", new BuiltinProc("negative?", args -> {
            if (args.size() != 1) throw new EvalError("negative?: expected 1 arg");
            return asLong(args.get(0)) < 0 ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("odd?", new BuiltinProc("odd?", args -> {
            if (args.size() != 1) throw new EvalError("odd?: expected 1 arg");
            return asLong(args.get(0)) % 2 != 0 ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("even?", new BuiltinProc("even?", args -> {
            if (args.size() != 1) throw new EvalError("even?: expected 1 arg");
            return asLong(args.get(0)) % 2 == 0 ? Boolean.TRUE : Boolean.FALSE;
        }));
    }

    private void registerListUtils(Env env) {
        env.define("list-ref", new BuiltinProc("list-ref", args -> {
            if (args.size() != 2) throw new EvalError("list-ref: expected 2 args");
            Object lst = args.get(0);
            int idx = (int) asLong(args.get(1));
            for (int i = 0; i < idx; i++) {
                if (!(lst instanceof Pair p)) throw new EvalError("list-ref: index out of range");
                lst = p.cdr;
            }
            if (!(lst instanceof Pair p)) throw new EvalError("list-ref: index out of range");
            return p.car;
        }));
        env.define("list-tail", new BuiltinProc("list-tail", args -> {
            if (args.size() != 2) throw new EvalError("list-tail: expected 2 args");
            Object lst = args.get(0);
            int idx = (int) asLong(args.get(1));
            for (int i = 0; i < idx; i++) {
                if (!(lst instanceof Pair p)) throw new EvalError("list-tail: index out of range");
                lst = p.cdr;
            }
            return lst;
        }));
        env.define("list?", new BuiltinProc("list?", args -> {
            if (args.size() != 1) throw new EvalError("list?: expected 1 arg");
            Object obj = args.get(0);
            while (obj instanceof Pair p) obj = p.cdr;
            return obj == EMPTY_LIST ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("assoc", new BuiltinProc("assoc", args -> {
            if (args.size() != 2) throw new EvalError("assoc: expected 2 args");
            Object key = args.get(0);
            Object alist = args.get(1);
            while (alist instanceof Pair p) {
                if (p.car instanceof Pair entry && schemeEqual(entry.car, key)) return entry;
                alist = p.cdr;
            }
            return Boolean.FALSE;
        }));
    }

    private void registerCharOps(Env env) {
        env.define("char-alphabetic?", new BuiltinProc("char-alphabetic?", args -> {
            if (args.size() != 1) throw new EvalError("char-alphabetic?: expected 1 arg");
            if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError("char-alphabetic?: expected char");
            return Character.isLetter(c.value()) ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("char-numeric?", new BuiltinProc("char-numeric?", args -> {
            if (args.size() != 1) throw new EvalError("char-numeric?: expected 1 arg");
            if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError("char-numeric?: expected char");
            return Character.isDigit(c.value()) ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("char-upcase", new BuiltinProc("char-upcase", args -> {
            if (args.size() != 1) throw new EvalError("char-upcase: expected 1 arg");
            if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError("char-upcase: expected char");
            return new SchemeChar(Character.toUpperCase(c.value()));
        }));
        env.define("char-downcase", new BuiltinProc("char-downcase", args -> {
            if (args.size() != 1) throw new EvalError("char-downcase: expected 1 arg");
            if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError("char-downcase: expected char");
            return new SchemeChar(Character.toLowerCase(c.value()));
        }));
        env.define("char=?", new BuiltinProc("char=?", args -> {
            if (args.size() != 2) throw new EvalError("char=?: expected 2 args");
            if (!(args.get(0) instanceof SchemeChar a) || !(args.get(1) instanceof SchemeChar b))
                throw new EvalError("char=?: expected chars");
            return a.value() == b.value() ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("char<?", new BuiltinProc("char<?", args -> {
            if (args.size() != 2) throw new EvalError("char<?: expected 2 args");
            if (!(args.get(0) instanceof SchemeChar a) || !(args.get(1) instanceof SchemeChar b))
                throw new EvalError("char<?: expected chars");
            return a.value() < b.value() ? Boolean.TRUE : Boolean.FALSE;
        }));
    }

    private void registerStringComparisons(Env env) {
        env.define("string=?", new BuiltinProc("string=?", args -> {
            if (args.size() != 2) throw new EvalError("string=?: expected 2 args");
            if (!(args.get(0) instanceof SchemeString a) || !(args.get(1) instanceof SchemeString b))
                throw new EvalError("string=?: expected strings");
            return a.value().equals(b.value()) ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("string<?", new BuiltinProc("string<?", args -> {
            if (args.size() != 2) throw new EvalError("string<?: expected 2 args");
            if (!(args.get(0) instanceof SchemeString a) || !(args.get(1) instanceof SchemeString b))
                throw new EvalError("string<?: expected strings");
            return a.value().compareTo(b.value()) < 0 ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("string-ci=?", new BuiltinProc("string-ci=?", args -> {
            if (args.size() != 2) throw new EvalError("string-ci=?: expected 2 args");
            if (!(args.get(0) instanceof SchemeString a) || !(args.get(1) instanceof SchemeString b))
                throw new EvalError("string-ci=?: expected strings");
            return a.value().equalsIgnoreCase(b.value()) ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("string-upcase", new BuiltinProc("string-upcase", args -> {
            if (args.size() != 1) throw new EvalError("string-upcase: expected 1 arg");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-upcase: expected string");
            return new SchemeString(s.value().toUpperCase());
        }));
        env.define("string-downcase", new BuiltinProc("string-downcase", args -> {
            if (args.size() != 1) throw new EvalError("string-downcase: expected 1 arg");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-downcase: expected string");
            return new SchemeString(s.value().toLowerCase());
        }));
    }

    private void registerHigherOrder(Env env) {
        env.define("eq?", new BuiltinProc("eq?", args -> {
            if (args.size() != 2) throw new EvalError("eq?: expected 2 args");
            Object a = args.get(0), b = args.get(1);
            if (a == b) return Boolean.TRUE;
            if (a instanceof Long && b instanceof Long) return a.equals(b) ? Boolean.TRUE : Boolean.FALSE;
            if (a instanceof Boolean && b instanceof Boolean) return a.equals(b) ? Boolean.TRUE : Boolean.FALSE;
            if (a instanceof SchemeChar && b instanceof SchemeChar) return a.equals(b) ? Boolean.TRUE : Boolean.FALSE;
            if (a instanceof String && b instanceof String) return a.equals(b) ? Boolean.TRUE : Boolean.FALSE;
            return Boolean.FALSE;
        }));
        env.define("equal?", new BuiltinProc("equal?", args -> {
            if (args.size() != 2) throw new EvalError("equal?: expected 2 args");
            return schemeEqual(args.get(0), args.get(1)) ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("map", new BuiltinProc("map", args -> {
            if (args.size() < 2) throw new EvalError("map: expected at least 2 args");
            Object proc = args.get(0);
            int numLists = args.size() - 1;
            Object[] currents = new Object[numLists];
            for (int i = 0; i < numLists; i++) currents[i] = args.get(i + 1);
            List<Object> results = new ArrayList<>();
            while (true) {
                boolean allPairs = true;
                for (Object c : currents) {
                    if (!(c instanceof Pair)) { allPairs = false; break; }
                }
                if (!allPairs) break;
                List<Object> callArgs = new ArrayList<>();
                for (int i = 0; i < numLists; i++) {
                    callArgs.add(((Pair) currents[i]).car);
                    currents[i] = ((Pair) currents[i]).cdr;
                }
                results.add(applyProc(proc, callArgs));
            }
            Object result = EMPTY_LIST;
            for (int i = results.size() - 1; i >= 0; i--) result = new Pair(results.get(i), result);
            return result;
        }));
        env.define("apply", new BuiltinProc("apply", args -> {
            if (args.size() < 2) throw new EvalError("apply: expected at least 2 args");
            Object proc = args.get(0);
            Object lastArg = args.get(args.size() - 1);
            List<Object> callArgs = new ArrayList<>();
            for (int i = 1; i < args.size() - 1; i++) {
                callArgs.add(args.get(i));
            }
            Object rest = lastArg;
            while (rest instanceof Pair p) {
                callArgs.add(p.car);
                rest = p.cdr;
            }
            return applyProc(proc, callArgs);
        }));
    }

    // --- Tokenizer ---

    private List<Token> tokenize(String input) throws EvalError {
        List<Token> tokens = new ArrayList<>();
        int i = 0;
        int len = input.length();
        int line = 1, col = 1;
        while (i < len) {
            char c = input.charAt(i);
            if (Character.isWhitespace(c)) {
                if (c == '\n') { line++; col = 1; } else { col++; }
                i++;
            } else if (c == ';') {
                while (i < len && input.charAt(i) != '\n') { i++; col++; }
            } else if (c == '(') {
                tokens.add(new Token("(", line, col));
                i++; col++;
            } else if (c == ')') {
                tokens.add(new Token(")", line, col));
                i++; col++;
            } else if (c == '\'') {
                tokens.add(new Token("'", line, col));
                i++; col++;
            } else if (c == '"') {
                int startLine = line, startCol = col;
                StringBuilder sb = new StringBuilder();
                i++; col++; // skip opening quote
                while (i < len && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\') {
                        i++; col++;
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
                        if (input.charAt(i) == '\n') { line++; col = 0; }
                        sb.append(input.charAt(i));
                    }
                    i++; col++;
                }
                if (i < len) { i++; col++; } // skip closing quote
                tokens.add(new Token(new SchemeString(sb.toString()), startLine, startCol));
            } else if (c == '#') {
                int startCol = col;
                if (i + 1 < len) {
                    char next = input.charAt(i + 1);
                    if (next == 't') {
                        tokens.add(new Token(Boolean.TRUE, line, startCol));
                        i += 2; col += 2;
                    } else if (next == 'f') {
                        tokens.add(new Token(Boolean.FALSE, line, startCol));
                        i += 2; col += 2;
                    } else if (next == '\\') {
                        // Character literal #\x or #\space etc.
                        i += 2; col += 2;
                        if (i >= len) throw new EvalError("unexpected end after #\\ at " + line + ":" + startCol);
                        // Read the character name
                        StringBuilder charName = new StringBuilder();
                        while (i < len && !Character.isWhitespace(input.charAt(i))
                                && input.charAt(i) != '(' && input.charAt(i) != ')'
                                && input.charAt(i) != '"' && input.charAt(i) != ';') {
                            charName.append(input.charAt(i));
                            i++; col++;
                        }
                        String cn = charName.toString();
                        char ch;
                        if (cn.length() == 1) {
                            ch = cn.charAt(0);
                        } else if (cn.equals("space")) {
                            ch = ' ';
                        } else if (cn.equals("newline")) {
                            ch = '\n';
                        } else if (cn.equals("tab")) {
                            ch = '\t';
                        } else {
                            throw new EvalError("unknown character name: " + cn + " at " + line + ":" + startCol);
                        }
                        tokens.add(new Token(new SchemeChar(ch), line, startCol));
                    } else {
                        throw new EvalError("unexpected token: #" + next + " at " + line + ":" + startCol);
                    }
                } else {
                    throw new EvalError("unexpected end after # at " + line + ":" + startCol);
                }
            } else {
                // symbol or number
                int startCol = col;
                StringBuilder sb = new StringBuilder();
                while (i < len && !Character.isWhitespace(input.charAt(i))
                        && input.charAt(i) != '(' && input.charAt(i) != ')'
                        && input.charAt(i) != '"' && input.charAt(i) != ';'
                        && input.charAt(i) != '\'') {
                    sb.append(input.charAt(i));
                    i++; col++;
                }
                String tok = sb.toString();
                try {
                    tokens.add(new Token(Long.parseLong(tok), line, startCol));
                } catch (NumberFormatException e) {
                    tokens.add(new Token(tok, line, startCol)); // symbol
                }
            }
        }
        return tokens;
    }

    // --- Parser ---

    private Object parse(List<Token> tokens, int[] pos) throws EvalError {
        if (pos[0] >= tokens.size()) {
            throw new EvalError("unexpected end of input");
        }
        Token tok = tokens.get(pos[0]);
        Object token = tok.value();
        int tLine = tok.line(), tCol = tok.col();
        if (token.equals("'")) {
            pos[0]++;
            Object datum = parse(tokens, pos);
            List<Object> quoted = new ArrayList<>();
            quoted.add("quote");
            quoted.add(datum instanceof Located loc ? loc.expr : datum);
            return new Located(quoted, tLine, tCol);
        }
        if (token.equals("(")) {
            pos[0]++;
            List<Object> list = new ArrayList<>();
            while (pos[0] < tokens.size() && !tokens.get(pos[0]).value().equals(")")) {
                Object parsed = parse(tokens, pos);
                list.add(parsed);
            }
            if (pos[0] >= tokens.size()) {
                throw new EvalError("missing closing paren at " + tLine + ":" + tCol);
            }
            pos[0]++; // skip )
            return new Located(list, tLine, tCol);
        } else if (token.equals(")")) {
            throw new EvalError("unexpected ) at " + tLine + ":" + tCol);
        } else {
            pos[0]++;
            return new Located(token, tLine, tCol);
        }
    }

    // --- Evaluator ---

    @SuppressWarnings("unchecked")
    private Object eval(Object expr, Env env) throws EvalError {
        // Unwrap Located to get position info
        int eLine = 0, eCol = 0;
        if (expr instanceof Located loc) {
            eLine = loc.line;
            eCol = loc.col;
            expr = loc.expr;
        }
        final String posStr = eLine > 0 ? " at " + eLine + ":" + eCol : "";

        if (expr instanceof Long || expr instanceof Boolean || expr instanceof SchemeString || expr instanceof SchemeChar) {
            return expr;
        }
        if (expr instanceof String sym) {
            return env.lookup(sym, new Pos(eLine, eCol));
        }
        if (expr instanceof List<?> list) {
            if (list.isEmpty()) {
                throw new EvalError("empty application" + posStr);
            }
            Object head = list.get(0);
            // Unwrap head if Located
            Object rawHead = head;
            if (rawHead instanceof Located loc) rawHead = loc.expr;

            // Special forms
            if (rawHead instanceof String op) {
                switch (op) {
                    case "quote" -> {
                        if (list.size() != 2) throw new EvalError("quote: expected 1 argument" + posStr);
                        Object datum = list.get(1);
                        if (datum instanceof Located loc) datum = loc.expr;
                        return quoteValue(datum);
                    }
                    case "if" -> {
                        if (list.size() < 3 || list.size() > 4)
                            throw new EvalError("if: expected 2-3 arguments" + posStr);
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
                        if (list.size() < 3) throw new EvalError("define: too few arguments" + posStr);
                        Object target = list.get(1);
                        if (target instanceof Located loc) target = loc.expr;
                        if (target instanceof String name) {
                            Object val = eval(list.get(2), env);
                            env.define(name, val);
                            return val;
                        } else if (target instanceof List<?> sig) {
                            if (sig.isEmpty()) throw new EvalError("define: invalid function signature" + posStr);
                            Object first = sig.get(0);
                            if (first instanceof Located loc) first = loc.expr;
                            if (!(first instanceof String fname))
                                throw new EvalError("define: invalid function signature" + posStr);
                            List<String> params = new ArrayList<>();
                            String restParam = null;
                            for (int i = 1; i < sig.size(); i++) {
                                Object p = sig.get(i);
                                if (p instanceof Located loc) p = loc.expr;
                                if (p instanceof String pname && pname.equals(".")) {
                                    if (i + 1 >= sig.size())
                                        throw new EvalError("define: missing rest parameter after dot" + posStr);
                                    Object rp = sig.get(i + 1);
                                    if (rp instanceof Located loc) rp = loc.expr;
                                    if (!(rp instanceof String rpname))
                                        throw new EvalError("define: rest parameter must be symbol" + posStr);
                                    restParam = rpname;
                                    break;
                                }
                                if (!(p instanceof String pname))
                                    throw new EvalError("define: parameter must be symbol" + posStr);
                                params.add(pname);
                            }
                            List<Object> body = new ArrayList<>(list.subList(2, list.size()));
                            Lambda lambda = new Lambda(params, restParam, body, env);
                            env.define(fname, lambda);
                            return lambda;
                        } else {
                            throw new EvalError("define: invalid syntax" + posStr);
                        }
                    }
                    case "set!" -> {
                        if (list.size() != 3) throw new EvalError("set!: expected 2 arguments" + posStr);
                        Object target = list.get(1);
                        if (target instanceof Located loc) target = loc.expr;
                        if (!(target instanceof String name))
                            throw new EvalError("set!: target must be a symbol" + posStr);
                        Object val = eval(list.get(2), env);
                        env.set(name, val, new Pos(eLine, eCol));
                        return val;
                    }
                    case "lambda" -> {
                        if (list.size() < 3) throw new EvalError("lambda: too few arguments" + posStr);
                        Object paramSpec = list.get(1);
                        if (paramSpec instanceof Located loc) paramSpec = loc.expr;
                        if (!(paramSpec instanceof List<?> paramList))
                            throw new EvalError("lambda: params must be a list" + posStr);
                        List<String> params = new ArrayList<>();
                        String restParam = null;
                        for (int pi = 0; pi < paramList.size(); pi++) {
                            Object p = paramList.get(pi);
                            if (p instanceof Located loc) p = loc.expr;
                            if (p instanceof String pname && pname.equals(".")) {
                                // Next element is rest param
                                if (pi + 1 >= paramList.size())
                                    throw new EvalError("lambda: missing rest parameter after dot" + posStr);
                                Object rp = paramList.get(pi + 1);
                                if (rp instanceof Located loc) rp = loc.expr;
                                if (!(rp instanceof String rpname))
                                    throw new EvalError("lambda: rest parameter must be symbol" + posStr);
                                restParam = rpname;
                                break;
                            }
                            if (!(p instanceof String pname))
                                throw new EvalError("lambda: parameter must be symbol" + posStr);
                            params.add(pname);
                        }
                        List<Object> body = new ArrayList<>(list.subList(2, list.size()));
                        return new Lambda(params, restParam, body, env);
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
                            Object clauseObj = list.get(i);
                            if (clauseObj instanceof Located loc) clauseObj = loc.expr;
                            if (!(clauseObj instanceof List<?> clause) || clause.isEmpty())
                                throw new EvalError("cond: invalid clause" + posStr);
                            Object test = clause.get(0);
                            if (test instanceof Located loc) test = loc.expr;
                            if (test instanceof String s && s.equals("else")) {
                                Object result = Boolean.FALSE;
                                for (int j = 1; j < clause.size(); j++) {
                                    result = eval(clause.get(j), env);
                                }
                                return result;
                            }
                            Object testVal = eval(clause.get(0), env);
                            if (isTruthy(testVal)) {
                                if (clause.size() == 1) return testVal;
                                Object result = testVal;
                                for (int j = 1; j < clause.size(); j++) {
                                    result = eval(clause.get(j), env);
                                }
                                return result;
                            }
                        }
                        return Boolean.FALSE;
                    }
                    case "let" -> {
                        int bindingsIdx = 1;
                        String namedLetName = null;
                        Object firstArg = list.get(1);
                        if (firstArg instanceof Located loc) firstArg = loc.expr;
                        if (firstArg instanceof String name) {
                            namedLetName = name;
                            bindingsIdx = 2;
                        }
                        Object bindingsObj = list.get(bindingsIdx);
                        if (bindingsObj instanceof Located loc) bindingsObj = loc.expr;
                        if (!(bindingsObj instanceof List<?> bindingList))
                            throw new EvalError("let: bindings must be a list" + posStr);
                        List<String> varNames = new ArrayList<>();
                        List<Object> initExprs = new ArrayList<>();
                        for (Object b : bindingList) {
                            if (b instanceof Located loc) b = loc.expr;
                            if (!(b instanceof List<?> binding) || binding.size() != 2)
                                throw new EvalError("let: invalid binding" + posStr);
                            Object bname = binding.get(0);
                            if (bname instanceof Located loc) bname = loc.expr;
                            if (!(bname instanceof String vname))
                                throw new EvalError("let: binding name must be symbol" + posStr);
                            varNames.add(vname);
                            initExprs.add(binding.get(1));
                        }
                        Env letEnv = new Env(env);
                        if (namedLetName != null) {
                            List<Object> bodyExprs = new ArrayList<>(list.subList(bindingsIdx + 1, list.size()));
                            Lambda loopLambda = new Lambda(varNames, null, bodyExprs, letEnv);
                            letEnv.define(namedLetName, loopLambda);
                            for (int i = 0; i < varNames.size(); i++) {
                                letEnv.define(varNames.get(i), eval(initExprs.get(i), env));
                            }
                            return evalBody(bodyExprs, letEnv);
                        } else {
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
            try {
                return applyProc(proc, args);
            } catch (EvalError e) {
                // Add position to errors from builtins if not already present
                if (eLine > 0 && !e.getMessage().matches(".*\\d+:\\d+.*")) {
                    throw new EvalError(e.getMessage() + posStr);
                }
                throw e;
            }
        }
        throw new EvalError("cannot evaluate: " + expr + posStr);
    }

    private Object applyProc(Object proc, List<Object> args) throws EvalError {
        if (proc instanceof Lambda lambda) {
            if (lambda.restParam != null) {
                if (args.size() < lambda.params.size()) {
                    throw new EvalError("wrong number of arguments: expected at least " +
                            lambda.params.size() + ", got " + args.size());
                }
            } else {
                if (args.size() != lambda.params.size()) {
                    throw new EvalError("wrong number of arguments: expected " +
                            lambda.params.size() + ", got " + args.size());
                }
            }
            Env callEnv = new Env(lambda.closureEnv);
            for (int i = 0; i < lambda.params.size(); i++) {
                callEnv.define(lambda.params.get(i), args.get(i));
            }
            if (lambda.restParam != null) {
                // Build list from remaining args
                Object rest = EMPTY_LIST;
                for (int i = args.size() - 1; i >= lambda.params.size(); i--) {
                    rest = new Pair(args.get(i), rest);
                }
                callEnv.define(lambda.restParam, rest);
            }
            return evalBody(lambda.body, callEnv);
        }
        if (proc instanceof BuiltinProc bp) {
            return bp.fn.apply(args);
        }
        throw new EvalError("not a procedure: " + schemeToString(proc));
    }

    // Helper to unwrap Located in quoteValue
    private Object unwrapLocated(Object obj) {
        if (obj instanceof Located loc) return loc.expr;
        return obj;
    }

    private Object evalBody(List<Object> body, Env env) throws EvalError {
        Object result = Boolean.FALSE;
        for (Object expr : body) {
            result = eval(expr, env);
        }
        return result;
    }

    private Object quoteValue(Object datum) {
        if (datum instanceof Located loc) datum = loc.expr;
        if (datum instanceof List<?> list) {
            Object result = EMPTY_LIST;
            for (int i = list.size() - 1; i >= 0; i--) {
                result = new Pair(quoteValue(list.get(i)), result);
            }
            return result;
        }
        return datum;
    }

    private boolean schemeEqual(Object a, Object b) {
        if (a == b) return true;
        if (a instanceof Long && b instanceof Long) return a.equals(b);
        if (a instanceof Boolean && b instanceof Boolean) return a.equals(b);
        if (a instanceof String && b instanceof String) return a.equals(b);
        if (a instanceof SchemeString sa && b instanceof SchemeString sb) return sa.value().equals(sb.value());
        if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) return ca.value() == cb.value();
        if (a == EMPTY_LIST && b == EMPTY_LIST) return true;
        if (a instanceof Pair pa && b instanceof Pair pb) return schemeEqual(pa.car, pb.car) && schemeEqual(pa.cdr, pb.cdr);
        return false;
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
        if (val instanceof SchemeChar c) {
            if (c.value() == ' ') return "#\\space";
            if (c.value() == '\n') return "#\\newline";
            if (c.value() == '\t') return "#\\tab";
            return "#\\" + c.value();
        }
        if (val instanceof Lambda) return "#<procedure>";
        if (val instanceof BuiltinProc) return "#<procedure>";
        if (val instanceof String s) return s;
        return val.toString();
    }

    private String displayString(Object val) {
        if (val instanceof SchemeString s) return s.value(); // no quotes
        if (val instanceof SchemeChar c) return String.valueOf(c.value());
        if (val == EMPTY_LIST) return "()";
        if (val instanceof Pair p) {
            StringBuilder sb = new StringBuilder("(");
            sb.append(displayString(p.car));
            Object rest = p.cdr;
            while (rest instanceof Pair rp) {
                sb.append(" ");
                sb.append(displayString(rp.car));
                rest = rp.cdr;
            }
            if (rest != EMPTY_LIST) {
                sb.append(" . ");
                sb.append(displayString(rest));
            }
            sb.append(")");
            return sb.toString();
        }
        return schemeToString(val);
    }

    // Internal type to distinguish Scheme strings from symbols (Java Strings)
    static class SchemeString {
        private char[] chars;
        SchemeString(String value) { this.chars = value.toCharArray(); }
        String value() { return new String(chars); }
        char charAt(int i) { return chars[i]; }
        int length() { return chars.length; }
        void setChar(int i, char c) { chars[i] = c; }
        SchemeString copy() { return new SchemeString(value()); }
        @Override public boolean equals(Object o) {
            return o instanceof SchemeString s && value().equals(s.value());
        }
        @Override public int hashCode() { return value().hashCode(); }
    }

    // Internal type for Scheme characters
    record SchemeChar(char value) {}
}
