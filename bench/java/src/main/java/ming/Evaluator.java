package ming;

import static ming.Numbers.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

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

    static class Env {
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

    // --- CaseLambda (multiple-arity closure) ---

    private record CaseLambda(List<Lambda> clauses) {}

    // --- Builtin procedure ---

    @FunctionalInterface
    private interface Builtin {
        Object apply(List<Object> args) throws EvalError;
    }

    private record BuiltinProc(String name, Builtin fn) {}

    private static final Set<String> SPECIAL_FORMS = Set.of(
        "if", "begin", "let", "set!", "define", "quote", "lambda", "case-lambda",
        "and", "or", "cond", "define-syntax", "syntax-rules", "define-record-type"
    );

    private int gensymCounter = 0;

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
        registerRationals(env);
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
            return isNumber(args.get(0)) ? Boolean.TRUE : Boolean.FALSE;
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
        env.define("procedure?", new BuiltinProc("procedure?", args -> {
            if (args.size() != 1) throw new EvalError("procedure?: expected 1 arg");
            Object val = args.get(0);
            return (val instanceof Lambda || val instanceof CaseLambda || val instanceof BuiltinProc)
                    ? Boolean.TRUE : Boolean.FALSE;
        }));
    }

    private void registerArithmetic(Env env) {
        env.define("+", new BuiltinProc("+", args -> {
            Object result = 0L;
            for (Object a : args) { requireNumber(a, "+"); result = addNum(result, a); }
            return result;
        }));
        env.define("-", new BuiltinProc("-", args -> {
            if (args.isEmpty()) throw new EvalError("-: need at least 1 argument");
            requireNumber(args.get(0), "-");
            if (args.size() == 1) return negateNum(args.get(0));
            Object result = args.get(0);
            for (int i = 1; i < args.size(); i++) { requireNumber(args.get(i), "-"); result = subNum(result, args.get(i)); }
            return result;
        }));
        env.define("*", new BuiltinProc("*", args -> {
            Object result = 1L;
            for (Object a : args) { requireNumber(a, "*"); result = mulNum(result, a); }
            return result;
        }));
        env.define("/", new BuiltinProc("/", args -> {
            if (args.size() < 2) throw new EvalError("/: need at least 2 arguments");
            requireNumber(args.get(0), "/");
            Object result = args.get(0);
            for (int i = 1; i < args.size(); i++) { requireNumber(args.get(i), "/"); result = divNum(result, args.get(i)); }
            return result;
        }));
        for (String op : new String[]{"<", ">", "=", "<=", ">="}) {
            env.define(op, new BuiltinProc(op, args -> {
                if (args.size() < 2) throw new EvalError(op + ": need at least 2 arguments");
                double prev = toDouble(args.get(0));
                for (int i = 1; i < args.size(); i++) {
                    double curr = toDouble(args.get(i));
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
            Object n = parseNumber(s.value());
            return n != null ? n : Boolean.FALSE;
        }));
        env.define("number->string", new BuiltinProc("number->string", args -> {
            if (args.size() != 1) throw new EvalError("number->string: expected 1 arg");
            return new SchemeString(schemeToString(args.get(0)));
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

    private void registerRationals(Env env) {
        env.define("exact?", new BuiltinProc("exact?", args -> {
            if (args.size() != 1) throw new EvalError("exact?: expected 1 arg");
            Object a = args.get(0);
            return (a instanceof Long || a instanceof Rational) ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("inexact?", new BuiltinProc("inexact?", args -> {
            if (args.size() != 1) throw new EvalError("inexact?: expected 1 arg");
            return args.get(0) instanceof Double ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("exact->inexact", new BuiltinProc("exact->inexact", args -> {
            if (args.size() != 1) throw new EvalError("exact->inexact: expected 1 arg");
            return toDouble(args.get(0));
        }));
        env.define("inexact->exact", new BuiltinProc("inexact->exact", args -> {
            if (args.size() != 1) throw new EvalError("inexact->exact: expected 1 arg");
            Object a = args.get(0);
            if (a instanceof Long || a instanceof Rational) return a;
            if (a instanceof Double d) {
                // Convert double to rational via continued fraction approximation
                // For simple cases like 0.5 -> 1/2
                long denom = 1;
                double val = d;
                while (val != Math.floor(val) && denom < 1000000000L) {
                    val *= 10;
                    denom *= 10;
                }
                long num = Math.round(d * denom);
                Rational r = new Rational(num, denom);
                return r.isInteger() ? r.toLong() : r;
            }
            throw new EvalError("inexact->exact: expected number");
        }));
        env.define("integer?", new BuiltinProc("integer?", args -> {
            if (args.size() != 1) throw new EvalError("integer?: expected 1 arg");
            Object a = args.get(0);
            if (a instanceof Long) return Boolean.TRUE;
            if (a instanceof Rational r) return r.isInteger() ? Boolean.TRUE : Boolean.FALSE;
            if (a instanceof Double d) return (d == Math.floor(d) && !Double.isInfinite(d)) ? Boolean.TRUE : Boolean.FALSE;
            return Boolean.FALSE;
        }));
        env.define("rational?", new BuiltinProc("rational?", args -> {
            if (args.size() != 1) throw new EvalError("rational?: expected 1 arg");
            Object a = args.get(0);
            return (a instanceof Long || a instanceof Rational) ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("numerator", new BuiltinProc("numerator", args -> {
            if (args.size() != 1) throw new EvalError("numerator: expected 1 arg");
            Object a = args.get(0);
            if (a instanceof Long l) return l;
            if (a instanceof Rational r) return r.num;
            throw new EvalError("numerator: expected rational");
        }));
        env.define("denominator", new BuiltinProc("denominator", args -> {
            if (args.size() != 1) throw new EvalError("denominator: expected 1 arg");
            Object a = args.get(0);
            if (a instanceof Long) return 1L;
            if (a instanceof Rational r) return r.den;
            throw new EvalError("denominator: expected rational");
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
                Object parsed = parseNumber(tok);
                if (parsed != null) {
                    tokens.add(new Token(parsed, line, startCol));
                } else {
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

        if (expr instanceof Long || expr instanceof Double || expr instanceof Rational || expr instanceof Boolean || expr instanceof SchemeString || expr instanceof SchemeChar) {
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
                    case "case-lambda" -> {
                        List<Lambda> clauses = new ArrayList<>();
                        for (int ci = 1; ci < list.size(); ci++) {
                            Object clauseObj = list.get(ci);
                            if (clauseObj instanceof Located loc) clauseObj = loc.expr;
                            if (!(clauseObj instanceof List<?> clause) || clause.size() < 2)
                                throw new EvalError("case-lambda: bad clause" + posStr);
                            Object paramSpec = clause.get(0);
                            if (paramSpec instanceof Located loc) paramSpec = loc.expr;
                            if (!(paramSpec instanceof List<?> paramList))
                                throw new EvalError("case-lambda: params must be a list" + posStr);
                            List<String> params = new ArrayList<>();
                            String restParam = null;
                            for (int pi = 0; pi < paramList.size(); pi++) {
                                Object p = paramList.get(pi);
                                if (p instanceof Located loc) p = loc.expr;
                                if (p instanceof String pname && pname.equals(".")) {
                                    if (pi + 1 >= paramList.size())
                                        throw new EvalError("case-lambda: missing rest parameter after dot" + posStr);
                                    Object rp = paramList.get(pi + 1);
                                    if (rp instanceof Located loc) rp = loc.expr;
                                    if (!(rp instanceof String rpname))
                                        throw new EvalError("case-lambda: rest parameter must be symbol" + posStr);
                                    restParam = rpname;
                                    break;
                                }
                                if (!(p instanceof String pname))
                                    throw new EvalError("case-lambda: parameter must be symbol" + posStr);
                                params.add(pname);
                            }
                            List<Object> body = new ArrayList<>(clause.subList(1, clause.size()));
                            clauses.add(new Lambda(params, restParam, body, env));
                        }
                        return new CaseLambda(clauses);
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
                    case "define-syntax" -> {
                        return evalDefineSyntax(list, env, posStr);
                    }
                    case "define-record-type" -> {
                        return evalDefineRecordType(list, env, posStr);
                    }
                }
                // Check for macro invocation
                Object macroVal = null;
                try { macroVal = env.lookup(op, new Pos(eLine, eCol)); } catch (EvalError ignored) {}
                if (macroVal instanceof SyntaxRulesMacro macro) {
                    return evalMacro(macro, list, env);
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
        if (proc instanceof CaseLambda cl) {
            for (Lambda clause : cl.clauses) {
                if (clause.restParam != null) {
                    if (args.size() >= clause.params.size()) {
                        return applyProc(clause, args);
                    }
                } else {
                    if (args.size() == clause.params.size()) {
                        return applyProc(clause, args);
                    }
                }
            }
            throw new EvalError("case-lambda: no matching clause for " + args.size() + " arguments");
        }
        if (proc instanceof BuiltinProc bp) {
            return bp.fn.apply(args);
        }
        throw new EvalError("not a procedure: " + schemeToString(proc));
    }

    private Object evalDefineRecordType(List<?> list, Env env, String posStr) throws EvalError {
        if (list.size() < 4) throw new EvalError("define-record-type: too few arguments" + posStr);
        Object typeNameObj = list.get(1);
        if (typeNameObj instanceof Located loc) typeNameObj = loc.expr;
        String typeName = (String) typeNameObj;

        Object ctorObj = list.get(2);
        if (ctorObj instanceof Located loc) ctorObj = loc.expr;
        List<?> ctorSpec = (List<?>) ctorObj;
        Object ctorNameObj = ctorSpec.get(0);
        if (ctorNameObj instanceof Located loc) ctorNameObj = loc.expr;
        String ctorName = (String) ctorNameObj;
        List<String> ctorFields = new ArrayList<>();
        for (int ci = 1; ci < ctorSpec.size(); ci++) {
            Object cf = ctorSpec.get(ci);
            if (cf instanceof Located loc) cf = loc.expr;
            ctorFields.add((String) cf);
        }

        Object predObj = list.get(3);
        if (predObj instanceof Located loc) predObj = loc.expr;
        String predName = (String) predObj;

        Map<String, Integer> fieldIndex = new HashMap<>();
        for (int fi = 0; fi < ctorFields.size(); fi++) {
            fieldIndex.put(ctorFields.get(fi), fi);
        }
        Map<String, Integer> accessorMap = new HashMap<>();
        for (int fi = 4; fi < list.size(); fi++) {
            Object fspec = list.get(fi);
            if (fspec instanceof Located loc) fspec = loc.expr;
            List<?> fieldSpec = (List<?>) fspec;
            Object fnObj = fieldSpec.get(0);
            if (fnObj instanceof Located loc) fnObj = loc.expr;
            String fieldName = (String) fnObj;
            Object accObj = fieldSpec.get(1);
            if (accObj instanceof Located loc) accObj = loc.expr;
            String accName = (String) accObj;
            accessorMap.put(accName, fieldIndex.get(fieldName));
        }

        RecordType rt = new RecordType(typeName, ctorFields);

        env.define(ctorName, new BuiltinProc(ctorName, args -> {
            if (args.size() != ctorFields.size())
                throw new EvalError(ctorName + ": expected " + ctorFields.size() + " arguments, got " + args.size());
            return new SchemeRecord(rt, args.toArray());
        }));
        env.define(predName, new BuiltinProc(predName, args -> {
            if (args.size() != 1) throw new EvalError(predName + ": expected 1 argument");
            return args.get(0) instanceof SchemeRecord sr && sr.type == rt;
        }));
        for (var entry : accessorMap.entrySet()) {
            String accName = entry.getKey();
            int idx = entry.getValue();
            env.define(accName, new BuiltinProc(accName, args -> {
                if (args.size() != 1) throw new EvalError(accName + ": expected 1 argument");
                if (!(args.get(0) instanceof SchemeRecord sr) || sr.type != rt)
                    throw new EvalError(accName + ": not a " + typeName);
                return sr.fields[idx];
            }));
        }
        return Boolean.FALSE;
    }

    private Object evalDefineSyntax(List<?> list, Env env, String posStr) throws EvalError {
        if (list.size() != 3) throw new EvalError("define-syntax: expected 2 arguments" + posStr);
        Object nameObj = list.get(1);
        if (nameObj instanceof Located loc) nameObj = loc.expr;
        if (!(nameObj instanceof String macroName))
            throw new EvalError("define-syntax: name must be symbol" + posStr);
        Object transformerExpr = list.get(2);
        if (transformerExpr instanceof Located loc) transformerExpr = loc.expr;
        if (!(transformerExpr instanceof List<?> transformer))
            throw new EvalError("define-syntax: expected syntax-rules" + posStr);
        Object srHead = transformer.get(0);
        if (srHead instanceof Located loc) srHead = loc.expr;
        if (!(srHead instanceof String srStr) || !srStr.equals("syntax-rules"))
            throw new EvalError("define-syntax: expected syntax-rules" + posStr);
        Object litsObj = transformer.get(1);
        if (litsObj instanceof Located loc) litsObj = loc.expr;
        List<String> macroLiterals = new ArrayList<>();
        if (litsObj instanceof List<?> litsList) {
            for (Object l : litsList) {
                if (l instanceof Located loc) l = loc.expr;
                if (l instanceof String s) macroLiterals.add(s);
            }
        }
        List<Object[]> macroRules = new ArrayList<>();
        for (int ri = 2; ri < transformer.size(); ri++) {
            Object ruleObj = transformer.get(ri);
            if (ruleObj instanceof Located loc) ruleObj = loc.expr;
            if (!(ruleObj instanceof List<?> rule) || rule.size() != 2)
                throw new EvalError("define-syntax: invalid rule" + posStr);
            macroRules.add(new Object[]{rule.get(0), rule.get(1)});
        }
        env.define(macroName, new SyntaxRulesMacro(macroName, macroLiterals, macroRules, env));
        return Boolean.FALSE;
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

    // --- Macro expansion ---

    @SuppressWarnings("unchecked")
    private Object evalMacro(SyntaxRulesMacro macro, List<?> form, Env useEnv) throws EvalError {
        for (Object[] rule : macro.rules) {
            Map<String, Object> bindings = matchPattern(rule[0], form, macro.literals);
            if (bindings != null) {
                Map<String, String> renameMap = new HashMap<>();
                Object expanded = expandTemplate(rule[1], bindings, renameMap);
                Env evalEnv = useEnv;
                if (!renameMap.isEmpty()) {
                    Map<String, Object> hygieneBindings = new HashMap<>();
                    for (var entry : renameMap.entrySet()) {
                        try {
                            Object val = macro.defEnv.lookup(entry.getKey(), new Pos(0, 0));
                            hygieneBindings.put(entry.getValue(), val);
                        } catch (EvalError ignored) {}
                    }
                    if (!hygieneBindings.isEmpty()) {
                        evalEnv = new Env(useEnv);
                        for (var e : hygieneBindings.entrySet()) {
                            evalEnv.define(e.getKey(), e.getValue());
                        }
                    }
                }
                return eval(expanded, evalEnv);
            }
        }
        throw new EvalError("no matching pattern for macro " + macro.name);
    }

    private Map<String, Object> matchPattern(Object pattern, List<?> input, List<String> literals) {
        if (pattern instanceof Located loc) pattern = loc.expr;
        if (!(pattern instanceof List<?> patList)) return null;
        Map<String, Object> bindings = new HashMap<>();
        int pi = 1, ii = 1; // skip macro name
        while (pi < patList.size()) {
            Object patElem = patList.get(pi);
            if (patElem instanceof Located loc) patElem = loc.expr;
            boolean hasEllipsis = false;
            if (pi + 1 < patList.size()) {
                Object next = patList.get(pi + 1);
                if (next instanceof Located loc) next = loc.expr;
                if ("...".equals(next)) hasEllipsis = true;
            }
            if (hasEllipsis) {
                if (!(patElem instanceof String varName)) return null;
                List<Object> collected = new ArrayList<>();
                while (ii < input.size()) {
                    collected.add(input.get(ii));
                    ii++;
                }
                bindings.put(varName, collected);
                pi += 2;
            } else if (patElem instanceof String s && literals.contains(s)) {
                if (ii >= input.size()) return null;
                Object inElem = input.get(ii);
                if (inElem instanceof Located loc) inElem = loc.expr;
                if (!s.equals(inElem)) return null;
                pi++; ii++;
            } else if (patElem instanceof String varName) {
                if (ii >= input.size()) return null;
                bindings.put(varName, input.get(ii));
                pi++; ii++;
            } else {
                return null;
            }
        }
        return ii == input.size() ? bindings : null;
    }

    @SuppressWarnings("unchecked")
    private Object expandTemplate(Object template, Map<String, Object> bindings,
                                   Map<String, String> renameMap) {
        if (template instanceof Located loc) template = loc.expr;
        if (template instanceof String id) {
            if (bindings.containsKey(id)) return bindings.get(id);
            if ("...".equals(id) || SPECIAL_FORMS.contains(id)) return id;
            if (!renameMap.containsKey(id)) {
                renameMap.put(id, "__" + id + "_" + (gensymCounter++));
            }
            return renameMap.get(id);
        }
        if (template instanceof List<?> tmplList) {
            List<Object> result = new ArrayList<>();
            for (int i = 0; i < tmplList.size(); i++) {
                Object elem = tmplList.get(i);
                Object rawElem = elem;
                if (rawElem instanceof Located loc) rawElem = loc.expr;
                boolean nextIsEllipsis = false;
                if (i + 1 < tmplList.size()) {
                    Object next = tmplList.get(i + 1);
                    if (next instanceof Located loc) next = loc.expr;
                    if ("...".equals(next)) nextIsEllipsis = true;
                }
                if (nextIsEllipsis) {
                    String ellipsisVar = findEllipsisVar(elem, bindings);
                    if (ellipsisVar != null) {
                        List<Object> elements = (List<Object>) bindings.get(ellipsisVar);
                        for (Object e : elements) {
                            Map<String, Object> subBindings = new HashMap<>(bindings);
                            subBindings.put(ellipsisVar, e);
                            result.add(expandTemplate(elem, subBindings, renameMap));
                        }
                    }
                    i++; // skip ...
                } else if (rawElem instanceof String s && "...".equals(s)) {
                    // skip, handled above
                } else {
                    result.add(expandTemplate(elem, bindings, renameMap));
                }
            }
            return result;
        }
        return template;
    }

    private String findEllipsisVar(Object template, Map<String, Object> bindings) {
        if (template instanceof Located loc) template = loc.expr;
        if (template instanceof String s && bindings.get(s) instanceof List) return s;
        if (template instanceof List<?> list) {
            for (Object elem : list) {
                String found = findEllipsisVar(elem, bindings);
                if (found != null) return found;
            }
        }
        return null;
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
        if (isNumber(a) && isNumber(b)) {
            try { return toDouble(a) == toDouble(b); } catch (EvalError e) { return false; }
        }
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

    private String schemeToString(Object val) {
        if (val instanceof Long l) return l.toString();
        if (val instanceof Rational r) return r.toString();
        if (val instanceof Double d) {
            if (d == Math.floor(d) && !Double.isInfinite(d)) return String.format("%.1f", d);
            return Double.toString(d);
        }
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
        if (val instanceof SchemeRecord) return "#<record>";
        if (val instanceof Lambda) return "#<procedure>";
        if (val instanceof CaseLambda) return "#<procedure>";
        if (val instanceof BuiltinProc) return "#<procedure>";
        if (val instanceof SyntaxRulesMacro) return "#<macro>";
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

}
