package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class Env {
    private final Map<String, Object> bindings;
    private final Env parent;

    Env(Env parent) {
        this.bindings = new HashMap<>();
        this.parent = parent;
    }

    void define(String name, Object value) {
        bindings.put(name, value);
    }

    void set(String name, Object value) throws EvalError {
        if (bindings.containsKey(name)) {
            bindings.put(name, value);
            return;
        }
        if (parent != null) {
            parent.set(name, value);
            return;
        }
        throw new EvalError("set!: unbound variable: " + name);
    }

    Object lookup(String name) throws EvalError {
        if (bindings.containsKey(name)) return bindings.get(name);
        if (parent != null) return parent.lookup(name);
        throw new EvalError("unbound variable: " + name);
    }

    static Env global() {
        Env env = new Env(null);
        // Arithmetic
        env.define("+", Builtin.named("+", args -> {
            long sum = 0;
            for (Object a : args) sum += requireLong("+", a);
            return sum;
        }));
        env.define("-", Builtin.named("-", args -> {
            if (args.isEmpty()) throw new EvalError("-: need at least 1 argument");
            if (args.size() == 1) return -requireLong("-", args.get(0));
            long r = requireLong("-", args.get(0));
            for (int i = 1; i < args.size(); i++) r -= requireLong("-", args.get(i));
            return r;
        }));
        env.define("*", Builtin.named("*", args -> {
            long p = 1;
            for (Object a : args) p *= requireLong("*", a);
            return p;
        }));
        env.define("/", Builtin.named("/", args -> {
            if (args.isEmpty()) throw new EvalError("/: need at least 1 argument");
            long r = requireLong("/", args.get(0));
            for (int i = 1; i < args.size(); i++) {
                long d = requireLong("/", args.get(i));
                if (d == 0) throw new EvalError("division by zero");
                r /= d;
            }
            return r;
        }));

        // Comparisons
        env.define("<", Builtin.named("<", args -> {
            requireArgCount("<", args, 2);
            return requireLong("<", args.get(0)) < requireLong("<", args.get(1));
        }));
        env.define(">", Builtin.named(">", args -> {
            requireArgCount(">", args, 2);
            return requireLong(">", args.get(0)) > requireLong(">", args.get(1));
        }));
        env.define("=", Builtin.named("=", args -> {
            requireArgCount("=", args, 2);
            return requireLong("=", args.get(0)) == requireLong("=", args.get(1));
        }));
        env.define("<=", Builtin.named("<=", args -> {
            requireArgCount("<=", args, 2);
            return requireLong("<=", args.get(0)) <= requireLong("<=", args.get(1));
        }));
        env.define(">=", Builtin.named(">=", args -> {
            requireArgCount(">=", args, 2);
            return requireLong(">=", args.get(0)) >= requireLong(">=", args.get(1));
        }));
        env.define("not", Builtin.named("not", args -> {
            requireArgCount("not", args, 1);
            return Evaluator.isFalse(args.get(0));
        }));

        // List operations
        env.define("cons", Builtin.named("cons", args -> {
            requireArgCount("cons", args, 2);
            return new Pair(args.get(0), args.get(1));
        }));
        env.define("car", Builtin.named("car", args -> {
            requireArgCount("car", args, 1);
            if (!(args.get(0) instanceof Pair p))
                throw new EvalError("car: expected pair, got: " + SchemeValue.toStr(args.get(0)));
            return p.car;
        }));
        env.define("cdr", Builtin.named("cdr", args -> {
            requireArgCount("cdr", args, 1);
            if (!(args.get(0) instanceof Pair p))
                throw new EvalError("cdr: expected pair, got: " + SchemeValue.toStr(args.get(0)));
            return p.cdr;
        }));
        env.define("null?", Builtin.named("null?", args -> {
            requireArgCount("null?", args, 1);
            return args.get(0) == SchemeValue.NIL;
        }));
        env.define("list", Builtin.named("list", args -> {
            Object result = SchemeValue.NIL;
            for (int i = args.size() - 1; i >= 0; i--) {
                result = new Pair(args.get(i), result);
            }
            return result;
        }));
        env.define("length", Builtin.named("length", args -> {
            requireArgCount("length", args, 1);
            long count = 0;
            Object cur = args.get(0);
            while (cur instanceof Pair p) {
                count++;
                cur = p.cdr;
            }
            return count;
        }));
        env.define("append", Builtin.named("append", args -> {
            if (args.isEmpty()) return SchemeValue.NIL;
            Object result = args.get(args.size() - 1);
            for (int i = args.size() - 2; i >= 0; i--) {
                Object lst = args.get(i);
                List<Object> elems = new ArrayList<>();
                Object cur = lst;
                while (cur instanceof Pair p) {
                    elems.add(p.car);
                    cur = p.cdr;
                }
                for (int j = elems.size() - 1; j >= 0; j--) {
                    result = new Pair(elems.get(j), result);
                }
            }
            return result;
        }));

        // Type predicates
        env.define("boolean?", Builtin.named("boolean?", args -> {
            requireArgCount("boolean?", args, 1);
            return args.get(0) instanceof Boolean;
        }));
        env.define("number?", Builtin.named("number?", args -> {
            requireArgCount("number?", args, 1);
            return args.get(0) instanceof Long;
        }));
        env.define("string?", Builtin.named("string?", args -> {
            requireArgCount("string?", args, 1);
            return (args.get(0) instanceof String s && s.startsWith("\"")) || args.get(0) instanceof MutableString;
        }));
        env.define("pair?", Builtin.named("pair?", args -> {
            requireArgCount("pair?", args, 1);
            return args.get(0) instanceof Pair;
        }));
        env.define("symbol?", Builtin.named("symbol?", args -> {
            requireArgCount("symbol?", args, 1);
            return args.get(0) instanceof String s && !s.startsWith("\"");
        }));

        // I/O
        env.define("display", Builtin.named("display", args -> {
            requireArgCount("display", args, 1);
            Evaluator.emitOutput(displayStr(args.get(0)));
            return null; // void
        }));
        env.define("write", Builtin.named("write", args -> {
            requireArgCount("write", args, 1);
            Evaluator.emitOutput(SchemeValue.toStr(args.get(0)));
            return null; // void
        }));
        env.define("newline", Builtin.named("newline", args -> {
            requireArgCount("newline", args, 0);
            Evaluator.emitOutput("\n");
            return null; // void
        }));

        // String operations
        env.define("string-append", Builtin.named("string-append", args -> {
            StringBuilder sb = new StringBuilder("\"");
            for (Object a : args) {
                sb.append(requireString("string-append", a));
            }
            sb.append("\"");
            return sb.toString();
        }));
        env.define("string-length", Builtin.named("string-length", args -> {
            requireArgCount("string-length", args, 1);
            return (long) requireString("string-length", args.get(0)).length();
        }));
        env.define("substring", Builtin.named("substring", args -> {
            requireArgCount("substring", args, 3);
            String s = requireString("substring", args.get(0));
            int start = (int) requireLong("substring", args.get(1));
            int end = (int) requireLong("substring", args.get(2));
            return "\"" + s.substring(start, end) + "\"";
        }));
        env.define("string->number", Builtin.named("string->number", args -> {
            requireArgCount("string->number", args, 1);
            String s = requireString("string->number", args.get(0));
            try {
                return Long.parseLong(s);
            } catch (NumberFormatException e) {
                return Boolean.FALSE;
            }
        }));
        env.define("number->string", Builtin.named("number->string", args -> {
            requireArgCount("number->string", args, 1);
            long n = requireLong("number->string", args.get(0));
            return "\"" + n + "\"";
        }));
        env.define("symbol->string", Builtin.named("symbol->string", args -> {
            requireArgCount("symbol->string", args, 1);
            Object a = args.get(0);
            if (!(a instanceof String s) || s.startsWith("\""))
                throw new EvalError("symbol->string: expected symbol");
            return "\"" + s + "\"";
        }));
        env.define("string->symbol", Builtin.named("string->symbol", args -> {
            requireArgCount("string->symbol", args, 1);
            return requireString("string->symbol", args.get(0));
        }));
        env.define("string-ref", Builtin.named("string-ref", args -> {
            requireArgCount("string-ref", args, 2);
            String s = requireString("string-ref", args.get(0));
            int idx = (int) requireLong("string-ref", args.get(1));
            return s.charAt(idx);
        }));
        env.define("char?", Builtin.named("char?", args -> {
            requireArgCount("char?", args, 1);
            return args.get(0) instanceof Character;
        }));
        env.define("string-copy", Builtin.named("string-copy", args -> {
            requireArgCount("string-copy", args, 1);
            String s = requireString("string-copy", args.get(0));
            return new MutableString(s.toCharArray());
        }));
        env.define("string-set!", Builtin.named("string-set!", args -> {
            requireArgCount("string-set!", args, 3);
            Object target = args.get(0);
            if (!(target instanceof MutableString ms))
                throw new EvalError("string-set!: expected mutable string");
            int idx = (int) requireLong("string-set!", args.get(1));
            Object chObj = args.get(2);
            if (!(chObj instanceof Character ch))
                throw new EvalError("string-set!: expected character, got: " + SchemeValue.toStr(chObj));
            ms.chars[idx] = ch;
            return null; // void
        }));

        return env;
    }

    private static String requireString(String name, Object val) throws EvalError {
        if (val instanceof String s && s.startsWith("\"") && s.endsWith("\"")) {
            return s.substring(1, s.length() - 1);
        }
        if (val instanceof MutableString ms) {
            return ms.inner();
        }
        throw new EvalError(name + ": expected string, got: " + SchemeValue.toStr(val));
    }

    private static String displayStr(Object val) {
        if (val == null) return "";
        if (val instanceof String s && s.startsWith("\"") && s.endsWith("\"")) {
            return s.substring(1, s.length() - 1);
        }
        if (val instanceof MutableString ms) {
            return ms.inner();
        }
        return SchemeValue.toStr(val);
    }

    private static long requireLong(String name, Object val) throws EvalError {
        if (val instanceof Long l) return l;
        throw new EvalError(name + ": expected number, got: " + SchemeValue.toStr(val));
    }

    private static void requireArgCount(String name, List<Object> args, int n) throws EvalError {
        if (args.size() != n)
            throw new EvalError(name + ": expected " + n + " arguments, got " + args.size());
    }
}
