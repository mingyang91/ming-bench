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
            return args.get(0) instanceof String s && s.startsWith("\"");
        }));
        env.define("pair?", Builtin.named("pair?", args -> {
            requireArgCount("pair?", args, 1);
            return args.get(0) instanceof Pair;
        }));
        env.define("symbol?", Builtin.named("symbol?", args -> {
            requireArgCount("symbol?", args, 1);
            return args.get(0) instanceof String s && !s.startsWith("\"");
        }));

        return env;
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
