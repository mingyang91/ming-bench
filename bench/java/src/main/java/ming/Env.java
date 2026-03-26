package ming;

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
