package ming;

import java.util.List;

@FunctionalInterface
interface Builtin {
    Object apply(List<Object> args) throws EvalError;

    default String name() { return "builtin"; }

    static Builtin named(String name, Builtin fn) {
        return new Builtin() {
            @Override public Object apply(List<Object> args) throws EvalError { return fn.apply(args); }
            @Override public String name() { return name; }
        };
    }
}
