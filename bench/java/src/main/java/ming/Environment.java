package ming;

import java.util.HashMap;
import java.util.Map;

final class Environment {
    private final Environment parent;
    private final Map<String, Value> bindings;

    Environment(Environment parent) {
        this.parent = parent;
        this.bindings = new HashMap<>();
    }

    void define(String name, Value value) {
        bindings.put(name, value);
    }

    Value lookup(String name, SourceLoc loc) throws EvalError {
        Value value = bindings.get(name);
        if (value != null) {
            return value;
        }
        if (parent != null) {
            return parent.lookup(name, loc);
        }
        throw SchemeErrors.at(loc, "unbound variable: " + name);
    }

    void set(String name, Value value, SourceLoc loc) throws EvalError {
        if (bindings.containsKey(name)) {
            bindings.put(name, value);
            return;
        }
        if (parent != null) {
            parent.set(name, value, loc);
            return;
        }
        throw SchemeErrors.at(loc, "unbound variable: " + name);
    }
}
