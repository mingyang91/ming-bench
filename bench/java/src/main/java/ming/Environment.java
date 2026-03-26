package ming;

import java.util.HashMap;
import java.util.Map;

final class Environment {
    private final Environment parent;
    private final Map<String, Value> bindings = new HashMap<>();

    Environment(Environment parent) {
        this.parent = parent;
    }

    void define(String name, Value value) {
        bindings.put(name, value);
    }

    void set(String name, Value value) throws EvalError {
        if (bindings.containsKey(name)) {
            bindings.put(name, value);
            return;
        }
        if (parent != null) {
            parent.set(name, value);
            return;
        }
        throw new EvalError("unbound symbol: " + name);
    }

    Value lookup(String name) throws EvalError {
        if (bindings.containsKey(name)) {
            return bindings.get(name);
        }
        if (parent != null) {
            return parent.lookup(name);
        }
        throw new EvalError("unbound symbol: " + name);
    }
}
