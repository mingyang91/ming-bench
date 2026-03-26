package ming;

import java.util.HashMap;
import java.util.Map;

final class Environment {
    private final Environment parent;
    private final Map<String, SchemeValue> bindings = new HashMap<>();

    Environment(Environment parent) {
        this.parent = parent;
    }

    void define(String name, SchemeValue value) {
        bindings.put(name, value);
    }

    SchemeValue lookup(String name) throws EvalError {
        if (bindings.containsKey(name)) {
            return bindings.get(name);
        }
        if (parent != null) {
            return parent.lookup(name);
        }
        throw new EvalError("unbound variable: " + name);
    }
}
