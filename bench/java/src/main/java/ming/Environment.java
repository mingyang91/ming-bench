package ming;

import java.util.HashMap;
import java.util.Map;

public class Environment {
    private final Map<String, Object> bindings = new HashMap<>();
    private final Environment parent;

    public Environment() {
        this.parent = null;
    }

    public Environment(Environment parent) {
        this.parent = parent;
    }

    public void define(String name, Object value) {
        bindings.put(name, value);
    }

    public Object lookup(String name) throws EvalError {
        if (bindings.containsKey(name)) {
            return bindings.get(name);
        }
        if (parent != null) {
            return parent.lookup(name);
        }
        throw new EvalError("unbound variable: " + name);
    }
}
