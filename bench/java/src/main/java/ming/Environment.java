package ming;

import java.util.HashMap;
import java.util.Map;

public class Environment {
    private final Map<String, SchemeValue> bindings = new HashMap<>();
    private final Environment parent;

    public Environment() {
        this.parent = null;
    }

    public Environment(Environment parent) {
        this.parent = parent;
    }

    public SchemeValue get(String name) throws EvalError {
        SchemeValue val = bindings.get(name);
        if (val != null) return val;
        if (parent != null) return parent.get(name);
        throw new EvalError("unbound variable: " + name);
    }

    public void define(String name, SchemeValue value) {
        bindings.put(name, value);
    }
}
