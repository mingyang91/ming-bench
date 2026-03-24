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

    public void set(String name, Object value) throws EvalError {
        Environment e = this;
        while (e != null) {
            if (e.bindings.containsKey(name)) {
                e.bindings.put(name, value);
                return;
            }
            e = e.parent;
        }
        throw new EvalError("unbound variable: " + name);
    }

    public Environment getParent() { return parent; }

    public Object lookup(String name) throws EvalError {
        Environment e = this;
        while (e != null) {
            Object val = e.bindings.get(name);
            if (val != null) return val;
            if (e.bindings.containsKey(name)) return null;
            e = e.parent;
        }
        throw new EvalError("unbound variable: " + name);
    }
}
