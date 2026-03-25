package ming;

import java.util.HashMap;
import java.util.Map;

final class Environment {
    private final Environment parent;
    private final Map<String, Evaluator.Value> bindings = new HashMap<>();

    Environment(Environment parent) {
        this.parent = parent;
    }

    void define(String name, Evaluator.Value value) {
        bindings.put(name, value);
    }

    Evaluator.Value lookup(String name, Evaluator.SourcePos pos) throws EvalError {
        if (bindings.containsKey(name)) {
            return bindings.get(name);
        }
        if (parent != null) {
            return parent.lookup(name, pos);
        }
        throw new EvalError("unbound variable: " + name, pos.line(), pos.column());
    }

    void assign(String name, Evaluator.Value value, Evaluator.SourcePos pos) throws EvalError {
        if (bindings.containsKey(name)) {
            bindings.put(name, value);
            return;
        }
        if (parent != null) {
            parent.assign(name, value, pos);
            return;
        }
        throw new EvalError("unbound variable: " + name, pos.line(), pos.column());
    }
}
