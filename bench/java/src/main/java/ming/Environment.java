package ming;

import java.util.HashMap;
import java.util.Map;

final class Environment {
    private final Environment parent;
    private final Map<String, Cell> bindings = new HashMap<>();
    private final Map<String, MacroBinding> syntaxBindings = new HashMap<>();

    Environment(Environment parent) {
        this.parent = parent;
    }

    void define(String name, Value value) {
        bindings.put(name, new Cell(value));
    }

    void defineAlias(String name, Cell cell) {
        bindings.put(name, cell);
    }

    void defineSyntax(String name, MacroBinding macro) {
        syntaxBindings.put(name, macro);
    }

    void defineSyntaxAlias(String name, MacroBinding macro) {
        syntaxBindings.put(name, macro);
    }

    Value lookup(String name) throws EvalError {
        Value value = lookupCell(name).value();
        if (value instanceof UninitializedValue) {
            throw new EvalError("uninitialized variable: " + name);
        }
        return value;
    }

    void set(String name, Value value) throws EvalError {
        lookupCell(name).set(value);
    }

    Cell lookupCell(String name) throws EvalError {
        Cell binding = bindings.get(name);
        if (binding != null) {
            return binding;
        }
        if (parent != null) {
            return parent.lookupCell(name);
        }
        throw new EvalError("unbound variable: " + name);
    }

    Cell lookupCellOrNull(String name) {
        Cell binding = bindings.get(name);
        if (binding != null) {
            return binding;
        }
        if (parent != null) {
            return parent.lookupCellOrNull(name);
        }
        return null;
    }

    MacroBinding lookupSyntax(String name) {
        MacroBinding binding = syntaxBindings.get(name);
        if (binding != null) {
            return binding;
        }
        if (parent != null) {
            return parent.lookupSyntax(name);
        }
        return null;
    }
}

final class Cell {
    private Value value;

    Cell(Value value) {
        this.value = value;
    }

    Value value() {
        return value;
    }

    void set(Value value) {
        this.value = value;
    }
}
