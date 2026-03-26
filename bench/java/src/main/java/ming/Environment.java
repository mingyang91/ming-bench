package ming;

import java.util.HashMap;
import java.util.Map;

final class Environment {
    private final Environment parent;
    private final Map<String, BindingCell> bindings = new HashMap<>();
    private final Map<String, MacroDefinition> macros = new HashMap<>();

    Environment(Environment parent) {
        this.parent = parent;
    }

    void define(String name, Value value) {
        BindingCell existing = bindings.get(name);
        if (existing != null) {
            existing.set(value);
            return;
        }
        defineCell(name, new BindingCell(value));
    }

    void defineCell(String name, BindingCell cell) {
        bindings.put(name, cell);
    }

    BindingCell lookupCell(String name) {
        BindingCell cell = bindings.get(name);
        if (cell != null) {
            return cell;
        }
        return parent == null ? null : parent.lookupCell(name);
    }

    void set(String name, Value value) throws EvalError {
        BindingCell cell = lookupCell(name);
        if (cell != null) {
            cell.set(value);
            return;
        }
        throw new EvalError("unbound symbol: " + name);
    }

    Value lookup(String name) throws EvalError {
        BindingCell cell = lookupCell(name);
        if (cell != null) {
            return cell.get();
        }
        throw new EvalError("unbound symbol: " + name);
    }

    void defineMacro(String name, MacroDefinition macroDefinition) {
        macros.put(name, macroDefinition);
    }

    MacroDefinition lookupMacro(String name) {
        MacroDefinition macroDefinition = macros.get(name);
        if (macroDefinition != null) {
            return macroDefinition;
        }
        return parent == null ? null : parent.lookupMacro(name);
    }
}

final class BindingCell {
    private Value value;

    BindingCell(Value value) {
        this.value = value;
    }

    Value get() {
        return value;
    }

    void set(Value value) {
        this.value = value;
    }
}
