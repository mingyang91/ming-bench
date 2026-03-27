package ming;

import java.util.HashMap;
import java.util.Map;

final class Environment {
    private final Environment parent;
    private final Map<String, BindingCell> bindings;
    private final Map<String, SyntaxRulesMacro> macros;

    Environment(Environment parent) {
        this.parent = parent;
        this.bindings = new HashMap<>();
        this.macros = new HashMap<>();
    }

    void define(String name, Value value) {
        defineCell(name, new BindingCell(value));
    }

    void defineCell(String name, BindingCell cell) {
        bindings.put(name, cell);
    }

    void defineMacro(String name, SyntaxRulesMacro definition) {
        macros.put(name, definition);
    }

    Value lookup(String name, SourceLoc loc) throws EvalError {
        BindingCell cell = lookupCell(name);
        if (cell != null) {
            return cell.value();
        }
        throw SchemeErrors.at(loc, "unbound variable: " + name);
    }

    void set(String name, Value value, SourceLoc loc) throws EvalError {
        BindingCell cell = lookupCell(name);
        if (cell != null) {
            cell.set(value);
            return;
        }
        throw SchemeErrors.at(loc, "unbound variable: " + name);
    }

    BindingCell lookupCell(String name) {
        BindingCell cell = bindings.get(name);
        if (cell != null) {
            return cell;
        }
        if (parent != null) {
            return parent.lookupCell(name);
        }
        return null;
    }

    SyntaxRulesMacro lookupMacro(String name) {
        SyntaxRulesMacro definition = macros.get(name);
        if (definition != null) {
            return definition;
        }
        if (parent != null) {
            return parent.lookupMacro(name);
        }
        return null;
    }
}

final class BindingCell {
    private Value value;

    BindingCell(Value value) {
        this.value = value;
    }

    Value value() {
        return value;
    }

    void set(Value value) {
        this.value = value;
    }
}
