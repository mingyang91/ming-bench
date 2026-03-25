package ming;

import java.util.HashMap;
import java.util.Map;

final class Environment {
    private final Environment parent;
    private final Map<String, Value> bindings = new HashMap<>();
    private final Map<String, SyntaxMacro> macros = new HashMap<>();

    Environment(Environment parent) {
        this.parent = parent;
    }

    void define(String name, Value value) {
        bindings.put(name, value);
    }

    void defineMacro(String name, SyntaxMacro macro) {
        macros.put(name, macro);
    }

    Value lookup(String name, SourcePos pos) throws EvalError {
        if (bindings.containsKey(name)) {
            return bindings.get(name);
        }
        if (parent != null) {
            return parent.lookup(name, pos);
        }
        throw new EvalError("unbound variable: " + name, pos.line(), pos.column());
    }

    SyntaxMacro lookupMacro(String name) {
        if (macros.containsKey(name)) {
            return macros.get(name);
        }
        if (parent != null) {
            return parent.lookupMacro(name);
        }
        return null;
    }

    void assign(String name, Value value, SourcePos pos) throws EvalError {
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
