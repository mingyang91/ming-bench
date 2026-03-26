package ming;

import java.util.HashMap;
import java.util.Map;

final class Environment {
    private final Environment parent;
    private final Map<String, ValueCell> bindings = new HashMap<>();
    private final Map<String, MacroCell> macros = new HashMap<>();

    Environment(Environment parent) {
        this.parent = parent;
    }

    void define(String name, SchemeValue value) {
        ValueCell cell = bindings.get(name);
        if (cell != null) {
            cell.set(value);
            return;
        }
        bindings.put(name, new ValueCell(value));
    }

    void defineMacro(String name, SyntaxRulesMacro macro) {
        MacroCell cell = macros.get(name);
        if (cell != null) {
            cell.set(macro);
            return;
        }
        macros.put(name, new MacroCell(macro));
    }

    void defineAlias(String alias, String originalName) {
        ValueCell valueCell = findValueCell(originalName);
        if (valueCell != null) {
            bindings.put(alias, valueCell);
        }

        MacroCell macroCell = findMacroCell(originalName);
        if (macroCell != null) {
            macros.put(alias, macroCell);
        }
    }

    void set(String name, SchemeValue value) throws EvalError {
        ValueCell cell = findValueCell(name);
        if (cell != null) {
            cell.set(value);
            return;
        }
        throw new EvalError("unbound variable: " + name);
    }

    SchemeValue lookup(String name) throws EvalError {
        ValueCell cell = findValueCell(name);
        if (cell != null) {
            return cell.get();
        }
        throw new EvalError("unbound variable: " + name);
    }

    SyntaxRulesMacro lookupMacro(String name) {
        MacroCell cell = findMacroCell(name);
        if (cell == null) {
            return null;
        }
        return cell.get();
    }

    private ValueCell findValueCell(String name) {
        if (bindings.containsKey(name)) {
            return bindings.get(name);
        }
        if (parent != null) {
            return parent.findValueCell(name);
        }
        return null;
    }

    private MacroCell findMacroCell(String name) {
        if (macros.containsKey(name)) {
            return macros.get(name);
        }
        if (parent != null) {
            return parent.findMacroCell(name);
        }
        return null;
    }

    private static final class ValueCell {
        private SchemeValue value;

        private ValueCell(SchemeValue value) {
            this.value = value;
        }

        private SchemeValue get() {
            return value;
        }

        private void set(SchemeValue value) {
            this.value = value;
        }
    }

    private static final class MacroCell {
        private SyntaxRulesMacro macro;

        private MacroCell(SyntaxRulesMacro macro) {
            this.macro = macro;
        }

        private SyntaxRulesMacro get() {
            return macro;
        }

        private void set(SyntaxRulesMacro macro) {
            this.macro = macro;
        }
    }
}
