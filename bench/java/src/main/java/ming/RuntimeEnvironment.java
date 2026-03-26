package ming;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

record Formals(List<String> parameters, String restParameter) {
    int fixedCount() {
        return parameters.size();
    }

    boolean matchesArity(int argumentCount) {
        return restParameter == null
                ? argumentCount == fixedCount()
                : argumentCount >= fixedCount();
    }
}

record BindingSpec(String name, Expr initExpr) {
}

record ProcedureClause(Formals formals, List<Expr> body) {
}

record ExactRational(long numerator, long denominator) {
}

final class Cell {
    private Value value;

    Cell(Value value) {
        this.value = value;
    }

    Value get() {
        return value;
    }

    void set(Value newValue) {
        value = newValue;
    }

    boolean isUninitialized() {
        return value instanceof UninitializedValue;
    }
}

final class Env {
    private final Env parent;
    private final Map<String, Cell> bindings = new HashMap<>();

    Env(Env parent) {
        this.parent = parent;
    }

    void define(String name, Value value) {
        bindings.put(name, new Cell(value));
    }

    Cell definePlaceholder(String name) {
        Cell cell = new Cell(new UninitializedValue());
        bindings.put(name, cell);
        return cell;
    }

    Value lookup(String name) throws EvalError {
        Cell cell = lookupCell(name);
        if (cell == null || cell.isUninitialized()) {
            throw new EvalError("unbound variable: " + name);
        }
        return cell.get();
    }

    void set(String name, Value value) throws EvalError {
        Cell cell = lookupCell(name);
        if (cell == null || cell.isUninitialized()) {
            throw new EvalError("unbound variable: " + name);
        }
        cell.set(value);
    }

    Cell lookupCell(String name) {
        if (bindings.containsKey(name)) {
            return bindings.get(name);
        }
        if (parent != null) {
            return parent.lookupCell(name);
        }
        return null;
    }
}
