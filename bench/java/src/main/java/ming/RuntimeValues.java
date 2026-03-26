package ming;

import java.util.ArrayList;
import java.util.List;

sealed interface Value permits IntValue, RationalValue, InexactValue, BoolValue,
        StringValue, SymbolValue, CharValue, EmptyListValue, PairValue, VectorValue,
        BuiltinValue, ClosureValue, CaseLambdaValue, RecordTypeValue, RecordInstanceValue,
        MultiValue, VoidValue, UninitializedValue {
}

record IntValue(long value) implements Value {
}

record RationalValue(long numerator, long denominator) implements Value {
}

record InexactValue(double value) implements Value {
}

record BoolValue(boolean value) implements Value {
}

record StringValue(StringBuilder contents, boolean mutable) implements Value {
    String text() {
        return contents.toString();
    }

    int length() {
        return contents.length();
    }

    char charAt(int index) {
        return contents.charAt(index);
    }

    void setCharAt(int index, char value) {
        contents.setCharAt(index, value);
    }

    StringValue copy(boolean mutableCopy) {
        return new StringValue(new StringBuilder(text()), mutableCopy);
    }
}

record SymbolValue(String name) implements Value {
}

record CharValue(char value) implements Value {
}

record EmptyListValue() implements Value {
}

final class PairValue implements Value {
    private Value car;
    private Value cdr;

    PairValue(Value car, Value cdr) {
        this.car = car;
        this.cdr = cdr;
    }

    Value car() {
        return car;
    }

    void setCar(Value value) {
        car = value;
    }

    Value cdr() {
        return cdr;
    }

    void setCdr(Value value) {
        cdr = value;
    }
}

final class VectorValue implements Value {
    private final List<Value> elements;

    VectorValue(List<Value> elements) {
        this.elements = new ArrayList<>(elements);
    }

    int length() {
        return elements.size();
    }

    Value ref(int index) {
        return elements.get(index);
    }

    void set(int index, Value value) {
        elements.set(index, value);
    }

    List<Value> elements() {
        return elements;
    }
}

record BuiltinValue(String name, BuiltinFunction implementation) implements Value {
}

record ClosureValue(Formals formals, List<Expr> body, Env env) implements Value {
}

record CaseLambdaValue(List<ProcedureClause> clauses, Env env) implements Value {
}

record RecordTypeValue(String name) implements Value {
}

record RecordInstanceValue(RecordTypeValue type, List<Value> fields) implements Value {
    Value field(int index) {
        return fields.get(index);
    }
}

record MultiValue(List<Value> values) implements Value {
    MultiValue {
        values = List.copyOf(values);
    }
}

record VoidValue() implements Value {
}

record UninitializedValue() implements Value {
}

@FunctionalInterface
interface BuiltinFunction {
    Value apply(List<Value> arguments) throws EvalError;
}
