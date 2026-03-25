package ming;

import java.math.BigInteger;
import java.util.List;

sealed interface Value permits IntValue, BoolValue, StringValue, CharValue,
        SymbolValue, EmptyListValue, PairValue, ProcedureValue, VoidValue {
}

sealed interface ProcedureValue extends Value permits BuiltinProcedure, ClosureProcedure {
}

@FunctionalInterface
interface BuiltinImplementation {
    Value apply(List<Value> arguments, SourcePos pos) throws EvalError;
}

record IntValue(BigInteger value) implements Value {
}

record BoolValue(boolean value) implements Value {
    static final BoolValue TRUE = new BoolValue(true);
    static final BoolValue FALSE = new BoolValue(false);

    static BoolValue of(boolean value) {
        return value ? TRUE : FALSE;
    }
}

final class StringValue implements Value {
    private final StringBuilder value;
    private final boolean mutable;

    StringValue(String value) {
        this(value, true);
    }

    StringValue(String value, boolean mutable) {
        this.value = new StringBuilder(value);
        this.mutable = mutable;
    }

    String value() {
        return value.toString();
    }

    int length() {
        return value.length();
    }

    char charAt(int index) {
        return value.charAt(index);
    }

    boolean mutable() {
        return mutable;
    }

    void setCharAt(int index, char updatedValue) {
        value.setCharAt(index, updatedValue);
    }

    StringValue copy(boolean mutable) {
        return new StringValue(value(), mutable);
    }
}

record CharValue(char value) implements Value {
}

record SymbolValue(String name) implements Value {
}

record EmptyListValue() implements Value {
    static final EmptyListValue INSTANCE = new EmptyListValue();
}

record PairValue(Value car, Value cdr) implements Value {
}

record BuiltinProcedure(String name, BuiltinImplementation implementation)
        implements ProcedureValue {
}

record ClosureProcedure(String name, List<String> parameters, String restParameter,
                        List<Expr> body, Environment environment)
        implements ProcedureValue {
}

record VoidValue() implements Value {
    static final VoidValue INSTANCE = new VoidValue();
}
