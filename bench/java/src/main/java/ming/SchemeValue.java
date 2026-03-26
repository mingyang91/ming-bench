package ming;

import java.util.List;

interface SchemeValue {
    String render();

    static BoolValue booleanValue(boolean value) {
        if (value) {
            return BoolValue.TRUE;
        }
        return BoolValue.FALSE;
    }
}

record IntValue(long value) implements SchemeValue {
    @Override
    public String render() {
        return Long.toString(value);
    }
}

final class BoolValue implements SchemeValue {
    static final BoolValue TRUE = new BoolValue(true);
    static final BoolValue FALSE = new BoolValue(false);

    private final boolean value;

    private BoolValue(boolean value) {
        this.value = value;
    }

    boolean value() {
        return value;
    }

    @Override
    public String render() {
        if (value) {
            return "#t";
        }
        return "#f";
    }
}

record StringValue(String value) implements SchemeValue {
    @Override
    public String render() {
        StringBuilder builder = new StringBuilder();
        builder.append('"');
        for (int index = 0; index < value.length(); index++) {
            appendEscaped(builder, value.charAt(index));
        }
        builder.append('"');
        return builder.toString();
    }

    private void appendEscaped(StringBuilder builder, char current) {
        switch (current) {
            case '\n' -> builder.append("\\n");
            case '\r' -> builder.append("\\r");
            case '\t' -> builder.append("\\t");
            case '"' -> builder.append("\\\"");
            case '\\' -> builder.append("\\\\");
            default -> builder.append(current);
        }
    }
}

@FunctionalInterface
interface BuiltinImplementation {
    SchemeValue apply(List<SchemeValue> arguments) throws EvalError;
}

record BuiltinProcedure(
        String name,
        BuiltinImplementation implementation
) implements SchemeValue {
    SchemeValue apply(List<SchemeValue> arguments) throws EvalError {
        return implementation.apply(arguments);
    }

    @Override
    public String render() {
        return "#<procedure:" + name + ">";
    }
}
