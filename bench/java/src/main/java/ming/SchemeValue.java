package ming;

import java.util.List;

interface SchemeValue {
    String render();

    default String display() {
        return render();
    }

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

    @Override
    public String display() {
        return value;
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

record CharValue(char value) implements SchemeValue {
    @Override
    public String render() {
        return switch (value) {
            case ' ' -> "#\\space";
            case '\n' -> "#\\newline";
            default -> "#\\" + value;
        };
    }

    @Override
    public String display() {
        return String.valueOf(value);
    }
}

record SymbolValue(String name) implements SchemeValue {
    @Override
    public String render() {
        return name;
    }
}

final class EmptyListValue implements SchemeValue {
    static final EmptyListValue INSTANCE = new EmptyListValue();

    private EmptyListValue() {
    }

    @Override
    public String render() {
        return "()";
    }
}

record PairValue(SchemeValue car, SchemeValue cdr) implements SchemeValue {
    @Override
    public String render() {
        return format(false);
    }

    @Override
    public String display() {
        return format(true);
    }

    private String format(boolean useDisplay) {
        StringBuilder builder = new StringBuilder();
        builder.append('(');
        appendContents(builder, this, useDisplay);
        builder.append(')');
        return builder.toString();
    }

    private static void appendContents(StringBuilder builder, SchemeValue value, boolean useDisplay) {
        SchemeValue current = value;
        boolean first = true;

        while (current instanceof PairValue pair) {
            if (!first) {
                builder.append(' ');
            }
            builder.append(renderValue(pair.car(), useDisplay));
            current = pair.cdr();
            first = false;
        }

        if (!(current instanceof EmptyListValue)) {
            builder.append(" . ");
            builder.append(renderValue(current, useDisplay));
        }
    }

    private static String renderValue(SchemeValue value, boolean useDisplay) {
        if (useDisplay) {
            return value.display();
        }
        return value.render();
    }
}

final class VoidValue implements SchemeValue {
    static final VoidValue INSTANCE = new VoidValue();

    private VoidValue() {
    }

    @Override
    public String render() {
        return "#<void>";
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

record LambdaProcedure(
        String name,
        List<String> parameters,
        List<SchemeExpression> body,
        Environment closureEnvironment
) implements SchemeValue {
    @Override
    public String render() {
        if (name == null || name.isEmpty()) {
            return "#<procedure:lambda>";
        }
        return "#<procedure:" + name + ">";
    }
}
