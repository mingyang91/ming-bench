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

record IntValue(long value) implements NumericValue {
    @Override
    public boolean isExact() {
        return true;
    }

    @Override
    public double doubleValue() {
        return value;
    }

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

final class StringValue implements SchemeValue {
    private final StringBuilder contents;
    private final boolean mutable;

    StringValue(String value) {
        this(value, false);
    }

    StringValue(String value, boolean mutable) {
        this.contents = new StringBuilder(value);
        this.mutable = mutable;
    }

    String value() {
        return contents.toString();
    }

    int length() {
        return contents.length();
    }

    char charAt(int index) {
        return contents.charAt(index);
    }

    boolean isMutable() {
        return mutable;
    }

    void setCharAt(int index, char value) {
        contents.setCharAt(index, value);
    }

    StringValue copy(boolean makeMutable) {
        return new StringValue(value(), makeMutable);
    }

    @Override
    public String render() {
        StringBuilder builder = new StringBuilder();
        builder.append('"');
        for (int index = 0; index < contents.length(); index++) {
            appendEscaped(builder, contents.charAt(index));
        }
        builder.append('"');
        return builder.toString();
    }

    @Override
    public String display() {
        return value();
    }

    private static void appendEscaped(StringBuilder builder, char current) {
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

interface ProcedureValue extends SchemeValue {
}

record BuiltinProcedure(
        String name,
        BuiltinImplementation implementation
) implements ProcedureValue {
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
        String restParameter,
        List<SchemeExpression> body,
        Environment closureEnvironment
) implements ProcedureValue {
    @Override
    public String render() {
        if (name == null || name.isEmpty()) {
            return "#<procedure:lambda>";
        }
        return "#<procedure:" + name + ">";
    }
}

record CaseLambdaClause(
        List<String> parameters,
        String restParameter,
        List<SchemeExpression> body
) {
}

record CaseLambdaProcedure(
        String name,
        List<CaseLambdaClause> clauses,
        Environment closureEnvironment
) implements ProcedureValue {
    @Override
    public String render() {
        if (name == null || name.isEmpty()) {
            return "#<procedure:case-lambda>";
        }
        return "#<procedure:" + name + ">";
    }
}
