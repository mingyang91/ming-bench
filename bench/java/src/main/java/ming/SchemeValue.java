package ming;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

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

final class PairValue implements SchemeValue {
    private SchemeValue car;
    private SchemeValue cdr;

    PairValue(SchemeValue car, SchemeValue cdr) {
        this.car = car;
        this.cdr = cdr;
    }

    SchemeValue car() {
        return car;
    }

    SchemeValue cdr() {
        return cdr;
    }

    void setCar(SchemeValue car) {
        this.car = car;
    }

    void setCdr(SchemeValue cdr) {
        this.cdr = cdr;
    }

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
        appendPair(builder, this, useDisplay, new HashSet<>());
        return builder.toString();
    }

    private static void appendPair(StringBuilder builder, PairValue pair, boolean useDisplay, Set<PairValue> active) {
        if (!active.add(pair)) {
            builder.append("#<cycle>");
            return;
        }
        builder.append('(');
        appendContents(builder, pair, useDisplay, active);
        builder.append(')');
        active.remove(pair);
    }

    private static void appendContents(StringBuilder builder, PairValue pair, boolean useDisplay, Set<PairValue> active) {
        appendValue(builder, pair.car, useDisplay, active);
        SchemeValue tail = pair.cdr;
        if (tail instanceof EmptyListValue) {
            return;
        }

        if (tail instanceof PairValue nextPair) {
            if (!active.add(nextPair)) {
                builder.append(" . #<cycle>");
                return;
            }
            builder.append(' ');
            appendContents(builder, nextPair, useDisplay, active);
            active.remove(nextPair);
            return;
        }

        builder.append(" . ");
        appendValue(builder, tail, useDisplay, active);
    }

    private static void appendValue(StringBuilder builder, SchemeValue value, boolean useDisplay, Set<PairValue> active) {
        if (value instanceof PairValue pair) {
            appendPair(builder, pair, useDisplay, active);
            return;
        }
        if (useDisplay) {
            builder.append(value.display());
            return;
        }
        builder.append(value.render());
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

final class UninitializedValue implements SchemeValue {
    static final UninitializedValue INSTANCE = new UninitializedValue();

    private UninitializedValue() {
    }

    @Override
    public String render() {
        return "#<uninitialized>";
    }
}

final class VectorValue implements SchemeValue {
    private final List<SchemeValue> elements;

    VectorValue(List<SchemeValue> elements) {
        this.elements = new ArrayList<>(elements);
    }

    int length() {
        return elements.size();
    }

    SchemeValue ref(int index) {
        return elements.get(index);
    }

    void set(int index, SchemeValue value) {
        elements.set(index, value);
    }

    List<SchemeValue> elements() {
        return List.copyOf(elements);
    }

    @Override
    public String render() {
        StringBuilder builder = new StringBuilder();
        builder.append("#(");
        for (int index = 0; index < elements.size(); index++) {
            if (index > 0) {
                builder.append(' ');
            }
            builder.append(elements.get(index).render());
        }
        builder.append(')');
        return builder.toString();
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

record ContinuationProcedure(
        Object continuation
) implements ProcedureValue {
    @Override
    public String render() {
        return "#<procedure:continuation>";
    }
}
