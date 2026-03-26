package ming;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

sealed interface Value permits IntValue, RationalValue, InexactValue,
        BoolValue, StringValue, CharValue, SymbolValue,
        PairValue, VectorValue, RecordValue, EmptyListValue, VoidValue,
        UninitializedValue, ProcedureValue {
    String render();
}

record IntValue(int value) implements Value {
    @Override
    public String render() {
        return Integer.toString(value);
    }
}

record RationalValue(BigInteger numerator, BigInteger denominator) implements Value {
    RationalValue {
        if (denominator.signum() == 0) {
            throw new IllegalArgumentException("denominator cannot be zero");
        }

        if (numerator.signum() == 0) {
            numerator = BigInteger.ZERO;
            denominator = BigInteger.ONE;
        } else {
            if (denominator.signum() < 0) {
                numerator = numerator.negate();
                denominator = denominator.negate();
            }

            BigInteger gcd = numerator.gcd(denominator);
            numerator = numerator.divide(gcd);
            denominator = denominator.divide(gcd);
        }
    }

    @Override
    public String render() {
        if (denominator.equals(BigInteger.ONE)) {
            return numerator.toString();
        }
        return numerator + "/" + denominator;
    }
}

record InexactValue(double value) implements Value {
    @Override
    public String render() {
        return Double.toString(value);
    }
}

record BoolValue(boolean value) implements Value {
    static final BoolValue TRUE = new BoolValue(true);
    static final BoolValue FALSE = new BoolValue(false);

    static BoolValue of(boolean value) {
        return value ? TRUE : FALSE;
    }

    @Override
    public String render() {
        return value ? "#t" : "#f";
    }
}

final class StringValue implements Value {
    private final StringBuilder value;
    private final boolean mutable;

    StringValue(String value) {
        this(value, false);
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

    void setCharAt(int index, char ch) throws EvalError {
        if (!mutable) {
            throw new EvalError("string is immutable");
        }
        value.setCharAt(index, ch);
    }

    StringValue copy(boolean mutable) {
        return new StringValue(value(), mutable);
    }

    @Override
    public String render() {
        return "\"" + ValueFormatting.escapeString(value()) + "\"";
    }
}

record CharValue(char value) implements Value {
    @Override
    public String render() {
        return switch (value) {
            case ' ' -> "#\\space";
            case '\n' -> "#\\newline";
            default -> "#\\" + value;
        };
    }
}

record SymbolValue(String name) implements Value {
    @Override
    public String render() {
        return name;
    }
}

record PairValue(Value car, Value cdr) implements Value {
    @Override
    public String render() {
        StringBuilder builder = new StringBuilder();
        builder.append('(');
        ValueFormatting.appendListContents(builder, this);
        builder.append(')');
        return builder.toString();
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

    Value element(int index) {
        return elements.get(index);
    }

    void setElement(int index, Value value) {
        elements.set(index, value);
    }

    List<Value> elements() {
        return List.copyOf(elements);
    }

    @Override
    public String render() {
        StringBuilder builder = new StringBuilder();
        builder.append("#(");
        ValueFormatting.appendVectorContents(builder, elements);
        builder.append(')');
        return builder.toString();
    }
}

final class RecordType {
    private final String name;
    private final List<String> fieldNames;
    private final Map<String, Integer> fieldIndexes = new HashMap<>();

    RecordType(String name, List<String> fieldNames) {
        this.name = name;
        this.fieldNames = List.copyOf(fieldNames);

        for (int index = 0; index < fieldNames.size(); index++) {
            String fieldName = fieldNames.get(index);
            if (fieldIndexes.put(fieldName, index) != null) {
                throw new IllegalArgumentException("duplicate record field: " + fieldName);
            }
        }
    }

    String name() {
        return name;
    }

    int fieldCount() {
        return fieldNames.size();
    }

    int fieldIndex(String fieldName) {
        Integer index = fieldIndexes.get(fieldName);
        return index == null ? -1 : index;
    }
}

final class RecordValue implements Value {
    private final RecordType type;
    private final List<Value> fields;

    RecordValue(RecordType type, List<Value> fields) {
        if (fields.size() != type.fieldCount()) {
            throw new IllegalArgumentException("record field count mismatch");
        }
        this.type = type;
        this.fields = new ArrayList<>(fields);
    }

    RecordType type() {
        return type;
    }

    Value field(int index) {
        return fields.get(index);
    }

    void setField(int index, Value value) {
        fields.set(index, value);
    }

    @Override
    public String render() {
        return "#<record:" + type.name() + ">";
    }
}

enum EmptyListValue implements Value {
    INSTANCE;

    @Override
    public String render() {
        return "()";
    }
}

enum VoidValue implements Value {
    INSTANCE;

    @Override
    public String render() {
        return "#<void>";
    }
}

enum UninitializedValue implements Value {
    INSTANCE;

    @Override
    public String render() {
        return "#<uninitialized>";
    }
}

abstract non-sealed class ProcedureValue implements Value {
    @Override
    public String render() {
        return "#<procedure>";
    }

    abstract Value apply(List<Value> args) throws EvalError;
}

@FunctionalInterface
interface BuiltinAction {
    Value apply(List<Value> args) throws EvalError;
}

@FunctionalInterface
interface ValuePredicate {
    boolean matches(Value value);
}

enum Comparison {
    STRICTLY_LESS("<") {
        @Override
        boolean matches(int relation) {
            return relation < 0;
        }
    },
    STRICTLY_GREATER(">") {
        @Override
        boolean matches(int relation) {
            return relation > 0;
        }
    },
    EQUAL("=") {
        @Override
        boolean matches(int relation) {
            return relation == 0;
        }
    },
    LESS_OR_EQUAL("<=") {
        @Override
        boolean matches(int relation) {
            return relation <= 0;
        }
    },
    GREATER_OR_EQUAL(">=") {
        @Override
        boolean matches(int relation) {
            return relation >= 0;
        }
    };

    private final String symbol;

    Comparison(String symbol) {
        this.symbol = symbol;
    }

    String symbol() {
        return symbol;
    }

    abstract boolean matches(int relation);
}

enum CharComparison {
    EQUAL {
        @Override
        boolean matches(char left, char right) {
            return left == right;
        }
    },
    LESS {
        @Override
        boolean matches(char left, char right) {
            return left < right;
        }
    };

    abstract boolean matches(char left, char right);
}

enum StringComparison {
    EQUAL {
        @Override
        boolean matches(String left, String right) {
            return left.equals(right);
        }
    },
    LESS {
        @Override
        boolean matches(String left, String right) {
            return left.compareTo(right) < 0;
        }
    };

    abstract boolean matches(String left, String right);
}

final class ValueFormatting {
    private ValueFormatting() {
    }

    static void appendListContents(StringBuilder builder, Value value) {
        Value current = value;
        boolean first = true;

        while (current instanceof PairValue pairValue) {
            if (!first) {
                builder.append(' ');
            }
            builder.append(pairValue.car().render());
            current = pairValue.cdr();
            first = false;
        }

        if (!(current instanceof EmptyListValue)) {
            if (!first) {
                builder.append(" . ");
            }
            builder.append(current.render());
        }
    }

    static String escapeString(String value) {
        StringBuilder builder = new StringBuilder(value.length());
        for (int index = 0; index < value.length(); index++) {
            char ch = value.charAt(index);
            switch (ch) {
                case '\\' -> builder.append("\\\\");
                case '"' -> builder.append("\\\"");
                case '\n' -> builder.append("\\n");
                case '\t' -> builder.append("\\t");
                case '\r' -> builder.append("\\r");
                default -> builder.append(ch);
            }
        }
        return builder.toString();
    }

    static void appendVectorContents(StringBuilder builder, List<Value> elements) {
        for (int index = 0; index < elements.size(); index++) {
            if (index > 0) {
                builder.append(' ');
            }
            builder.append(elements.get(index).render());
        }
    }
}
