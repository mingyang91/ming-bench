package ming;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

sealed interface Value permits IntValue, RationalValue, InexactValue,
        BoolValue, StringValue, CharValue, SymbolValue,
        PairValue, VectorValue, RecordValue, EmptyListValue, VoidValue,
        UninitializedValue, MultiValueValue, ProcedureValue,
        SyntaxValue, SyntaxSequenceValue {
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

record SyntaxValue(Expr expr) implements Value {
    @Override
    public String render() {
        return "#<syntax>";
    }
}

record SyntaxSequenceValue(List<Expr> expressions) implements Value {
    SyntaxSequenceValue {
        expressions = List.copyOf(expressions);
    }

    @Override
    public String render() {
        return "#<syntax-sequence>";
    }
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

    Value cdr() {
        return cdr;
    }

    void setCar(Value value) {
        car = value;
    }

    void setCdr(Value value) {
        cdr = value;
    }

    @Override
    public String render() {
        return ValueFormatting.render(this);
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
        return ValueFormatting.render(this);
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

record MultiValueValue(List<Value> values) implements Value {
    MultiValueValue {
        values = List.copyOf(values);
    }

    @Override
    public String render() {
        return "#<values>";
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

    static String render(Value value) {
        StringBuilder builder = new StringBuilder();
        appendValue(builder, value, new IdentityHashMap<>());
        return builder.toString();
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

    private static void appendValue(StringBuilder builder, Value value,
                                    IdentityHashMap<Value, Boolean> active) {
        if (value instanceof PairValue pairValue) {
            appendPair(builder, pairValue, active);
            return;
        }
        if (value instanceof VectorValue vectorValue) {
            appendVector(builder, vectorValue, active);
            return;
        }
        builder.append(value.render());
    }

    private static void appendPair(StringBuilder builder, PairValue pairValue,
                                   IdentityHashMap<Value, Boolean> active) {
        if (active.containsKey(pairValue)) {
            builder.append("#<cycle>");
            return;
        }

        List<PairValue> markedPairs = new ArrayList<>();

        try {
            builder.append('(');

            Value current = pairValue;
            boolean first = true;
            while (current instanceof PairValue currentPair) {
                if (active.put(currentPair, Boolean.TRUE) != null) {
                    if (!first) {
                        builder.append(" . ");
                    }
                    builder.append("#<cycle>");
                    current = EmptyListValue.INSTANCE;
                    break;
                }
                markedPairs.add(currentPair);

                if (!first) {
                    builder.append(' ');
                }
                appendValue(builder, currentPair.car(), active);
                current = currentPair.cdr();
                first = false;
            }

            if (!(current instanceof EmptyListValue)) {
                if (!first) {
                    builder.append(" . ");
                }
                appendValue(builder, current, active);
            }

            builder.append(')');
        } finally {
            for (int index = markedPairs.size() - 1; index >= 0; index--) {
                active.remove(markedPairs.get(index));
            }
        }
    }

    private static void appendVector(StringBuilder builder, VectorValue vectorValue,
                                     IdentityHashMap<Value, Boolean> active) {
        if (active.put(vectorValue, Boolean.TRUE) != null) {
            builder.append("#<cycle>");
            return;
        }

        try {
            builder.append("#(");
            appendVectorContents(builder, vectorValue.elements(), active);
            builder.append(')');
        } finally {
            active.remove(vectorValue);
        }
    }

    private static void appendVectorContents(StringBuilder builder, List<Value> elements,
                                             IdentityHashMap<Value, Boolean> active) {
        for (int index = 0; index < elements.size(); index++) {
            if (index > 0) {
                builder.append(' ');
            }
            appendValue(builder, elements.get(index), active);
        }
    }
}
