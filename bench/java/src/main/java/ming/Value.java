package ming;

import java.util.List;

sealed interface Value permits IntValue, BoolValue, StringValue, CharValue, SymbolValue,
        PairValue, EmptyListValue, VoidValue, ProcedureValue {
    String render();
}

record IntValue(int value) implements Value {
    @Override
    public String render() {
        return Integer.toString(value);
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
        boolean matches(int left, int right) {
            return left < right;
        }
    },
    STRICTLY_GREATER(">") {
        @Override
        boolean matches(int left, int right) {
            return left > right;
        }
    },
    EQUAL("=") {
        @Override
        boolean matches(int left, int right) {
            return left == right;
        }
    },
    LESS_OR_EQUAL("<=") {
        @Override
        boolean matches(int left, int right) {
            return left <= right;
        }
    };

    private final String symbol;

    Comparison(String symbol) {
        this.symbol = symbol;
    }

    String symbol() {
        return symbol;
    }

    abstract boolean matches(int left, int right);
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
}
