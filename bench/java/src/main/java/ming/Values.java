package ming;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

interface Procedure {
    Value apply(List<Value> arguments, SourceLoc callLoc) throws EvalError;
}

interface Value {
    String render();

    default String displayRender() {
        return render();
    }

    default boolean isTruthy() {
        return true;
    }
}

record NumberValue(SchemeNumber value) implements Value {
    @Override
    public String render() {
        return value.render();
    }
}

record BooleanValue(boolean value) implements Value {
    @Override
    public String render() {
        return value ? "#t" : "#f";
    }

    @Override
    public boolean isTruthy() {
        return value;
    }
}

final class StringValue implements Value {
    private final StringBuilder contents;
    private final boolean mutable;

    StringValue(String value) {
        this(value, false);
    }

    StringValue(String value, boolean mutable) {
        this.contents = new StringBuilder(value);
        this.mutable = mutable;
    }

    @Override
    public String render() {
        return ValuePrinter.renderString(text());
    }

    @Override
    public String displayRender() {
        return text();
    }

    String text() {
        return contents.toString();
    }

    StringValue copy(boolean mutableCopy) {
        return new StringValue(text(), mutableCopy);
    }

    void setCodePoint(int index, int codePoint, SourceLoc callLoc) throws EvalError {
        if (!mutable) {
            throw SchemeErrors.at(callLoc, "string-set! expects a mutable string");
        }

        int length = contents.codePointCount(0, contents.length());
        if (index >= length) {
            throw SchemeErrors.at(callLoc, "string-set! index is out of bounds");
        }

        int startOffset = contents.offsetByCodePoints(0, index);
        int endOffset = contents.offsetByCodePoints(startOffset, 1);
        contents.replace(startOffset, endOffset, new String(Character.toChars(codePoint)));
    }
}

final class VectorValue implements Value {
    private final List<Value> elements;

    VectorValue(List<Value> elements) {
        this.elements = new ArrayList<>(elements);
    }

    int size() {
        return elements.size();
    }

    Value element(int index) {
        return elements.get(index);
    }

    void setElement(int index, Value value, SourceLoc callLoc, String procedureName) throws EvalError {
        if (index >= elements.size()) {
            throw SchemeErrors.at(callLoc, procedureName + " index is out of bounds");
        }
        elements.set(index, value);
    }

    List<Value> elements() {
        return List.copyOf(elements);
    }

    @Override
    public String render() {
        return ValuePrinter.renderValue(this);
    }
}

record CharValue(int codePoint) implements Value {
    @Override
    public String render() {
        return ValuePrinter.renderChar(codePoint);
    }

    @Override
    public String displayRender() {
        return new String(Character.toChars(codePoint));
    }
}

record SymbolValue(String name) implements Value {
    @Override
    public String render() {
        return name;
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
        this.car = value;
    }

    void setCdr(Value value) {
        this.cdr = value;
    }

    @Override
    public String render() {
        return ValuePrinter.renderValue(this);
    }

    @Override
    public String displayRender() {
        return ValuePrinter.displayValue(this);
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
        return "";
    }
}

final class ValuePrinter {
    private ValuePrinter() {
    }

    static String renderValue(Value value) {
        return renderValue(value, false, new HashSet<>());
    }

    static String displayValue(Value value) {
        return renderValue(value, true, new HashSet<>());
    }

    static String renderString(String value) {
        StringBuilder builder = new StringBuilder();
        builder.append('"');
        for (int index = 0; index < value.length(); index++) {
            char ch = value.charAt(index);
            switch (ch) {
                case '\\' -> builder.append("\\\\");
                case '"' -> builder.append("\\\"");
                case '\n' -> builder.append("\\n");
                case '\r' -> builder.append("\\r");
                case '\t' -> builder.append("\\t");
                default -> builder.append(ch);
            }
        }
        builder.append('"');
        return builder.toString();
    }

    static String renderChar(int codePoint) {
        if (codePoint == ' ') {
            return "#\\space";
        }
        if (codePoint == '\n') {
            return "#\\newline";
        }
        return "#\\" + new String(Character.toChars(codePoint));
    }

    private static String renderValue(Value value, boolean displayMode, Set<PairValue> activePairs) {
        if (value instanceof PairValue pairValue) {
            return renderPair(pairValue, displayMode, activePairs);
        }
        if (value instanceof VectorValue vectorValue) {
            return renderVector(vectorValue, activePairs);
        }
        if (value instanceof StringValue stringValue) {
            return displayMode ? stringValue.text() : renderString(stringValue.text());
        }
        if (value instanceof CharValue charValue) {
            return displayMode
                    ? new String(Character.toChars(charValue.codePoint()))
                    : renderChar(charValue.codePoint());
        }
        return displayMode ? value.displayRender() : value.render();
    }

    private static String renderVector(VectorValue vector, Set<PairValue> activePairs) {
        StringBuilder builder = new StringBuilder();
        builder.append("#(");
        for (int index = 0; index < vector.size(); index++) {
            if (index > 0) {
                builder.append(' ');
            }
            builder.append(renderValue(vector.element(index), false, activePairs));
        }
        builder.append(')');
        return builder.toString();
    }

    private static String renderPair(
            PairValue pair,
            boolean displayMode,
            Set<PairValue> activePairs
    ) {
        if (!activePairs.add(pair)) {
            return "#<circular>";
        }

        try {
            StringBuilder builder = new StringBuilder();
            builder.append('(');
            appendPairContents(builder, pair, displayMode, activePairs);
            builder.append(')');
            return builder.toString();
        } finally {
            activePairs.remove(pair);
        }
    }

    private static void appendPairContents(
            StringBuilder builder,
            PairValue pair,
            boolean displayMode,
            Set<PairValue> activePairs
    ) {
        builder.append(renderValue(pair.car(), displayMode, activePairs));

        Value tail = pair.cdr();
        if (tail instanceof EmptyListValue) {
            return;
        }
        if (tail instanceof PairValue nextPair) {
            if (activePairs.contains(nextPair)) {
                builder.append(" . #<circular>");
                return;
            }

            builder.append(' ');
            activePairs.add(nextPair);
            try {
                appendPairContents(builder, nextPair, displayMode, activePairs);
            } finally {
                activePairs.remove(nextPair);
            }
            return;
        }

        builder.append(" . ");
        builder.append(renderValue(tail, displayMode, activePairs));
    }
}
