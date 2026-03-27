package ming;

import java.util.ArrayList;
import java.util.List;

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

record PairValue(Value car, Value cdr) implements Value {
    @Override
    public String render() {
        return renderContents(false);
    }

    @Override
    public String displayRender() {
        return renderContents(true);
    }

    private String renderContents(boolean displayMode) {
        StringBuilder builder = new StringBuilder();
        builder.append('(');
        appendPairContents(builder, this, displayMode);
        builder.append(')');
        return builder.toString();
    }

    private static void appendPairContents(
            StringBuilder builder,
            PairValue pair,
            boolean displayMode
    ) {
        builder.append(displayMode ? pair.car.displayRender() : pair.car.render());
        if (pair.cdr instanceof EmptyListValue) {
            return;
        }
        if (pair.cdr instanceof PairValue nextPair) {
            builder.append(' ');
            appendPairContents(builder, nextPair, displayMode);
            return;
        }
        builder.append(" . ");
        builder.append(displayMode ? pair.cdr.displayRender() : pair.cdr.render());
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
}
