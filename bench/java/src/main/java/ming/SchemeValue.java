package ming;

import java.util.List;
import java.util.ArrayList;

public sealed interface SchemeValue {
    record IntVal(long value) implements SchemeValue {}
    record BoolVal(boolean value) implements SchemeValue {}
    record StringVal(String value) implements SchemeValue {}
    record SymbolVal(String name) implements SchemeValue {}
    record ListVal(List<SchemeValue> elements) implements SchemeValue {}
    record Void() implements SchemeValue {}

    default String display() {
        return switch (this) {
            case IntVal v -> Long.toString(v.value());
            case BoolVal v -> v.value() ? "#t" : "#f";
            case StringVal v -> "\"" + v.value() + "\"";
            case SymbolVal v -> v.name();
            case ListVal v -> {
                var sb = new StringBuilder("(");
                for (int i = 0; i < v.elements().size(); i++) {
                    if (i > 0) sb.append(" ");
                    sb.append(v.elements().get(i).display());
                }
                sb.append(")");
                yield sb.toString();
            }
            case Void v -> "#<void>";
        };
    }

    default boolean isTruthy() {
        return !(this instanceof BoolVal b && !b.value());
    }
}
