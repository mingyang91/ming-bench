package ming;

import java.util.ArrayList;
import java.util.List;

public sealed interface SchemeValue {

    record IntVal(long value) implements SchemeValue {
        @Override public String display() { return Long.toString(value); }
    }

    record BoolVal(boolean value) implements SchemeValue {
        @Override public String display() { return value ? "#t" : "#f"; }
    }

    record StringVal(String value) implements SchemeValue {
        @Override public String display() { return "\"" + value + "\""; }
    }

    record SymbolVal(String name) implements SchemeValue {
        @Override public String display() { return name; }
    }

    record ListVal(List<SchemeValue> elements) implements SchemeValue {
        @Override public String display() {
            if (elements.isEmpty()) return "()";
            var sb = new StringBuilder("(");
            for (int i = 0; i < elements.size(); i++) {
                if (i > 0) sb.append(' ');
                sb.append(elements.get(i).display());
            }
            sb.append(')');
            return sb.toString();
        }
    }

    record VoidVal() implements SchemeValue {
        @Override public String display() { return ""; }
    }

    String display();

    default boolean isTruthy() {
        return !(this instanceof BoolVal b && !b.value());
    }
}
