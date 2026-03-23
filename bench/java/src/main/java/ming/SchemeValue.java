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

    record SymbolVal(String name, int line, int col) implements SchemeValue {
        public SymbolVal(String name) { this(name, 0, 0); }
        @Override public String display() { return name; }
    }

    record ListVal(List<SchemeValue> elements, int line, int col) implements SchemeValue {
        public ListVal(List<SchemeValue> elements) { this(elements, 0, 0); }
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

    record NilVal() implements SchemeValue {
        @Override public String display() { return "()"; }
    }

    record PairVal(SchemeValue car, SchemeValue cdr) implements SchemeValue {
        @Override public String display() {
            var sb = new StringBuilder("(");
            sb.append(car.display());
            SchemeValue current = cdr;
            while (current instanceof PairVal p) {
                sb.append(' ');
                sb.append(p.car().display());
                current = p.cdr();
            }
            if (!(current instanceof NilVal)) {
                sb.append(" . ");
                sb.append(current.display());
            }
            sb.append(')');
            return sb.toString();
        }
    }

    record VoidVal() implements SchemeValue {
        @Override public String display() { return ""; }
    }

    record LambdaVal(java.util.List<String> params, java.util.List<SchemeValue> body, Environment env) implements SchemeValue {
        @Override public String display() { return "#<procedure>"; }
    }

    String display();

    default boolean isTruthy() {
        return !(this instanceof BoolVal b && !b.value());
    }
}
