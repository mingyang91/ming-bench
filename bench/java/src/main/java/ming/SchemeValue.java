package ming;

import java.util.List;
import java.util.function.Function;

public sealed interface SchemeValue {
    record IntVal(long value) implements SchemeValue {}
    record BoolVal(boolean value) implements SchemeValue {}
    record StringVal(String value) implements SchemeValue {}
    record SymbolVal(String name) implements SchemeValue {}
    record ListVal(List<SchemeValue> elements) implements SchemeValue {}
    record LambdaVal(List<String> params, List<SchemeValue> body, Environment env) implements SchemeValue {}
    record BuiltinVal(String name, Function<List<SchemeValue>, SchemeValue> fn) implements SchemeValue {}

    default String display() {
        return switch (this) {
            case IntVal v -> String.valueOf(v.value());
            case BoolVal v -> v.value() ? "#t" : "#f";
            case StringVal v -> "\"" + v.value() + "\"";
            case SymbolVal v -> v.name();
            case LambdaVal v -> "#<procedure>";
            case BuiltinVal v -> "#<procedure:" + v.name() + ">";
            case ListVal v -> {
                if (v.elements().isEmpty()) yield "()";
                var sb = new StringBuilder("(");
                for (int i = 0; i < v.elements().size(); i++) {
                    if (i > 0) sb.append(' ');
                    sb.append(v.elements().get(i).display());
                }
                sb.append(')');
                yield sb.toString();
            }
        };
    }

    default boolean isTruthy() {
        return !(this instanceof BoolVal b && !b.value());
    }
}
