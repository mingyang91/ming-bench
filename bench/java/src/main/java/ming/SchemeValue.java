package ming;

import java.util.List;

public sealed interface SchemeValue {
    record IntVal(long value) implements SchemeValue {}
    record BoolVal(boolean value) implements SchemeValue {}
    record StringVal(String value) implements SchemeValue {}
    record SymbolVal(String name) implements SchemeValue {}
    record ListVal(List<SchemeValue> elements) implements SchemeValue {}
    record LambdaVal(List<String> params, List<SchemeValue> body, Environment env) implements SchemeValue {}
    record VoidVal() implements SchemeValue {}

    @FunctionalInterface
    interface BuiltinFunc {
        SchemeValue apply(List<SchemeValue> args) throws EvalError;
    }
    record BuiltinVal(String name, BuiltinFunc func) implements SchemeValue {}

    default boolean isTruthy() {
        return !(this instanceof BoolVal b && !b.value());
    }

    default String display() {
        return switch (this) {
            case IntVal v -> String.valueOf(v.value());
            case BoolVal v -> v.value() ? "#t" : "#f";
            case StringVal v -> "\"" + v.value() + "\"";
            case SymbolVal v -> v.name();
            case ListVal v -> {
                StringBuilder sb = new StringBuilder("(");
                for (int i = 0; i < v.elements().size(); i++) {
                    if (i > 0) sb.append(' ');
                    sb.append(v.elements().get(i).display());
                }
                sb.append(')');
                yield sb.toString();
            }
            case LambdaVal v -> "#<procedure>";
            case VoidVal v -> "#<void>";
            case BuiltinVal v -> "#<procedure:" + v.name() + ">";
        };
    }
}
