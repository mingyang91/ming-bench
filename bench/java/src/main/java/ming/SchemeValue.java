package ming;

import java.util.List;

public sealed interface SchemeValue {
    record IntVal(long value, SourcePos pos) implements SchemeValue {}
    record BoolVal(boolean value, SourcePos pos) implements SchemeValue {}
    record StringVal(String value, SourcePos pos) implements SchemeValue {}
    record SymbolVal(String name, SourcePos pos) implements SchemeValue {}
    record ListVal(List<SchemeValue> elements, SourcePos pos) implements SchemeValue {}
    record LambdaVal(List<String> params, List<SchemeValue> body, Environment env) implements SchemeValue {}

    default SourcePos sourcePos() {
        return switch (this) {
            case IntVal v -> v.pos();
            case BoolVal v -> v.pos();
            case StringVal v -> v.pos();
            case SymbolVal v -> v.pos();
            case ListVal v -> v.pos();
            case LambdaVal v -> SourcePos.NONE;
        };
    }

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
                if (v.elements().isEmpty()) yield "()";
                var sb = new StringBuilder("(");
                for (int i = 0; i < v.elements().size(); i++) {
                    if (i > 0) sb.append(' ');
                    sb.append(v.elements().get(i).display());
                }
                sb.append(')');
                yield sb.toString();
            }
            case LambdaVal v -> "#<procedure>";
        };
    }
}
