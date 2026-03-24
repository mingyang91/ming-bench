package ming;

import java.util.List;

public sealed interface SchemeValue {
    record IntVal(long value) implements SchemeValue {}
    record BoolVal(boolean value) implements SchemeValue {}
    record StringVal(String value) implements SchemeValue {}
    record SymbolVal(String name) implements SchemeValue {}
    record ListVal(List<SchemeValue> elements) implements SchemeValue {}
    record VoidVal() implements SchemeValue {}
    record LambdaVal(List<String> params, List<SchemeValue> body, Environment env) implements SchemeValue {}
    record PairVal(SchemeValue car, SchemeValue cdr) implements SchemeValue {}
    record BuiltinVal(String name, Builtin proc) implements SchemeValue {}

    @FunctionalInterface
    interface Builtin {
        SchemeValue apply(SchemeValue[] args) throws EvalError;
    }

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
            case PairVal v -> {
                var sb = new StringBuilder("(");
                sb.append(v.car().display());
                SchemeValue rest = v.cdr();
                while (rest instanceof PairVal p) {
                    sb.append(" ");
                    sb.append(p.car().display());
                    rest = p.cdr();
                }
                if (rest instanceof ListVal l && l.elements().isEmpty()) {
                    // proper list, done
                } else {
                    sb.append(" . ");
                    sb.append(rest.display());
                }
                sb.append(")");
                yield sb.toString();
            }
            case VoidVal v -> "#<void>";
            case LambdaVal v -> "#<procedure>";
            case BuiltinVal v -> "#<procedure:" + v.name() + ">";
        };
    }

    default boolean isTruthy() {
        return !(this instanceof BoolVal b && !b.value());
    }
}
