package ming;

import java.util.List;

public sealed interface SchemeValue {
    record IntVal(long value) implements SchemeValue {}
    record BoolVal(boolean value) implements SchemeValue {}
    record StringVal(String value) implements SchemeValue {}
    record SymbolVal(String name, int line, int col) implements SchemeValue {
        SymbolVal(String name) { this(name, 0, 0); }
    }
    record ListVal(List<SchemeValue> elements, int line, int col) implements SchemeValue {
        ListVal(List<SchemeValue> elements) { this(elements, 0, 0); }
    }
    record PairVal(SchemeValue car, SchemeValue cdr) implements SchemeValue {}
    record NilVal() implements SchemeValue {}
    record LambdaVal(List<String> params, List<SchemeValue> body, Environment env) implements SchemeValue {}
    record VoidVal() implements SchemeValue {}

    @FunctionalInterface
    interface BuiltinFunc {
        SchemeValue apply(List<SchemeValue> args) throws EvalError;
    }
    record BuiltinVal(String name, BuiltinFunc func) implements SchemeValue {}

    static SchemeValue NIL = new NilVal();

    default boolean isTruthy() {
        return !(this instanceof BoolVal b && !b.value());
    }

    default String display() {
        return switch (this) {
            case IntVal v -> String.valueOf(v.value());
            case BoolVal v -> v.value() ? "#t" : "#f";
            case StringVal v -> "\"" + v.value() + "\"";
            case SymbolVal v -> v.name();
            case NilVal v -> "()";
            case PairVal v -> displayPair(v);
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

    private static String displayPair(PairVal pair) {
        StringBuilder sb = new StringBuilder("(");
        sb.append(pair.car().display());
        SchemeValue rest = pair.cdr();
        while (rest instanceof PairVal p) {
            sb.append(' ');
            sb.append(p.car().display());
            rest = p.cdr();
        }
        if (!(rest instanceof NilVal)) {
            sb.append(" . ");
            sb.append(rest.display());
        }
        sb.append(')');
        return sb.toString();
    }
}
