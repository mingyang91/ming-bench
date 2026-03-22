package ming;

import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

public sealed interface SchemeValue {
    Map<SchemeValue, SourcePos> POS_MAP = new IdentityHashMap<>();

    default SchemeValue withPos(SourcePos pos) {
        if (pos != null) POS_MAP.put(this, pos);
        return this;
    }

    default SourcePos pos() {
        return POS_MAP.get(this);
    }
    record IntVal(long value) implements SchemeValue {}
    record BoolVal(boolean value) implements SchemeValue {}
    record StringVal(String value) implements SchemeValue {}
    record SymbolVal(String name) implements SchemeValue {}
    record ListVal(List<SchemeValue> elements) implements SchemeValue {}
    record LambdaVal(List<String> params, List<SchemeValue> body, Environment env) implements SchemeValue {}
    record BuiltinVal(String name, Function<List<SchemeValue>, SchemeValue> fn) implements SchemeValue {}
    record PairVal(SchemeValue car, SchemeValue cdr) implements SchemeValue {}
    record VoidVal() implements SchemeValue {}
    record CharVal(char value) implements SchemeValue {}

    /** Scheme `display` output: no quotes on strings, chars as bare characters. */
    default String displayStr() {
        return switch (this) {
            case StringVal v -> v.value();
            case CharVal v -> String.valueOf(v.value());
            default -> display();
        };
    }

    default String display() {
        return switch (this) {
            case IntVal v -> String.valueOf(v.value());
            case BoolVal v -> v.value() ? "#t" : "#f";
            case StringVal v -> "\"" + v.value() + "\"";
            case SymbolVal v -> v.name();
            case LambdaVal v -> "#<procedure>";
            case BuiltinVal v -> "#<procedure:" + v.name() + ">";
            case VoidVal v -> "";
            case CharVal v -> "#\\" + (v.value() == ' ' ? "space" : v.value() == '\n' ? "newline" : String.valueOf(v.value()));
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
            case PairVal v -> {
                var sb = new StringBuilder("(");
                sb.append(v.car().display());
                SchemeValue rest = v.cdr();
                while (rest instanceof PairVal p) {
                    sb.append(' ');
                    sb.append(p.car().display());
                    rest = p.cdr();
                }
                if (rest instanceof ListVal l) {
                    for (var elem : l.elements()) {
                        sb.append(' ');
                        sb.append(elem.display());
                    }
                } else {
                    sb.append(" . ");
                    sb.append(rest.display());
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
