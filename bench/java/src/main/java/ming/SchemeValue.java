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
    record LambdaVal(List<String> params, String restParam, List<SchemeValue> body, Environment env) implements SchemeValue {}
    record BuiltinVal(String name, Function<List<SchemeValue>, SchemeValue> fn) implements SchemeValue {}
    final class PairVal implements SchemeValue {
        SchemeValue car;
        SchemeValue cdr;
        PairVal(SchemeValue car, SchemeValue cdr) { this.car = car; this.cdr = cdr; }
        public SchemeValue car() { return car; }
        public SchemeValue cdr() { return cdr; }
    }
    record VoidVal() implements SchemeValue {}
    record CharVal(char value) implements SchemeValue {}
    record MutableStringVal(StringBuilder chars) implements SchemeValue {
        public String value() { return chars.toString(); }
    }
    record ContinuationVal(int id) implements SchemeValue {}
    record SyntaxRulesVal(List<String> literals, List<SchemeValue> patterns, List<SchemeValue> templates, Environment defEnv) implements SchemeValue {}
    record VectorVal(SchemeValue[] elements) implements SchemeValue {}
    record ValuesVal(List<SchemeValue> values) implements SchemeValue {}

    /** Scheme `display` output: no quotes on strings, chars as bare characters. */
    default String displayStr() {
        return switch (this) {
            case StringVal v -> v.value();
            case MutableStringVal v -> v.value();
            case CharVal v -> String.valueOf(v.value());
            default -> display();
        };
    }

    default String display() {
        return switch (this) {
            case IntVal v -> String.valueOf(v.value());
            case BoolVal v -> v.value() ? "#t" : "#f";
            case StringVal v -> "\"" + v.value() + "\"";
            case MutableStringVal v -> "\"" + v.value() + "\"";
            case SymbolVal v -> v.name();
            case LambdaVal v -> "#<procedure>";
            case BuiltinVal v -> "#<procedure:" + v.name() + ">";
            case ContinuationVal v -> "#<continuation>";
            case SyntaxRulesVal v -> "#<syntax>";
            case VoidVal v -> "";
            case ValuesVal v -> v.values().isEmpty() ? "" : v.values().getFirst().display();
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
            case VectorVal v -> {
                var sb = new StringBuilder("#(");
                for (int i = 0; i < v.elements().length; i++) {
                    if (i > 0) sb.append(' ');
                    sb.append(v.elements()[i].display());
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
