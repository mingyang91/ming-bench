package ming;

import java.util.List;

public sealed interface SchemeValue {
    record IntVal(long value) implements SchemeValue {}
    record BoolVal(boolean value) implements SchemeValue {}
    final class StringVal implements SchemeValue {
        private final StringBuilder value;
        public StringVal(String value) { this.value = new StringBuilder(value); }
        public String value() { return value.toString(); }
        public char charAt(int i) { return value.charAt(i); }
        public void setCharAt(int i, char c) { value.setCharAt(i, c); }
        public int length() { return value.length(); }
    }
    record SymbolVal(String name) implements SchemeValue {}
    record ListVal(List<SchemeValue> elements) implements SchemeValue {}
    record VoidVal() implements SchemeValue {}
    record LambdaVal(List<String> params, String restParam, List<SchemeValue> body, Environment env) implements SchemeValue {}
    record PairVal(SchemeValue car, SchemeValue cdr) implements SchemeValue {}
    record CharVal(char value) implements SchemeValue {}
    record DoubleVal(double value) implements SchemeValue {}
    record RationalVal(long num, long den) implements SchemeValue {} // always simplified, den > 0
    record BuiltinVal(String name, Builtin proc) implements SchemeValue {}
    record MacroVal(List<String> literals, List<SchemeValue> patterns, List<SchemeValue> templates, Environment defEnv) implements SchemeValue {}
    record CaseLambdaVal(List<LambdaVal> clauses) implements SchemeValue {}
    record RecordVal(Object tag, String typeName, String[] fieldNames, SchemeValue[] fields) implements SchemeValue {}

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
            case CharVal v -> "#\\" + v.value();
            case DoubleVal v -> Double.toString(v.value());
            case RationalVal v -> v.num() + "/" + v.den();
            case VoidVal v -> "#<void>";
            case LambdaVal v -> "#<procedure>";
            case CaseLambdaVal v -> "#<procedure>";
            case BuiltinVal v -> "#<procedure:" + v.name() + ">";
            case MacroVal v -> "#<macro>";
            case RecordVal v -> "#<record:" + v.typeName() + ">";
        };
    }

    /** Like display but strings without quotes, chars as raw char. Used by Scheme `display`. */
    default String displayOutput() {
        if (this instanceof StringVal s) return s.value();
        if (this instanceof CharVal c) return String.valueOf(c.value());
        return display();
    }

    default boolean isTruthy() {
        return !(this instanceof BoolVal b && !b.value());
    }
}
