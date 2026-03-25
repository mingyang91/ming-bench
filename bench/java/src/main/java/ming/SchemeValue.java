package ming;

import java.util.List;

public sealed interface SchemeValue {
    record IntVal(long value) implements SchemeValue {}
    record BoolVal(boolean value) implements SchemeValue {}
    final class StringVal implements SchemeValue {
        private final StringBuilder value;
        private final boolean mutable;
        public StringVal(String value) { this.value = new StringBuilder(value); this.mutable = false; }
        public StringVal(String value, boolean mutable) { this.value = new StringBuilder(value); this.mutable = mutable; }
        public boolean isMutable() { return mutable; }
        public String value() { return value.toString(); }
        public char charAt(int i) { return value.charAt(i); }
        public void setCharAt(int i, char c) { value.setCharAt(i, c); }
        public int length() { return value.length(); }
    }
    record SymbolVal(String name) implements SchemeValue {}
    record ListVal(List<SchemeValue> elements) implements SchemeValue {}
    record VoidVal() implements SchemeValue {}
    record LambdaVal(List<String> params, String restParam, List<SchemeValue> body, Environment env) implements SchemeValue {}
    final class PairVal implements SchemeValue {
        private SchemeValue car;
        private SchemeValue cdr;
        public PairVal(SchemeValue car, SchemeValue cdr) { this.car = car; this.cdr = cdr; }
        public SchemeValue car() { return car; }
        public SchemeValue cdr() { return cdr; }
        public void setCar(SchemeValue v) { this.car = v; }
        public void setCdr(SchemeValue v) { this.cdr = v; }
    }
    record CharVal(char value) implements SchemeValue {}
    record DoubleVal(double value) implements SchemeValue {}
    record RationalVal(long num, long den) implements SchemeValue {} // always simplified, den > 0
    record BuiltinVal(String name, Builtin proc) implements SchemeValue {}
    record MacroVal(List<String> literals, List<SchemeValue> patterns, List<SchemeValue> templates, Environment defEnv) implements SchemeValue {}
    record CaseLambdaVal(List<LambdaVal> clauses) implements SchemeValue {}
    record RecordVal(Object tag, String typeName, String[] fieldNames, SchemeValue[] fields) implements SchemeValue {}
    record TailCall(SchemeValue expr, Environment env) implements SchemeValue {}
    final class VectorVal implements SchemeValue {
        private final SchemeValue[] elements;
        public VectorVal(SchemeValue[] elements) { this.elements = elements; }
        public SchemeValue[] elements() { return elements; }
        public SchemeValue ref(int i) { return elements[i]; }
        public void set(int i, SchemeValue v) { elements[i] = v; }
        public int length() { return elements.length; }
    }

    @FunctionalInterface
    interface Builtin {
        SchemeValue apply(SchemeValue[] args) throws EvalError;
    }

    default String display() {
        return displaySafe(java.util.Collections.newSetFromMap(new java.util.IdentityHashMap<>()));
    }

    default String displaySafe(java.util.Set<Object> visited) {
        return switch (this) {
            case IntVal v -> Long.toString(v.value());
            case BoolVal v -> v.value() ? "#t" : "#f";
            case StringVal v -> "\"" + v.value() + "\"";
            case SymbolVal v -> v.name();
            case ListVal v -> {
                var sb = new StringBuilder("(");
                for (int i = 0; i < v.elements().size(); i++) {
                    if (i > 0) sb.append(" ");
                    sb.append(v.elements().get(i).displaySafe(visited));
                }
                sb.append(")");
                yield sb.toString();
            }
            case PairVal v -> {
                if (!visited.add(v)) yield "...";
                var sb = new StringBuilder("(");
                sb.append(v.car().displaySafe(visited));
                SchemeValue rest = v.cdr();
                while (rest instanceof PairVal p) {
                    if (!visited.add(p)) { sb.append(" ..."); break; }
                    sb.append(" ");
                    sb.append(p.car().displaySafe(visited));
                    rest = p.cdr();
                }
                if (rest instanceof PairVal) {
                    // cycle detected above, already appended "..."
                } else if (rest instanceof ListVal l && l.elements().isEmpty()) {
                    // proper list, done
                } else {
                    sb.append(" . ");
                    sb.append(rest.displaySafe(visited));
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
            case TailCall t -> "#<tailcall>";
            case VectorVal v -> {
                var sb = new StringBuilder("#(");
                for (int i = 0; i < v.length(); i++) {
                    if (i > 0) sb.append(" ");
                    sb.append(v.ref(i).displaySafe(visited));
                }
                sb.append(")");
                yield sb.toString();
            }
        };
    }

    /** Like display but strings without quotes, chars as raw char. Used by Scheme `display`. */
    default String displayOutput() {
        if (this instanceof StringVal s) return s.value();
        if (this instanceof CharVal c) return String.valueOf(c.value());
        return displayOutputSafe(java.util.Collections.newSetFromMap(new java.util.IdentityHashMap<>()));
    }

    default String displayOutputSafe(java.util.Set<Object> visited) {
        if (this instanceof StringVal s) return s.value();
        if (this instanceof CharVal c) return String.valueOf(c.value());
        return displaySafe(visited);
    }

    default boolean isTruthy() {
        return !(this instanceof BoolVal b && !b.value());
    }
}
