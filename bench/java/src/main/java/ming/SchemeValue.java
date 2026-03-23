package ming;

import java.util.List;

public sealed interface SchemeValue {
    record IntVal(long value) implements SchemeValue {}
    record BoolVal(boolean value) implements SchemeValue {}
    final class StringVal implements SchemeValue {
        private final char[] chars;
        private final boolean mutable;
        StringVal(String value) { this.chars = value.toCharArray(); this.mutable = false; }
        StringVal(char[] chars) { this.chars = chars; this.mutable = true; }
        public String value() { return new String(chars); }
        public char charAt(int i) { return chars[i]; }
        public void setCharAt(int i, char c) { chars[i] = c; }
        public boolean isMutable() { return mutable; }
        public int length() { return chars.length; }
    }
    record SymbolVal(String name, int line, int col) implements SchemeValue {
        SymbolVal(String name) { this(name, 0, 0); }
    }
    record ListVal(List<SchemeValue> elements, int line, int col) implements SchemeValue {
        ListVal(List<SchemeValue> elements) { this(elements, 0, 0); }
    }
    record PairVal(SchemeValue car, SchemeValue cdr) implements SchemeValue {}
    record NilVal() implements SchemeValue {}
    record LambdaVal(List<String> params, String restParam, List<SchemeValue> body, Environment env) implements SchemeValue {
        LambdaVal(List<String> params, List<SchemeValue> body, Environment env) {
            this(params, null, body, env);
        }
    }
    record CharVal(char value) implements SchemeValue {}
    record VoidVal() implements SchemeValue {}
    record RationalVal(long num, long den) implements SchemeValue {}
    record DoubleVal(double value) implements SchemeValue {}

    static SchemeValue rational(long num, long den) {
        if (den < 0) { num = -num; den = -den; }
        if (num == 0) return new IntVal(0);
        long g = gcd(Math.abs(num), den);
        num /= g; den /= g;
        if (den == 1) return new IntVal(num);
        return new RationalVal(num, den);
    }

    private static long gcd(long a, long b) {
        while (b != 0) { long t = b; b = a % b; a = t; }
        return a;
    }

    final class VectorVal implements SchemeValue {
        private final SchemeValue[] elements;
        VectorVal(SchemeValue[] elements) { this.elements = elements; }
        public SchemeValue get(int i) { return elements[i]; }
        public void set(int i, SchemeValue v) { elements[i] = v; }
        public int length() { return elements.length; }
        public SchemeValue[] elements() { return elements; }
    }

    @FunctionalInterface
    interface BuiltinFunc {
        SchemeValue apply(List<SchemeValue> args) throws EvalError;
    }
    record BuiltinVal(String name, BuiltinFunc func) implements SchemeValue {}

    @FunctionalInterface
    interface CpsBuiltinFunc {
        Bounce apply(List<SchemeValue> args, Cont k);
    }
    record CpsBuiltinVal(String name, CpsBuiltinFunc func) implements SchemeValue {}

    @FunctionalInterface
    interface Cont {
        Bounce apply(SchemeValue value);
    }
    record ContinuationVal(Cont k, Object windState) implements SchemeValue {}

    record SyntaxRulesVal(List<String> literals, List<SchemeValue> patterns,
                           List<SchemeValue> templates, Environment defEnv) implements SchemeValue {}

    record ValuesVal(List<SchemeValue> values) implements SchemeValue {}

    static SchemeValue NIL = new NilVal();

    default boolean isTruthy() {
        return !(this instanceof BoolVal b && !b.value());
    }

    default String display() {
        return switch (this) {
            case IntVal v -> String.valueOf(v.value());
            case RationalVal v -> v.num() + "/" + v.den();
            case DoubleVal v -> String.valueOf(v.value());
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
            case VectorVal v -> {
                StringBuilder sb = new StringBuilder("#(");
                for (int i = 0; i < v.length(); i++) {
                    if (i > 0) sb.append(' ');
                    sb.append(v.get(i).display());
                }
                sb.append(')');
                yield sb.toString();
            }
            case CharVal v -> "#\\" + (v.value() == ' ' ? "space" : v.value() == '\n' ? "newline" : String.valueOf(v.value()));
            case LambdaVal v -> "#<procedure>";
            case VoidVal v -> "#<void>";
            case BuiltinVal v -> "#<procedure:" + v.name() + ">";
            case CpsBuiltinVal v -> "#<procedure:" + v.name() + ">";
            case ContinuationVal v -> "#<procedure>";
            case SyntaxRulesVal v -> "#<syntax>";
            case ValuesVal v -> "#<values>";
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
