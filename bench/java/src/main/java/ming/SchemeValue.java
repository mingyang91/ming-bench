package ming;

import java.util.List;

public sealed interface SchemeValue {
    record IntVal(long value, SourcePos pos) implements SchemeValue {}
    record BoolVal(boolean value, SourcePos pos) implements SchemeValue {}
    record StringVal(char[] chars, boolean mutable, SourcePos pos) implements SchemeValue {
        StringVal(String value, SourcePos pos) { this(value.toCharArray(), false, pos); }
        StringVal(String value, boolean mutable, SourcePos pos) { this(value.toCharArray(), mutable, pos); }
        String value() { return new String(chars); }
    }
    record SymbolVal(String name, SourcePos pos) implements SchemeValue {}
    record ListVal(List<SchemeValue> elements, SourcePos pos) implements SchemeValue {}
    record LambdaVal(List<String> params, String restParam, List<SchemeValue> body, Environment env) implements SchemeValue {}
    record CharVal(char value, SourcePos pos) implements SchemeValue {}
    record BuiltinVal(String name) implements SchemeValue {}
    record Thunk(SchemeValue expr, Environment env) implements SchemeValue {}
    record ContinuationVal(Continuation cont) implements SchemeValue {}
    record SyntaxRulesVal(List<String> literals, List<SchemeValue> patterns, List<SchemeValue> templates, Environment defEnv) implements SchemeValue {}
    record PairVal(SchemeValue car, SchemeValue cdr, SourcePos pos) implements SchemeValue {}
    record VectorVal(SchemeValue[] elements, SourcePos pos) implements SchemeValue {}
    record ValuesVal(List<SchemeValue> values) implements SchemeValue {}
    record RationalVal(long num, long den, SourcePos pos) implements SchemeValue {}
    record DoubleVal(double value, SourcePos pos) implements SchemeValue {}

    default SourcePos sourcePos() {
        return switch (this) {
            case IntVal v -> v.pos();
            case BoolVal v -> v.pos();
            case StringVal v -> v.pos();
            case SymbolVal v -> v.pos();
            case ListVal v -> v.pos();
            case LambdaVal v -> SourcePos.NONE;
            case CharVal v -> v.pos();
            case BuiltinVal v -> SourcePos.NONE;
            case Thunk v -> SourcePos.NONE;
            case ContinuationVal v -> SourcePos.NONE;
            case SyntaxRulesVal v -> SourcePos.NONE;
            case PairVal v -> v.pos();
            case VectorVal v -> v.pos();
            case ValuesVal v -> SourcePos.NONE;
            case RationalVal v -> v.pos();
            case DoubleVal v -> v.pos();
        };
    }

    default boolean isTruthy() {
        return !(this instanceof BoolVal b && !b.value());
    }

    /** Write representation (strings quoted). */
    default String display() {
        return switch (this) {
            case IntVal v -> String.valueOf(v.value());
            case RationalVal v -> v.num() + "/" + v.den();
            case DoubleVal v -> formatDouble(v.value());
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
            case CharVal v -> "#\\" + v.value();
            case BuiltinVal v -> "#<procedure>";
            case Thunk v -> "#<thunk>";
            case ContinuationVal v -> "#<continuation>";
            case SyntaxRulesVal v -> "#<macro>";
            case ValuesVal v -> "#<values>";
            case PairVal v -> "(" + v.car().display() + " . " + v.cdr().display() + ")";
            case VectorVal v -> {
                var sb = new StringBuilder("#(");
                for (int i = 0; i < v.elements().length; i++) {
                    if (i > 0) sb.append(' ');
                    sb.append(v.elements()[i].display());
                }
                sb.append(')');
                yield sb.toString();
            }
        };
    }

    /** Display representation (strings unquoted). */
    default String displayOutput() {
        return switch (this) {
            case StringVal v -> v.value();
            case ListVal v -> {
                if (v.elements().isEmpty()) yield "()";
                var sb = new StringBuilder("(");
                for (int i = 0; i < v.elements().size(); i++) {
                    if (i > 0) sb.append(' ');
                    sb.append(v.elements().get(i).displayOutput());
                }
                sb.append(')');
                yield sb.toString();
            }
            case Thunk v -> "#<thunk>";
            case ContinuationVal v -> "#<continuation>";
            case SyntaxRulesVal v -> "#<macro>";
            case ValuesVal v -> "#<values>";
            case PairVal v -> "(" + v.car().displayOutput() + " . " + v.cdr().displayOutput() + ")";
            case VectorVal v -> {
                var sb = new StringBuilder("#(");
                for (int i = 0; i < v.elements().length; i++) {
                    if (i > 0) sb.append(' ');
                    sb.append(v.elements()[i].displayOutput());
                }
                sb.append(')');
                yield sb.toString();
            }
            default -> display();
        };
    }

    static String formatDouble(double d) {
        if (d == Math.floor(d) && !Double.isInfinite(d)) {
            long l = (long) d;
            return l + ".0";
        }
        return Double.toString(d);
    }
}
