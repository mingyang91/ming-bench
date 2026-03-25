package ming;

import java.util.List;

import ming.Continuations.SchemeContinuation;

/**
 * Converts Scheme values to their string representations.
 */
final class SchemeFormatter {

    private SchemeFormatter() {}

    static String display(Object val) {
        if (val instanceof SchemeString s) return s.value();
        return toString(val);
    }

    static String toString(Object val) {
        return toStringRec(val, new java.util.IdentityHashMap<>());
    }

    static Object javaToScheme(Object val) {
        if (val instanceof Token t) return javaToScheme(t.value());
        if (val instanceof SchemeVector v) {
            Object[] converted = new Object[v.data.length];
            for (int i = 0; i < v.data.length; i++) {
                converted[i] = javaToScheme(v.data[i]);
            }
            return new SchemeVector(converted);
        }
        if (val instanceof SExpr sexpr) {
            Object tail = sexpr.dotTail != null ? javaToScheme(sexpr.dotTail) : Evaluator.NIL;
            for (int i = sexpr.size() - 1; i >= 0; i--) {
                tail = new Cons(javaToScheme(sexpr.get(i)), tail);
            }
            return tail;
        }
        if (val instanceof List<?> list) {
            Object result = Evaluator.NIL;
            for (int i = list.size() - 1; i >= 0; i--) {
                result = new Cons(javaToScheme(list.get(i)), result);
            }
            return result;
        }
        return val;
    }

    private static String toStringRec(Object val, java.util.IdentityHashMap<Object, Boolean> seen) {
        if (val == Evaluator.NIL) return "()";
        if (val instanceof Long l) return l.toString();
        if (val instanceof SchemeRational r) return r.isInteger() ? String.valueOf(r.toLong()) : r.num + "/" + r.den;
        if (val instanceof Double d) {
            if (d == Math.floor(d) && !Double.isInfinite(d)) return String.valueOf(d);
            return String.valueOf(d);
        }
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value() + "\"";
        if (val instanceof SchemeChar c) {
            return switch (c.value()) {
                case ' ' -> "#\\space";
                case '\n' -> "#\\newline";
                case '\t' -> "#\\tab";
                default -> "#\\" + c.value();
            };
        }
        if (val instanceof SchemeVector v) {
            if (seen.containsKey(v)) return "#<cycle>";
            seen.put(v, Boolean.TRUE);
            StringBuilder sb = new StringBuilder("#(");
            for (int i = 0; i < v.length(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(toStringRec(v.data[i], seen));
            }
            sb.append(")");
            seen.remove(v);
            return sb.toString();
        }
        if (val instanceof SchemeRecord r) return "#<record " + r.type.name + ">";
        if (val instanceof Evaluator.Builtin b) return "#<procedure " + b.name() + ">";
        if (val instanceof Evaluator.Lambda) return "#<procedure>";
        if (val instanceof Evaluator.CaseLambda) return "#<procedure>";
        if (val instanceof SchemeContinuation) return "#<procedure>";
        if (val instanceof Cons) {
            if (seen.containsKey(val)) return "#<cycle>";
            seen.put(val, Boolean.TRUE);
            StringBuilder sb = new StringBuilder("(");
            Object cur = val;
            boolean first = true;
            while (cur instanceof Cons c) {
                if (!first) {
                    if (seen.containsKey(cur)) { sb.append(" . #<cycle>"); break; }
                    seen.put(cur, Boolean.TRUE);
                }
                if (!first) sb.append(" ");
                first = false;
                sb.append(toStringRec(c.car, seen));
                cur = c.cdr;
            }
            if (cur != Evaluator.NIL && !(cur instanceof Cons)) {
                sb.append(" . ");
                sb.append(toStringRec(cur, seen));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof List<?> list) {
            StringBuilder sb = new StringBuilder("(");
            for (int i = 0; i < list.size(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(toStringRec(list.get(i), seen));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof String s) return s;
        return String.valueOf(val);
    }
}
