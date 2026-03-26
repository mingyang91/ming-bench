package ming;

import java.util.IdentityHashMap;
import java.util.List;
import java.util.Set;

import static ming.Evaluator.NIL;

/**
 * Formatting and equality utilities for Scheme values.
 * Extracted from Evaluator to keep file sizes within quality-gate limits.
 */
final class SchemeFormatter {

    private SchemeFormatter() {}

    static String displayString(Object val) {
        if (val instanceof Evaluator.SchemeString s) return s.value;
        if (val instanceof Evaluator.SchemeChar c) return String.valueOf(c.value);
        return toStringImpl(val, new IdentityHashMap<>());
    }

    static String schemeToString(Object val) {
        return toStringImpl(val, new IdentityHashMap<>());
    }

    private static String toStringImpl(Object val, IdentityHashMap<Object, Boolean> seen) {
        if (val == null) return "";
        if (val == NIL) return "()";
        if (val instanceof Long l) return l.toString();
        if (val instanceof Double d) return Double.toString(d);
        if (val instanceof Evaluator.SchemeRational r) return r.numer + "/" + r.denom;
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof Evaluator.SchemeString s) return "\"" + s.value + "\"";
        if (val instanceof Evaluator.SchemeChar c) {
            return switch (c.value) {
                case ' ' -> "#\\space";
                case '\n' -> "#\\newline";
                case '\t' -> "#\\tab";
                default -> "#\\" + c.value;
            };
        }
        if (val instanceof String s) return s;
        if (val instanceof Evaluator.SchemeVector v) {
            if (seen.containsKey(v)) return "#<cycle>";
            seen.put(v, Boolean.TRUE);
            StringBuilder sb = new StringBuilder("#(");
            for (int i = 0; i < v.data.length; i++) {
                if (i > 0) sb.append(" ");
                sb.append(toStringImpl(v.data[i], seen));
            }
            sb.append(")");
            seen.remove(v);
            return sb.toString();
        }
        if (val instanceof Evaluator.Pair) {
            if (seen.containsKey(val)) return "#<cycle>";
            seen.put(val, Boolean.TRUE);
            StringBuilder sb = new StringBuilder("(");
            Object cur = val;
            boolean first = true;
            while (cur instanceof Evaluator.Pair p) {
                if (!first) {
                    if (seen.containsKey(cur)) { sb.append(" . #<cycle>"); break; }
                    seen.put(cur, Boolean.TRUE);
                }
                if (!first) sb.append(" ");
                first = false;
                sb.append(toStringImpl(p.car, seen));
                cur = p.cdr;
            }
            if (cur != NIL && !(cur instanceof Evaluator.Pair)) {
                sb.append(" . ").append(toStringImpl(cur, seen));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof List<?> list) {
            StringBuilder sb = new StringBuilder("(");
            for (int i = 0; i < list.size(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(toStringImpl(list.get(i), seen));
            }
            sb.append(")");
            return sb.toString();
        }
        return val.toString();
    }

    static boolean schemeEqual(Object a, Object b) {
        return schemeEqualImpl(a, b, new IdentityHashMap<>());
    }

    private static boolean schemeEqualImpl(Object a, Object b, IdentityHashMap<Object, Set<Object>> seen) {
        if (a == b) return true;
        if (isNumber(a) && isNumber(b)) return numToDouble(a) == numToDouble(b);
        if (a instanceof Boolean && b instanceof Boolean) return a.equals(b);
        if (a instanceof String && b instanceof String) return a.equals(b);
        if (a instanceof Evaluator.SchemeString sa && b instanceof Evaluator.SchemeString sb) return sa.value.equals(sb.value);
        if (a instanceof Evaluator.SchemeChar ca && b instanceof Evaluator.SchemeChar cb) return ca.value == cb.value;
        if (a == NIL && b == NIL) return true;
        if (a instanceof Evaluator.Pair pa && b instanceof Evaluator.Pair pb) {
            Set<Object> aSet = seen.get(a);
            if (aSet != null && aSet.contains(b)) return true;
            if (aSet == null) { aSet = java.util.Collections.newSetFromMap(new IdentityHashMap<>()); seen.put(a, aSet); }
            aSet.add(b);
            return schemeEqualImpl(pa.car, pb.car, seen) && schemeEqualImpl(pa.cdr, pb.cdr, seen);
        }
        if (a instanceof Evaluator.SchemeVector va && b instanceof Evaluator.SchemeVector vb) {
            if (va.data.length != vb.data.length) return false;
            Set<Object> aSet = seen.get(a);
            if (aSet != null && aSet.contains(b)) return true;
            if (aSet == null) { aSet = java.util.Collections.newSetFromMap(new IdentityHashMap<>()); seen.put(a, aSet); }
            aSet.add(b);
            for (int i = 0; i < va.data.length; i++) {
                if (!schemeEqualImpl(va.data[i], vb.data[i], seen)) return false;
            }
            return true;
        }
        return false;
    }

    static boolean schemeEqv(Object a, Object b) {
        if (a == b) return true;
        if (isNumber(a) && isNumber(b)) return numToDouble(a) == numToDouble(b);
        if (a instanceof Boolean && b instanceof Boolean) return a.equals(b);
        if (a instanceof String && b instanceof String) return a.equals(b);
        if (a instanceof Evaluator.SchemeChar ca && b instanceof Evaluator.SchemeChar cb) return ca.value == cb.value;
        return false;
    }

    static boolean isNumber(Object val) {
        return val instanceof Long || val instanceof Double || val instanceof Evaluator.SchemeRational;
    }

    static double numToDouble(Object val) {
        if (val instanceof Long l) return l.doubleValue();
        if (val instanceof Double d) return d;
        if (val instanceof Evaluator.SchemeRational r) return r.toDouble();
        return 0;
    }
}
