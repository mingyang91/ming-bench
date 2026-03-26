package ming;

import java.util.IdentityHashMap;
import java.util.List;

class SchemeValue {
    static final Object NIL = new Object() {
        @Override public String toString() { return "()"; }
    };

    static String toStr(Object val) {
        return toStr(val, new IdentityHashMap<>());
    }

    private static String toStr(Object val, IdentityHashMap<Object, Boolean> seen) {
        if (val == null) return "void";
        if (val == NIL) return "()";
        if (val instanceof Long l) return l.toString();
        if (val instanceof Rational r) return r.toString();
        if (val instanceof Double d) {
            if (d == Math.floor(d) && !Double.isInfinite(d) && Math.abs(d) < 1e15) {
                return String.valueOf(d);
            }
            return String.valueOf(d);
        }
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof String s) return s;
        if (val instanceof Pair p) {
            if (seen.containsKey(p)) return "...";
            seen.put(p, Boolean.TRUE);
            StringBuilder sb = new StringBuilder("(");
            sb.append(toStr(p.car, seen));
            Object rest = p.cdr;
            while (rest instanceof Pair next) {
                if (seen.containsKey(next)) {
                    sb.append(" . ...");
                    break;
                }
                seen.put(next, Boolean.TRUE);
                sb.append(" ");
                sb.append(toStr(next.car, seen));
                rest = next.cdr;
            }
            if (rest != NIL && !(rest instanceof Pair)) {
                sb.append(" . ");
                sb.append(toStr(rest, seen));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof List<?> list) {
            StringBuilder sb = new StringBuilder("(");
            for (int i = 0; i < list.size(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(toStr(list.get(i), seen));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof SchemeVector v) {
            StringBuilder sb = new StringBuilder("#(");
            for (int i = 0; i < v.data.length; i++) {
                if (i > 0) sb.append(" ");
                sb.append(toStr(v.data[i], seen));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof MutableString ms) return ms.toSchemeStr();
        if (val instanceof Character c) {
            if (c == ' ') return "#\\space";
            if (c == '\n') return "#\\newline";
            if (c == '\t') return "#\\tab";
            return "#\\" + c;
        }
        if (val instanceof Builtin b) return "#<procedure " + b.name() + ">";
        if (val instanceof Lambda) return "#<procedure>";
        if (val instanceof CaseLambda) return "#<procedure>";
        if (val instanceof Evaluator.ContinuationObj) return "#<continuation>";
        if (val == Evaluator.CALLCC) return "#<call/cc>";
        return val.toString();
    }

    static Object quotedToScheme(Object val) {
        if (val instanceof List<?> list) {
            Object result = NIL;
            for (int i = list.size() - 1; i >= 0; i--) {
                result = new Pair(quotedToScheme(list.get(i)), result);
            }
            return result;
        }
        if (val instanceof Pair p) {
            return new Pair(quotedToScheme(p.car), quotedToScheme(p.cdr));
        }
        if (val instanceof SchemeVector v) {
            Object[] data = new Object[v.data.length];
            for (int i = 0; i < v.data.length; i++) data[i] = quotedToScheme(v.data[i]);
            return new SchemeVector(data);
        }
        return val;
    }
}
