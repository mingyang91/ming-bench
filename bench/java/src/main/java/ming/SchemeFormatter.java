package ming;

import java.util.IdentityHashMap;
import java.util.List;

final class SchemeFormatter {

    private SchemeFormatter() {}

    @SuppressWarnings("unchecked")
    static String schemeToString(Object val) {
        return schemeToString(val, new IdentityHashMap<>());
    }

    private static String schemeToString(Object val, IdentityHashMap<Object, Boolean> seen) {
        if (val instanceof Long l) return l.toString();
        if (val instanceof Double d) {
            if (d == Math.floor(d) && !Double.isInfinite(d) && Math.abs(d) < 1e15) {
                return String.valueOf(d);
            }
            return String.valueOf(d);
        }
        if (val instanceof SchemeRational r) return r.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof String s) return s;
        if (val instanceof SchemeString ss) return ss.toString();
        if (val instanceof SchemeSymbol sym) return sym.name();
        if (val instanceof SchemeChar ch) return "#\\" + ch.value();
        if (val instanceof SchemeVector v) {
            if (seen.containsKey(v)) return "#(...)";
            seen.put(v, Boolean.TRUE);
            StringBuilder sb = new StringBuilder("#(");
            for (int i = 0; i < v.length(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(schemeToString(v.elements[i], seen));
            }
            sb.append(")");
            seen.remove(v);
            return sb.toString();
        }
        if (val instanceof SchemeNil) return "()";
        if (val instanceof SchemePair p) {
            if (seen.containsKey(p)) return "(...)";
            seen.put(p, Boolean.TRUE);
            StringBuilder sb = new StringBuilder("(");
            sb.append(schemeToString(p.car, seen));
            Object rest = p.cdr;
            while (rest instanceof SchemePair rp) {
                if (seen.containsKey(rp)) {
                    sb.append(" ...");
                    break;
                }
                seen.put(rp, Boolean.TRUE);
                sb.append(" ");
                sb.append(schemeToString(rp.car, seen));
                rest = rp.cdr;
            }
            if (!(rest instanceof SchemeNil) && !(rest instanceof SchemePair)) {
                sb.append(" . ");
                sb.append(schemeToString(rest, seen));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof List<?> list) {
            StringBuilder sb = new StringBuilder("(");
            for (int i = 0; i < list.size(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(schemeToString(list.get(i), seen));
            }
            sb.append(")");
            return sb.toString();
        }
        return val.toString();
    }
}
