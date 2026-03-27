package ming;

class SchemeFormatter {

    static String schemeToString(Object val) {
        return schemeToStringRec(val, new java.util.IdentityHashMap<>());
    }

    private static String schemeToStringRec(Object val, java.util.IdentityHashMap<Object, Boolean> seen) {
        if (val instanceof Long l) return l.toString();
        if (val instanceof Rational r) return r.toString();
        if (val instanceof Double d) {
            if (d == Math.floor(d) && !Double.isInfinite(d)) return String.format("%.1f", d);
            return Double.toString(d);
        }
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value() + "\"";
        if (val == Evaluator.EMPTY_LIST) return "()";
        if (val instanceof Evaluator.Pair p) {
            if (seen.containsKey(p)) return "...";
            seen.put(p, Boolean.TRUE);
            StringBuilder sb = new StringBuilder("(");
            sb.append(schemeToStringRec(p.car, seen));
            Object rest = p.cdr;
            while (rest instanceof Evaluator.Pair rp) {
                if (seen.containsKey(rp)) { sb.append(" ..."); break; }
                seen.put(rp, Boolean.TRUE);
                sb.append(" ");
                sb.append(schemeToStringRec(rp.car, seen));
                rest = rp.cdr;
            }
            if (!(rest instanceof Evaluator.Pair) && rest != Evaluator.EMPTY_LIST) {
                sb.append(" . ");
                sb.append(schemeToStringRec(rest, seen));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof SchemeChar c) {
            if (c.value() == ' ') return "#\\space";
            if (c.value() == '\n') return "#\\newline";
            if (c.value() == '\t') return "#\\tab";
            return "#\\" + c.value();
        }
        if (val instanceof SchemeVector v) {
            StringBuilder sb = new StringBuilder("#(");
            for (int i = 0; i < v.length(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(schemeToStringRec(v.ref(i), seen));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof SchemeRecord) return "#<record>";
        if (val instanceof Evaluator.Lambda) return "#<procedure>";
        if (val instanceof Evaluator.CaseLambda) return "#<procedure>";
        if (val instanceof Evaluator.BuiltinProc) return "#<procedure>";
        if (val instanceof Evaluator.Continuation) return "#<continuation>";
        if (val == Evaluator.CALLCC_PROC) return "#<procedure>";
        if (val instanceof SyntaxRulesMacro) return "#<macro>";
        if (val instanceof String s) return s;
        return val.toString();
    }

    static String displayString(Object val) {
        return displayStringRec(val, new java.util.IdentityHashMap<>());
    }

    private static String displayStringRec(Object val, java.util.IdentityHashMap<Object, Boolean> seen) {
        if (val instanceof SchemeString s) return s.value();
        if (val instanceof SchemeChar c) return String.valueOf(c.value());
        if (val == Evaluator.EMPTY_LIST) return "()";
        if (val instanceof Evaluator.Pair p) {
            if (seen.containsKey(p)) return "...";
            seen.put(p, Boolean.TRUE);
            StringBuilder sb = new StringBuilder("(");
            sb.append(displayStringRec(p.car, seen));
            Object rest = p.cdr;
            while (rest instanceof Evaluator.Pair rp) {
                if (seen.containsKey(rp)) { sb.append(" ..."); break; }
                seen.put(rp, Boolean.TRUE);
                sb.append(" ");
                sb.append(displayStringRec(rp.car, seen));
                rest = rp.cdr;
            }
            if (!(rest instanceof Evaluator.Pair) && rest != Evaluator.EMPTY_LIST) {
                sb.append(" . ");
                sb.append(displayStringRec(rest, seen));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof SchemeVector v) {
            StringBuilder sb = new StringBuilder("#(");
            for (int i = 0; i < v.length(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(displayStringRec(v.ref(i), seen));
            }
            sb.append(")");
            return sb.toString();
        }
        return schemeToStringRec(val, seen);
    }
}
