package ming;

class SchemeFormatter {

    static String schemeToString(Object val) {
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
            StringBuilder sb = new StringBuilder("(");
            sb.append(schemeToString(p.car));
            Object rest = p.cdr;
            while (rest instanceof Evaluator.Pair rp) {
                sb.append(" ");
                sb.append(schemeToString(rp.car));
                rest = rp.cdr;
            }
            if (rest != Evaluator.EMPTY_LIST) {
                sb.append(" . ");
                sb.append(schemeToString(rest));
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
                sb.append(schemeToString(v.ref(i)));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof SchemeRecord) return "#<record>";
        if (val instanceof Evaluator.Lambda) return "#<procedure>";
        if (val instanceof Evaluator.CaseLambda) return "#<procedure>";
        if (val instanceof Evaluator.BuiltinProc) return "#<procedure>";
        if (val instanceof SyntaxRulesMacro) return "#<macro>";
        if (val instanceof String s) return s;
        return val.toString();
    }

    static String displayString(Object val) {
        if (val instanceof SchemeString s) return s.value();
        if (val instanceof SchemeChar c) return String.valueOf(c.value());
        if (val == Evaluator.EMPTY_LIST) return "()";
        if (val instanceof Evaluator.Pair p) {
            StringBuilder sb = new StringBuilder("(");
            sb.append(displayString(p.car));
            Object rest = p.cdr;
            while (rest instanceof Evaluator.Pair rp) {
                sb.append(" ");
                sb.append(displayString(rp.car));
                rest = rp.cdr;
            }
            if (rest != Evaluator.EMPTY_LIST) {
                sb.append(" . ");
                sb.append(displayString(rest));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof SchemeVector v) {
            StringBuilder sb = new StringBuilder("#(");
            for (int i = 0; i < v.length(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(displayString(v.ref(i)));
            }
            sb.append(")");
            return sb.toString();
        }
        return schemeToString(val);
    }
}
