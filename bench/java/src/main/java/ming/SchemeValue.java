package ming;

import java.util.List;

class SchemeValue {
    static final Object NIL = new Object() {
        @Override public String toString() { return "()"; }
    };

    static String toStr(Object val) {
        if (val == null) return "void";
        if (val == NIL) return "()";
        if (val instanceof Long l) return l.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof String s) return s;
        if (val instanceof Pair p) {
            StringBuilder sb = new StringBuilder("(");
            sb.append(toStr(p.car));
            Object rest = p.cdr;
            while (rest instanceof Pair next) {
                sb.append(" ");
                sb.append(toStr(next.car));
                rest = next.cdr;
            }
            if (rest != NIL) {
                sb.append(" . ");
                sb.append(toStr(rest));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof List<?> list) {
            StringBuilder sb = new StringBuilder("(");
            for (int i = 0; i < list.size(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(toStr(list.get(i)));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof Character c) return "#\\" + c;
        if (val instanceof Builtin b) return "#<procedure " + b.name() + ">";
        if (val instanceof Lambda) return "#<procedure>";
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
        return val;
    }
}
