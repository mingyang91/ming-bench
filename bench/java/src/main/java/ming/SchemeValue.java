package ming;

import java.util.List;

class SchemeValue {
    static String toStr(Object val) {
        if (val == null) return "void";
        if (val instanceof Long l) return l.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof String s) return s;
        if (val instanceof List<?> list) {
            StringBuilder sb = new StringBuilder("(");
            for (int i = 0; i < list.size(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(toStr(list.get(i)));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof Builtin b) return "#<procedure " + b.name() + ">";
        if (val instanceof Lambda) return "#<procedure>";
        return val.toString();
    }
}
