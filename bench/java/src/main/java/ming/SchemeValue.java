package ming;

class SchemeValue {
    static String toStr(Object val) {
        if (val == null) return "void";
        if (val instanceof Long l) return l.toString();
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof String s) return s;
        if (val instanceof Builtin b) return "#<procedure " + b.name() + ">";
        return val.toString();
    }
}
