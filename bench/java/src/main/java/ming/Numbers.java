package ming;

final class Numbers {

    private Numbers() {}

    static Object parseNumber(String tok) {
        try { return Long.parseLong(tok); } catch (NumberFormatException ignored) {}
        int slash = tok.indexOf('/');
        if (slash > 0 && slash < tok.length() - 1) {
            try {
                long num = Long.parseLong(tok.substring(0, slash));
                long den = Long.parseLong(tok.substring(slash + 1));
                if (den == 0) return null;
                Rational r = new Rational(num, den);
                return r.isInteger() ? r.toLong() : r;
            } catch (NumberFormatException ignored) {}
        }
        try {
            if (tok.contains(".") || tok.contains("e") || tok.contains("E")) {
                return Double.parseDouble(tok);
            }
        } catch (NumberFormatException ignored) {}
        return null;
    }

    static boolean isNumber(Object val) {
        return val instanceof Long || val instanceof Rational || val instanceof Double;
    }

    static double toDouble(Object val) throws EvalError {
        if (val instanceof Long l) return l.doubleValue();
        if (val instanceof Rational r) return r.toDouble();
        if (val instanceof Double d) return d;
        throw new EvalError("expected number, got: " + val);
    }

    static Object addNum(Object a, Object b) throws EvalError {
        if (a instanceof Double || b instanceof Double) return toDouble(a) + toDouble(b);
        long an, ad, bn, bd;
        if (a instanceof Long l) { an = l; ad = 1; } else { Rational r = (Rational) a; an = r.num; ad = r.den; }
        if (b instanceof Long l) { bn = l; bd = 1; } else { Rational r = (Rational) b; bn = r.num; bd = r.den; }
        Rational result = new Rational(an * bd + bn * ad, ad * bd);
        return result.isInteger() ? result.toLong() : result;
    }

    static Object subNum(Object a, Object b) throws EvalError {
        if (a instanceof Double || b instanceof Double) return toDouble(a) - toDouble(b);
        long an, ad, bn, bd;
        if (a instanceof Long l) { an = l; ad = 1; } else { Rational r = (Rational) a; an = r.num; ad = r.den; }
        if (b instanceof Long l) { bn = l; bd = 1; } else { Rational r = (Rational) b; bn = r.num; bd = r.den; }
        Rational result = new Rational(an * bd - bn * ad, ad * bd);
        return result.isInteger() ? result.toLong() : result;
    }

    static Object mulNum(Object a, Object b) throws EvalError {
        if (a instanceof Double || b instanceof Double) return toDouble(a) * toDouble(b);
        long an, ad, bn, bd;
        if (a instanceof Long l) { an = l; ad = 1; } else { Rational r = (Rational) a; an = r.num; ad = r.den; }
        if (b instanceof Long l) { bn = l; bd = 1; } else { Rational r = (Rational) b; bn = r.num; bd = r.den; }
        Rational result = new Rational(an * bn, ad * bd);
        return result.isInteger() ? result.toLong() : result;
    }

    static Object divNum(Object a, Object b) throws EvalError {
        if (a instanceof Double || b instanceof Double) {
            double dv = toDouble(b);
            if (dv == 0) throw new EvalError("division by zero");
            return toDouble(a) / dv;
        }
        long an, ad, bn, bd;
        if (a instanceof Long l) { an = l; ad = 1; } else { Rational r = (Rational) a; an = r.num; ad = r.den; }
        if (b instanceof Long l) { bn = l; bd = 1; } else { Rational r = (Rational) b; bn = r.num; bd = r.den; }
        if (bn == 0) throw new EvalError("division by zero");
        Rational result = new Rational(an * bd, ad * bn);
        return result.isInteger() ? result.toLong() : result;
    }

    static Object negateNum(Object a) throws EvalError {
        if (a instanceof Long l) return -l;
        if (a instanceof Double d) return -d;
        if (a instanceof Rational r) return new Rational(-r.num, r.den);
        throw new EvalError("expected number, got: " + a);
    }

    static void requireNumber(Object val, String context) throws EvalError {
        if (!isNumber(val)) throw new EvalError(context + ": expected number, got: " + val);
    }

    static long asLong(Object val) throws EvalError {
        if (val instanceof Long l) return l;
        if (val instanceof Rational r && r.isInteger()) return r.toLong();
        throw new EvalError("expected integer, got: " + val);
    }
}
