package ming;

import java.util.List;

import static ming.SchemeFormatter.schemeToString;

final class ArithmeticOps {

    private ArithmeticOps() {}

    static boolean isNumber(Object o) {
        return o instanceof Long || o instanceof Double || o instanceof SchemeRational;
    }

    static double toDouble(Object o) throws EvalError {
        if (o instanceof Long l) return l.doubleValue();
        if (o instanceof Double d) return d;
        if (o instanceof SchemeRational r) return r.toDouble();
        throw new EvalError("expected number, got: " + schemeToString(o));
    }

    static long requireLong(Object val) throws EvalError {
        if (val instanceof Long l) return l;
        if (val instanceof Double d) return d.longValue();
        throw new EvalError("expected integer, got: " + schemeToString(val));
    }

    private static boolean hasInexact(List<Object> args) {
        for (Object a : args) if (a instanceof Double) return true;
        return false;
    }

    private static long[] toRational(Object a) throws EvalError {
        if (a instanceof Long l) return new long[]{l, 1};
        if (a instanceof SchemeRational r) return new long[]{r.numerator, r.denominator};
        throw new EvalError("expected number, got: " + schemeToString(a));
    }

    private static Object exactAdd(Object a, Object b) throws EvalError {
        long[] ra = toRational(a), rb = toRational(b);
        return SchemeRational.make(ra[0] * rb[1] + rb[0] * ra[1], ra[1] * rb[1]);
    }

    private static Object exactSub(Object a, Object b) throws EvalError {
        long[] ra = toRational(a), rb = toRational(b);
        return SchemeRational.make(ra[0] * rb[1] - rb[0] * ra[1], ra[1] * rb[1]);
    }

    private static Object exactMul(Object a, Object b) throws EvalError {
        long[] ra = toRational(a), rb = toRational(b);
        return SchemeRational.make(ra[0] * rb[0], ra[1] * rb[1]);
    }

    private static Object exactDiv(Object a, Object b) throws EvalError {
        long[] ra = toRational(a), rb = toRational(b);
        if (rb[0] == 0) throw new EvalError("division by zero");
        return SchemeRational.make(ra[0] * rb[1], ra[1] * rb[0]);
    }

    private static Object exactNeg(Object a) throws EvalError {
        if (a instanceof Long l) return -l;
        if (a instanceof SchemeRational r) return SchemeRational.make(-r.numerator, r.denominator);
        throw new EvalError("expected number, got: " + schemeToString(a));
    }

    static Object apply(String name, List<Object> args) throws EvalError {
        return switch (name) {
            case "+" -> {
                if (hasInexact(args)) {
                    double result = 0;
                    for (Object arg : args) result += toDouble(arg);
                    yield result;
                }
                Object result = 0L;
                for (Object arg : args) result = exactAdd(result, arg);
                yield result;
            }
            case "-" -> {
                if (args.isEmpty()) throw new EvalError("- requires at least one argument");
                if (hasInexact(args)) {
                    if (args.size() == 1) yield -toDouble(args.get(0));
                    double result = toDouble(args.get(0));
                    for (int i = 1; i < args.size(); i++) result -= toDouble(args.get(i));
                    yield result;
                }
                if (args.size() == 1) yield exactNeg(args.get(0));
                Object result = args.get(0);
                for (int i = 1; i < args.size(); i++) result = exactSub(result, args.get(i));
                yield result;
            }
            case "*" -> {
                if (hasInexact(args)) {
                    double result = 1;
                    for (Object arg : args) result *= toDouble(arg);
                    yield result;
                }
                Object result = 1L;
                for (Object arg : args) result = exactMul(result, arg);
                yield result;
            }
            case "/" -> {
                if (args.isEmpty()) throw new EvalError("/ requires at least one argument");
                if (hasInexact(args)) {
                    double result = toDouble(args.get(0));
                    if (args.size() == 1) yield 1.0 / result;
                    for (int i = 1; i < args.size(); i++) {
                        double d = toDouble(args.get(i));
                        if (d == 0) throw new EvalError("division by zero");
                        result /= d;
                    }
                    yield result;
                }
                Object result = args.get(0);
                if (args.size() == 1) yield exactDiv(1L, result);
                for (int i = 1; i < args.size(); i++) result = exactDiv(result, args.get(i));
                yield result;
            }
            case "abs" -> Math.abs(requireLong(args.get(0)));
            case "modulo" -> Math.floorMod(requireLong(args.get(0)), requireLong(args.get(1)));
            case "remainder" -> requireLong(args.get(0)) % requireLong(args.get(1));
            case "quotient" -> requireLong(args.get(0)) / requireLong(args.get(1));
            case "min" -> {
                if (args.isEmpty()) throw new EvalError("min: expected at least 1 argument");
                long result = requireLong(args.get(0));
                for (int i = 1; i < args.size(); i++) result = Math.min(result, requireLong(args.get(i)));
                yield result;
            }
            case "max" -> {
                if (args.isEmpty()) throw new EvalError("max: expected at least 1 argument");
                long result = requireLong(args.get(0));
                for (int i = 1; i < args.size(); i++) result = Math.max(result, requireLong(args.get(i)));
                yield result;
            }
            case "expt" -> {
                long base = requireLong(args.get(0)), exp = requireLong(args.get(1));
                long result = 1;
                for (long i = 0; i < exp; i++) result *= base;
                yield result;
            }
            case "zero?" -> requireLong(args.get(0)) == 0;
            case "positive?" -> requireLong(args.get(0)) > 0;
            case "negative?" -> requireLong(args.get(0)) < 0;
            case "odd?" -> Math.abs(requireLong(args.get(0))) % 2 == 1;
            case "even?" -> requireLong(args.get(0)) % 2 == 0;
            case "gcd" -> {
                if (args.isEmpty()) yield 0L;
                long result = Math.abs(requireLong(args.get(0)));
                for (int i = 1; i < args.size(); i++) {
                    long b = Math.abs(requireLong(args.get(i)));
                    while (b != 0) { long t = b; b = result % b; result = t; }
                }
                yield result;
            }
            case "lcm" -> {
                if (args.isEmpty()) yield 1L;
                long result = Math.abs(requireLong(args.get(0)));
                for (int i = 1; i < args.size(); i++) {
                    long b = Math.abs(requireLong(args.get(i)));
                    if (result == 0 && b == 0) { result = 0; continue; }
                    long g = result; long t = b;
                    while (t != 0) { long tmp = t; t = g % t; g = tmp; }
                    result = result / g * b;
                }
                yield result;
            }
            case "truncate" -> {
                Object v = args.get(0);
                if (v instanceof Long) yield v;
                if (v instanceof Double d) { long r = (long) d.doubleValue(); yield r; }
                if (v instanceof SchemeRational r) { long res = r.numerator / r.denominator; yield res; }
                throw new EvalError("truncate: not a number");
            }
            case "round" -> {
                Object v = args.get(0);
                if (v instanceof Long) yield v;
                if (v instanceof Double d) { long r = Math.round(d); yield r; }
                if (v instanceof SchemeRational r) { long res = (r.numerator + r.denominator / 2) / r.denominator; yield res; }
                throw new EvalError("round: not a number");
            }
            default -> throw new EvalError("unknown arithmetic procedure: " + name);
        };
    }

    static Object applyNumericType(String name, List<Object> args) throws EvalError {
        return switch (name) {
            case "number?" -> isNumber(args.get(0));
            case "integer?" -> {
                Object v = args.get(0);
                if (v instanceof Long) yield true;
                if (v instanceof Double d) yield d == Math.floor(d) && !Double.isInfinite(d);
                yield false;
            }
            case "rational?" -> args.get(0) instanceof Long || args.get(0) instanceof SchemeRational;
            case "exact?" -> args.get(0) instanceof Long || args.get(0) instanceof SchemeRational;
            case "inexact?" -> args.get(0) instanceof Double;
            case "exact->inexact" -> toDouble(args.get(0));
            case "inexact->exact" -> {
                Object v = args.get(0);
                if (v instanceof Long || v instanceof SchemeRational) yield v;
                if (v instanceof Double d) {
                    if (d == Math.floor(d) && !Double.isInfinite(d)) yield d.longValue();
                    long bits = Double.doubleToLongBits(d);
                    long mantissa = bits & 0x000fffffffffffffL;
                    int exponent = (int) ((bits >> 52) & 0x7ffL) - 1023 - 52;
                    mantissa |= 0x0010000000000000L;
                    if ((bits & 0x8000000000000000L) != 0) mantissa = -mantissa;
                    if (exponent >= 0) yield mantissa * (1L << exponent);
                    else yield SchemeRational.make(mantissa, 1L << (-exponent));
                }
                throw new EvalError("inexact->exact: expected number");
            }
            case "numerator" -> {
                Object v = args.get(0);
                if (v instanceof Long l) yield l;
                if (v instanceof SchemeRational r) yield r.numerator;
                throw new EvalError("numerator: expected rational");
            }
            case "denominator" -> {
                Object v = args.get(0);
                if (v instanceof Long) yield 1L;
                if (v instanceof SchemeRational r) yield r.denominator;
                throw new EvalError("denominator: expected rational");
            }
            default -> throw new EvalError("unknown numeric type procedure: " + name);
        };
    }
}
