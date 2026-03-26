package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class Env {
    private final Map<String, Object> bindings;
    private final Env parent;

    Env(Env parent) {
        this.bindings = new HashMap<>();
        this.parent = parent;
    }

    void define(String name, Object value) {
        bindings.put(name, value);
    }

    void set(String name, Object value) throws EvalError {
        if (bindings.containsKey(name)) {
            bindings.put(name, value);
            return;
        }
        if (parent != null) {
            parent.set(name, value);
            return;
        }
        throw new EvalError("set!: unbound variable: " + name);
    }

    Object lookup(String name) throws EvalError {
        if (bindings.containsKey(name)) return bindings.get(name);
        if (parent != null) return parent.lookup(name);
        throw new EvalError("unbound variable: " + name);
    }

    static Env global() {
        Env env = new Env(null);
        // Arithmetic
        env.define("+", Builtin.named("+", args -> {
            Object sum = 0L;
            for (Object a : args) sum = numAdd(sum, requireNumber("+", a));
            return sum;
        }));
        env.define("-", Builtin.named("-", args -> {
            if (args.isEmpty()) throw new EvalError("-: need at least 1 argument");
            Object first = requireNumber("-", args.get(0));
            if (args.size() == 1) return numNegate(first);
            Object r = first;
            for (int i = 1; i < args.size(); i++) r = numSub(r, requireNumber("-", args.get(i)));
            return r;
        }));
        env.define("*", Builtin.named("*", args -> {
            Object p = 1L;
            for (Object a : args) p = numMul(p, requireNumber("*", a));
            return p;
        }));
        env.define("/", Builtin.named("/", args -> {
            if (args.isEmpty()) throw new EvalError("/: need at least 1 argument");
            Object r = requireNumber("/", args.get(0));
            if (args.size() == 1) return numDiv(1L, r);
            for (int i = 1; i < args.size(); i++) r = numDiv(r, requireNumber("/", args.get(i)));
            return r;
        }));

        // Comparisons
        env.define("<", Builtin.named("<", args -> {
            requireArgCount("<", args, 2);
            return numCompare(requireNumber("<", args.get(0)), requireNumber("<", args.get(1))) < 0;
        }));
        env.define(">", Builtin.named(">", args -> {
            requireArgCount(">", args, 2);
            return numCompare(requireNumber(">", args.get(0)), requireNumber(">", args.get(1))) > 0;
        }));
        env.define("=", Builtin.named("=", args -> {
            requireArgCount("=", args, 2);
            return numCompare(requireNumber("=", args.get(0)), requireNumber("=", args.get(1))) == 0;
        }));
        env.define("<=", Builtin.named("<=", args -> {
            requireArgCount("<=", args, 2);
            return numCompare(requireNumber("<=", args.get(0)), requireNumber("<=", args.get(1))) <= 0;
        }));
        env.define(">=", Builtin.named(">=", args -> {
            requireArgCount(">=", args, 2);
            return numCompare(requireNumber(">=", args.get(0)), requireNumber(">=", args.get(1))) >= 0;
        }));
        env.define("not", Builtin.named("not", args -> {
            requireArgCount("not", args, 1);
            return Evaluator.isFalse(args.get(0));
        }));

        // List operations
        env.define("cons", Builtin.named("cons", args -> {
            requireArgCount("cons", args, 2);
            return new Pair(args.get(0), args.get(1));
        }));
        env.define("car", Builtin.named("car", args -> {
            requireArgCount("car", args, 1);
            if (!(args.get(0) instanceof Pair p))
                throw new EvalError("car: expected pair, got: " + SchemeValue.toStr(args.get(0)));
            return p.car;
        }));
        env.define("cdr", Builtin.named("cdr", args -> {
            requireArgCount("cdr", args, 1);
            if (!(args.get(0) instanceof Pair p))
                throw new EvalError("cdr: expected pair, got: " + SchemeValue.toStr(args.get(0)));
            return p.cdr;
        }));
        env.define("null?", Builtin.named("null?", args -> {
            requireArgCount("null?", args, 1);
            return args.get(0) == SchemeValue.NIL;
        }));
        env.define("list", Builtin.named("list", args -> {
            Object result = SchemeValue.NIL;
            for (int i = args.size() - 1; i >= 0; i--) {
                result = new Pair(args.get(i), result);
            }
            return result;
        }));
        env.define("length", Builtin.named("length", args -> {
            requireArgCount("length", args, 1);
            long count = 0;
            Object cur = args.get(0);
            while (cur instanceof Pair p) {
                count++;
                cur = p.cdr;
            }
            return count;
        }));
        env.define("append", Builtin.named("append", args -> {
            if (args.isEmpty()) return SchemeValue.NIL;
            Object result = args.get(args.size() - 1);
            for (int i = args.size() - 2; i >= 0; i--) {
                Object lst = args.get(i);
                List<Object> elems = new ArrayList<>();
                Object cur = lst;
                while (cur instanceof Pair p) {
                    elems.add(p.car);
                    cur = p.cdr;
                }
                for (int j = elems.size() - 1; j >= 0; j--) {
                    result = new Pair(elems.get(j), result);
                }
            }
            return result;
        }));

        // apply
        env.define("apply", Builtin.named("apply", args -> {
            if (args.size() < 2) throw new EvalError("apply: need at least 2 arguments");
            Object func = args.get(0);
            Object lastArg = args.get(args.size() - 1);
            List<Object> callArgs = new ArrayList<>();
            for (int i = 1; i < args.size() - 1; i++) {
                callArgs.add(args.get(i));
            }
            Object cur = lastArg;
            while (cur instanceof Pair p) {
                callArgs.add(p.car);
                cur = p.cdr;
            }
            return Evaluator.applyProc(func, callArgs);
        }));

        // Type predicates
        env.define("boolean?", Builtin.named("boolean?", args -> {
            requireArgCount("boolean?", args, 1);
            return args.get(0) instanceof Boolean;
        }));
        env.define("number?", Builtin.named("number?", args -> {
            requireArgCount("number?", args, 1);
            return isNumber(args.get(0));
        }));
        env.define("string?", Builtin.named("string?", args -> {
            requireArgCount("string?", args, 1);
            return (args.get(0) instanceof String s && s.startsWith("\"")) || args.get(0) instanceof MutableString;
        }));
        env.define("pair?", Builtin.named("pair?", args -> {
            requireArgCount("pair?", args, 1);
            return args.get(0) instanceof Pair;
        }));
        env.define("symbol?", Builtin.named("symbol?", args -> {
            requireArgCount("symbol?", args, 1);
            return args.get(0) instanceof String s && !s.startsWith("\"");
        }));

        // I/O
        env.define("display", Builtin.named("display", args -> {
            requireArgCount("display", args, 1);
            Evaluator.emitOutput(displayStr(args.get(0)));
            return null; // void
        }));
        env.define("write", Builtin.named("write", args -> {
            requireArgCount("write", args, 1);
            Evaluator.emitOutput(SchemeValue.toStr(args.get(0)));
            return null; // void
        }));
        env.define("newline", Builtin.named("newline", args -> {
            requireArgCount("newline", args, 0);
            Evaluator.emitOutput("\n");
            return null; // void
        }));

        // String operations
        env.define("string-append", Builtin.named("string-append", args -> {
            StringBuilder sb = new StringBuilder("\"");
            for (Object a : args) {
                sb.append(requireString("string-append", a));
            }
            sb.append("\"");
            return sb.toString();
        }));
        env.define("string-length", Builtin.named("string-length", args -> {
            requireArgCount("string-length", args, 1);
            return (long) requireString("string-length", args.get(0)).length();
        }));
        env.define("substring", Builtin.named("substring", args -> {
            requireArgCount("substring", args, 3);
            String s = requireString("substring", args.get(0));
            int start = (int) requireLong("substring", args.get(1));
            int end = (int) requireLong("substring", args.get(2));
            return "\"" + s.substring(start, end) + "\"";
        }));
        env.define("string->number", Builtin.named("string->number", args -> {
            requireArgCount("string->number", args, 1);
            String s = requireString("string->number", args.get(0));
            try {
                return Long.parseLong(s);
            } catch (NumberFormatException e) {
                return Boolean.FALSE;
            }
        }));
        env.define("number->string", Builtin.named("number->string", args -> {
            requireArgCount("number->string", args, 1);
            Object a = args.get(0);
            return "\"" + SchemeValue.toStr(a) + "\"";
        }));
        env.define("symbol->string", Builtin.named("symbol->string", args -> {
            requireArgCount("symbol->string", args, 1);
            Object a = args.get(0);
            if (!(a instanceof String s) || s.startsWith("\""))
                throw new EvalError("symbol->string: expected symbol");
            return "\"" + s + "\"";
        }));
        env.define("string->symbol", Builtin.named("string->symbol", args -> {
            requireArgCount("string->symbol", args, 1);
            return requireString("string->symbol", args.get(0));
        }));
        env.define("string-ref", Builtin.named("string-ref", args -> {
            requireArgCount("string-ref", args, 2);
            String s = requireString("string-ref", args.get(0));
            int idx = (int) requireLong("string-ref", args.get(1));
            return s.charAt(idx);
        }));
        env.define("char?", Builtin.named("char?", args -> {
            requireArgCount("char?", args, 1);
            return args.get(0) instanceof Character;
        }));
        env.define("string-copy", Builtin.named("string-copy", args -> {
            requireArgCount("string-copy", args, 1);
            String s = requireString("string-copy", args.get(0));
            return new MutableString(s.toCharArray());
        }));
        env.define("string-set!", Builtin.named("string-set!", args -> {
            requireArgCount("string-set!", args, 3);
            Object target = args.get(0);
            if (!(target instanceof MutableString ms))
                throw new EvalError("string-set!: expected mutable string");
            int idx = (int) requireLong("string-set!", args.get(1));
            Object chObj = args.get(2);
            if (!(chObj instanceof Character ch))
                throw new EvalError("string-set!: expected character, got: " + SchemeValue.toStr(chObj));
            ms.chars[idx] = ch;
            return null; // void
        }));

        // L09 — Numeric utilities
        env.define("abs", Builtin.named("abs", args -> {
            requireArgCount("abs", args, 1);
            return Math.abs(requireLong("abs", args.get(0)));
        }));
        env.define("modulo", Builtin.named("modulo", args -> {
            requireArgCount("modulo", args, 2);
            long a = requireLong("modulo", args.get(0));
            long b = requireLong("modulo", args.get(1));
            if (b == 0) throw new EvalError("modulo: division by zero");
            return Math.floorMod(a, b);
        }));
        env.define("remainder", Builtin.named("remainder", args -> {
            requireArgCount("remainder", args, 2);
            long a = requireLong("remainder", args.get(0));
            long b = requireLong("remainder", args.get(1));
            if (b == 0) throw new EvalError("remainder: division by zero");
            return a % b;
        }));
        env.define("quotient", Builtin.named("quotient", args -> {
            requireArgCount("quotient", args, 2);
            long a = requireLong("quotient", args.get(0));
            long b = requireLong("quotient", args.get(1));
            if (b == 0) throw new EvalError("quotient: division by zero");
            long q = a / b;
            // Truncate toward zero (Java default for integer division)
            return q;
        }));
        env.define("min", Builtin.named("min", args -> {
            if (args.isEmpty()) throw new EvalError("min: need at least 1 argument");
            long result = requireLong("min", args.get(0));
            for (int i = 1; i < args.size(); i++) {
                long v = requireLong("min", args.get(i));
                if (v < result) result = v;
            }
            return result;
        }));
        env.define("max", Builtin.named("max", args -> {
            if (args.isEmpty()) throw new EvalError("max: need at least 1 argument");
            long result = requireLong("max", args.get(0));
            for (int i = 1; i < args.size(); i++) {
                long v = requireLong("max", args.get(i));
                if (v > result) result = v;
            }
            return result;
        }));
        env.define("expt", Builtin.named("expt", args -> {
            requireArgCount("expt", args, 2);
            long base = requireLong("expt", args.get(0));
            long exp = requireLong("expt", args.get(1));
            long result = 1;
            for (long i = 0; i < exp; i++) result *= base;
            return result;
        }));
        env.define("zero?", Builtin.named("zero?", args -> {
            requireArgCount("zero?", args, 1);
            return requireLong("zero?", args.get(0)) == 0;
        }));
        env.define("positive?", Builtin.named("positive?", args -> {
            requireArgCount("positive?", args, 1);
            return requireLong("positive?", args.get(0)) > 0;
        }));
        env.define("negative?", Builtin.named("negative?", args -> {
            requireArgCount("negative?", args, 1);
            return requireLong("negative?", args.get(0)) < 0;
        }));
        env.define("odd?", Builtin.named("odd?", args -> {
            requireArgCount("odd?", args, 1);
            return requireLong("odd?", args.get(0)) % 2 != 0;
        }));
        env.define("even?", Builtin.named("even?", args -> {
            requireArgCount("even?", args, 1);
            return requireLong("even?", args.get(0)) % 2 == 0;
        }));

        // L09 — List utilities
        env.define("list-ref", Builtin.named("list-ref", args -> {
            requireArgCount("list-ref", args, 2);
            int idx = (int) requireLong("list-ref", args.get(1));
            Object cur = args.get(0);
            for (int i = 0; i < idx; i++) {
                if (!(cur instanceof Pair p)) throw new EvalError("list-ref: index out of range");
                cur = p.cdr;
            }
            if (!(cur instanceof Pair p)) throw new EvalError("list-ref: index out of range");
            return p.car;
        }));
        env.define("list-tail", Builtin.named("list-tail", args -> {
            requireArgCount("list-tail", args, 2);
            int idx = (int) requireLong("list-tail", args.get(1));
            Object cur = args.get(0);
            for (int i = 0; i < idx; i++) {
                if (!(cur instanceof Pair p)) throw new EvalError("list-tail: index out of range");
                cur = p.cdr;
            }
            return cur;
        }));
        env.define("list?", Builtin.named("list?", args -> {
            requireArgCount("list?", args, 1);
            Object cur = args.get(0);
            while (cur instanceof Pair p) {
                cur = p.cdr;
            }
            return cur == SchemeValue.NIL;
        }));

        // L09 — eq? and equal?
        env.define("eq?", Builtin.named("eq?", args -> {
            requireArgCount("eq?", args, 2);
            return schemeEq(args.get(0), args.get(1));
        }));
        env.define("equal?", Builtin.named("equal?", args -> {
            requireArgCount("equal?", args, 2);
            return schemeEqual(args.get(0), args.get(1));
        }));

        // L09 — assoc
        env.define("assoc", Builtin.named("assoc", args -> {
            requireArgCount("assoc", args, 2);
            Object key = args.get(0);
            Object alist = args.get(1);
            while (alist instanceof Pair p) {
                if (p.car instanceof Pair entry) {
                    if (schemeEqual(key, entry.car)) return entry;
                }
                alist = p.cdr;
            }
            return Boolean.FALSE;
        }));

        // L09 — Built-in map (multi-list)
        env.define("map", Builtin.named("map", args -> {
            if (args.size() < 2) throw new EvalError("map: need at least 2 arguments");
            Object func = args.get(0);
            int numLists = args.size() - 1;
            Object[] cursors = new Object[numLists];
            for (int i = 0; i < numLists; i++) cursors[i] = args.get(i + 1);
            List<Object> results = new ArrayList<>();
            while (true) {
                boolean allPairs = true;
                for (int i = 0; i < numLists; i++) {
                    if (!(cursors[i] instanceof Pair)) { allPairs = false; break; }
                }
                if (!allPairs) break;
                List<Object> callArgs = new ArrayList<>();
                for (int i = 0; i < numLists; i++) {
                    Pair p = (Pair) cursors[i];
                    callArgs.add(p.car);
                    cursors[i] = p.cdr;
                }
                results.add(Evaluator.applyProc(func, callArgs));
            }
            Object result = SchemeValue.NIL;
            for (int i = results.size() - 1; i >= 0; i--) {
                result = new Pair(results.get(i), result);
            }
            return result;
        }));

        // L09 — Character operations
        env.define("char-alphabetic?", Builtin.named("char-alphabetic?", args -> {
            requireArgCount("char-alphabetic?", args, 1);
            return Character.isLetter(requireChar("char-alphabetic?", args.get(0)));
        }));
        env.define("char-numeric?", Builtin.named("char-numeric?", args -> {
            requireArgCount("char-numeric?", args, 1);
            return Character.isDigit(requireChar("char-numeric?", args.get(0)));
        }));
        env.define("char-upcase", Builtin.named("char-upcase", args -> {
            requireArgCount("char-upcase", args, 1);
            return Character.toUpperCase(requireChar("char-upcase", args.get(0)));
        }));
        env.define("char-downcase", Builtin.named("char-downcase", args -> {
            requireArgCount("char-downcase", args, 1);
            return Character.toLowerCase(requireChar("char-downcase", args.get(0)));
        }));
        env.define("char=?", Builtin.named("char=?", args -> {
            requireArgCount("char=?", args, 2);
            return requireChar("char=?", args.get(0)) == requireChar("char=?", args.get(1));
        }));
        env.define("char<?", Builtin.named("char<?", args -> {
            requireArgCount("char<?", args, 2);
            return requireChar("char<?", args.get(0)) < requireChar("char<?", args.get(1));
        }));

        // L09 — String comparison/case operations
        env.define("string=?", Builtin.named("string=?", args -> {
            requireArgCount("string=?", args, 2);
            return requireString("string=?", args.get(0)).equals(requireString("string=?", args.get(1)));
        }));
        env.define("string<?", Builtin.named("string<?", args -> {
            requireArgCount("string<?", args, 2);
            return requireString("string<?", args.get(0)).compareTo(requireString("string<?", args.get(1))) < 0;
        }));
        env.define("string-ci=?", Builtin.named("string-ci=?", args -> {
            requireArgCount("string-ci=?", args, 2);
            return requireString("string-ci=?", args.get(0)).equalsIgnoreCase(requireString("string-ci=?", args.get(1)));
        }));
        env.define("string-upcase", Builtin.named("string-upcase", args -> {
            requireArgCount("string-upcase", args, 1);
            return "\"" + requireString("string-upcase", args.get(0)).toUpperCase() + "\"";
        }));
        env.define("string-downcase", Builtin.named("string-downcase", args -> {
            requireArgCount("string-downcase", args, 1);
            return "\"" + requireString("string-downcase", args.get(0)).toLowerCase() + "\"";
        }));

        // L09 — integer? predicate (updated for L11: 4/2 is integer)
        env.define("integer?", Builtin.named("integer?", args -> {
            requireArgCount("integer?", args, 1);
            Object a = args.get(0);
            if (a instanceof Long) return true;
            if (a instanceof Rational r) return r.isInteger();
            if (a instanceof Double d) return d == Math.floor(d) && !Double.isInfinite(d);
            return false;
        }));

        // L11 — Exact/inexact predicates and conversions
        env.define("exact?", Builtin.named("exact?", args -> {
            requireArgCount("exact?", args, 1);
            Object a = args.get(0);
            return a instanceof Long || a instanceof Rational;
        }));
        env.define("inexact?", Builtin.named("inexact?", args -> {
            requireArgCount("inexact?", args, 1);
            return args.get(0) instanceof Double;
        }));
        env.define("rational?", Builtin.named("rational?", args -> {
            requireArgCount("rational?", args, 1);
            Object a = args.get(0);
            return a instanceof Long || a instanceof Rational;
        }));
        env.define("exact->inexact", Builtin.named("exact->inexact", args -> {
            requireArgCount("exact->inexact", args, 1);
            Object a = args.get(0);
            if (a instanceof Long l) return (double) l;
            if (a instanceof Rational r) return r.toDouble();
            if (a instanceof Double) return a;
            throw new EvalError("exact->inexact: expected number");
        }));
        env.define("inexact->exact", Builtin.named("inexact->exact", args -> {
            requireArgCount("inexact->exact", args, 1);
            Object a = args.get(0);
            if (a instanceof Double d) return Rational.fromDouble(d).simplify();
            if (a instanceof Long || a instanceof Rational) return a;
            throw new EvalError("inexact->exact: expected number");
        }));
        env.define("numerator", Builtin.named("numerator", args -> {
            requireArgCount("numerator", args, 1);
            Object a = args.get(0);
            if (a instanceof Long l) return l;
            if (a instanceof Rational r) return r.num;
            throw new EvalError("numerator: expected rational");
        }));
        env.define("denominator", Builtin.named("denominator", args -> {
            requireArgCount("denominator", args, 1);
            Object a = args.get(0);
            if (a instanceof Long) return 1L;
            if (a instanceof Rational r) return r.den;
            throw new EvalError("denominator: expected rational");
        }));

        return env;
    }

    private static String requireString(String name, Object val) throws EvalError {
        if (val instanceof String s && s.startsWith("\"") && s.endsWith("\"")) {
            return s.substring(1, s.length() - 1);
        }
        if (val instanceof MutableString ms) {
            return ms.inner();
        }
        throw new EvalError(name + ": expected string, got: " + SchemeValue.toStr(val));
    }

    private static String displayStr(Object val) {
        if (val == null) return "";
        if (val instanceof String s && s.startsWith("\"") && s.endsWith("\"")) {
            return s.substring(1, s.length() - 1);
        }
        if (val instanceof MutableString ms) {
            return ms.inner();
        }
        return SchemeValue.toStr(val);
    }

    private static long requireLong(String name, Object val) throws EvalError {
        if (val instanceof Long l) return l;
        throw new EvalError(name + ": expected number, got: " + SchemeValue.toStr(val));
    }

    private static boolean isNumber(Object val) {
        return val instanceof Long || val instanceof Double || val instanceof Rational;
    }

    private static Object requireNumber(String name, Object val) throws EvalError {
        if (isNumber(val)) return val;
        throw new EvalError(name + ": expected number, got: " + SchemeValue.toStr(val));
    }

    private static double toDouble(Object n) {
        if (n instanceof Long l) return l;
        if (n instanceof Double d) return d;
        if (n instanceof Rational r) return r.toDouble();
        throw new IllegalArgumentException();
    }

    private static Rational toRational(Object n) {
        if (n instanceof Long l) return Rational.fromLong(l);
        if (n instanceof Rational r) return r;
        throw new IllegalArgumentException();
    }

    private static boolean isInexact(Object n) { return n instanceof Double; }

    private static Object numAdd(Object a, Object b) {
        if (isInexact(a) || isInexact(b)) return toDouble(a) + toDouble(b);
        return toRational(a).add(toRational(b)).simplify();
    }

    private static Object numSub(Object a, Object b) {
        if (isInexact(a) || isInexact(b)) return toDouble(a) - toDouble(b);
        return toRational(a).sub(toRational(b)).simplify();
    }

    private static Object numMul(Object a, Object b) {
        if (isInexact(a) || isInexact(b)) return toDouble(a) * toDouble(b);
        return toRational(a).mul(toRational(b)).simplify();
    }

    private static Object numDiv(Object a, Object b) throws EvalError {
        if (isInexact(a) || isInexact(b)) {
            double d = toDouble(b);
            if (d == 0) throw new EvalError("division by zero");
            return toDouble(a) / d;
        }
        Rational rb = toRational(b);
        if (rb.num == 0) throw new EvalError("division by zero");
        return toRational(a).div(rb).simplify();
    }

    private static Object numNegate(Object a) {
        if (a instanceof Long l) return -l;
        if (a instanceof Double d) return -d;
        if (a instanceof Rational r) return r.negate().simplify();
        throw new IllegalArgumentException();
    }

    private static int numCompare(Object a, Object b) {
        if (isInexact(a) || isInexact(b)) return Double.compare(toDouble(a), toDouble(b));
        return toRational(a).compareTo(toRational(b));
    }

    private static void requireArgCount(String name, List<Object> args, int n) throws EvalError {
        if (args.size() != n)
            throw new EvalError(name + ": expected " + n + " arguments, got " + args.size());
    }

    private static char requireChar(String name, Object val) throws EvalError {
        if (val instanceof Character c) return c;
        throw new EvalError(name + ": expected character, got: " + SchemeValue.toStr(val));
    }

    static boolean schemeEq(Object a, Object b) {
        if (a == b) return true;
        if (a instanceof Long la && b instanceof Long lb) return la.equals(lb);
        if (a instanceof Rational ra && b instanceof Rational rb) return ra.equals(rb);
        if (a instanceof Boolean ba && b instanceof Boolean bb) return ba.equals(bb);
        if (a instanceof Character ca && b instanceof Character cb) return ca.equals(cb);
        if (a instanceof String sa && b instanceof String sb) return sa.equals(sb);
        return false;
    }

    static boolean schemeEqual(Object a, Object b) {
        if (schemeEq(a, b)) return true;
        if (a instanceof Pair pa && b instanceof Pair pb) {
            return schemeEqual(pa.car, pb.car) && schemeEqual(pa.cdr, pb.cdr);
        }
        // Compare strings (quoted strings and MutableStrings)
        String sa = toRawString(a), sb = toRawString(b);
        if (sa != null && sb != null) return sa.equals(sb);
        return false;
    }

    private static String toRawString(Object val) {
        if (val instanceof String s && s.startsWith("\"") && s.endsWith("\""))
            return s.substring(1, s.length() - 1);
        if (val instanceof MutableString ms) return ms.inner();
        return null;
    }
}
