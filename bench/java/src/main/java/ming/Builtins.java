package ming;

import java.util.ArrayList;
import java.util.List;

import ming.Continuations.SchemeContinuation;
import ming.Evaluator.Builtin;
import ming.Evaluator.Env;

import static ming.Evaluator.NIL;
import static ming.Evaluator.VOID;

/**
 * Registers all built-in procedures into a given environment.
 */
final class Builtins {

    private final Env env;
    private final Evaluator evaluator;

    Builtins(Env env, Evaluator evaluator) {
        this.env = env;
        this.evaluator = evaluator;
    }

    void registerAll() {
        registerArithmetic();
        registerComparison();
        registerLogic();
        registerPairs();
        registerLists();
        registerTypePredicates();
        registerIO();
        registerStrings();
        registerApply();
        registerNumericUtils();
        registerChars();
        registerStringComparison();
        registerRationals();
        registerVectors();
    }

    // --- Numeric tower helpers ---

    private boolean isNumber(Object v) {
        return v instanceof Long || v instanceof SchemeRational || v instanceof Double;
    }

    private boolean isExact(Object v) {
        return v instanceof Long || v instanceof SchemeRational;
    }

    private double toDouble(Object v) throws EvalError {
        if (v instanceof Long l) return l;
        if (v instanceof SchemeRational r) return r.toDouble();
        if (v instanceof Double d) return d;
        throw new EvalError("expected number, got: " + schemeToString(v));
    }

    private SchemeRational toRational(Object v) throws EvalError {
        if (v instanceof Long l) return new SchemeRational(l, 1);
        if (v instanceof SchemeRational r) return r;
        throw new EvalError("expected exact number, got: " + schemeToString(v));
    }

    private boolean hasInexact(List<Object> args) {
        for (Object a : args) if (a instanceof Double) return true;
        return false;
    }

    private Object normalizeRational(SchemeRational r) {
        return r.isInteger() ? r.toLong() : r;
    }

    private Object addTwo(Object a, Object b) throws EvalError {
        if (a instanceof Double || b instanceof Double) return toDouble(a) + toDouble(b);
        SchemeRational ra = toRational(a), rb = toRational(b);
        return normalizeRational(new SchemeRational(ra.num * rb.den + rb.num * ra.den, ra.den * rb.den));
    }

    private Object subTwo(Object a, Object b) throws EvalError {
        if (a instanceof Double || b instanceof Double) return toDouble(a) - toDouble(b);
        SchemeRational ra = toRational(a), rb = toRational(b);
        return normalizeRational(new SchemeRational(ra.num * rb.den - rb.num * ra.den, ra.den * rb.den));
    }

    private Object mulTwo(Object a, Object b) throws EvalError {
        if (a instanceof Double || b instanceof Double) return toDouble(a) * toDouble(b);
        SchemeRational ra = toRational(a), rb = toRational(b);
        return normalizeRational(new SchemeRational(ra.num * rb.num, ra.den * rb.den));
    }

    private Object divTwo(Object a, Object b) throws EvalError {
        if (a instanceof Double || b instanceof Double) {
            double db = toDouble(b);
            if (db == 0) throw new EvalError("division by zero");
            return toDouble(a) / db;
        }
        SchemeRational ra = toRational(a), rb = toRational(b);
        if (rb.num == 0) throw new EvalError("division by zero");
        return normalizeRational(new SchemeRational(ra.num * rb.den, ra.den * rb.num));
    }

    private void requireNumber(Object v) throws EvalError {
        if (!isNumber(v)) throw new EvalError("expected number, got: " + schemeToString(v));
    }

    private static long gcdLong(long a, long b) {
        while (b != 0) { long t = b; b = a % b; a = t; }
        return a;
    }

    private void define(String name, Evaluator.BuiltinFn fn) {
        env.define(name, new Builtin(name, fn));
    }

    private long requireLong(Object val) throws EvalError {
        return evaluator.requireLong(val);
    }

    private void requireArgCount(List<Object> args, int n, String name) throws EvalError {
        evaluator.requireArgCount(args, n, name);
    }

    private String schemeToString(Object val) {
        return evaluator.schemeToString(val);
    }

    private String displayString(Object val) {
        return evaluator.displayString(val);
    }

    private boolean schemeEqual(Object a, Object b) {
        return evaluator.schemeEqual(a, b);
    }

    private Object applyProc(Object proc, List<Object> args) throws EvalError {
        return evaluator.applyProc(proc, args, null);
    }

    private void appendOutput(String s) {
        evaluator.appendOutput(s);
    }

    // --- Arithmetic ---

    private void registerArithmetic() {
        define("+", args -> {
            Object result = 0L;
            for (Object a : args) { requireNumber(a); result = addTwo(result, a); }
            return result;
        });
        define("-", args -> {
            if (args.isEmpty()) throw new EvalError("- requires at least 1 argument");
            requireNumber(args.get(0));
            if (args.size() == 1) return subTwo(0L, args.get(0));
            Object result = args.get(0);
            for (int i = 1; i < args.size(); i++) { requireNumber(args.get(i)); result = subTwo(result, args.get(i)); }
            return result;
        });
        define("*", args -> {
            Object result = 1L;
            for (Object a : args) { requireNumber(a); result = mulTwo(result, a); }
            return result;
        });
        define("/", args -> {
            if (args.size() < 2) throw new EvalError("/ requires at least 2 arguments");
            requireNumber(args.get(0));
            Object result = args.get(0);
            for (int i = 1; i < args.size(); i++) { requireNumber(args.get(i)); result = divTwo(result, args.get(i)); }
            return result;
        });
    }

    // --- Comparison ---

    private int numCompare(Object a, Object b) throws EvalError {
        requireNumber(a); requireNumber(b);
        if (a instanceof Double || b instanceof Double) return Double.compare(toDouble(a), toDouble(b));
        SchemeRational ra = toRational(a), rb = toRational(b);
        return Long.compare(ra.num * rb.den, rb.num * ra.den);
    }

    private boolean numEquals(Object a, Object b) throws EvalError {
        requireNumber(a); requireNumber(b);
        if (a instanceof Double || b instanceof Double) return toDouble(a) == toDouble(b);
        SchemeRational ra = toRational(a), rb = toRational(b);
        return ra.num * rb.den == rb.num * ra.den;
    }

    private void registerComparison() {
        define("<", args -> { requireArgCount(args, 2, "<"); return numCompare(args.get(0), args.get(1)) < 0; });
        define(">", args -> { requireArgCount(args, 2, ">"); return numCompare(args.get(0), args.get(1)) > 0; });
        define("=", args -> { requireArgCount(args, 2, "="); return numEquals(args.get(0), args.get(1)); });
        define("<=", args -> { requireArgCount(args, 2, "<="); return numCompare(args.get(0), args.get(1)) <= 0; });
        define(">=", args -> { requireArgCount(args, 2, ">="); return numCompare(args.get(0), args.get(1)) >= 0; });
    }

    // --- Logic ---

    private void registerLogic() {
        define("not", args -> {
            requireArgCount(args, 1, "not");
            return evaluator.isFalse(args.get(0));
        });
        define("error", args -> {
            if (args.isEmpty()) throw new EvalError("error");
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < args.size(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(displayString(args.get(i)));
            }
            throw new EvalError(sb.toString());
        });
    }

    // --- Pairs ---

    private void registerPairs() {
        define("cons", args -> {
            requireArgCount(args, 2, "cons");
            return new Cons(args.get(0), args.get(1));
        });
        define("car", args -> {
            requireArgCount(args, 1, "car");
            if (!(args.get(0) instanceof Cons c)) throw new EvalError("car: not a pair");
            return c.car;
        });
        define("cdr", args -> {
            requireArgCount(args, 1, "cdr");
            if (!(args.get(0) instanceof Cons c)) throw new EvalError("cdr: not a pair");
            return c.cdr;
        });
        define("set-car!", args -> {
            requireArgCount(args, 2, "set-car!");
            if (!(args.get(0) instanceof Cons c)) throw new EvalError("set-car!: not a pair");
            c.car = args.get(1);
            return VOID;
        });
        define("set-cdr!", args -> {
            requireArgCount(args, 2, "set-cdr!");
            if (!(args.get(0) instanceof Cons c)) throw new EvalError("set-cdr!: not a pair");
            c.cdr = args.get(1);
            return VOID;
        });
        define("caar", args -> {
            requireArgCount(args, 1, "caar");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("caar: not a pair");
            if (!(c1.car instanceof Cons c2)) throw new EvalError("caar: not a pair");
            return c2.car;
        });
        define("cadr", args -> {
            requireArgCount(args, 1, "cadr");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("cadr: not a pair");
            if (!(c1.cdr instanceof Cons c2)) throw new EvalError("cadr: not a pair");
            return c2.car;
        });
        define("cdar", args -> {
            requireArgCount(args, 1, "cdar");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("cdar: not a pair");
            if (!(c1.car instanceof Cons c2)) throw new EvalError("cdar: not a pair");
            return c2.cdr;
        });
        define("cddr", args -> {
            requireArgCount(args, 1, "cddr");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("cddr: not a pair");
            if (!(c1.cdr instanceof Cons c2)) throw new EvalError("cddr: not a pair");
            return c2.cdr;
        });
        define("caddr", args -> {
            requireArgCount(args, 1, "caddr");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("caddr: not a pair");
            if (!(c1.cdr instanceof Cons c2)) throw new EvalError("caddr: not a pair");
            if (!(c2.cdr instanceof Cons c3)) throw new EvalError("caddr: not a pair");
            return c3.car;
        });
        define("cdddr", args -> {
            requireArgCount(args, 1, "cdddr");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("cdddr: not a pair");
            if (!(c1.cdr instanceof Cons c2)) throw new EvalError("cdddr: not a pair");
            if (!(c2.cdr instanceof Cons c3)) throw new EvalError("cdddr: not a pair");
            return c3.cdr;
        });
        define("cadddr", args -> {
            requireArgCount(args, 1, "cadddr");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("cadddr: not a pair");
            if (!(c1.cdr instanceof Cons c2)) throw new EvalError("cadddr: not a pair");
            if (!(c2.cdr instanceof Cons c3)) throw new EvalError("cadddr: not a pair");
            if (!(c3.cdr instanceof Cons c4)) throw new EvalError("cadddr: not a pair");
            return c4.car;
        });
        define("cddddr", args -> {
            requireArgCount(args, 1, "cddddr");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("cddddr: not a pair");
            if (!(c1.cdr instanceof Cons c2)) throw new EvalError("cddddr: not a pair");
            if (!(c2.cdr instanceof Cons c3)) throw new EvalError("cddddr: not a pair");
            if (!(c3.cdr instanceof Cons c4)) throw new EvalError("cddddr: not a pair");
            return c4.cdr;
        });
        define("caaar", args -> {
            requireArgCount(args, 1, "caaar");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("caaar: not a pair");
            if (!(c1.car instanceof Cons c2)) throw new EvalError("caaar: not a pair");
            if (!(c2.car instanceof Cons c3)) throw new EvalError("caaar: not a pair");
            return c3.car;
        });
        define("caadr", args -> {
            requireArgCount(args, 1, "caadr");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("caadr: not a pair");
            if (!(c1.cdr instanceof Cons c2)) throw new EvalError("caadr: not a pair");
            if (!(c2.car instanceof Cons c3)) throw new EvalError("caadr: not a pair");
            return c3.car;
        });
        define("cadar", args -> {
            requireArgCount(args, 1, "cadar");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("cadar: not a pair");
            if (!(c1.car instanceof Cons c2)) throw new EvalError("cadar: not a pair");
            if (!(c2.cdr instanceof Cons c3)) throw new EvalError("cadar: not a pair");
            return c3.car;
        });
        define("cadaar", args -> {
            requireArgCount(args, 1, "cadaar");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("cadaar: not a pair");
            if (!(c1.car instanceof Cons c2)) throw new EvalError("cadaar: not a pair");
            if (!(c2.car instanceof Cons c3)) throw new EvalError("cadaar: not a pair");
            if (!(c3.cdr instanceof Cons c4)) throw new EvalError("cadaar: not a pair");
            return c4.car;
        });
        define("cadadr", args -> {
            requireArgCount(args, 1, "cadadr");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("cadadr: not a pair");
            if (!(c1.cdr instanceof Cons c2)) throw new EvalError("cadadr: not a pair");
            if (!(c2.car instanceof Cons c3)) throw new EvalError("cadadr: not a pair");
            if (!(c3.cdr instanceof Cons c4)) throw new EvalError("cadadr: not a pair");
            return c4.car;
        });
        define("caddar", args -> {
            requireArgCount(args, 1, "caddar");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("caddar: not a pair");
            if (!(c1.car instanceof Cons c2)) throw new EvalError("caddar: not a pair");
            if (!(c2.cdr instanceof Cons c3)) throw new EvalError("caddar: not a pair");
            if (!(c3.cdr instanceof Cons c4)) throw new EvalError("caddar: not a pair");
            return c4.car;
        });
        define("cdaar", args -> {
            requireArgCount(args, 1, "cdaar");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("cdaar: not a pair");
            if (!(c1.car instanceof Cons c2)) throw new EvalError("cdaar: not a pair");
            if (!(c2.car instanceof Cons c3)) throw new EvalError("cdaar: not a pair");
            return c3.cdr;
        });
        define("cdadr", args -> {
            requireArgCount(args, 1, "cdadr");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("cdadr: not a pair");
            if (!(c1.cdr instanceof Cons c2)) throw new EvalError("cdadr: not a pair");
            if (!(c2.car instanceof Cons c3)) throw new EvalError("cdadr: not a pair");
            return c3.cdr;
        });
        define("cddar", args -> {
            requireArgCount(args, 1, "cddar");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("cddar: not a pair");
            if (!(c1.car instanceof Cons c2)) throw new EvalError("cddar: not a pair");
            if (!(c2.cdr instanceof Cons c3)) throw new EvalError("cddar: not a pair");
            return c3.cdr;
        });
        define("caaaar", args -> {
            requireArgCount(args, 1, "caaaar");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("caaaar: not a pair");
            if (!(c1.car instanceof Cons c2)) throw new EvalError("caaaar: not a pair");
            if (!(c2.car instanceof Cons c3)) throw new EvalError("caaaar: not a pair");
            if (!(c3.car instanceof Cons c4)) throw new EvalError("caaaar: not a pair");
            return c4.car;
        });
        define("caaadr", args -> {
            requireArgCount(args, 1, "caaadr");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("caaadr: not a pair");
            if (!(c1.cdr instanceof Cons c2)) throw new EvalError("caaadr: not a pair");
            if (!(c2.car instanceof Cons c3)) throw new EvalError("caaadr: not a pair");
            if (!(c3.car instanceof Cons c4)) throw new EvalError("caaadr: not a pair");
            return c4.car;
        });
        define("caadar", args -> {
            requireArgCount(args, 1, "caadar");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("caadar: not a pair");
            if (!(c1.car instanceof Cons c2)) throw new EvalError("caadar: not a pair");
            if (!(c2.cdr instanceof Cons c3)) throw new EvalError("caadar: not a pair");
            if (!(c3.car instanceof Cons c4)) throw new EvalError("caadar: not a pair");
            return c4.car;
        });
        define("caaddr", args -> {
            requireArgCount(args, 1, "caaddr");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("caaddr: not a pair");
            if (!(c1.cdr instanceof Cons c2)) throw new EvalError("caaddr: not a pair");
            if (!(c2.cdr instanceof Cons c3)) throw new EvalError("caaddr: not a pair");
            if (!(c3.car instanceof Cons c4)) throw new EvalError("caaddr: not a pair");
            return c4.car;
        });
        define("cdaaar", args -> {
            requireArgCount(args, 1, "cdaaar");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("cdaaar: not a pair");
            if (!(c1.car instanceof Cons c2)) throw new EvalError("cdaaar: not a pair");
            if (!(c2.car instanceof Cons c3)) throw new EvalError("cdaaar: not a pair");
            if (!(c3.car instanceof Cons c4)) throw new EvalError("cdaaar: not a pair");
            return c4.cdr;
        });
        define("cdaadr", args -> {
            requireArgCount(args, 1, "cdaadr");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("cdaadr: not a pair");
            if (!(c1.cdr instanceof Cons c2)) throw new EvalError("cdaadr: not a pair");
            if (!(c2.car instanceof Cons c3)) throw new EvalError("cdaadr: not a pair");
            if (!(c3.car instanceof Cons c4)) throw new EvalError("cdaadr: not a pair");
            return c4.cdr;
        });
        define("cdadar", args -> {
            requireArgCount(args, 1, "cdadar");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("cdadar: not a pair");
            if (!(c1.car instanceof Cons c2)) throw new EvalError("cdadar: not a pair");
            if (!(c2.cdr instanceof Cons c3)) throw new EvalError("cdadar: not a pair");
            if (!(c3.car instanceof Cons c4)) throw new EvalError("cdadar: not a pair");
            return c4.cdr;
        });
        define("cdaddr", args -> {
            requireArgCount(args, 1, "cdaddr");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("cdaddr: not a pair");
            if (!(c1.cdr instanceof Cons c2)) throw new EvalError("cdaddr: not a pair");
            if (!(c2.cdr instanceof Cons c3)) throw new EvalError("cdaddr: not a pair");
            if (!(c3.car instanceof Cons c4)) throw new EvalError("cdaddr: not a pair");
            return c4.cdr;
        });
        define("cddaar", args -> {
            requireArgCount(args, 1, "cddaar");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("cddaar: not a pair");
            if (!(c1.car instanceof Cons c2)) throw new EvalError("cddaar: not a pair");
            if (!(c2.car instanceof Cons c3)) throw new EvalError("cddaar: not a pair");
            if (!(c3.cdr instanceof Cons c4)) throw new EvalError("cddaar: not a pair");
            return c4.cdr;
        });
        define("cddadr", args -> {
            requireArgCount(args, 1, "cddadr");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("cddadr: not a pair");
            if (!(c1.cdr instanceof Cons c2)) throw new EvalError("cddadr: not a pair");
            if (!(c2.car instanceof Cons c3)) throw new EvalError("cddadr: not a pair");
            if (!(c3.cdr instanceof Cons c4)) throw new EvalError("cddadr: not a pair");
            return c4.cdr;
        });
        define("cdddar", args -> {
            requireArgCount(args, 1, "cdddar");
            if (!(args.get(0) instanceof Cons c1)) throw new EvalError("cdddar: not a pair");
            if (!(c1.car instanceof Cons c2)) throw new EvalError("cdddar: not a pair");
            if (!(c2.cdr instanceof Cons c3)) throw new EvalError("cdddar: not a pair");
            if (!(c3.cdr instanceof Cons c4)) throw new EvalError("cdddar: not a pair");
            return c4.cdr;
        });
    }

    // --- Lists ---

    private void registerLists() {
        define("null?", args -> {
            requireArgCount(args, 1, "null?");
            return args.get(0) == NIL;
        });
        define("list", args -> {
            Object result = NIL;
            for (int i = args.size() - 1; i >= 0; i--) {
                result = new Cons(args.get(i), result);
            }
            return result;
        });
        define("length", args -> {
            requireArgCount(args, 1, "length");
            long count = 0;
            Object slow = args.get(0), fast = args.get(0);
            while (fast instanceof Cons cf) {
                fast = cf.cdr;
                count++;
                if (!(fast instanceof Cons cf2)) break;
                fast = cf2.cdr;
                count++;
                slow = ((Cons) slow).cdr;
                if (slow == fast) throw new EvalError("length: not a proper list");
            }
            if (fast != NIL) throw new EvalError("length: not a proper list");
            return count;
        });
        define("append", args -> {
            if (args.isEmpty()) return NIL;
            if (args.size() == 1) return args.get(0);
            Object result = args.get(args.size() - 1);
            for (int i = args.size() - 2; i >= 0; i--) {
                List<Object> elems = new ArrayList<>();
                Object cur = args.get(i);
                while (cur instanceof Cons c) { elems.add(c.car); cur = c.cdr; }
                for (int j = elems.size() - 1; j >= 0; j--) {
                    result = new Cons(elems.get(j), result);
                }
            }
            return result;
        });
        define("list-ref", args -> {
            requireArgCount(args, 2, "list-ref");
            int idx = (int) requireLong(args.get(1));
            Object cur = args.get(0);
            for (int i = 0; i < idx; i++) {
                if (!(cur instanceof Cons c)) throw new EvalError("list-ref: index out of range");
                cur = c.cdr;
            }
            if (!(cur instanceof Cons c)) throw new EvalError("list-ref: index out of range");
            return c.car;
        });
        define("list-tail", args -> {
            requireArgCount(args, 2, "list-tail");
            int idx = (int) requireLong(args.get(1));
            Object cur = args.get(0);
            for (int i = 0; i < idx; i++) {
                if (!(cur instanceof Cons c)) throw new EvalError("list-tail: index out of range");
                cur = c.cdr;
            }
            return cur;
        });
        define("list?", args -> {
            requireArgCount(args, 1, "list?");
            Object slow = args.get(0), fast = args.get(0);
            while (fast instanceof Cons cf) {
                fast = cf.cdr;
                if (!(fast instanceof Cons cf2)) return fast == NIL;
                fast = cf2.cdr;
                slow = ((Cons) slow).cdr;
                if (slow == fast) return false; // cycle detected
            }
            return fast == NIL;
        });
        define("eq?", args -> {
            requireArgCount(args, 2, "eq?");
            Object a = args.get(0), b = args.get(1);
            if (a instanceof Long la && b instanceof Long lb) return la.equals(lb);
            if (a instanceof Boolean ba && b instanceof Boolean bb) return ba.equals(bb);
            if (a instanceof String sa && b instanceof String sb) return sa.equals(sb);
            return a == b;
        });
        define("eqv?", args -> {
            requireArgCount(args, 2, "eqv?");
            return evaluator.schemeEqv(args.get(0), args.get(1));
        });
        define("equal?", args -> {
            requireArgCount(args, 2, "equal?");
            return schemeEqual(args.get(0), args.get(1));
        });
        define("memq", args -> {
            requireArgCount(args, 2, "memq");
            Object obj = args.get(0);
            Object lst = args.get(1);
            while (lst instanceof Cons c) {
                if (obj == c.car || (obj instanceof Long la && c.car instanceof Long lb && la.equals(lb))
                    || (obj instanceof Boolean ba && c.car instanceof Boolean bb && ba.equals(bb))
                    || (obj instanceof String sa && c.car instanceof String sb && sa.equals(sb)))
                    return lst;
                lst = c.cdr;
            }
            return Boolean.FALSE;
        });
        define("memv", args -> {
            requireArgCount(args, 2, "memv");
            Object obj = args.get(0);
            Object lst = args.get(1);
            while (lst instanceof Cons c) {
                if (evaluator.schemeEqv(obj, c.car)) return lst;
                lst = c.cdr;
            }
            return Boolean.FALSE;
        });
        define("member", args -> {
            requireArgCount(args, 2, "member");
            Object obj = args.get(0);
            Object lst = args.get(1);
            while (lst instanceof Cons c) {
                if (schemeEqual(obj, c.car)) return lst;
                lst = c.cdr;
            }
            return Boolean.FALSE;
        });
        define("assq", args -> {
            requireArgCount(args, 2, "assq");
            Object key = args.get(0);
            Object lst = args.get(1);
            while (lst instanceof Cons c) {
                if (c.car instanceof Cons pair) {
                    if (key == pair.car || (key instanceof Long la && pair.car instanceof Long lb && la.equals(lb))
                        || (key instanceof Boolean ba && pair.car instanceof Boolean bb && ba.equals(bb))
                        || (key instanceof String sa && pair.car instanceof String sb && sa.equals(sb)))
                        return c.car;
                }
                lst = c.cdr;
            }
            return Boolean.FALSE;
        });
        define("assv", args -> {
            requireArgCount(args, 2, "assv");
            Object key = args.get(0);
            Object lst = args.get(1);
            while (lst instanceof Cons c) {
                if (c.car instanceof Cons pair && evaluator.schemeEqv(key, pair.car)) return c.car;
                lst = c.cdr;
            }
            return Boolean.FALSE;
        });
        define("assoc", args -> {
            requireArgCount(args, 2, "assoc");
            Object key = args.get(0);
            Object lst = args.get(1);
            while (lst instanceof Cons c) {
                if (c.car instanceof Cons pair && schemeEqual(key, pair.car)) return c.car;
                lst = c.cdr;
            }
            return Boolean.FALSE;
        });
        // map is intercepted by the CEK machine
        define("map", args -> { throw new EvalError("map: should be handled by CEK machine"); });
        define("reverse", args -> {
            requireArgCount(args, 1, "reverse");
            Object result = NIL;
            Object cur = args.get(0);
            while (cur instanceof Cons c) { result = new Cons(c.car, result); cur = c.cdr; }
            return result;
        });
        // for-each is intercepted by the CEK machine
        define("for-each", args -> { throw new EvalError("for-each: should be handled by CEK machine"); });
    }

    // --- Type predicates ---

    private void registerTypePredicates() {
        define("number?", args -> { requireArgCount(args, 1, "number?"); return isNumber(args.get(0)); });
        define("string?", args -> { requireArgCount(args, 1, "string?"); return args.get(0) instanceof SchemeString; });
        define("boolean?", args -> { requireArgCount(args, 1, "boolean?"); return args.get(0) instanceof Boolean; });
        define("pair?", args -> { requireArgCount(args, 1, "pair?"); return args.get(0) instanceof Cons; });
        define("symbol?", args -> { requireArgCount(args, 1, "symbol?"); return args.get(0) instanceof String; });
        define("char?", args -> { requireArgCount(args, 1, "char?"); return args.get(0) instanceof SchemeChar; });
        define("vector?", args -> { requireArgCount(args, 1, "vector?"); return args.get(0) instanceof SchemeVector; });
        define("procedure?", args -> { requireArgCount(args, 1, "procedure?"); Object a = args.get(0); return a instanceof Evaluator.Lambda || a instanceof Evaluator.CaseLambda || a instanceof Evaluator.Builtin || a instanceof SchemeContinuation; });
    }

    // --- I/O ---

    private void registerIO() {
        define("display", args -> {
            requireArgCount(args, 1, "display");
            appendOutput(displayString(args.get(0)));
            return VOID;
        });
        define("write", args -> {
            requireArgCount(args, 1, "write");
            appendOutput(schemeToString(args.get(0)));
            return VOID;
        });
        define("newline", args -> {
            requireArgCount(args, 0, "newline");
            appendOutput("\n");
            return VOID;
        });
    }

    // --- Strings ---

    private void registerStrings() {
        define("make-string", args -> {
            if (args.isEmpty() || args.size() > 2) throw new EvalError("make-string requires 1 or 2 arguments");
            int len = (int) requireLong(args.get(0));
            char fill = args.size() >= 2 && args.get(1) instanceof SchemeChar sc ? sc.value() : ' ';
            char[] chars = new char[len];
            java.util.Arrays.fill(chars, fill);
            return new SchemeString(chars);
        });
        define("string", args -> {
            char[] chars = new char[args.size()];
            for (int i = 0; i < args.size(); i++) {
                if (!(args.get(i) instanceof SchemeChar sc)) throw new EvalError("string: not a character");
                chars[i] = sc.value();
            }
            return new SchemeString(chars);
        });
        define("string-append", args -> {
            StringBuilder sb = new StringBuilder();
            for (Object a : args) {
                if (!(a instanceof SchemeString s)) throw new EvalError("string-append: not a string");
                sb.append(s.value());
            }
            return new SchemeString(sb.toString());
        });
        define("string-length", args -> {
            requireArgCount(args, 1, "string-length");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-length: not a string");
            return (long) s.value().length();
        });
        define("substring", args -> {
            requireArgCount(args, 3, "substring");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("substring: not a string");
            int start = (int) requireLong(args.get(1));
            int end = (int) requireLong(args.get(2));
            return new SchemeString(s.value().substring(start, end));
        });
        define("string->number", args -> {
            requireArgCount(args, 1, "string->number");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string->number: not a string");
            try { return Long.parseLong(s.value()); }
            catch (NumberFormatException e) { return Boolean.FALSE; }
        });
        define("number->string", args -> {
            requireArgCount(args, 1, "number->string");
            Object v = args.get(0);
            if (v instanceof Long l) return new SchemeString(l.toString());
            if (v instanceof SchemeRational r) return new SchemeString(r.isInteger() ? String.valueOf(r.toLong()) : r.num + "/" + r.den);
            if (v instanceof Double d) return new SchemeString(String.valueOf(d));
            throw new EvalError("number->string: expected number");
        });
        define("string-ref", args -> {
            requireArgCount(args, 2, "string-ref");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-ref: not a string");
            int idx = (int) requireLong(args.get(1));
            return new SchemeChar(s.value().charAt(idx));
        });
        define("symbol->string", args -> {
            requireArgCount(args, 1, "symbol->string");
            if (!(args.get(0) instanceof String s)) throw new EvalError("symbol->string: not a symbol");
            return new SchemeString(s);
        });
        define("string->symbol", args -> {
            requireArgCount(args, 1, "string->symbol");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string->symbol: not a string");
            return s.value();
        });
        define("string-copy", args -> {
            requireArgCount(args, 1, "string-copy");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-copy: not a string");
            return new SchemeString(s.value().toCharArray());
        });
        define("string-set!", args -> {
            requireArgCount(args, 3, "string-set!");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-set!: not a string");
            if (!(args.get(1) instanceof Long idx)) throw new EvalError("string-set!: not an integer");
            if (!(args.get(2) instanceof SchemeChar c)) throw new EvalError("string-set!: not a character");
            int i = idx.intValue();
            if (i < 0 || i >= s.length()) throw new EvalError("string-set!: index out of range");
            s.setChar(i, c.value());
            return null;
        });
        define("string->list", args -> {
            requireArgCount(args, 1, "string->list");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string->list: not a string");
            Object result = NIL;
            for (int i = s.length() - 1; i >= 0; i--) {
                result = new Cons(new SchemeChar(s.charAt(i)), result);
            }
            return result;
        });
        define("list->string", args -> {
            requireArgCount(args, 1, "list->string");
            List<Character> chars = new ArrayList<>();
            Object cur = args.get(0);
            while (cur instanceof Cons c) {
                if (!(c.car instanceof SchemeChar sc)) throw new EvalError("list->string: not a character");
                chars.add(sc.value());
                cur = c.cdr;
            }
            char[] arr = new char[chars.size()];
            for (int i = 0; i < chars.size(); i++) arr[i] = chars.get(i);
            return new SchemeString(arr);
        });
        define("char->integer", args -> {
            requireArgCount(args, 1, "char->integer");
            if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError("char->integer: not a character");
            return (long) c.value();
        });
        define("integer->char", args -> {
            requireArgCount(args, 1, "integer->char");
            long n = requireLong(args.get(0));
            return new SchemeChar((char) n);
        });
    }

    // --- Apply & call/cc ---

    private void registerApply() {
        // apply, map, for-each are intercepted by the CEK machine but need to be registered as builtins
        define("apply", args -> { throw new EvalError("apply: should be handled by CEK machine"); });
        // call/cc and call-with-current-continuation are handled by the CEK machine
        define("call/cc", args -> { throw new EvalError("call/cc: should be handled by CEK machine"); });
        define("call-with-current-continuation", args -> { throw new EvalError("call-with-current-continuation: should be handled by CEK machine"); });
        define("dynamic-wind", args -> { throw new EvalError("dynamic-wind: should be handled by CEK machine"); });
        define("raise", args -> { throw new EvalError("raise: should be handled by CEK machine"); });
        define("with-exception-handler", args -> { throw new EvalError("with-exception-handler: should be handled by CEK machine"); });
    }

    // --- Numeric utilities (L09) ---

    private void registerNumericUtils() {
        define("abs", args -> {
            requireArgCount(args, 1, "abs");
            Object v = args.get(0);
            if (v instanceof Long l) return Math.abs(l);
            if (v instanceof SchemeRational r) return normalizeRational(new SchemeRational(Math.abs(r.num), r.den));
            if (v instanceof Double d) return Math.abs(d);
            throw new EvalError("abs: expected number");
        });
        define("modulo", args -> {
            requireArgCount(args, 2, "modulo");
            long a = requireLong(args.get(0)), b = requireLong(args.get(1));
            if (b == 0) throw new EvalError("modulo: division by zero");
            return Math.floorMod(a, b);
        });
        define("remainder", args -> {
            requireArgCount(args, 2, "remainder");
            long a = requireLong(args.get(0)), b = requireLong(args.get(1));
            if (b == 0) throw new EvalError("remainder: division by zero");
            return a % b;
        });
        define("quotient", args -> {
            requireArgCount(args, 2, "quotient");
            long a = requireLong(args.get(0)), b = requireLong(args.get(1));
            if (b == 0) throw new EvalError("quotient: division by zero");
            return a / b;
        });
        define("min", args -> {
            if (args.isEmpty()) throw new EvalError("min requires at least 1 argument");
            Object result = args.get(0); requireNumber(result);
            for (int i = 1; i < args.size(); i++) { requireNumber(args.get(i)); if (numCompare(args.get(i), result) < 0) result = args.get(i); }
            return result;
        });
        define("max", args -> {
            if (args.isEmpty()) throw new EvalError("max requires at least 1 argument");
            Object result = args.get(0); requireNumber(result);
            for (int i = 1; i < args.size(); i++) { requireNumber(args.get(i)); if (numCompare(args.get(i), result) > 0) result = args.get(i); }
            return result;
        });
        define("expt", args -> {
            requireArgCount(args, 2, "expt");
            long base = requireLong(args.get(0)), exp = requireLong(args.get(1));
            long result = 1;
            for (long i = 0; i < exp; i++) result *= base;
            return result;
        });
        define("gcd", args -> {
            if (args.isEmpty()) return 0L;
            long result = Math.abs(requireLong(args.get(0)));
            for (int i = 1; i < args.size(); i++) {
                long b = Math.abs(requireLong(args.get(i)));
                while (b != 0) { long t = b; b = result % b; result = t; }
            }
            return result;
        });
        define("lcm", args -> {
            if (args.isEmpty()) return 1L;
            long result = Math.abs(requireLong(args.get(0)));
            for (int i = 1; i < args.size(); i++) {
                long b = Math.abs(requireLong(args.get(i)));
                if (result == 0 && b == 0) { result = 0; continue; }
                result = result / gcdLong(result, b) * b;
            }
            return result;
        });
        define("truncate", args -> {
            requireArgCount(args, 1, "truncate");
            Object v = args.get(0);
            if (v instanceof Long) return v;
            if (v instanceof Double d) return (long)(double) d;
            if (v instanceof SchemeRational r) return r.num / r.den;
            throw new EvalError("truncate: expected number");
        });
        define("round", args -> {
            requireArgCount(args, 1, "round");
            Object v = args.get(0);
            if (v instanceof Long) return v;
            if (v instanceof Double d) return Math.round(d);
            if (v instanceof SchemeRational r) {
                long q = r.num / r.den;
                long rem = Math.abs(r.num % r.den);
                long halfDen = r.den / 2;
                if (rem > halfDen || (rem == halfDen && r.den % 2 == 0 && q % 2 != 0)) {
                    return r.num > 0 ? q + 1 : q - 1;
                }
                return q;
            }
            throw new EvalError("round: expected number");
        });
        define("zero?", args -> { requireArgCount(args, 1, "zero?"); requireNumber(args.get(0)); return numEquals(args.get(0), 0L); });
        define("positive?", args -> { requireArgCount(args, 1, "positive?"); requireNumber(args.get(0)); return numCompare(args.get(0), 0L) > 0; });
        define("negative?", args -> { requireArgCount(args, 1, "negative?"); requireNumber(args.get(0)); return numCompare(args.get(0), 0L) < 0; });
        define("odd?", args -> { requireArgCount(args, 1, "odd?"); return requireLong(args.get(0)) % 2 != 0; });
        define("even?", args -> { requireArgCount(args, 1, "even?"); return requireLong(args.get(0)) % 2 == 0; });
    }

    // --- Char operations ---

    private void registerChars() {
        define("char-alphabetic?", args -> {
            requireArgCount(args, 1, "char-alphabetic?");
            if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError("char-alphabetic?: not a char");
            return Character.isLetter(c.value());
        });
        define("char-numeric?", args -> {
            requireArgCount(args, 1, "char-numeric?");
            if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError("char-numeric?: not a char");
            return Character.isDigit(c.value());
        });
        define("char-upcase", args -> {
            requireArgCount(args, 1, "char-upcase");
            if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError("char-upcase: not a char");
            return new SchemeChar(Character.toUpperCase(c.value()));
        });
        define("char-downcase", args -> {
            requireArgCount(args, 1, "char-downcase");
            if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError("char-downcase: not a char");
            return new SchemeChar(Character.toLowerCase(c.value()));
        });
        define("char=?", args -> {
            requireArgCount(args, 2, "char=?");
            if (!(args.get(0) instanceof SchemeChar a) || !(args.get(1) instanceof SchemeChar b))
                throw new EvalError("char=?: not a char");
            return a.value() == b.value();
        });
        define("char<?", args -> {
            requireArgCount(args, 2, "char<?");
            if (!(args.get(0) instanceof SchemeChar a) || !(args.get(1) instanceof SchemeChar b))
                throw new EvalError("char<?: not a char");
            return a.value() < b.value();
        });
    }

    // --- String comparison and case ---

    private void registerStringComparison() {
        define("string=?", args -> {
            requireArgCount(args, 2, "string=?");
            if (!(args.get(0) instanceof SchemeString a) || !(args.get(1) instanceof SchemeString b))
                throw new EvalError("string=?: not a string");
            return a.value().equals(b.value());
        });
        define("string<?", args -> {
            requireArgCount(args, 2, "string<?");
            if (!(args.get(0) instanceof SchemeString a) || !(args.get(1) instanceof SchemeString b))
                throw new EvalError("string<?: not a string");
            return a.value().compareTo(b.value()) < 0;
        });
        define("string>?", args -> {
            requireArgCount(args, 2, "string>?");
            if (!(args.get(0) instanceof SchemeString a) || !(args.get(1) instanceof SchemeString b))
                throw new EvalError("string>?: not a string");
            return a.value().compareTo(b.value()) > 0;
        });
        define("string<=?", args -> {
            requireArgCount(args, 2, "string<=?");
            if (!(args.get(0) instanceof SchemeString a) || !(args.get(1) instanceof SchemeString b))
                throw new EvalError("string<=?: not a string");
            return a.value().compareTo(b.value()) <= 0;
        });
        define("string>=?", args -> {
            requireArgCount(args, 2, "string>=?");
            if (!(args.get(0) instanceof SchemeString a) || !(args.get(1) instanceof SchemeString b))
                throw new EvalError("string>=?: not a string");
            return a.value().compareTo(b.value()) >= 0;
        });
        define("string-ci=?", args -> {
            requireArgCount(args, 2, "string-ci=?");
            if (!(args.get(0) instanceof SchemeString a) || !(args.get(1) instanceof SchemeString b))
                throw new EvalError("string-ci=?: not a string");
            return a.value().equalsIgnoreCase(b.value());
        });
        define("string-upcase", args -> {
            requireArgCount(args, 1, "string-upcase");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-upcase: not a string");
            return new SchemeString(s.value().toUpperCase());
        });
        define("string-downcase", args -> {
            requireArgCount(args, 1, "string-downcase");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-downcase: not a string");
            return new SchemeString(s.value().toLowerCase());
        });
    }

    // --- Rationals (L11) ---

    private void registerRationals() {
        define("exact?", args -> { requireArgCount(args, 1, "exact?"); return isExact(args.get(0)); });
        define("inexact?", args -> { requireArgCount(args, 1, "inexact?"); return args.get(0) instanceof Double; });
        define("exact->inexact", args -> {
            requireArgCount(args, 1, "exact->inexact");
            return toDouble(args.get(0));
        });
        define("inexact->exact", args -> {
            requireArgCount(args, 1, "inexact->exact");
            Object v = args.get(0);
            if (v instanceof Long) return v;
            if (v instanceof SchemeRational) return v;
            if (v instanceof Double d) {
                // Convert double to rational
                if (d == Math.floor(d) && !Double.isInfinite(d)) return (long)(double) d;
                // Use fraction approximation: multiply to remove decimal
                long bits = Double.doubleToLongBits(d);
                int exp = (int)((bits >> 52) & 0x7FFL) - 1023 - 52;
                long mantissa = (bits & 0x000FFFFFFFFFFFFFL) | 0x0010000000000000L;
                if ((bits >> 63) != 0) mantissa = -mantissa;
                if (exp >= 0) return mantissa * (1L << exp);
                return normalizeRational(new SchemeRational(mantissa, 1L << (-exp)));
            }
            throw new EvalError("inexact->exact: expected number");
        });
        define("numerator", args -> {
            requireArgCount(args, 1, "numerator");
            Object v = args.get(0);
            if (v instanceof Long l) return l;
            if (v instanceof SchemeRational r) return r.num;
            throw new EvalError("numerator: expected exact number");
        });
        define("denominator", args -> {
            requireArgCount(args, 1, "denominator");
            Object v = args.get(0);
            if (v instanceof Long) return 1L;
            if (v instanceof SchemeRational r) return r.den;
            throw new EvalError("denominator: expected exact number");
        });
        define("integer?", args -> {
            requireArgCount(args, 1, "integer?");
            Object v = args.get(0);
            if (v instanceof Long) return true;
            if (v instanceof SchemeRational r) return r.isInteger();
            if (v instanceof Double d) return d == Math.floor(d) && !Double.isInfinite(d);
            return false;
        });
        define("rational?", args -> {
            requireArgCount(args, 1, "rational?");
            return isExact(args.get(0));
        });
    }

    // --- Vectors (L14) ---

    private void registerVectors() {
        define("vector", args -> {
            return new SchemeVector(args.toArray());
        });
        define("make-vector", args -> {
            if (args.size() < 1 || args.size() > 2) throw new EvalError("make-vector requires 1 or 2 arguments");
            int len = (int) requireLong(args.get(0));
            Object fill = args.size() >= 2 ? args.get(1) : 0L;
            Object[] data = new Object[len];
            java.util.Arrays.fill(data, fill);
            return new SchemeVector(data);
        });
        define("vector-ref", args -> {
            requireArgCount(args, 2, "vector-ref");
            if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector-ref: not a vector");
            int idx = (int) requireLong(args.get(1));
            if (idx < 0 || idx >= v.length()) throw new EvalError("vector-ref: index out of range");
            return v.ref(idx);
        });
        define("vector-set!", args -> {
            requireArgCount(args, 3, "vector-set!");
            if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector-set!: not a vector");
            int idx = (int) requireLong(args.get(1));
            if (idx < 0 || idx >= v.length()) throw new EvalError("vector-set!: index out of range");
            v.set(idx, args.get(2));
            return VOID;
        });
        define("vector-length", args -> {
            requireArgCount(args, 1, "vector-length");
            if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector-length: not a vector");
            return (long) v.length();
        });
        define("vector->list", args -> {
            requireArgCount(args, 1, "vector->list");
            if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector->list: not a vector");
            Object result = NIL;
            for (int i = v.length() - 1; i >= 0; i--) {
                result = new Cons(v.data[i], result);
            }
            return result;
        });
        define("list->vector", args -> {
            requireArgCount(args, 1, "list->vector");
            List<Object> elems = new ArrayList<>();
            Object cur = args.get(0);
            while (cur instanceof Cons c) { elems.add(c.car); cur = c.cdr; }
            return new SchemeVector(elems.toArray());
        });
    }
}
