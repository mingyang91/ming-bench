package ming;

import java.util.ArrayList;
import java.util.List;

import ming.Evaluator.Builtin;
import ming.Evaluator.Cons;
import ming.Evaluator.Env;
import ming.Evaluator.SchemeChar;
import ming.Evaluator.SchemeRational;
import ming.Evaluator.SchemeString;

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
            Object cur = args.get(0);
            while (cur instanceof Cons c) { count++; cur = c.cdr; }
            if (cur != NIL) throw new EvalError("length: not a proper list");
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
            Object cur = args.get(0);
            while (cur instanceof Cons c) { cur = c.cdr; }
            return cur == NIL;
        });
        define("eq?", args -> {
            requireArgCount(args, 2, "eq?");
            Object a = args.get(0), b = args.get(1);
            if (a instanceof Long la && b instanceof Long lb) return la.equals(lb);
            if (a instanceof Boolean ba && b instanceof Boolean bb) return ba.equals(bb);
            if (a instanceof String sa && b instanceof String sb) return sa.equals(sb);
            return a == b;
        });
        define("equal?", args -> {
            requireArgCount(args, 2, "equal?");
            return schemeEqual(args.get(0), args.get(1));
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
        define("map", args -> {
            if (args.size() < 2) throw new EvalError("map requires at least 2 arguments");
            Object proc = args.get(0);
            int numLists = args.size() - 1;
            Object[] cursors = new Object[numLists];
            for (int i = 0; i < numLists; i++) cursors[i] = args.get(i + 1);
            List<Object> results = new ArrayList<>();
            while (true) {
                boolean done = false;
                for (int i = 0; i < numLists; i++) {
                    if (!(cursors[i] instanceof Cons)) { done = true; break; }
                }
                if (done) break;
                List<Object> callArgs = new ArrayList<>();
                for (int i = 0; i < numLists; i++) {
                    callArgs.add(((Cons) cursors[i]).car);
                    cursors[i] = ((Cons) cursors[i]).cdr;
                }
                results.add(applyProc(proc, callArgs));
            }
            Object result = NIL;
            for (int i = results.size() - 1; i >= 0; i--) result = new Cons(results.get(i), result);
            return result;
        });
    }

    // --- Type predicates ---

    private void registerTypePredicates() {
        define("number?", args -> { requireArgCount(args, 1, "number?"); return isNumber(args.get(0)); });
        define("string?", args -> { requireArgCount(args, 1, "string?"); return args.get(0) instanceof SchemeString; });
        define("boolean?", args -> { requireArgCount(args, 1, "boolean?"); return args.get(0) instanceof Boolean; });
        define("pair?", args -> { requireArgCount(args, 1, "pair?"); return args.get(0) instanceof Cons; });
        define("symbol?", args -> { requireArgCount(args, 1, "symbol?"); return args.get(0) instanceof String; });
        define("char?", args -> { requireArgCount(args, 1, "char?"); return args.get(0) instanceof SchemeChar; });
        define("procedure?", args -> { requireArgCount(args, 1, "procedure?"); Object a = args.get(0); return a instanceof Evaluator.Lambda || a instanceof Evaluator.CaseLambda || a instanceof Evaluator.Builtin; });
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
            int idx = (int) requireLong(args.get(1));
            if (!(args.get(2) instanceof SchemeChar c)) throw new EvalError("string-set!: not a character");
            s.setChar(idx, c.value());
            return VOID;
        });
    }

    // --- Apply ---

    private void registerApply() {
        define("apply", args -> {
            if (args.size() < 2) throw new EvalError("apply requires at least 2 arguments");
            Object proc = args.get(0);
            Object lastArg = args.get(args.size() - 1);
            List<Object> callArgs = new ArrayList<>();
            for (int i = 1; i < args.size() - 1; i++) callArgs.add(args.get(i));
            Object cur = lastArg;
            while (cur instanceof Cons c) { callArgs.add(c.car); cur = c.cdr; }
            return applyProc(proc, callArgs);
        });
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
}
