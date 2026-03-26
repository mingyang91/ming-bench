package ming;

import java.util.ArrayList;
import java.util.List;

import static ming.Evaluator.NIL;

/**
 * Dispatches and implements all built-in Scheme procedures.
 * Split from Evaluator to keep method sizes within quality-gate limits.
 */
final class Builtins {

    private final Evaluator evaluator;

    Builtins(Evaluator evaluator) {
        this.evaluator = evaluator;
    }

    Object apply(String name, List<Object> args) throws EvalError {
        return switch (name) {
            // Arithmetic
            case "+", "-", "*", "/", "abs", "modulo", "remainder", "quotient",
                 "min", "max", "expt" -> applyArithmetic(name, args);

            // Numeric predicates
            case "zero?", "positive?", "negative?", "odd?", "even?" ->
                applyNumericPredicate(name, args);

            // Comparison
            case "<", ">", "=", "<=", ">=" -> applyComparison(name, args);
            case "not" -> { requireArgCount(args, 1, "not"); yield isFalse(args.get(0)); }
            case "equal?" -> { requireArgCount(args, 2, "equal?"); yield evaluator.schemeEqual(args.get(0), args.get(1)); }
            case "eq?" -> applyEq(args);
            case "eqv?" -> { requireArgCount(args, 2, "eqv?"); yield evaluator.schemeEqv(args.get(0), args.get(1)); }

            // Pair / List
            case "cons", "car", "cdr", "null?", "list", "length", "append",
                 "list-ref", "list-tail", "list?", "assoc", "pair?" ->
                applyList(name, args);

            // Type predicates
            case "string?", "number?", "boolean?", "symbol?", "char?" ->
                applyTypePredicate(name, args);

            // I/O
            case "display", "write", "newline" -> applyIO(name, args);

            // String operations
            case "string-append", "string-length", "substring",
                 "string->number", "number->string", "symbol->string", "string->symbol",
                 "string-ref", "string-copy", "string-set!",
                 "string->list", "list->string",
                 "string=?", "string<?", "string-ci=?",
                 "string-upcase", "string-downcase" ->
                applyString(name, args);

            // Char operations
            case "char-alphabetic?", "char-numeric?", "char-upcase", "char-downcase",
                 "char=?", "char<?",
                 "char->integer", "integer->char" ->
                applyChar(name, args);

            // Exact/inexact
            case "exact?", "inexact?", "exact->inexact", "inexact->exact",
                 "numerator", "denominator", "integer?", "rational?" ->
                applyExact(name, args);

            case "procedure?" -> {
                requireArgCount(args, 1, "procedure?");
                Object val = args.get(0);
                yield val instanceof Evaluator.BuiltinProc
                    || val instanceof Evaluator.Lambda
                    || val instanceof Evaluator.CaseLambda
                    || val instanceof Evaluator.RecordConstructor
                    || val instanceof Evaluator.RecordPredicate
                    || val instanceof Evaluator.RecordAccessor;
            }

            // Vectors
            case "vector", "make-vector", "vector-ref", "vector-set!",
                 "vector-length", "vector?", "vector->list", "list->vector" ->
                applyVector(name, args);

            // Higher-order
            case "apply" -> applyApply(args);
            case "map" -> applyMap(args);
            case "for-each" -> applyForEach(args);

            default -> throw evaluator.posError("unbound variable: " + name);
        };
    }

    // ---- Arithmetic ----

    private Object applyArithmetic(String name, List<Object> args) throws EvalError {
        return switch (name) {
            case "+" -> {
                for (Object a : args) requireNumber(a, "+");
                if (hasInexact(args)) {
                    double sum = 0;
                    for (Object a : args) sum += toDouble(a);
                    yield sum;
                }
                long rn = 0, rd = 1;
                for (Object a : args) {
                    long[] e = toExact(a);
                    rn = rn * e[1] + e[0] * rd;
                    rd = rd * e[1];
                    long g = Evaluator.gcd(rn, rd); rn /= g; rd /= g;
                }
                yield Evaluator.makeRational(rn, rd);
            }
            case "-" -> {
                if (args.isEmpty()) throw evaluator.posError("-: need at least 1 argument");
                for (Object a : args) requireNumber(a, "-");
                if (hasInexact(args)) {
                    double result = toDouble(args.get(0));
                    if (args.size() == 1) yield -result;
                    for (int i = 1; i < args.size(); i++) result -= toDouble(args.get(i));
                    yield result;
                }
                long[] first = toExact(args.get(0));
                long rn = first[0], rd = first[1];
                if (args.size() == 1) yield Evaluator.makeRational(-rn, rd);
                for (int i = 1; i < args.size(); i++) {
                    long[] e = toExact(args.get(i));
                    rn = rn * e[1] - e[0] * rd;
                    rd = rd * e[1];
                    long g = Evaluator.gcd(rn, rd); rn /= g; rd /= g;
                }
                yield Evaluator.makeRational(rn, rd);
            }
            case "*" -> {
                for (Object a : args) requireNumber(a, "*");
                if (hasInexact(args)) {
                    double product = 1;
                    for (Object a : args) product *= toDouble(a);
                    yield product;
                }
                long rn = 1, rd = 1;
                for (Object a : args) {
                    long[] e = toExact(a);
                    rn *= e[0]; rd *= e[1];
                    long g = Evaluator.gcd(rn, rd); rn /= g; rd /= g;
                }
                yield Evaluator.makeRational(rn, rd);
            }
            case "/" -> {
                if (args.isEmpty()) throw evaluator.posError("/: need at least 1 argument");
                for (Object a : args) requireNumber(a, "/");
                if (hasInexact(args)) {
                    double result = toDouble(args.get(0));
                    if (args.size() == 1) { if (result == 0) throw evaluator.posError("division by zero"); yield 1.0 / result; }
                    for (int i = 1; i < args.size(); i++) {
                        double d = toDouble(args.get(i));
                        if (d == 0) throw evaluator.posError("division by zero");
                        result /= d;
                    }
                    yield result;
                }
                long[] first = toExact(args.get(0));
                long rn = first[0], rd = first[1];
                if (args.size() == 1) {
                    if (rn == 0) throw evaluator.posError("division by zero");
                    yield Evaluator.makeRational(rd, rn);
                }
                for (int i = 1; i < args.size(); i++) {
                    long[] e = toExact(args.get(i));
                    if (e[0] == 0) throw evaluator.posError("division by zero");
                    rn *= e[1]; rd *= e[0];
                    long g = Evaluator.gcd(rn, rd); rn /= g; rd /= g;
                }
                yield Evaluator.makeRational(rn, rd);
            }
            case "abs" -> {
                requireArgCount(args, 1, "abs");
                Object a = requireNumber(args.get(0), "abs");
                if (a instanceof Long l) yield Math.abs(l);
                if (a instanceof Double d) yield Math.abs(d);
                var r = (Evaluator.SchemeRational) a;
                yield Evaluator.makeRational(Math.abs(r.numer), r.denom);
            }
            case "modulo" -> {
                requireArgCount(args, 2, "modulo");
                long a = requireLong(args.get(0), "modulo"), b = requireLong(args.get(1), "modulo");
                if (b == 0) throw evaluator.posError("modulo: division by zero");
                yield Math.floorMod(a, b);
            }
            case "remainder" -> {
                requireArgCount(args, 2, "remainder");
                long a = requireLong(args.get(0), "remainder"), b = requireLong(args.get(1), "remainder");
                if (b == 0) throw evaluator.posError("remainder: division by zero");
                yield a % b;
            }
            case "quotient" -> {
                requireArgCount(args, 2, "quotient");
                long a = requireLong(args.get(0), "quotient"), b = requireLong(args.get(1), "quotient");
                if (b == 0) throw evaluator.posError("quotient: division by zero");
                yield a / b;
            }
            case "min" -> {
                if (args.isEmpty()) throw evaluator.posError("min: need at least 1 argument");
                Object result = requireNumber(args.get(0), "min");
                for (int i = 1; i < args.size(); i++) {
                    Object v = requireNumber(args.get(i), "min");
                    if (toDouble(v) < toDouble(result)) result = v;
                }
                yield result;
            }
            case "max" -> {
                if (args.isEmpty()) throw evaluator.posError("max: need at least 1 argument");
                Object result = requireNumber(args.get(0), "max");
                for (int i = 1; i < args.size(); i++) {
                    Object v = requireNumber(args.get(i), "max");
                    if (toDouble(v) > toDouble(result)) result = v;
                }
                yield result;
            }
            case "expt" -> {
                requireArgCount(args, 2, "expt");
                long base = requireLong(args.get(0), "expt"), exp = requireLong(args.get(1), "expt");
                long result = 1;
                for (long i = 0; i < exp; i++) result *= base;
                yield result;
            }
            default -> throw evaluator.posError("unknown arithmetic op: " + name);
        };
    }

    // ---- Numeric predicates ----

    private Object applyNumericPredicate(String name, List<Object> args) throws EvalError {
        requireArgCount(args, 1, name);
        return switch (name) {
            case "zero?" -> { requireNumber(args.get(0), name); yield toDouble(args.get(0)) == 0; }
            case "positive?" -> { requireNumber(args.get(0), name); yield toDouble(args.get(0)) > 0; }
            case "negative?" -> { requireNumber(args.get(0), name); yield toDouble(args.get(0)) < 0; }
            case "odd?" -> { long v = requireLong(args.get(0), "odd?"); yield v % 2 != 0; }
            case "even?" -> { long v = requireLong(args.get(0), "even?"); yield v % 2 == 0; }
            default -> throw evaluator.posError("unknown predicate: " + name);
        };
    }

    // ---- Comparison ----

    private Object applyComparison(String name, List<Object> args) throws EvalError {
        requireArgCount(args, 2, name);
        requireNumber(args.get(0), name);
        requireNumber(args.get(1), name);
        double a = toDouble(args.get(0)), b = toDouble(args.get(1));
        return switch (name) {
            case "<" -> a < b;
            case ">" -> a > b;
            case "=" -> a == b;
            case "<=" -> a <= b;
            case ">=" -> a >= b;
            default -> throw evaluator.posError("unknown comparison: " + name);
        };
    }

    private Object applyEq(List<Object> args) throws EvalError {
        requireArgCount(args, 2, "eq?");
        Object a = args.get(0), b = args.get(1);
        if (a == b) return true;
        if (a instanceof Long && b instanceof Long) return a.equals(b);
        if (a instanceof Boolean && b instanceof Boolean) return a.equals(b);
        if (a instanceof String && b instanceof String) return a.equals(b);
        if (a instanceof Evaluator.SchemeChar ca && b instanceof Evaluator.SchemeChar cb) return ca.value == cb.value;
        return false;
    }

    // ---- Type predicates ----

    private Object applyTypePredicate(String name, List<Object> args) throws EvalError {
        requireArgCount(args, 1, name);
        Object val = args.get(0);
        return switch (name) {
            case "string?" -> val instanceof Evaluator.SchemeString;
            case "number?" -> val instanceof Long || val instanceof Double || val instanceof Evaluator.SchemeRational;
            case "boolean?" -> val instanceof Boolean;
            case "pair?" -> val instanceof Evaluator.Pair;
            case "symbol?" -> val instanceof String;
            case "char?" -> val instanceof Evaluator.SchemeChar;
            default -> throw evaluator.posError("unknown type predicate: " + name);
        };
    }

    // ---- Pair / List operations ----

    private Object applyList(String name, List<Object> args) throws EvalError {
        return switch (name) {
            case "cons" -> { requireArgCount(args, 2, "cons"); yield new Evaluator.Pair(args.get(0), args.get(1)); }
            case "car" -> {
                requireArgCount(args, 1, "car");
                if (!(args.get(0) instanceof Evaluator.Pair p)) throw evaluator.posError("car: expected pair");
                yield p.car;
            }
            case "cdr" -> {
                requireArgCount(args, 1, "cdr");
                if (!(args.get(0) instanceof Evaluator.Pair p)) throw evaluator.posError("cdr: expected pair");
                yield p.cdr;
            }
            case "null?" -> { requireArgCount(args, 1, "null?"); yield args.get(0) == NIL; }
            case "pair?" -> { requireArgCount(args, 1, "pair?"); yield args.get(0) instanceof Evaluator.Pair; }
            case "list" -> {
                Object result = NIL;
                for (int i = args.size() - 1; i >= 0; i--) result = new Evaluator.Pair(args.get(i), result);
                yield result;
            }
            case "length" -> {
                requireArgCount(args, 1, "length");
                long count = 0;
                Object cur = args.get(0);
                while (cur instanceof Evaluator.Pair p) { count++; cur = p.cdr; }
                if (cur != NIL) throw evaluator.posError("length: not a proper list");
                yield count;
            }
            case "append" -> applyAppend(args);
            case "list-ref" -> {
                requireArgCount(args, 2, "list-ref");
                Object cur = args.get(0);
                int idx = (int) requireLong(args.get(1), "list-ref");
                for (int i = 0; i < idx; i++) {
                    if (!(cur instanceof Evaluator.Pair p)) throw evaluator.posError("list-ref: index out of range");
                    cur = p.cdr;
                }
                if (!(cur instanceof Evaluator.Pair p)) throw evaluator.posError("list-ref: index out of range");
                yield p.car;
            }
            case "list-tail" -> {
                requireArgCount(args, 2, "list-tail");
                Object cur = args.get(0);
                int idx = (int) requireLong(args.get(1), "list-tail");
                for (int i = 0; i < idx; i++) {
                    if (!(cur instanceof Evaluator.Pair p)) throw evaluator.posError("list-tail: index out of range");
                    cur = p.cdr;
                }
                yield cur;
            }
            case "list?" -> {
                requireArgCount(args, 1, "list?");
                Object cur = args.get(0);
                while (cur instanceof Evaluator.Pair p) cur = p.cdr;
                yield cur == NIL;
            }
            case "assoc" -> {
                requireArgCount(args, 2, "assoc");
                Object key = args.get(0);
                Object alist = args.get(1);
                while (alist instanceof Evaluator.Pair p) {
                    if (p.car instanceof Evaluator.Pair entry && evaluator.schemeEqual(key, entry.car)) yield entry;
                    alist = p.cdr;
                }
                yield Boolean.FALSE;
            }
            default -> throw evaluator.posError("unknown list op: " + name);
        };
    }

    private Object applyAppend(List<Object> args) {
        if (args.isEmpty()) return NIL;
        Object result = args.get(args.size() - 1);
        for (int i = args.size() - 2; i >= 0; i--) {
            List<Object> elems = new ArrayList<>();
            Object cur = args.get(i);
            while (cur instanceof Evaluator.Pair p) { elems.add(p.car); cur = p.cdr; }
            for (int j = elems.size() - 1; j >= 0; j--) result = new Evaluator.Pair(elems.get(j), result);
        }
        return result;
    }

    // ---- I/O ----

    private Object applyIO(String name, List<Object> args) throws EvalError {
        return switch (name) {
            case "display" -> { requireArgCount(args, 1, "display"); evaluator.outputBuffer.append(evaluator.displayString(args.get(0))); yield null; }
            case "write" -> { requireArgCount(args, 1, "write"); evaluator.outputBuffer.append(evaluator.schemeToString(args.get(0))); yield null; }
            case "newline" -> { evaluator.outputBuffer.append("\n"); yield null; }
            default -> throw evaluator.posError("unknown io op: " + name);
        };
    }

    // ---- String operations ----

    private Object applyString(String name, List<Object> args) throws EvalError {
        return switch (name) {
            case "string-append" -> {
                StringBuilder sb = new StringBuilder();
                for (Object a : args) {
                    if (!(a instanceof Evaluator.SchemeString s)) throw evaluator.posError("string-append: expected string");
                    sb.append(s.value);
                }
                yield new Evaluator.SchemeString(sb.toString());
            }
            case "string-length" -> {
                requireArgCount(args, 1, "string-length");
                if (!(args.get(0) instanceof Evaluator.SchemeString s)) throw evaluator.posError("string-length: expected string");
                yield (long) s.value.length();
            }
            case "substring" -> {
                if (args.size() < 2 || args.size() > 3) throw evaluator.posError("substring: expected 2 or 3 arguments");
                if (!(args.get(0) instanceof Evaluator.SchemeString s)) throw evaluator.posError("substring: expected string");
                int start = (int) requireLong(args.get(1), "substring");
                int end = args.size() == 3 ? (int) requireLong(args.get(2), "substring") : s.value.length();
                yield new Evaluator.SchemeString(s.value.substring(start, end));
            }
            case "string->number" -> {
                requireArgCount(args, 1, "string->number");
                if (!(args.get(0) instanceof Evaluator.SchemeString s)) throw evaluator.posError("string->number: expected string");
                try { yield Long.parseLong(s.value); } catch (NumberFormatException e) { yield Boolean.FALSE; }
            }
            case "number->string" -> {
                requireArgCount(args, 1, "number->string");
                requireNumber(args.get(0), "number->string");
                yield new Evaluator.SchemeString(evaluator.schemeToString(args.get(0)));
            }
            case "symbol->string" -> {
                requireArgCount(args, 1, "symbol->string");
                if (!(args.get(0) instanceof String s)) throw evaluator.posError("symbol->string: expected symbol");
                yield new Evaluator.SchemeString(s);
            }
            case "string->symbol" -> {
                requireArgCount(args, 1, "string->symbol");
                if (!(args.get(0) instanceof Evaluator.SchemeString s)) throw evaluator.posError("string->symbol: expected string");
                yield s.value;
            }
            case "string-ref" -> {
                requireArgCount(args, 2, "string-ref");
                if (!(args.get(0) instanceof Evaluator.SchemeString s)) throw evaluator.posError("string-ref: expected string");
                int idx = (int) requireLong(args.get(1), "string-ref");
                yield new Evaluator.SchemeChar(s.value.charAt(idx));
            }
            case "string-copy" -> {
                requireArgCount(args, 1, "string-copy");
                if (!(args.get(0) instanceof Evaluator.SchemeString s)) throw evaluator.posError("string-copy: expected string");
                yield new Evaluator.SchemeString(s.value, true);
            }
            case "string-set!" -> {
                requireArgCount(args, 3, "string-set!");
                if (!(args.get(0) instanceof Evaluator.SchemeString s)) throw evaluator.posError("string-set!: expected string");
                if (!s.mutable) throw evaluator.posError("string-set!: strings are immutable");
                int idx = (int) requireLong(args.get(1), "string-set!");
                if (!(args.get(2) instanceof Evaluator.SchemeChar c)) throw evaluator.posError("string-set!: expected char");
                if (idx < 0 || idx >= s.value.length()) throw evaluator.posError("string-set!: index out of range");
                char[] chars = s.value.toCharArray();
                chars[idx] = c.value;
                s.value = new String(chars);
                yield null; // void
            }
            case "string->list" -> {
                requireArgCount(args, 1, "string->list");
                if (!(args.get(0) instanceof Evaluator.SchemeString s)) throw evaluator.posError("string->list: expected string");
                Object result = NIL;
                for (int i = s.value.length() - 1; i >= 0; i--) {
                    result = new Evaluator.Pair(new Evaluator.SchemeChar(s.value.charAt(i)), result);
                }
                yield result;
            }
            case "list->string" -> {
                requireArgCount(args, 1, "list->string");
                StringBuilder sb = new StringBuilder();
                Object lst = args.get(0);
                while (lst instanceof Evaluator.Pair p) {
                    if (!(p.car instanceof Evaluator.SchemeChar c)) throw evaluator.posError("list->string: expected list of chars");
                    sb.append(c.value);
                    lst = p.cdr;
                }
                yield new Evaluator.SchemeString(sb.toString());
            }
            case "string=?" -> {
                requireArgCount(args, 2, "string=?");
                if (!(args.get(0) instanceof Evaluator.SchemeString a)) throw evaluator.posError("string=?: expected string");
                if (!(args.get(1) instanceof Evaluator.SchemeString b)) throw evaluator.posError("string=?: expected string");
                yield a.value.equals(b.value);
            }
            case "string<?" -> {
                requireArgCount(args, 2, "string<?");
                if (!(args.get(0) instanceof Evaluator.SchemeString a)) throw evaluator.posError("string<?: expected string");
                if (!(args.get(1) instanceof Evaluator.SchemeString b)) throw evaluator.posError("string<?: expected string");
                yield a.value.compareTo(b.value) < 0;
            }
            case "string-ci=?" -> {
                requireArgCount(args, 2, "string-ci=?");
                if (!(args.get(0) instanceof Evaluator.SchemeString a)) throw evaluator.posError("string-ci=?: expected string");
                if (!(args.get(1) instanceof Evaluator.SchemeString b)) throw evaluator.posError("string-ci=?: expected string");
                yield a.value.equalsIgnoreCase(b.value);
            }
            case "string-upcase" -> {
                requireArgCount(args, 1, "string-upcase");
                if (!(args.get(0) instanceof Evaluator.SchemeString s)) throw evaluator.posError("string-upcase: expected string");
                yield new Evaluator.SchemeString(s.value.toUpperCase());
            }
            case "string-downcase" -> {
                requireArgCount(args, 1, "string-downcase");
                if (!(args.get(0) instanceof Evaluator.SchemeString s)) throw evaluator.posError("string-downcase: expected string");
                yield new Evaluator.SchemeString(s.value.toLowerCase());
            }
            default -> throw evaluator.posError("unknown string op: " + name);
        };
    }

    // ---- Char operations ----

    private Object applyChar(String name, List<Object> args) throws EvalError {
        return switch (name) {
            case "char-alphabetic?" -> {
                requireArgCount(args, 1, "char-alphabetic?");
                if (!(args.get(0) instanceof Evaluator.SchemeChar c)) throw evaluator.posError("char-alphabetic?: expected char");
                yield Character.isLetter(c.value);
            }
            case "char-numeric?" -> {
                requireArgCount(args, 1, "char-numeric?");
                if (!(args.get(0) instanceof Evaluator.SchemeChar c)) throw evaluator.posError("char-numeric?: expected char");
                yield Character.isDigit(c.value);
            }
            case "char-upcase" -> {
                requireArgCount(args, 1, "char-upcase");
                if (!(args.get(0) instanceof Evaluator.SchemeChar c)) throw evaluator.posError("char-upcase: expected char");
                yield new Evaluator.SchemeChar(Character.toUpperCase(c.value));
            }
            case "char-downcase" -> {
                requireArgCount(args, 1, "char-downcase");
                if (!(args.get(0) instanceof Evaluator.SchemeChar c)) throw evaluator.posError("char-downcase: expected char");
                yield new Evaluator.SchemeChar(Character.toLowerCase(c.value));
            }
            case "char=?" -> {
                requireArgCount(args, 2, "char=?");
                if (!(args.get(0) instanceof Evaluator.SchemeChar a)) throw evaluator.posError("char=?: expected char");
                if (!(args.get(1) instanceof Evaluator.SchemeChar b)) throw evaluator.posError("char=?: expected char");
                yield a.value == b.value;
            }
            case "char<?" -> {
                requireArgCount(args, 2, "char<?");
                if (!(args.get(0) instanceof Evaluator.SchemeChar a)) throw evaluator.posError("char<?: expected char");
                if (!(args.get(1) instanceof Evaluator.SchemeChar b)) throw evaluator.posError("char<?: expected char");
                yield a.value < b.value;
            }
            case "char->integer" -> {
                requireArgCount(args, 1, "char->integer");
                if (!(args.get(0) instanceof Evaluator.SchemeChar c)) throw evaluator.posError("char->integer: expected char");
                yield (long) c.value;
            }
            case "integer->char" -> {
                requireArgCount(args, 1, "integer->char");
                long n = requireLong(args.get(0), "integer->char");
                yield new Evaluator.SchemeChar((char) n);
            }
            default -> throw evaluator.posError("unknown char op: " + name);
        };
    }

    // ---- Vector operations ----

    private Object applyVector(String name, List<Object> args) throws EvalError {
        return switch (name) {
            case "vector" -> new Evaluator.SchemeVector(args.toArray());
            case "make-vector" -> {
                if (args.size() < 1 || args.size() > 2)
                    throw evaluator.posError("make-vector: expected 1 or 2 arguments");
                int len = (int) requireLong(args.get(0), "make-vector");
                Object fill = args.size() == 2 ? args.get(1) : 0L;
                Object[] data = new Object[len];
                for (int i = 0; i < len; i++) data[i] = fill;
                yield new Evaluator.SchemeVector(data);
            }
            case "vector-ref" -> {
                requireArgCount(args, 2, "vector-ref");
                if (!(args.get(0) instanceof Evaluator.SchemeVector v))
                    throw evaluator.posError("vector-ref: expected vector");
                int idx = (int) requireLong(args.get(1), "vector-ref");
                yield v.data[idx];
            }
            case "vector-set!" -> {
                requireArgCount(args, 3, "vector-set!");
                if (!(args.get(0) instanceof Evaluator.SchemeVector v))
                    throw evaluator.posError("vector-set!: expected vector");
                int idx = (int) requireLong(args.get(1), "vector-set!");
                v.data[idx] = args.get(2);
                yield (Object) null;
            }
            case "vector-length" -> {
                requireArgCount(args, 1, "vector-length");
                if (!(args.get(0) instanceof Evaluator.SchemeVector v))
                    throw evaluator.posError("vector-length: expected vector");
                yield (long) v.data.length;
            }
            case "vector?" -> {
                requireArgCount(args, 1, "vector?");
                yield args.get(0) instanceof Evaluator.SchemeVector;
            }
            case "vector->list" -> {
                requireArgCount(args, 1, "vector->list");
                if (!(args.get(0) instanceof Evaluator.SchemeVector v))
                    throw evaluator.posError("vector->list: expected vector");
                Object result = NIL;
                for (int i = v.data.length - 1; i >= 0; i--) {
                    result = new Evaluator.Pair(v.data[i], result);
                }
                yield result;
            }
            case "list->vector" -> {
                requireArgCount(args, 1, "list->vector");
                List<Object> elems = new ArrayList<>();
                Object cur = args.get(0);
                while (cur instanceof Evaluator.Pair p) {
                    elems.add(p.car);
                    cur = p.cdr;
                }
                yield new Evaluator.SchemeVector(elems.toArray());
            }
            default -> throw evaluator.posError("unknown vector op: " + name);
        };
    }

    // ---- Higher-order: apply, map, for-each ----

    private Object applyApply(List<Object> args) throws EvalError {
        if (args.size() < 2) throw evaluator.posError("apply: need at least 2 arguments");
        Object proc = args.get(0);
        List<Object> callArgs = new ArrayList<>();
        for (int i = 1; i < args.size() - 1; i++) callArgs.add(args.get(i));
        Object cur = args.get(args.size() - 1);
        while (cur instanceof Evaluator.Pair p) { callArgs.add(p.car); cur = p.cdr; }
        return evaluator.apply(proc, callArgs);
    }

    private Object applyMap(List<Object> args) throws EvalError {
        if (args.size() < 2) throw evaluator.posError("map: need at least 2 arguments");
        Object proc = args.get(0);
        List<Object> lists = new ArrayList<>();
        for (int i = 1; i < args.size(); i++) lists.add(args.get(i));
        List<Object> results = new ArrayList<>();
        while (true) {
            boolean done = false;
            for (Object l : lists) { if (!(l instanceof Evaluator.Pair)) { done = true; break; } }
            if (done) break;
            List<Object> callArgs = new ArrayList<>();
            List<Object> newLists = new ArrayList<>();
            for (int i = 0; i < lists.size(); i++) {
                Evaluator.Pair p = (Evaluator.Pair) lists.get(i);
                callArgs.add(p.car);
                newLists.add(p.cdr);
            }
            results.add(evaluator.apply(proc, callArgs));
            lists = newLists;
        }
        Object result = NIL;
        for (int i = results.size() - 1; i >= 0; i--) result = new Evaluator.Pair(results.get(i), result);
        return result;
    }

    private Object applyForEach(List<Object> args) throws EvalError {
        if (args.size() < 2) throw evaluator.posError("for-each: need at least 2 arguments");
        Object proc = args.get(0);
        List<Object> lists = new ArrayList<>();
        for (int i = 1; i < args.size(); i++) lists.add(args.get(i));
        while (true) {
            boolean done = false;
            for (Object l : lists) { if (!(l instanceof Evaluator.Pair)) { done = true; break; } }
            if (done) break;
            List<Object> callArgs = new ArrayList<>();
            List<Object> newLists = new ArrayList<>();
            for (int i = 0; i < lists.size(); i++) {
                Evaluator.Pair p = (Evaluator.Pair) lists.get(i);
                callArgs.add(p.car);
                newLists.add(p.cdr);
            }
            evaluator.apply(proc, callArgs);
            lists = newLists;
        }
        return null;
    }

    // ---- Exact/Inexact ----

    private Object applyExact(String name, List<Object> args) throws EvalError {
        requireArgCount(args, 1, name);
        Object val = args.get(0);
        return switch (name) {
            case "exact?" -> val instanceof Long || val instanceof Evaluator.SchemeRational;
            case "inexact?" -> val instanceof Double;
            case "exact->inexact" -> {
                requireNumber(val, name);
                yield toDouble(val);
            }
            case "inexact->exact" -> {
                requireNumber(val, name);
                if (val instanceof Long) yield val;
                if (val instanceof Evaluator.SchemeRational) yield val;
                yield Evaluator.doubleToExact((Double) val);
            }
            case "numerator" -> {
                requireNumber(val, name);
                if (val instanceof Long l) yield l;
                if (val instanceof Evaluator.SchemeRational r) yield r.numer;
                throw evaluator.posError("numerator: expected exact number");
            }
            case "denominator" -> {
                requireNumber(val, name);
                if (val instanceof Long) yield 1L;
                if (val instanceof Evaluator.SchemeRational r) yield r.denom;
                throw evaluator.posError("denominator: expected exact number");
            }
            case "integer?" -> val instanceof Long;
            case "rational?" -> val instanceof Long || val instanceof Evaluator.SchemeRational;
            default -> throw evaluator.posError("unknown exact op: " + name);
        };
    }

    // ---- Helpers ----

    private Object requireNumber(Object val, String context) throws EvalError {
        if (val instanceof Long || val instanceof Double || val instanceof Evaluator.SchemeRational) return val;
        throw evaluator.posError(context + ": expected number, got " + evaluator.schemeToString(val));
    }

    private long requireLong(Object val, String context) throws EvalError {
        if (val instanceof Long l) return l;
        throw evaluator.posError(context + ": expected integer, got " + evaluator.schemeToString(val));
    }

    private boolean hasInexact(List<Object> args) {
        for (Object a : args) if (a instanceof Double) return true;
        return false;
    }

    private double toDouble(Object val) {
        if (val instanceof Long l) return l.doubleValue();
        if (val instanceof Double d) return d;
        if (val instanceof Evaluator.SchemeRational r) return r.toDouble();
        return 0;
    }

    private long[] toExact(Object val) {
        if (val instanceof Long l) return new long[]{l, 1};
        if (val instanceof Evaluator.SchemeRational r) return new long[]{r.numer, r.denom};
        return new long[]{0, 1};
    }

    private void requireArgCount(List<Object> args, int expected, String name) throws EvalError {
        if (args.size() != expected) {
            throw evaluator.posError(name + ": expected " + expected + " arguments, got " + args.size());
        }
    }

    private boolean isFalse(Object val) {
        return val instanceof Boolean b && !b;
    }
}
