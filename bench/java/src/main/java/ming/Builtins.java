package ming;

import static ming.Numbers.*;
import static ming.SchemeFormatter.*;

import java.util.ArrayList;
import java.util.List;

/**
 * Registers all built-in procedures into a given environment.
 */
final class Builtins {

    @FunctionalInterface
    interface ProcApplier {
        Object apply(Object proc, List<Object> args) throws EvalError;
    }

    private Builtins() {}

    static void registerAll(Evaluator.Env env, StringBuilder outputBuffer, ProcApplier applier) {
        registerListOps(env);
        registerCxrAndReverse(env);
        registerTypePredicates(env);
        registerArithmetic(env);
        registerIOOps(env, outputBuffer);
        registerStringOps(env);
        registerNumericUtils(env);
        registerListUtils(env);
        registerCharOps(env);
        registerStringComparisons(env);
        registerHigherOrder(env, applier);
        registerRationals(env);
        registerVectors(env);
        registerMutation(env, applier);
    }

    private static void registerListOps(Evaluator.Env env) {
        env.define("cons", new Evaluator.BuiltinProc("cons", args -> {
            if (args.size() != 2) throw new EvalError("cons: expected 2 args");
            return new Evaluator.Pair(args.get(0), args.get(1));
        }));
        env.define("car", new Evaluator.BuiltinProc("car", args -> {
            if (args.size() != 1) throw new EvalError("car: expected 1 arg");
            if (!(args.get(0) instanceof Evaluator.Pair p)) throw new EvalError("car: not a pair");
            return p.car;
        }));
        env.define("cdr", new Evaluator.BuiltinProc("cdr", args -> {
            if (args.size() != 1) throw new EvalError("cdr: expected 1 arg");
            if (!(args.get(0) instanceof Evaluator.Pair p)) throw new EvalError("cdr: not a pair");
            return p.cdr;
        }));
        env.define("null?", new Evaluator.BuiltinProc("null?", args -> {
            if (args.size() != 1) throw new EvalError("null?: expected 1 arg");
            return args.get(0) == Evaluator.EMPTY_LIST ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("list", new Evaluator.BuiltinProc("list", args -> {
            Object result = Evaluator.EMPTY_LIST;
            for (int i = args.size() - 1; i >= 0; i--) {
                result = new Evaluator.Pair(args.get(i), result);
            }
            return result;
        }));
        env.define("length", new Evaluator.BuiltinProc("length", args -> {
            if (args.size() != 1) throw new EvalError("length: expected 1 arg");
            Object obj = args.get(0);
            long count = 0;
            while (obj instanceof Evaluator.Pair p) {
                count++;
                obj = p.cdr;
            }
            if (obj != Evaluator.EMPTY_LIST) throw new EvalError("length: not a proper list");
            return count;
        }));
        env.define("append", new Evaluator.BuiltinProc("append", args -> {
            if (args.size() == 0) return Evaluator.EMPTY_LIST;
            if (args.size() == 1) return args.get(0);
            Object a = args.get(0);
            Object b = args.get(1);
            if (a == Evaluator.EMPTY_LIST) return b;
            List<Object> elems = new ArrayList<>();
            Object curr = a;
            while (curr instanceof Evaluator.Pair p) {
                elems.add(p.car);
                curr = p.cdr;
            }
            Object result = b;
            for (int i = elems.size() - 1; i >= 0; i--) {
                result = new Evaluator.Pair(elems.get(i), result);
            }
            return result;
        }));
    }

    private static void registerCxrAndReverse(Evaluator.Env env) {
        env.define("caar", new Evaluator.BuiltinProc("caar", args -> {
            if (args.size() != 1) throw new EvalError("caar: expected 1 arg");
            if (!(args.get(0) instanceof Evaluator.Pair p)) throw new EvalError("caar: not a pair");
            if (!(p.car instanceof Evaluator.Pair pp)) throw new EvalError("caar: car is not a pair");
            return pp.car;
        }));
        env.define("cadr", new Evaluator.BuiltinProc("cadr", args -> {
            if (args.size() != 1) throw new EvalError("cadr: expected 1 arg");
            if (!(args.get(0) instanceof Evaluator.Pair p)) throw new EvalError("cadr: not a pair");
            if (!(p.cdr instanceof Evaluator.Pair pp)) throw new EvalError("cadr: cdr is not a pair");
            return pp.car;
        }));
        env.define("cdar", new Evaluator.BuiltinProc("cdar", args -> {
            if (args.size() != 1) throw new EvalError("cdar: expected 1 arg");
            if (!(args.get(0) instanceof Evaluator.Pair p)) throw new EvalError("cdar: not a pair");
            if (!(p.car instanceof Evaluator.Pair pp)) throw new EvalError("cdar: car is not a pair");
            return pp.cdr;
        }));
        env.define("cddr", new Evaluator.BuiltinProc("cddr", args -> {
            if (args.size() != 1) throw new EvalError("cddr: expected 1 arg");
            if (!(args.get(0) instanceof Evaluator.Pair p)) throw new EvalError("cddr: not a pair");
            if (!(p.cdr instanceof Evaluator.Pair pp)) throw new EvalError("cddr: cdr is not a pair");
            return pp.cdr;
        }));
        env.define("member", new Evaluator.BuiltinProc("member", args -> {
            if (args.size() != 2) throw new EvalError("member: expected 2 args");
            Object key = args.get(0);
            Object lst = args.get(1);
            while (lst instanceof Evaluator.Pair p) {
                if (Evaluator.schemeEqual(key, p.car)) return lst;
                lst = p.cdr;
            }
            return Boolean.FALSE;
        }));
        env.define("reverse", new Evaluator.BuiltinProc("reverse", args -> {
            if (args.size() != 1) throw new EvalError("reverse: expected 1 arg");
            Object result = Evaluator.EMPTY_LIST;
            Object lst = args.get(0);
            while (lst instanceof Evaluator.Pair p) {
                result = new Evaluator.Pair(p.car, result);
                lst = p.cdr;
            }
            return result;
        }));
    }

    private static void registerTypePredicates(Evaluator.Env env) {
        env.define("number?", new Evaluator.BuiltinProc("number?", args -> {
            if (args.size() != 1) throw new EvalError("number?: expected 1 arg");
            return isNumber(args.get(0)) ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("string?", new Evaluator.BuiltinProc("string?", args -> {
            if (args.size() != 1) throw new EvalError("string?: expected 1 arg");
            return args.get(0) instanceof SchemeString ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("boolean?", new Evaluator.BuiltinProc("boolean?", args -> {
            if (args.size() != 1) throw new EvalError("boolean?: expected 1 arg");
            return args.get(0) instanceof Boolean ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("pair?", new Evaluator.BuiltinProc("pair?", args -> {
            if (args.size() != 1) throw new EvalError("pair?: expected 1 arg");
            return args.get(0) instanceof Evaluator.Pair ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("symbol?", new Evaluator.BuiltinProc("symbol?", args -> {
            if (args.size() != 1) throw new EvalError("symbol?: expected 1 arg");
            return args.get(0) instanceof String ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("procedure?", new Evaluator.BuiltinProc("procedure?", args -> {
            if (args.size() != 1) throw new EvalError("procedure?: expected 1 arg");
            Object val = args.get(0);
            return (val instanceof Evaluator.Lambda || val instanceof Evaluator.CaseLambda || val instanceof Evaluator.BuiltinProc)
                    ? Boolean.TRUE : Boolean.FALSE;
        }));
    }

    private static void registerArithmetic(Evaluator.Env env) {
        env.define("+", new Evaluator.BuiltinProc("+", args -> {
            Object result = 0L;
            for (Object a : args) { requireNumber(a, "+"); result = addNum(result, a); }
            return result;
        }));
        env.define("-", new Evaluator.BuiltinProc("-", args -> {
            if (args.isEmpty()) throw new EvalError("-: need at least 1 argument");
            requireNumber(args.get(0), "-");
            if (args.size() == 1) return negateNum(args.get(0));
            Object result = args.get(0);
            for (int i = 1; i < args.size(); i++) { requireNumber(args.get(i), "-"); result = subNum(result, args.get(i)); }
            return result;
        }));
        env.define("*", new Evaluator.BuiltinProc("*", args -> {
            Object result = 1L;
            for (Object a : args) { requireNumber(a, "*"); result = mulNum(result, a); }
            return result;
        }));
        env.define("/", new Evaluator.BuiltinProc("/", args -> {
            if (args.size() < 2) throw new EvalError("/: need at least 2 arguments");
            requireNumber(args.get(0), "/");
            Object result = args.get(0);
            for (int i = 1; i < args.size(); i++) { requireNumber(args.get(i), "/"); result = divNum(result, args.get(i)); }
            return result;
        }));
        for (String op : new String[]{"<", ">", "=", "<=", ">="}) {
            env.define(op, new Evaluator.BuiltinProc(op, args -> {
                if (args.size() < 2) throw new EvalError(op + ": need at least 2 arguments");
                double prev = toDouble(args.get(0));
                for (int i = 1; i < args.size(); i++) {
                    double curr = toDouble(args.get(i));
                    boolean ok = switch (op) {
                        case "<" -> prev < curr;
                        case ">" -> prev > curr;
                        case "=" -> prev == curr;
                        case "<=" -> prev <= curr;
                        case ">=" -> prev >= curr;
                        default -> false;
                    };
                    if (!ok) return Boolean.FALSE;
                    prev = curr;
                }
                return Boolean.TRUE;
            }));
        }
        env.define("not", new Evaluator.BuiltinProc("not", args -> {
            if (args.size() != 1) throw new EvalError("not: expected 1 arg");
            return Evaluator.isTruthy(args.get(0)) ? Boolean.FALSE : Boolean.TRUE;
        }));
    }

    private static void registerIOOps(Evaluator.Env env, StringBuilder outputBuffer) {
        env.define("display", new Evaluator.BuiltinProc("display", args -> {
            if (args.size() != 1) throw new EvalError("display: expected 1 arg");
            outputBuffer.append(displayString(args.get(0)));
            return Boolean.FALSE;
        }));
        env.define("write", new Evaluator.BuiltinProc("write", args -> {
            if (args.size() != 1) throw new EvalError("write: expected 1 arg");
            outputBuffer.append(schemeToString(args.get(0)));
            return Boolean.FALSE;
        }));
        env.define("newline", new Evaluator.BuiltinProc("newline", args -> {
            if (args.size() != 0) throw new EvalError("newline: expected 0 args");
            outputBuffer.append("\n");
            return Boolean.FALSE;
        }));
    }

    private static void registerStringOps(Evaluator.Env env) {
        env.define("string-append", new Evaluator.BuiltinProc("string-append", args -> {
            StringBuilder sb = new StringBuilder();
            for (Object a : args) {
                if (!(a instanceof SchemeString s)) throw new EvalError("string-append: expected string");
                sb.append(s.value());
            }
            return new SchemeString(sb.toString());
        }));
        env.define("string-length", new Evaluator.BuiltinProc("string-length", args -> {
            if (args.size() != 1) throw new EvalError("string-length: expected 1 arg");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-length: expected string");
            return (long) s.value().length();
        }));
        env.define("substring", new Evaluator.BuiltinProc("substring", args -> {
            if (args.size() < 2 || args.size() > 3) throw new EvalError("substring: expected 2-3 args");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("substring: expected string");
            int start = (int) asLong(args.get(1));
            int end = args.size() == 3 ? (int) asLong(args.get(2)) : s.value().length();
            return new SchemeString(s.value().substring(start, end));
        }));
        env.define("string->number", new Evaluator.BuiltinProc("string->number", args -> {
            if (args.size() != 1) throw new EvalError("string->number: expected 1 arg");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string->number: expected string");
            Object n = parseNumber(s.value());
            return n != null ? n : Boolean.FALSE;
        }));
        env.define("number->string", new Evaluator.BuiltinProc("number->string", args -> {
            if (args.size() != 1) throw new EvalError("number->string: expected 1 arg");
            return new SchemeString(schemeToString(args.get(0)));
        }));
        env.define("symbol->string", new Evaluator.BuiltinProc("symbol->string", args -> {
            if (args.size() != 1) throw new EvalError("symbol->string: expected 1 arg");
            if (!(args.get(0) instanceof String s)) throw new EvalError("symbol->string: expected symbol");
            return new SchemeString(s);
        }));
        env.define("string->symbol", new Evaluator.BuiltinProc("string->symbol", args -> {
            if (args.size() != 1) throw new EvalError("string->symbol: expected 1 arg");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string->symbol: expected string");
            return s.value();
        }));
        env.define("string-ref", new Evaluator.BuiltinProc("string-ref", args -> {
            if (args.size() != 2) throw new EvalError("string-ref: expected 2 args");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-ref: expected string");
            int idx = (int) asLong(args.get(1));
            return new SchemeChar(s.value().charAt(idx));
        }));
        env.define("char?", new Evaluator.BuiltinProc("char?", args -> {
            if (args.size() != 1) throw new EvalError("char?: expected 1 arg");
            return args.get(0) instanceof SchemeChar ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("string-copy", new Evaluator.BuiltinProc("string-copy", args -> {
            if (args.size() != 1) throw new EvalError("string-copy: expected 1 arg");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-copy: expected string");
            return s.copy();
        }));
        env.define("string-set!", new Evaluator.BuiltinProc("string-set!", args -> {
            if (args.size() != 3) throw new EvalError("string-set!: expected 3 args");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-set!: expected string");
            if (s.isImmutable()) throw new EvalError("string-set!: string is immutable");
            if (!(args.get(1) instanceof Number n)) throw new EvalError("string-set!: expected integer index");
            if (!(args.get(2) instanceof SchemeChar c)) throw new EvalError("string-set!: expected char");
            s.setChar(n.intValue(), c.value());
            return Boolean.FALSE;
        }));
        env.define("string->list", new Evaluator.BuiltinProc("string->list", args -> {
            if (args.size() != 1) throw new EvalError("string->list: expected 1 arg");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string->list: expected string");
            Object result = Evaluator.EMPTY_LIST;
            for (int i = s.length() - 1; i >= 0; i--) {
                result = new Evaluator.Pair(new SchemeChar(s.charAt(i)), result);
            }
            return result;
        }));
        env.define("list->string", new Evaluator.BuiltinProc("list->string", args -> {
            if (args.size() != 1) throw new EvalError("list->string: expected 1 arg");
            StringBuilder sb = new StringBuilder();
            Object lst = args.get(0);
            while (lst instanceof Evaluator.Pair p) {
                if (!(p.car instanceof SchemeChar c)) throw new EvalError("list->string: expected list of chars");
                sb.append(c.value());
                lst = p.cdr;
            }
            return new SchemeString(sb.toString());
        }));
        env.define("char->integer", new Evaluator.BuiltinProc("char->integer", args -> {
            if (args.size() != 1) throw new EvalError("char->integer: expected 1 arg");
            if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError("char->integer: expected char");
            return (long) c.value();
        }));
        env.define("integer->char", new Evaluator.BuiltinProc("integer->char", args -> {
            if (args.size() != 1) throw new EvalError("integer->char: expected 1 arg");
            return new SchemeChar((char) asLong(args.get(0)));
        }));
    }

    private static void registerNumericUtils(Evaluator.Env env) {
        env.define("abs", new Evaluator.BuiltinProc("abs", args -> {
            if (args.size() != 1) throw new EvalError("abs: expected 1 arg");
            return Math.abs(asLong(args.get(0)));
        }));
        env.define("modulo", new Evaluator.BuiltinProc("modulo", args -> {
            if (args.size() != 2) throw new EvalError("modulo: expected 2 args");
            long a = asLong(args.get(0)), b = asLong(args.get(1));
            return Math.floorMod(a, b);
        }));
        env.define("remainder", new Evaluator.BuiltinProc("remainder", args -> {
            if (args.size() != 2) throw new EvalError("remainder: expected 2 args");
            long a = asLong(args.get(0)), b = asLong(args.get(1));
            return a % b;
        }));
        env.define("quotient", new Evaluator.BuiltinProc("quotient", args -> {
            if (args.size() != 2) throw new EvalError("quotient: expected 2 args");
            long a = asLong(args.get(0)), b = asLong(args.get(1));
            return a / b;
        }));
        env.define("min", new Evaluator.BuiltinProc("min", args -> {
            if (args.isEmpty()) throw new EvalError("min: need at least 1 argument");
            long result = asLong(args.get(0));
            for (int i = 1; i < args.size(); i++) result = Math.min(result, asLong(args.get(i)));
            return result;
        }));
        env.define("max", new Evaluator.BuiltinProc("max", args -> {
            if (args.isEmpty()) throw new EvalError("max: need at least 1 argument");
            long result = asLong(args.get(0));
            for (int i = 1; i < args.size(); i++) result = Math.max(result, asLong(args.get(i)));
            return result;
        }));
        env.define("expt", new Evaluator.BuiltinProc("expt", args -> {
            if (args.size() != 2) throw new EvalError("expt: expected 2 args");
            long base = asLong(args.get(0)), exp = asLong(args.get(1));
            long result = 1;
            for (long i = 0; i < exp; i++) result *= base;
            return result;
        }));
        env.define("zero?", new Evaluator.BuiltinProc("zero?", args -> {
            if (args.size() != 1) throw new EvalError("zero?: expected 1 arg");
            return asLong(args.get(0)) == 0 ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("positive?", new Evaluator.BuiltinProc("positive?", args -> {
            if (args.size() != 1) throw new EvalError("positive?: expected 1 arg");
            return asLong(args.get(0)) > 0 ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("negative?", new Evaluator.BuiltinProc("negative?", args -> {
            if (args.size() != 1) throw new EvalError("negative?: expected 1 arg");
            return asLong(args.get(0)) < 0 ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("odd?", new Evaluator.BuiltinProc("odd?", args -> {
            if (args.size() != 1) throw new EvalError("odd?: expected 1 arg");
            return asLong(args.get(0)) % 2 != 0 ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("even?", new Evaluator.BuiltinProc("even?", args -> {
            if (args.size() != 1) throw new EvalError("even?: expected 1 arg");
            return asLong(args.get(0)) % 2 == 0 ? Boolean.TRUE : Boolean.FALSE;
        }));
    }

    private static void registerListUtils(Evaluator.Env env) {
        env.define("list-ref", new Evaluator.BuiltinProc("list-ref", args -> {
            if (args.size() != 2) throw new EvalError("list-ref: expected 2 args");
            Object lst = args.get(0);
            int idx = (int) asLong(args.get(1));
            for (int i = 0; i < idx; i++) {
                if (!(lst instanceof Evaluator.Pair p)) throw new EvalError("list-ref: index out of range");
                lst = p.cdr;
            }
            if (!(lst instanceof Evaluator.Pair p)) throw new EvalError("list-ref: index out of range");
            return p.car;
        }));
        env.define("list-tail", new Evaluator.BuiltinProc("list-tail", args -> {
            if (args.size() != 2) throw new EvalError("list-tail: expected 2 args");
            Object lst = args.get(0);
            int idx = (int) asLong(args.get(1));
            for (int i = 0; i < idx; i++) {
                if (!(lst instanceof Evaluator.Pair p)) throw new EvalError("list-tail: index out of range");
                lst = p.cdr;
            }
            return lst;
        }));
        env.define("list?", new Evaluator.BuiltinProc("list?", args -> {
            if (args.size() != 1) throw new EvalError("list?: expected 1 arg");
            Object slow = args.get(0), fast = args.get(0);
            while (true) {
                if (!(fast instanceof Evaluator.Pair fp)) return fast == Evaluator.EMPTY_LIST ? Boolean.TRUE : Boolean.FALSE;
                fast = fp.cdr;
                if (!(fast instanceof Evaluator.Pair fp2)) return fast == Evaluator.EMPTY_LIST ? Boolean.TRUE : Boolean.FALSE;
                fast = fp2.cdr;
                slow = ((Evaluator.Pair) slow).cdr;
                if (slow == fast) return Boolean.FALSE; // cycle detected
            }
        }));
        env.define("assoc", new Evaluator.BuiltinProc("assoc", args -> {
            if (args.size() != 2) throw new EvalError("assoc: expected 2 args");
            Object key = args.get(0);
            Object alist = args.get(1);
            while (alist instanceof Evaluator.Pair p) {
                if (p.car instanceof Evaluator.Pair entry && Evaluator.schemeEqual(entry.car, key)) return entry;
                alist = p.cdr;
            }
            return Boolean.FALSE;
        }));
    }

    private static void registerCharOps(Evaluator.Env env) {
        env.define("char-alphabetic?", new Evaluator.BuiltinProc("char-alphabetic?", args -> {
            if (args.size() != 1) throw new EvalError("char-alphabetic?: expected 1 arg");
            if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError("char-alphabetic?: expected char");
            return Character.isLetter(c.value()) ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("char-numeric?", new Evaluator.BuiltinProc("char-numeric?", args -> {
            if (args.size() != 1) throw new EvalError("char-numeric?: expected 1 arg");
            if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError("char-numeric?: expected char");
            return Character.isDigit(c.value()) ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("char-upcase", new Evaluator.BuiltinProc("char-upcase", args -> {
            if (args.size() != 1) throw new EvalError("char-upcase: expected 1 arg");
            if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError("char-upcase: expected char");
            return new SchemeChar(Character.toUpperCase(c.value()));
        }));
        env.define("char-downcase", new Evaluator.BuiltinProc("char-downcase", args -> {
            if (args.size() != 1) throw new EvalError("char-downcase: expected 1 arg");
            if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError("char-downcase: expected char");
            return new SchemeChar(Character.toLowerCase(c.value()));
        }));
        env.define("char=?", new Evaluator.BuiltinProc("char=?", args -> {
            if (args.size() != 2) throw new EvalError("char=?: expected 2 args");
            if (!(args.get(0) instanceof SchemeChar a) || !(args.get(1) instanceof SchemeChar b))
                throw new EvalError("char=?: expected chars");
            return a.value() == b.value() ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("char<?", new Evaluator.BuiltinProc("char<?", args -> {
            if (args.size() != 2) throw new EvalError("char<?: expected 2 args");
            if (!(args.get(0) instanceof SchemeChar a) || !(args.get(1) instanceof SchemeChar b))
                throw new EvalError("char<?: expected chars");
            return a.value() < b.value() ? Boolean.TRUE : Boolean.FALSE;
        }));
    }

    private static void registerStringComparisons(Evaluator.Env env) {
        env.define("string=?", new Evaluator.BuiltinProc("string=?", args -> {
            if (args.size() != 2) throw new EvalError("string=?: expected 2 args");
            if (!(args.get(0) instanceof SchemeString a) || !(args.get(1) instanceof SchemeString b))
                throw new EvalError("string=?: expected strings");
            return a.value().equals(b.value()) ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("string<?", new Evaluator.BuiltinProc("string<?", args -> {
            if (args.size() != 2) throw new EvalError("string<?: expected 2 args");
            if (!(args.get(0) instanceof SchemeString a) || !(args.get(1) instanceof SchemeString b))
                throw new EvalError("string<?: expected strings");
            return a.value().compareTo(b.value()) < 0 ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("string-ci=?", new Evaluator.BuiltinProc("string-ci=?", args -> {
            if (args.size() != 2) throw new EvalError("string-ci=?: expected 2 args");
            if (!(args.get(0) instanceof SchemeString a) || !(args.get(1) instanceof SchemeString b))
                throw new EvalError("string-ci=?: expected strings");
            return a.value().equalsIgnoreCase(b.value()) ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("string-upcase", new Evaluator.BuiltinProc("string-upcase", args -> {
            if (args.size() != 1) throw new EvalError("string-upcase: expected 1 arg");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-upcase: expected string");
            return new SchemeString(s.value().toUpperCase());
        }));
        env.define("string-downcase", new Evaluator.BuiltinProc("string-downcase", args -> {
            if (args.size() != 1) throw new EvalError("string-downcase: expected 1 arg");
            if (!(args.get(0) instanceof SchemeString s)) throw new EvalError("string-downcase: expected string");
            return new SchemeString(s.value().toLowerCase());
        }));
    }

    private static void registerHigherOrder(Evaluator.Env env, ProcApplier applier) {
        env.define("eq?", new Evaluator.BuiltinProc("eq?", args -> {
            if (args.size() != 2) throw new EvalError("eq?: expected 2 args");
            Object a = args.get(0), b = args.get(1);
            if (a == b) return Boolean.TRUE;
            if (a instanceof Long && b instanceof Long) return a.equals(b) ? Boolean.TRUE : Boolean.FALSE;
            if (a instanceof Boolean && b instanceof Boolean) return a.equals(b) ? Boolean.TRUE : Boolean.FALSE;
            if (a instanceof SchemeChar && b instanceof SchemeChar) return a.equals(b) ? Boolean.TRUE : Boolean.FALSE;
            if (a instanceof String && b instanceof String) return a.equals(b) ? Boolean.TRUE : Boolean.FALSE;
            return Boolean.FALSE;
        }));
        env.define("eqv?", new Evaluator.BuiltinProc("eqv?", args -> {
            if (args.size() != 2) throw new EvalError("eqv?: expected 2 args");
            Object a = args.get(0), b = args.get(1);
            if (a == b) return Boolean.TRUE;
            if (a instanceof Long && b instanceof Long) return a.equals(b) ? Boolean.TRUE : Boolean.FALSE;
            if (a instanceof Double && b instanceof Double) return a.equals(b) ? Boolean.TRUE : Boolean.FALSE;
            if (a instanceof Rational && b instanceof Rational) return a.equals(b) ? Boolean.TRUE : Boolean.FALSE;
            if (a instanceof Boolean && b instanceof Boolean) return a.equals(b) ? Boolean.TRUE : Boolean.FALSE;
            if (a instanceof SchemeChar && b instanceof SchemeChar) return a.equals(b) ? Boolean.TRUE : Boolean.FALSE;
            if (a instanceof String && b instanceof String) return a.equals(b) ? Boolean.TRUE : Boolean.FALSE;
            return Boolean.FALSE;
        }));
        env.define("equal?", new Evaluator.BuiltinProc("equal?", args -> {
            if (args.size() != 2) throw new EvalError("equal?: expected 2 args");
            return Evaluator.schemeEqual(args.get(0), args.get(1)) ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("map", new Evaluator.BuiltinProc("map", args -> {
            if (args.size() < 2) throw new EvalError("map: expected at least 2 args");
            Object proc = args.get(0);
            int numLists = args.size() - 1;
            Object[] currents = new Object[numLists];
            for (int i = 0; i < numLists; i++) currents[i] = args.get(i + 1);
            List<Object> results = new ArrayList<>();
            while (true) {
                boolean allPairs = true;
                for (Object c : currents) {
                    if (!(c instanceof Evaluator.Pair)) { allPairs = false; break; }
                }
                if (!allPairs) break;
                List<Object> callArgs = new ArrayList<>();
                for (int i = 0; i < numLists; i++) {
                    callArgs.add(((Evaluator.Pair) currents[i]).car);
                    currents[i] = ((Evaluator.Pair) currents[i]).cdr;
                }
                results.add(applier.apply(proc, callArgs));
            }
            Object result = Evaluator.EMPTY_LIST;
            for (int i = results.size() - 1; i >= 0; i--) result = new Evaluator.Pair(results.get(i), result);
            return result;
        }));
        env.define("apply", new Evaluator.BuiltinProc("apply", args -> {
            if (args.size() < 2) throw new EvalError("apply: expected at least 2 args");
            Object proc = args.get(0);
            Object lastArg = args.get(args.size() - 1);
            List<Object> callArgs = new ArrayList<>();
            for (int i = 1; i < args.size() - 1; i++) {
                callArgs.add(args.get(i));
            }
            Object rest = lastArg;
            while (rest instanceof Evaluator.Pair p) {
                callArgs.add(p.car);
                rest = p.cdr;
            }
            return applier.apply(proc, callArgs);
        }));
    }

    private static void registerRationals(Evaluator.Env env) {
        env.define("exact?", new Evaluator.BuiltinProc("exact?", args -> {
            if (args.size() != 1) throw new EvalError("exact?: expected 1 arg");
            Object a = args.get(0);
            return (a instanceof Long || a instanceof Rational) ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("inexact?", new Evaluator.BuiltinProc("inexact?", args -> {
            if (args.size() != 1) throw new EvalError("inexact?: expected 1 arg");
            return args.get(0) instanceof Double ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("exact->inexact", new Evaluator.BuiltinProc("exact->inexact", args -> {
            if (args.size() != 1) throw new EvalError("exact->inexact: expected 1 arg");
            return toDouble(args.get(0));
        }));
        env.define("inexact->exact", new Evaluator.BuiltinProc("inexact->exact", args -> {
            if (args.size() != 1) throw new EvalError("inexact->exact: expected 1 arg");
            Object a = args.get(0);
            if (a instanceof Long || a instanceof Rational) return a;
            if (a instanceof Double d) {
                long denom = 1;
                double val = d;
                while (val != Math.floor(val) && denom < 1000000000L) {
                    val *= 10;
                    denom *= 10;
                }
                long num = Math.round(d * denom);
                Rational r = new Rational(num, denom);
                return r.isInteger() ? r.toLong() : r;
            }
            throw new EvalError("inexact->exact: expected number");
        }));
        env.define("integer?", new Evaluator.BuiltinProc("integer?", args -> {
            if (args.size() != 1) throw new EvalError("integer?: expected 1 arg");
            Object a = args.get(0);
            if (a instanceof Long) return Boolean.TRUE;
            if (a instanceof Rational r) return r.isInteger() ? Boolean.TRUE : Boolean.FALSE;
            if (a instanceof Double d) return (d == Math.floor(d) && !Double.isInfinite(d)) ? Boolean.TRUE : Boolean.FALSE;
            return Boolean.FALSE;
        }));
        env.define("rational?", new Evaluator.BuiltinProc("rational?", args -> {
            if (args.size() != 1) throw new EvalError("rational?: expected 1 arg");
            Object a = args.get(0);
            return (a instanceof Long || a instanceof Rational) ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("numerator", new Evaluator.BuiltinProc("numerator", args -> {
            if (args.size() != 1) throw new EvalError("numerator: expected 1 arg");
            Object a = args.get(0);
            if (a instanceof Long l) return l;
            if (a instanceof Rational r) return r.num;
            throw new EvalError("numerator: expected rational");
        }));
        env.define("denominator", new Evaluator.BuiltinProc("denominator", args -> {
            if (args.size() != 1) throw new EvalError("denominator: expected 1 arg");
            Object a = args.get(0);
            if (a instanceof Long) return 1L;
            if (a instanceof Rational r) return r.den;
            throw new EvalError("denominator: expected rational");
        }));
    }

    private static void registerMutation(Evaluator.Env env, ProcApplier applier) {
        env.define("set-car!", new Evaluator.BuiltinProc("set-car!", args -> {
            if (args.size() != 2) throw new EvalError("set-car!: expected 2 args");
            if (!(args.get(0) instanceof Evaluator.Pair p)) throw new EvalError("set-car!: not a pair");
            p.car = args.get(1);
            return Boolean.FALSE;
        }));
        env.define("set-cdr!", new Evaluator.BuiltinProc("set-cdr!", args -> {
            if (args.size() != 2) throw new EvalError("set-cdr!: expected 2 args");
            if (!(args.get(0) instanceof Evaluator.Pair p)) throw new EvalError("set-cdr!: not a pair");
            p.cdr = args.get(1);
            return Boolean.FALSE;
        }));
        env.define("for-each", new Evaluator.BuiltinProc("for-each", args -> {
            if (args.size() < 2) throw new EvalError("for-each: expected at least 2 args");
            Object proc = args.get(0);
            int numLists = args.size() - 1;
            Object[] currents = new Object[numLists];
            for (int i = 0; i < numLists; i++) currents[i] = args.get(i + 1);
            while (true) {
                boolean allPairs = true;
                for (Object c : currents) {
                    if (!(c instanceof Evaluator.Pair)) { allPairs = false; break; }
                }
                if (!allPairs) break;
                List<Object> callArgs = new ArrayList<>();
                for (int i = 0; i < numLists; i++) {
                    callArgs.add(((Evaluator.Pair) currents[i]).car);
                    currents[i] = ((Evaluator.Pair) currents[i]).cdr;
                }
                applier.apply(proc, callArgs);
            }
            return Boolean.FALSE;
        }));
        env.define("error", new Evaluator.BuiltinProc("error", args -> {
            if (args.isEmpty()) throw new EvalError("error");
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < args.size(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(SchemeFormatter.displayString(args.get(i)));
            }
            throw new EvalError(sb.toString());
        }));
        env.define("memq", new Evaluator.BuiltinProc("memq", args -> {
            if (args.size() != 2) throw new EvalError("memq: expected 2 args");
            Object key = args.get(0);
            Object lst = args.get(1);
            while (lst instanceof Evaluator.Pair p) {
                if (key == p.car || (key instanceof Long && key.equals(p.car))
                    || (key instanceof Boolean && key.equals(p.car))
                    || (key instanceof SchemeChar && key.equals(p.car))
                    || (key instanceof String && key.equals(p.car))) return lst;
                lst = p.cdr;
            }
            return Boolean.FALSE;
        }));
        env.define("memv", new Evaluator.BuiltinProc("memv", args -> {
            if (args.size() != 2) throw new EvalError("memv: expected 2 args");
            Object key = args.get(0);
            Object lst = args.get(1);
            while (lst instanceof Evaluator.Pair p) {
                if (Evaluator.schemeEqvStatic(key, p.car)) return lst;
                lst = p.cdr;
            }
            return Boolean.FALSE;
        }));
        env.define("assq", new Evaluator.BuiltinProc("assq", args -> {
            if (args.size() != 2) throw new EvalError("assq: expected 2 args");
            Object key = args.get(0);
            Object alist = args.get(1);
            while (alist instanceof Evaluator.Pair p) {
                if (p.car instanceof Evaluator.Pair entry) {
                    if (key == entry.car || (key instanceof Long && key.equals(entry.car))
                        || (key instanceof Boolean && key.equals(entry.car))
                        || (key instanceof SchemeChar && key.equals(entry.car))
                        || (key instanceof String && key.equals(entry.car))) return entry;
                }
                alist = p.cdr;
            }
            return Boolean.FALSE;
        }));
        env.define("assv", new Evaluator.BuiltinProc("assv", args -> {
            if (args.size() != 2) throw new EvalError("assv: expected 2 args");
            Object key = args.get(0);
            Object alist = args.get(1);
            while (alist instanceof Evaluator.Pair p) {
                if (p.car instanceof Evaluator.Pair entry) {
                    if (Evaluator.schemeEqvStatic(key, entry.car)) return entry;
                }
                alist = p.cdr;
            }
            return Boolean.FALSE;
        }));
        env.define("gcd", new Evaluator.BuiltinProc("gcd", args -> {
            if (args.size() == 0) return 0L;
            long result = Math.abs(asLong(args.get(0)));
            for (int i = 1; i < args.size(); i++) {
                long b = Math.abs(asLong(args.get(i)));
                while (b != 0) { long t = b; b = result % b; result = t; }
            }
            return result;
        }));
        env.define("lcm", new Evaluator.BuiltinProc("lcm", args -> {
            if (args.size() == 0) return 1L;
            long result = Math.abs(asLong(args.get(0)));
            for (int i = 1; i < args.size(); i++) {
                long b = Math.abs(asLong(args.get(i)));
                if (result == 0 && b == 0) { result = 0; } else {
                    long g = result; long t = b;
                    while (t != 0) { long tmp = t; t = g % t; g = tmp; }
                    result = (result / g) * b;
                }
            }
            return result;
        }));
        env.define("truncate", new Evaluator.BuiltinProc("truncate", args -> {
            if (args.size() != 1) throw new EvalError("truncate: expected 1 arg");
            Object a = args.get(0);
            if (a instanceof Long) return a;
            if (a instanceof Double d) return (long) d.doubleValue();
            if (a instanceof Rational r) return r.toLong();
            throw new EvalError("truncate: expected number");
        }));
        env.define("round", new Evaluator.BuiltinProc("round", args -> {
            if (args.size() != 1) throw new EvalError("round: expected 1 arg");
            Object a = args.get(0);
            if (a instanceof Long) return a;
            if (a instanceof Double d) return Math.round(d);
            if (a instanceof Rational r) return Math.round(r.toDouble());
            throw new EvalError("round: expected number");
        }));
        env.define("make-string", new Evaluator.BuiltinProc("make-string", args -> {
            if (args.size() < 1 || args.size() > 2) throw new EvalError("make-string: expected 1-2 args");
            int len = (int) asLong(args.get(0));
            char fill = args.size() == 2 && args.get(1) instanceof SchemeChar c ? c.value() : '\0';
            char[] chars = new char[len];
            java.util.Arrays.fill(chars, fill);
            return new SchemeString(new String(chars));
        }));
        env.define("string", new Evaluator.BuiltinProc("string", args -> {
            char[] chars = new char[args.size()];
            for (int i = 0; i < args.size(); i++) {
                if (!(args.get(i) instanceof SchemeChar c)) throw new EvalError("string: expected char");
                chars[i] = c.value();
            }
            return new SchemeString(new String(chars));
        }));
        env.define("string>?", new Evaluator.BuiltinProc("string>?", args -> {
            if (args.size() != 2) throw new EvalError("string>?: expected 2 args");
            if (!(args.get(0) instanceof SchemeString a) || !(args.get(1) instanceof SchemeString b))
                throw new EvalError("string>?: expected strings");
            return a.value().compareTo(b.value()) > 0 ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("string<=?", new Evaluator.BuiltinProc("string<=?", args -> {
            if (args.size() != 2) throw new EvalError("string<=?: expected 2 args");
            if (!(args.get(0) instanceof SchemeString a) || !(args.get(1) instanceof SchemeString b))
                throw new EvalError("string<=?: expected strings");
            return a.value().compareTo(b.value()) <= 0 ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("string>=?", new Evaluator.BuiltinProc("string>=?", args -> {
            if (args.size() != 2) throw new EvalError("string>=?: expected 2 args");
            if (!(args.get(0) instanceof SchemeString a) || !(args.get(1) instanceof SchemeString b))
                throw new EvalError("string>=?: expected strings");
            return a.value().compareTo(b.value()) >= 0 ? Boolean.TRUE : Boolean.FALSE;
        }));
    }

    private static void registerVectors(Evaluator.Env env) {
        env.define("vector", new Evaluator.BuiltinProc("vector", args -> {
            return new SchemeVector(args.toArray());
        }));
        env.define("make-vector", new Evaluator.BuiltinProc("make-vector", args -> {
            if (args.size() < 1 || args.size() > 2) throw new EvalError("make-vector: expected 1-2 args");
            int size = (int) asLong(args.get(0));
            Object fill = args.size() == 2 ? args.get(1) : 0L;
            return new SchemeVector(size, fill);
        }));
        env.define("vector-ref", new Evaluator.BuiltinProc("vector-ref", args -> {
            if (args.size() != 2) throw new EvalError("vector-ref: expected 2 args");
            if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector-ref: expected vector");
            return v.ref((int) asLong(args.get(1)));
        }));
        env.define("vector-set!", new Evaluator.BuiltinProc("vector-set!", args -> {
            if (args.size() != 3) throw new EvalError("vector-set!: expected 3 args");
            if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector-set!: expected vector");
            v.set((int) asLong(args.get(1)), args.get(2));
            return Boolean.FALSE;
        }));
        env.define("vector-length", new Evaluator.BuiltinProc("vector-length", args -> {
            if (args.size() != 1) throw new EvalError("vector-length: expected 1 arg");
            if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector-length: expected vector");
            return (long) v.length();
        }));
        env.define("vector?", new Evaluator.BuiltinProc("vector?", args -> {
            if (args.size() != 1) throw new EvalError("vector?: expected 1 arg");
            return args.get(0) instanceof SchemeVector ? Boolean.TRUE : Boolean.FALSE;
        }));
        env.define("vector->list", new Evaluator.BuiltinProc("vector->list", args -> {
            if (args.size() != 1) throw new EvalError("vector->list: expected 1 arg");
            if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector->list: expected vector");
            Object result = Evaluator.EMPTY_LIST;
            for (int i = v.length() - 1; i >= 0; i--) {
                result = new Evaluator.Pair(v.ref(i), result);
            }
            return result;
        }));
        env.define("list->vector", new Evaluator.BuiltinProc("list->vector", args -> {
            if (args.size() != 1) throw new EvalError("list->vector: expected 1 arg");
            List<Object> elems = new ArrayList<>();
            Object lst = args.get(0);
            while (lst instanceof Evaluator.Pair p) {
                elems.add(p.car);
                lst = p.cdr;
            }
            return new SchemeVector(elems.toArray());
        }));
    }
}
