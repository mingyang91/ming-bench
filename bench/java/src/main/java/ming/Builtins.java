package ming;

import java.util.ArrayList;
import java.util.List;

import ming.Evaluator.Builtin;
import ming.Evaluator.Cons;
import ming.Evaluator.Env;
import ming.Evaluator.SchemeChar;
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
            long sum = 0;
            for (Object a : args) sum += requireLong(a);
            return sum;
        });
        define("-", args -> {
            if (args.isEmpty()) throw new EvalError("- requires at least 1 argument");
            if (args.size() == 1) return -requireLong(args.get(0));
            long result = requireLong(args.get(0));
            for (int i = 1; i < args.size(); i++) result -= requireLong(args.get(i));
            return result;
        });
        define("*", args -> {
            long product = 1;
            for (Object a : args) product *= requireLong(a);
            return product;
        });
        define("/", args -> {
            if (args.size() < 2) throw new EvalError("/ requires at least 2 arguments");
            long result = requireLong(args.get(0));
            for (int i = 1; i < args.size(); i++) {
                long divisor = requireLong(args.get(i));
                if (divisor == 0) throw new EvalError("division by zero");
                result /= divisor;
            }
            return result;
        });
    }

    // --- Comparison ---

    private void registerComparison() {
        define("<", args -> {
            requireArgCount(args, 2, "<");
            return requireLong(args.get(0)) < requireLong(args.get(1));
        });
        define(">", args -> {
            requireArgCount(args, 2, ">");
            return requireLong(args.get(0)) > requireLong(args.get(1));
        });
        define("=", args -> {
            requireArgCount(args, 2, "=");
            return requireLong(args.get(0)) == requireLong(args.get(1));
        });
        define("<=", args -> {
            requireArgCount(args, 2, "<=");
            return requireLong(args.get(0)) <= requireLong(args.get(1));
        });
        define(">=", args -> {
            requireArgCount(args, 2, ">=");
            return requireLong(args.get(0)) >= requireLong(args.get(1));
        });
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
        define("number?", args -> { requireArgCount(args, 1, "number?"); return args.get(0) instanceof Long; });
        define("string?", args -> { requireArgCount(args, 1, "string?"); return args.get(0) instanceof SchemeString; });
        define("boolean?", args -> { requireArgCount(args, 1, "boolean?"); return args.get(0) instanceof Boolean; });
        define("pair?", args -> { requireArgCount(args, 1, "pair?"); return args.get(0) instanceof Cons; });
        define("symbol?", args -> { requireArgCount(args, 1, "symbol?"); return args.get(0) instanceof String; });
        define("char?", args -> { requireArgCount(args, 1, "char?"); return args.get(0) instanceof SchemeChar; });
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
            return new SchemeString(String.valueOf(requireLong(args.get(0))));
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
        define("abs", args -> { requireArgCount(args, 1, "abs"); return Math.abs(requireLong(args.get(0))); });
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
            long result = requireLong(args.get(0));
            for (int i = 1; i < args.size(); i++) result = Math.min(result, requireLong(args.get(i)));
            return result;
        });
        define("max", args -> {
            if (args.isEmpty()) throw new EvalError("max requires at least 1 argument");
            long result = requireLong(args.get(0));
            for (int i = 1; i < args.size(); i++) result = Math.max(result, requireLong(args.get(i)));
            return result;
        });
        define("expt", args -> {
            requireArgCount(args, 2, "expt");
            long base = requireLong(args.get(0)), exp = requireLong(args.get(1));
            long result = 1;
            for (long i = 0; i < exp; i++) result *= base;
            return result;
        });
        define("zero?", args -> { requireArgCount(args, 1, "zero?"); return requireLong(args.get(0)) == 0; });
        define("positive?", args -> { requireArgCount(args, 1, "positive?"); return requireLong(args.get(0)) > 0; });
        define("negative?", args -> { requireArgCount(args, 1, "negative?"); return requireLong(args.get(0)) < 0; });
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
}
