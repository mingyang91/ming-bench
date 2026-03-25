package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class Evaluator {

    // --- Source position tracking ---

    private record Token(Object value, int line, int col) {}

    private record SourceExpr(Object expr, int line, int col) {}

    // --- Environment ---

    private static class Env {
        final Map<String, Object> bindings = new HashMap<>();
        final Env parent;

        Env(Env parent) {
            this.parent = parent;
        }

        Object lookup(String name) throws EvalError {
            if (bindings.containsKey(name)) return bindings.get(name);
            if (parent != null) return parent.lookup(name);
            throw new EvalError("unbound variable: " + name);
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
            throw new EvalError("unbound variable: " + name);
        }
    }

    // --- Lambda (closure) ---

    private record Lambda(List<String> params, String restParam, List<Object> body, Env closureEnv) {}

    // --- case-lambda (multiple-arity dispatch) ---

    private static class CaseLambda {
        final List<Lambda> clauses;
        CaseLambda(List<Lambda> clauses) { this.clauses = clauses; }
    }

    // Sentinel for void (define returns this)
    private static final Object VOID = new Object() {
        @Override public String toString() { return "#<void>"; }
    };

    // --- Scheme Pair (cons cell) ---

    private static class Pair {
        Object car;
        Object cdr;
        Pair(Object car, Object cdr) { this.car = car; this.cdr = cdr; }
    }

    // --- Syntax-rules macro ---

    private static class SyntaxRulesMacro {
        final List<String> literals;
        final List<Object[]> rules;
        final Env defEnv;
        SyntaxRulesMacro(List<String> literals, List<Object[]> rules, Env defEnv) {
            this.literals = literals;
            this.rules = rules;
            this.defEnv = defEnv;
        }
    }

    private static class ResolvedValue {
        final Object value;
        ResolvedValue(Object value) { this.value = value; }
    }

    // --- Vector type ---

    private static class SchemeVector {
        final Object[] elements;
        SchemeVector(Object[] elements) { this.elements = elements; }
    }

    // --- Record types (define-record-type) ---

    private static class RecordType {
        final String name;
        final List<String> fieldNames;
        RecordType(String name, List<String> fieldNames) {
            this.name = name;
            this.fieldNames = fieldNames;
        }
    }

    private static class RecordInstance {
        final RecordType type;
        final Object[] fields;
        RecordInstance(RecordType type, Object[] fields) {
            this.type = type;
            this.fields = fields;
        }
    }

    private static final Set<String> SPECIAL_FORMS = Set.of(
        "quote", "if", "define", "lambda", "let", "set!", "begin", "cond",
        "and", "or", "define-syntax", "syntax-rules", "else", "define-record-type",
        "letrec", "letrec*", "case", "do"
    );

    // Sentinel for empty list '()
    private static final Object NIL = new Object() {
        @Override public String toString() { return "()"; }
    };

    // --- Builtin procedure ---

    @FunctionalInterface
    private interface BuiltinProc {
        Object apply(List<Object> args) throws EvalError;
    }

    private final Env globalEnv = new Env(null);
    private StringBuilder outputBuffer = new StringBuilder();
    private int gensymCounter = 0;

    public Evaluator() {
        // Arithmetic
        globalEnv.define("+", (BuiltinProc) args -> {
            Object result = 0L;
            for (Object a : args) result = numAdd(result, a);
            return result;
        });
        globalEnv.define("-", (BuiltinProc) args -> {
            if (args.isEmpty()) throw new EvalError("- requires at least 1 argument");
            if (args.size() == 1) return numNeg(args.get(0));
            Object result = args.get(0);
            for (int i = 1; i < args.size(); i++) result = numSub(result, args.get(i));
            return result;
        });
        globalEnv.define("*", (BuiltinProc) args -> {
            Object result = 1L;
            for (Object a : args) result = numMul(result, a);
            return result;
        });
        globalEnv.define("/", (BuiltinProc) args -> {
            if (args.size() < 2) throw new EvalError("/ requires at least 2 arguments");
            Object result = args.get(0);
            for (int i = 1; i < args.size(); i++) result = numDiv(result, args.get(i));
            return result;
        });

        // Comparisons
        globalEnv.define("<", (BuiltinProc) args -> numCompare(args.get(0), args.get(1)) < 0);
        globalEnv.define(">", (BuiltinProc) args -> numCompare(args.get(0), args.get(1)) > 0);
        globalEnv.define("=", (BuiltinProc) args -> numCompare(args.get(0), args.get(1)) == 0);
        globalEnv.define("<=", (BuiltinProc) args -> numCompare(args.get(0), args.get(1)) <= 0);
        globalEnv.define(">=", (BuiltinProc) args -> numCompare(args.get(0), args.get(1)) >= 0);
        globalEnv.define("not", (BuiltinProc) args -> isFalse(args.get(0)));

        // List operations
        globalEnv.define("cons", (BuiltinProc) args -> new Pair(args.get(0), args.get(1)));
        globalEnv.define("car", (BuiltinProc) args -> {
            if (args.get(0) instanceof Pair p) return p.car;
            throw new EvalError("car: not a pair");
        });
        globalEnv.define("cdr", (BuiltinProc) args -> {
            if (args.get(0) instanceof Pair p) return p.cdr;
            throw new EvalError("cdr: not a pair");
        });
        globalEnv.define("null?", (BuiltinProc) args -> args.get(0) == NIL);
        globalEnv.define("list", (BuiltinProc) args -> {
            Object result = NIL;
            for (int i = args.size() - 1; i >= 0; i--) {
                result = new Pair(args.get(i), result);
            }
            return result;
        });
        globalEnv.define("length", (BuiltinProc) args -> {
            long count = 0;
            Object curr = args.get(0);
            while (curr instanceof Pair p) {
                count++;
                curr = p.cdr;
            }
            return count;
        });
        globalEnv.define("append", (BuiltinProc) args -> {
            if (args.isEmpty()) return NIL;
            if (args.size() == 1) return args.get(0);
            Object result = args.get(args.size() - 1);
            for (int i = args.size() - 2; i >= 0; i--) {
                result = appendTwo(args.get(i), result);
            }
            return result;
        });

        // Type predicates
        globalEnv.define("boolean?", (BuiltinProc) args -> args.get(0) instanceof Boolean);
        globalEnv.define("number?", (BuiltinProc) args -> isSchemeNumber(args.get(0)));
        globalEnv.define("pair?", (BuiltinProc) args -> args.get(0) instanceof Pair);
        globalEnv.define("string?", (BuiltinProc) args -> args.get(0) instanceof SchemeString);
        globalEnv.define("symbol?", (BuiltinProc) args -> args.get(0) instanceof String);
        globalEnv.define("char?", (BuiltinProc) args -> args.get(0) instanceof SchemeChar);
        globalEnv.define("procedure?", (BuiltinProc) args -> args.get(0) instanceof Lambda || args.get(0) instanceof BuiltinProc || args.get(0) instanceof CaseLambda);

        // I/O
        globalEnv.define("display", (BuiltinProc) args -> {
            outputBuffer.append(displayString(args.get(0)));
            return VOID;
        });
        globalEnv.define("write", (BuiltinProc) args -> {
            outputBuffer.append(schemeToString(args.get(0)));
            return VOID;
        });
        globalEnv.define("newline", (BuiltinProc) args -> {
            outputBuffer.append('\n');
            return VOID;
        });

        // String operations
        globalEnv.define("string-append", (BuiltinProc) args -> {
            StringBuilder sb = new StringBuilder();
            for (Object a : args) sb.append(asSchemeString(a).value());
            return new SchemeString(sb.toString());
        });
        globalEnv.define("string-length", (BuiltinProc) args -> (long) asSchemeString(args.get(0)).length());
        globalEnv.define("substring", (BuiltinProc) args -> {
            String s = asSchemeString(args.get(0)).value();
            int start = (int) asLong(args.get(1));
            int end = (int) asLong(args.get(2));
            return new SchemeString(s.substring(start, end));
        });
        globalEnv.define("string->number", (BuiltinProc) args -> {
            String s = asSchemeString(args.get(0)).value();
            Object n = parseNumber(s);
            return n != null ? n : Boolean.FALSE;
        });
        globalEnv.define("number->string", (BuiltinProc) args -> new SchemeString(numberToString(args.get(0))));

        // --- Level 11 builtins: exact arithmetic & rationals ---
        globalEnv.define("exact?", (BuiltinProc) args -> args.get(0) instanceof Long || args.get(0) instanceof SchemeRational);
        globalEnv.define("inexact?", (BuiltinProc) args -> args.get(0) instanceof Double);
        globalEnv.define("integer?", (BuiltinProc) args -> {
            Object v = args.get(0);
            if (v instanceof Long) return true;
            if (v instanceof Double d) return d == Math.floor(d) && !Double.isInfinite(d);
            return false; // SchemeRational with den != 1 already simplified, so not integer
        });
        globalEnv.define("rational?", (BuiltinProc) args -> {
            Object v = args.get(0);
            return v instanceof Long || v instanceof SchemeRational;
        });
        globalEnv.define("exact->inexact", (BuiltinProc) args -> toDouble(args.get(0)));
        globalEnv.define("inexact->exact", (BuiltinProc) args -> {
            Object v = args.get(0);
            if (v instanceof Long || v instanceof SchemeRational) return v;
            if (v instanceof Double d) {
                // Convert double to exact rational
                if (d == Math.floor(d) && !Double.isInfinite(d)) return (long) d.doubleValue();
                // Use continued fraction / multiply-out approach
                // For simple fractions like 0.5, 0.25 etc.
                long bits = Double.doubleToLongBits(d);
                int exponent = (int) ((bits >> 52) & 0x7FFL) - 1023;
                long mantissa = (bits & 0x000FFFFFFFFFFFFFL) | 0x0010000000000000L;
                boolean negative = (bits & 0x8000000000000000L) != 0;
                // d = mantissa * 2^(exponent - 52)
                int shift = exponent - 52;
                long num, den;
                if (shift >= 0) {
                    num = mantissa << shift;
                    den = 1;
                } else {
                    num = mantissa;
                    den = 1L << (-shift);
                }
                if (negative) num = -num;
                return makeRational(num, den);
            }
            throw new EvalError("inexact->exact: not a number");
        });
        globalEnv.define("numerator", (BuiltinProc) args -> {
            Object v = args.get(0);
            if (v instanceof Long l) return l;
            if (v instanceof SchemeRational r) return r.num;
            throw new EvalError("numerator: not an exact number");
        });
        globalEnv.define("denominator", (BuiltinProc) args -> {
            Object v = args.get(0);
            if (v instanceof Long) return 1L;
            if (v instanceof SchemeRational r) return r.den;
            throw new EvalError("denominator: not an exact number");
        });
        globalEnv.define("symbol->string", (BuiltinProc) args -> {
            if (args.get(0) instanceof String s) return new SchemeString(s);
            throw new EvalError("symbol->string: not a symbol");
        });
        globalEnv.define("string->symbol", (BuiltinProc) args -> asSchemeString(args.get(0)).value());
        globalEnv.define("string-ref", (BuiltinProc) args -> {
            SchemeString s = asSchemeString(args.get(0));
            int idx = (int) asLong(args.get(1));
            return new SchemeChar(s.charAt(idx));
        });
        globalEnv.define("string-copy", (BuiltinProc) args -> {
            return new SchemeString(asSchemeString(args.get(0)).value());
        });
        globalEnv.define("string-set!", (BuiltinProc) args -> {
            SchemeString s = asSchemeString(args.get(0));
            int idx = (int) asLong(args.get(1));
            if (!(args.get(2) instanceof SchemeChar ch)) throw new EvalError("string-set!: expected char");
            s.setChar(idx, ch.value());
            return VOID;
        });
        globalEnv.define("string->list", (BuiltinProc) args -> {
            SchemeString s = asSchemeString(args.get(0));
            Object result = NIL;
            for (int i = s.length() - 1; i >= 0; i--) {
                result = new Pair(new SchemeChar(s.charAt(i)), result);
            }
            return result;
        });
        globalEnv.define("list->string", (BuiltinProc) args -> {
            Object list = args.get(0);
            StringBuilder sb = new StringBuilder();
            while (list instanceof Pair p) {
                if (!(p.car instanceof SchemeChar ch)) throw new EvalError("list->string: expected char");
                sb.append(ch.value());
                list = p.cdr;
            }
            return new SchemeString(sb.toString());
        });
        globalEnv.define("char->integer", (BuiltinProc) args -> {
            if (!(args.get(0) instanceof SchemeChar ch)) throw new EvalError("char->integer: expected char");
            return (long) ch.value();
        });
        globalEnv.define("integer->char", (BuiltinProc) args -> {
            return new SchemeChar((char) asLong(args.get(0)));
        });
        // --- Level 9 builtins ---

        // Numeric utilities
        globalEnv.define("abs", (BuiltinProc) args -> Math.abs(asLong(args.get(0))));
        globalEnv.define("modulo", (BuiltinProc) args -> {
            long a = asLong(args.get(0)), b = asLong(args.get(1));
            return Math.floorMod(a, b);
        });
        globalEnv.define("remainder", (BuiltinProc) args -> {
            long a = asLong(args.get(0)), b = asLong(args.get(1));
            return a % b;
        });
        globalEnv.define("quotient", (BuiltinProc) args -> {
            long a = asLong(args.get(0)), b = asLong(args.get(1));
            // Truncate toward zero (Java default for /)
            return a / b;
        });
        globalEnv.define("min", (BuiltinProc) args -> {
            long result = asLong(args.get(0));
            for (int i = 1; i < args.size(); i++) result = Math.min(result, asLong(args.get(i)));
            return result;
        });
        globalEnv.define("max", (BuiltinProc) args -> {
            long result = asLong(args.get(0));
            for (int i = 1; i < args.size(); i++) result = Math.max(result, asLong(args.get(i)));
            return result;
        });
        globalEnv.define("expt", (BuiltinProc) args -> {
            long base = asLong(args.get(0)), exp = asLong(args.get(1));
            long result = 1;
            for (long i = 0; i < exp; i++) result *= base;
            return result;
        });
        globalEnv.define("zero?", (BuiltinProc) args -> asLong(args.get(0)) == 0);
        globalEnv.define("positive?", (BuiltinProc) args -> asLong(args.get(0)) > 0);
        globalEnv.define("negative?", (BuiltinProc) args -> asLong(args.get(0)) < 0);
        globalEnv.define("odd?", (BuiltinProc) args -> asLong(args.get(0)) % 2 != 0);
        globalEnv.define("even?", (BuiltinProc) args -> asLong(args.get(0)) % 2 == 0);

        // List utilities
        globalEnv.define("list?", (BuiltinProc) args -> {
            Object curr = args.get(0);
            while (curr instanceof Pair p) curr = p.cdr;
            return curr == NIL;
        });
        globalEnv.define("list-ref", (BuiltinProc) args -> {
            Object curr = args.get(0);
            long idx = asLong(args.get(1));
            for (long i = 0; i < idx; i++) {
                if (!(curr instanceof Pair p)) throw new EvalError("list-ref: index out of range");
                curr = p.cdr;
            }
            if (!(curr instanceof Pair p)) throw new EvalError("list-ref: index out of range");
            return p.car;
        });
        globalEnv.define("list-tail", (BuiltinProc) args -> {
            Object curr = args.get(0);
            long idx = asLong(args.get(1));
            for (long i = 0; i < idx; i++) {
                if (!(curr instanceof Pair p)) throw new EvalError("list-tail: index out of range");
                curr = p.cdr;
            }
            return curr;
        });
        globalEnv.define("assoc", (BuiltinProc) args -> {
            Object key = args.get(0);
            Object alist = args.get(1);
            while (alist instanceof Pair p) {
                if (p.car instanceof Pair entry && schemeEqual(key, entry.car)) return entry;
                alist = p.cdr;
            }
            return Boolean.FALSE;
        });

        // Equality
        globalEnv.define("eq?", (BuiltinProc) args -> schemeEq(args.get(0), args.get(1)));
        globalEnv.define("eqv?", (BuiltinProc) args -> schemeEq(args.get(0), args.get(1)));
        globalEnv.define("equal?", (BuiltinProc) args -> schemeEqual(args.get(0), args.get(1)));

        // Vector operations
        globalEnv.define("vector", (BuiltinProc) args -> new SchemeVector(args.toArray()));
        globalEnv.define("make-vector", (BuiltinProc) args -> {
            int len = (int) asLong(args.get(0));
            Object fill = args.size() > 1 ? args.get(1) : 0L;
            Object[] elts = new Object[len];
            java.util.Arrays.fill(elts, fill);
            return new SchemeVector(elts);
        });
        globalEnv.define("vector-ref", (BuiltinProc) args -> {
            if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector-ref: not a vector");
            return v.elements[(int) asLong(args.get(1))];
        });
        globalEnv.define("vector-set!", (BuiltinProc) args -> {
            if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector-set!: not a vector");
            v.elements[(int) asLong(args.get(1))] = args.get(2);
            return VOID;
        });
        globalEnv.define("vector-length", (BuiltinProc) args -> {
            if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector-length: not a vector");
            return (long) v.elements.length;
        });
        globalEnv.define("vector?", (BuiltinProc) args -> args.get(0) instanceof SchemeVector);
        globalEnv.define("vector->list", (BuiltinProc) args -> {
            if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector->list: not a vector");
            Object result = NIL;
            for (int i = v.elements.length - 1; i >= 0; i--) result = new Pair(v.elements[i], result);
            return result;
        });
        globalEnv.define("list->vector", (BuiltinProc) args -> {
            List<Object> elts = new ArrayList<>();
            Object curr = args.get(0);
            while (curr instanceof Pair p) { elts.add(p.car); curr = p.cdr; }
            return new SchemeVector(elts.toArray());
        });

        // Built-in map (supports multiple lists)
        globalEnv.define("map", (BuiltinProc) args -> {
            if (args.size() < 2) throw new EvalError("map: requires at least 2 arguments");
            Object proc = args.get(0);
            int numLists = args.size() - 1;
            // Convert all list args to arrays of Pair cursors
            Object[] cursors = new Object[numLists];
            for (int i = 0; i < numLists; i++) cursors[i] = args.get(i + 1);
            List<Object> results = new ArrayList<>();
            while (true) {
                // Check if any list is exhausted
                boolean done = false;
                for (Object c : cursors) { if (!(c instanceof Pair)) { done = true; break; } }
                if (done) break;
                List<Object> callArgs = new ArrayList<>();
                for (int i = 0; i < numLists; i++) {
                    Pair p = (Pair) cursors[i];
                    callArgs.add(p.car);
                    cursors[i] = p.cdr;
                }
                if (proc instanceof BuiltinProc builtin) {
                    results.add(builtin.apply(callArgs));
                } else if (proc instanceof Lambda lambda) {
                    results.add(applyLambda(lambda, callArgs));
                } else if (proc instanceof CaseLambda cl) {
                    results.add(applyCaseLambda(cl, callArgs));
                } else {
                    throw new EvalError("map: not a procedure");
                }
            }
            Object result = NIL;
            for (int i = results.size() - 1; i >= 0; i--) result = new Pair(results.get(i), result);
            return result;
        });

        // Character utilities
        globalEnv.define("char-alphabetic?", (BuiltinProc) args -> {
            if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError("char-alphabetic?: expected char");
            return Character.isLetter(c.value());
        });
        globalEnv.define("char-numeric?", (BuiltinProc) args -> {
            if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError("char-numeric?: expected char");
            return Character.isDigit(c.value());
        });
        globalEnv.define("char-upcase", (BuiltinProc) args -> {
            if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError("char-upcase: expected char");
            return new SchemeChar(Character.toUpperCase(c.value()));
        });
        globalEnv.define("char-downcase", (BuiltinProc) args -> {
            if (!(args.get(0) instanceof SchemeChar c)) throw new EvalError("char-downcase: expected char");
            return new SchemeChar(Character.toLowerCase(c.value()));
        });
        globalEnv.define("char=?", (BuiltinProc) args -> {
            if (!(args.get(0) instanceof SchemeChar a && args.get(1) instanceof SchemeChar b))
                throw new EvalError("char=?: expected chars");
            return a.value() == b.value();
        });
        globalEnv.define("char<?", (BuiltinProc) args -> {
            if (!(args.get(0) instanceof SchemeChar a && args.get(1) instanceof SchemeChar b))
                throw new EvalError("char<?: expected chars");
            return a.value() < b.value();
        });

        // String comparison utilities
        globalEnv.define("string=?", (BuiltinProc) args ->
                asSchemeString(args.get(0)).value().equals(asSchemeString(args.get(1)).value()));
        globalEnv.define("string<?", (BuiltinProc) args ->
                asSchemeString(args.get(0)).value().compareTo(asSchemeString(args.get(1)).value()) < 0);
        globalEnv.define("string-ci=?", (BuiltinProc) args ->
                asSchemeString(args.get(0)).value().equalsIgnoreCase(asSchemeString(args.get(1)).value()));
        globalEnv.define("string-upcase", (BuiltinProc) args ->
                new SchemeString(asSchemeString(args.get(0)).value().toUpperCase()));
        globalEnv.define("string-downcase", (BuiltinProc) args ->
                new SchemeString(asSchemeString(args.get(0)).value().toLowerCase()));

        globalEnv.define("apply", (BuiltinProc) args -> {
            if (args.size() < 2) throw new EvalError("apply: requires at least 2 arguments");
            Object proc = args.get(0);
            // Last argument must be a list; preceding args are prepended
            Object lastArg = args.get(args.size() - 1);
            List<Object> callArgs = new ArrayList<>();
            for (int i = 1; i < args.size() - 1; i++) {
                callArgs.add(args.get(i));
            }
            // Convert last arg (scheme list) to java list
            Object curr = lastArg;
            while (curr instanceof Pair p) {
                callArgs.add(p.car);
                curr = p.cdr;
            }
            if (proc instanceof BuiltinProc builtin) {
                return builtin.apply(callArgs);
            }
            if (proc instanceof Lambda lambda) {
                return applyLambda(lambda, callArgs);
            }
            if (proc instanceof CaseLambda cl) {
                return applyCaseLambda(cl, callArgs);
            }
            throw new EvalError("apply: not a procedure");
        });
    }

    private Object appendTwo(Object a, Object b) {
        if (a == NIL) return b;
        if (a instanceof Pair p) {
            return new Pair(p.car, appendTwo(p.cdr, b));
        }
        return b;
    }

    public String evalStr(String input) throws EvalError {
        List<Token> tokens = tokenize(input);
        int[] pos = {0};
        Object lastResult = null;
        while (pos[0] < tokens.size()) {
            Object expr = parse(tokens, pos);
            lastResult = eval(expr, globalEnv);
        }
        if (lastResult == null) {
            throw new EvalError("no expression");
        }
        if (lastResult == VOID) {
            throw new EvalError("no expression");
        }
        return schemeToString(lastResult);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        outputBuffer.setLength(0);
        List<Token> tokens = tokenize(input);
        int[] pos = {0};
        Object lastResult = null;
        while (pos[0] < tokens.size()) {
            Object expr = parse(tokens, pos);
            lastResult = eval(expr, globalEnv);
        }
        String result = (lastResult == null || lastResult == VOID) ? "" : schemeToString(lastResult);
        return new EvalResult(result, outputBuffer.toString());
    }

    // --- Tokenizer ---

    private List<Token> tokenize(String input) throws EvalError {
        List<Token> tokens = new ArrayList<>();
        int i = 0;
        int len = input.length();
        int line = 1;
        int col = 1;
        while (i < len) {
            char c = input.charAt(i);
            if (c == '\n') {
                i++;
                line++;
                col = 1;
            } else if (Character.isWhitespace(c)) {
                i++;
                col++;
            } else if (c == ';') {
                while (i < len && input.charAt(i) != '\n') { i++; col++; }
            } else if (c == '\'') {
                tokens.add(new Token("'", line, col));
                i++;
                col++;
            } else if (c == '(') {
                tokens.add(new Token("(", line, col));
                i++;
                col++;
            } else if (c == ')') {
                tokens.add(new Token(")", line, col));
                i++;
                col++;
            } else if (c == '"') {
                int startCol = col;
                StringBuilder sb = new StringBuilder();
                i++;
                col++;
                while (i < len && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\') {
                        i++;
                        col++;
                        if (i < len) {
                            char esc = input.charAt(i);
                            switch (esc) {
                                case 'n' -> sb.append('\n');
                                case 't' -> sb.append('\t');
                                case '\\' -> sb.append('\\');
                                case '"' -> sb.append('"');
                                default -> { sb.append('\\'); sb.append(esc); }
                            }
                        }
                    } else {
                        if (input.charAt(i) == '\n') {
                            sb.append('\n');
                            line++;
                            col = 0;
                        } else {
                            sb.append(input.charAt(i));
                        }
                    }
                    i++;
                    col++;
                }
                if (i >= len) throw new EvalError("unterminated string at " + line + ":" + startCol);
                i++;
                col++;
                tokens.add(new Token(new SchemeString(sb.toString(), true), line, startCol));
            } else if (c == '#') {
                int startCol = col;
                if (i + 1 < len) {
                    char next = input.charAt(i + 1);
                    if (next == 't') {
                        tokens.add(new Token(Boolean.TRUE, line, startCol));
                        i += 2;
                        col += 2;
                    } else if (next == 'f') {
                        tokens.add(new Token(Boolean.FALSE, line, startCol));
                        i += 2;
                        col += 2;
                    } else if (next == '\\') {
                        i += 2;
                        col += 2;
                        if (i >= len) throw new EvalError("unexpected end after #\\ at " + line + ":" + col);
                        // Read character name or single char
                        int nameStart = i;
                        while (i < len && !Character.isWhitespace(input.charAt(i))
                                && input.charAt(i) != '(' && input.charAt(i) != ')'
                                && input.charAt(i) != '"' && input.charAt(i) != ';') {
                            i++;
                            col++;
                        }
                        String charName = input.substring(nameStart, i);
                        char ch;
                        switch (charName) {
                            case "space" -> ch = ' ';
                            case "newline" -> ch = '\n';
                            case "tab" -> ch = '\t';
                            default -> {
                                if (charName.length() == 1) ch = charName.charAt(0);
                                else throw new EvalError("unknown character name: " + charName + " at " + line + ":" + startCol);
                            }
                        }
                        tokens.add(new Token(new SchemeChar(ch), line, startCol));
                    } else {
                        throw new EvalError("unexpected token: #" + next + " at " + line + ":" + col);
                    }
                } else {
                    throw new EvalError("unexpected end after # at " + line + ":" + col);
                }
            } else {
                int startCol = col;
                StringBuilder sb = new StringBuilder();
                while (i < len && !Character.isWhitespace(input.charAt(i))
                        && input.charAt(i) != '(' && input.charAt(i) != ')'
                        && input.charAt(i) != '"' && input.charAt(i) != ';'
                        && input.charAt(i) != '\'') {
                    sb.append(input.charAt(i));
                    i++;
                    col++;
                }
                String tok = sb.toString();
                Object parsed = parseNumber(tok);
                tokens.add(new Token(parsed != null ? parsed : tok, line, startCol));
            }
        }
        return tokens;
    }

    // --- Parser ---

    private Object parse(List<Token> tokens, int[] pos) throws EvalError {
        if (pos[0] >= tokens.size()) {
            throw new EvalError("unexpected end of input");
        }
        Token token = tokens.get(pos[0]);
        if ("'".equals(token.value)) {
            pos[0]++;
            Object quoted = parse(tokens, pos);
            List<Object> quoteExpr = new ArrayList<>();
            quoteExpr.add("quote");
            quoteExpr.add(quoted);
            return new SourceExpr(quoteExpr, token.line, token.col);
        }
        if ("(".equals(token.value)) {
            pos[0]++;
            List<Object> list = new ArrayList<>();
            while (pos[0] < tokens.size() && !")".equals(tokens.get(pos[0]).value)) {
                list.add(parse(tokens, pos));
            }
            if (pos[0] >= tokens.size()) {
                throw new EvalError("missing closing parenthesis at " + token.line + ":" + token.col);
            }
            pos[0]++;
            return new SourceExpr(list, token.line, token.col);
        } else if (")".equals(token.value)) {
            throw new EvalError("unexpected ) at " + token.line + ":" + token.col);
        } else {
            pos[0]++;
            return new SourceExpr(token.value, token.line, token.col);
        }
    }

    // Convert parsed AST list to Scheme cons-cell list (for quote)
    private Object astToScheme(Object ast) {
        if (ast instanceof SourceExpr se) {
            return astToScheme(se.expr);
        }
        if (ast instanceof List<?> list) {
            Object result = NIL;
            for (int i = list.size() - 1; i >= 0; i--) {
                result = new Pair(astToScheme(list.get(i)), result);
            }
            return result;
        }
        return ast;
    }

    // --- Evaluator ---

    @SuppressWarnings("unchecked")
    private Object eval(Object expr, Env env) throws EvalError {
        trampolineLoop:
        while (true) {
        // Unwrap SourceExpr - just record position and continue the loop
        int srcLine = -1, srcCol = -1;
        while (expr instanceof SourceExpr se) {
            srcLine = se.line;
            srcCol = se.col;
            expr = se.expr;
        }
        try {

        if (expr instanceof ResolvedValue rv) {
            return rv.value;
        }
        if (expr instanceof Long || expr instanceof Double || expr instanceof SchemeRational || expr instanceof Boolean || expr instanceof SchemeString || expr instanceof SchemeChar) {
            return expr;
        }
        if (expr instanceof String sym) {
            return env.lookup(sym);
        }
        if (expr instanceof List<?> rawList) {
            List<Object> list = (List<Object>) rawList;
            if (list.isEmpty()) {
                throw new EvalError("empty application");
            }
            Object head = list.get(0);

            // Unwrap SourceExpr for head to check special forms
            Object rawHead = head;
            if (rawHead instanceof SourceExpr she) {
                rawHead = she.expr;
            }

            // Special forms
            if (rawHead instanceof String op) {
                switch (op) {
                    case "quote" -> {
                        if (list.size() < 2) throw new EvalError("bad syntax: quote");
                        return astToScheme(list.get(1));
                    }
                    case "if" -> {
                        if (list.size() < 3) throw new EvalError("bad syntax: if requires at least 2 parts");
                        Object cond = eval(list.get(1), env);
                        if (!isFalse(cond)) {
                            expr = list.get(2);
                            continue trampolineLoop;
                        } else if (list.size() > 3) {
                            expr = list.get(3);
                            continue trampolineLoop;
                        }
                        return VOID;
                    }
                    case "define" -> {
                        if (list.size() < 2) throw new EvalError("bad syntax: define");
                        Object target = list.get(1);
                        if (target instanceof SourceExpr se) target = se.expr;
                        if (target instanceof String name) {
                            if (list.size() < 3) throw new EvalError("bad syntax: define");
                            env.define(name, eval(list.get(2), env));
                        } else if (target instanceof List<?> sig) {
                            if (sig.isEmpty()) throw new EvalError("bad syntax: define");
                            Object nameObj = sig.get(0);
                            if (nameObj instanceof SourceExpr se2) nameObj = se2.expr;
                            String name = (String) nameObj;
                            List<String> params = new ArrayList<>();
                            String restParam = null;
                            for (int i = 1; i < sig.size(); i++) {
                                Object p = sig.get(i);
                                if (p instanceof SourceExpr se2) p = se2.expr;
                                if (".".equals(p)) {
                                    if (i + 1 >= sig.size()) throw new EvalError("bad syntax: define");
                                    Object rp = sig.get(i + 1);
                                    if (rp instanceof SourceExpr se3) rp = se3.expr;
                                    restParam = (String) rp;
                                    break;
                                }
                                params.add((String) p);
                            }
                            List<Object> body = new ArrayList<>(list.subList(2, list.size()));
                            env.define(name, new Lambda(params, restParam, body, env));
                        } else {
                            throw new EvalError("bad syntax: define");
                        }
                        return VOID;
                    }
                    case "lambda" -> {
                        if (list.size() < 3) throw new EvalError("bad syntax: lambda");
                        Object paramObj = list.get(1);
                        if (paramObj instanceof SourceExpr se) paramObj = se.expr;
                        List<Object> body = new ArrayList<>(list.subList(2, list.size()));
                        if (paramObj instanceof String restOnly) {
                            // (lambda args body) - single rest param
                            return new Lambda(List.of(), restOnly, body, env);
                        }
                        List<?> paramList = (List<?>) paramObj;
                        List<String> params = new ArrayList<>();
                        String restParam = null;
                        for (int i = 0; i < paramList.size(); i++) {
                            Object p = paramList.get(i);
                            if (p instanceof SourceExpr se) p = se.expr;
                            if (".".equals(p)) {
                                if (i + 1 >= paramList.size()) throw new EvalError("bad syntax: lambda");
                                Object rp = paramList.get(i + 1);
                                if (rp instanceof SourceExpr se2) rp = se2.expr;
                                restParam = (String) rp;
                                break;
                            }
                            params.add((String) p);
                        }
                        return new Lambda(params, restParam, body, env);
                    }
                    case "case-lambda" -> {
                        List<Lambda> clauses = new ArrayList<>();
                        for (int i = 1; i < list.size(); i++) {
                            Object clauseObj = list.get(i);
                            if (clauseObj instanceof SourceExpr se) clauseObj = se.expr;
                            List<?> clause = (List<?>) clauseObj;
                            Object cpObj = clause.get(0);
                            if (cpObj instanceof SourceExpr se) cpObj = se.expr;
                            List<?> cpList = (List<?>) cpObj;
                            List<String> cParams = new ArrayList<>();
                            String cRest = null;
                            for (int j = 0; j < cpList.size(); j++) {
                                Object p = cpList.get(j);
                                if (p instanceof SourceExpr se) p = se.expr;
                                if (".".equals(p)) {
                                    Object rp = cpList.get(j + 1);
                                    if (rp instanceof SourceExpr se) rp = se.expr;
                                    cRest = (String) rp;
                                    break;
                                }
                                cParams.add((String) p);
                            }
                            List<Object> cBody = new ArrayList<>(clause.subList(1, clause.size()));
                            clauses.add(new Lambda(cParams, cRest, cBody, env));
                        }
                        return new CaseLambda(clauses);
                    }
                    case "letrec" -> {
                        Object bindingsObj = list.get(1);
                        if (bindingsObj instanceof SourceExpr se) bindingsObj = se.expr;
                        List<?> bindings = (List<?>) bindingsObj;
                        Env letEnv = new Env(env);
                        List<String> varNames = new ArrayList<>();
                        List<Object> initExprs = new ArrayList<>();
                        for (Object b : bindings) {
                            if (b instanceof SourceExpr se) b = se.expr;
                            List<?> binding = (List<?>) b;
                            Object pname = binding.get(0);
                            if (pname instanceof SourceExpr se) pname = se.expr;
                            varNames.add((String) pname);
                            initExprs.add(binding.get(1));
                            letEnv.define((String) pname, VOID);
                        }
                        for (int i = 0; i < varNames.size(); i++) {
                            letEnv.define(varNames.get(i), eval(initExprs.get(i), letEnv));
                        }
                        if (list.size() <= 2) return VOID;
                        for (int i = 2; i < list.size() - 1; i++) {
                            eval(list.get(i), letEnv);
                        }
                        expr = list.get(list.size() - 1);
                        env = letEnv;
                        continue trampolineLoop;
                    }
                    case "letrec*" -> {
                        Object bindingsObj = list.get(1);
                        if (bindingsObj instanceof SourceExpr se) bindingsObj = se.expr;
                        List<?> bindings = (List<?>) bindingsObj;
                        Env letEnv = new Env(env);
                        for (Object b : bindings) {
                            if (b instanceof SourceExpr se) b = se.expr;
                            List<?> binding = (List<?>) b;
                            Object pname = binding.get(0);
                            if (pname instanceof SourceExpr se) pname = se.expr;
                            letEnv.define((String) pname, eval(binding.get(1), letEnv));
                        }
                        if (list.size() <= 2) return VOID;
                        for (int i = 2; i < list.size() - 1; i++) {
                            eval(list.get(i), letEnv);
                        }
                        expr = list.get(list.size() - 1);
                        env = letEnv;
                        continue trampolineLoop;
                    }
                    case "case" -> {
                        Object key = eval(list.get(1), env);
                        for (int i = 2; i < list.size(); i++) {
                            Object clauseObj = list.get(i);
                            if (clauseObj instanceof SourceExpr se) clauseObj = se.expr;
                            @SuppressWarnings("unchecked")
                            List<Object> clause = (List<Object>) clauseObj;
                            Object datums = clause.get(0);
                            if (datums instanceof SourceExpr se) datums = se.expr;
                            if ("else".equals(datums)) {
                                for (int j = 1; j < clause.size() - 1; j++) eval(clause.get(j), env);
                                expr = clause.get(clause.size() - 1);
                                continue trampolineLoop;
                            }
                            List<?> datumList = (List<?>) datums;
                            for (Object d : datumList) {
                                Object datum = d;
                                if (datum instanceof SourceExpr se) datum = se.expr;
                                if (schemeEq(key, datum)) {
                                    for (int j = 1; j < clause.size() - 1; j++) eval(clause.get(j), env);
                                    expr = clause.get(clause.size() - 1);
                                    continue trampolineLoop;
                                }
                            }
                        }
                        return VOID;
                    }
                    case "do" -> {
                        // (do ((var init step) ...) (test expr ...) body ...)
                        Object varsObj = list.get(1);
                        if (varsObj instanceof SourceExpr se) varsObj = se.expr;
                        List<?> varSpecs = (List<?>) varsObj;
                        Object testObj = list.get(2);
                        if (testObj instanceof SourceExpr se) testObj = se.expr;
                        @SuppressWarnings("unchecked")
                        List<Object> testClause = (List<Object>) testObj;

                        List<String> varNamesDo = new ArrayList<>();
                        List<Object> initsDo = new ArrayList<>();
                        List<Object> stepsDo = new ArrayList<>(); // null if no step
                        for (Object vs : varSpecs) {
                            if (vs instanceof SourceExpr se) vs = se.expr;
                            List<?> spec = (List<?>) vs;
                            Object vn = spec.get(0);
                            if (vn instanceof SourceExpr se) vn = se.expr;
                            varNamesDo.add((String) vn);
                            initsDo.add(spec.get(1));
                            stepsDo.add(spec.size() > 2 ? spec.get(2) : null);
                        }

                        Env doEnv = new Env(env);
                        for (int i = 0; i < varNamesDo.size(); i++) {
                            doEnv.define(varNamesDo.get(i), eval(initsDo.get(i), env));
                        }

                        while (true) {
                            Object testVal = eval(testClause.get(0), doEnv);
                            if (!isFalse(testVal)) {
                                // Test passed - evaluate result exprs
                                if (testClause.size() > 1) {
                                    Object result = VOID;
                                    for (int i = 1; i < testClause.size(); i++) {
                                        result = eval(testClause.get(i), doEnv);
                                    }
                                    return result;
                                }
                                return VOID;
                            }
                            // Execute body
                            for (int i = 3; i < list.size(); i++) {
                                eval(list.get(i), doEnv);
                            }
                            // Parallel step: evaluate all steps with old values
                            Object[] newVals = new Object[varNamesDo.size()];
                            for (int i = 0; i < varNamesDo.size(); i++) {
                                if (stepsDo.get(i) != null) {
                                    newVals[i] = eval(stepsDo.get(i), doEnv);
                                } else {
                                    newVals[i] = doEnv.lookup(varNamesDo.get(i));
                                }
                            }
                            for (int i = 0; i < varNamesDo.size(); i++) {
                                doEnv.define(varNamesDo.get(i), newVals[i]);
                            }
                        }
                    }
                    case "let" -> {
                        int offset;
                        String loopName = null;
                        Object second = list.get(1);
                        if (second instanceof SourceExpr se) second = se.expr;
                        if (second instanceof String name) {
                            loopName = name;
                            offset = 2;
                        } else {
                            offset = 1;
                        }
                        Object bindingsObj = list.get(offset);
                        if (bindingsObj instanceof SourceExpr se) bindingsObj = se.expr;
                        List<?> bindings = (List<?>) bindingsObj;
                        List<Object> body = new ArrayList<>(list.subList(offset + 1, list.size()));

                        List<String> params = new ArrayList<>();
                        List<Object> inits = new ArrayList<>();
                        for (Object b : bindings) {
                            if (b instanceof SourceExpr se) b = se.expr;
                            List<?> binding = (List<?>) b;
                            Object pname = binding.get(0);
                            if (pname instanceof SourceExpr se) pname = se.expr;
                            params.add((String) pname);
                            inits.add(binding.get(1));
                        }

                        if (loopName != null) {
                            Env letEnv = new Env(env);
                            Lambda loopLambda = new Lambda(params, null, body, letEnv);
                            letEnv.define(loopName, loopLambda);
                            List<Object> args = new ArrayList<>();
                            for (Object init : inits) {
                                args.add(eval(init, env));
                            }
                            Env callEnv = new Env(loopLambda.closureEnv);
                            for (int i = 0; i < params.size(); i++) {
                                callEnv.define(params.get(i), args.get(i));
                            }
                            for (int i = 0; i < body.size() - 1; i++) {
                                eval(body.get(i), callEnv);
                            }
                            expr = body.get(body.size() - 1);
                            env = callEnv;
                            continue trampolineLoop;
                        } else {
                            Env letEnv = new Env(env);
                            for (int i = 0; i < params.size(); i++) {
                                letEnv.define(params.get(i), eval(inits.get(i), env));
                            }
                            if (body.isEmpty()) return VOID;
                            for (int i = 0; i < body.size() - 1; i++) {
                                eval(body.get(i), letEnv);
                            }
                            expr = body.get(body.size() - 1);
                            env = letEnv;
                            continue trampolineLoop;
                        }
                    }
                    case "set!" -> {
                        if (list.size() != 3) throw new EvalError("bad syntax: set!");
                        Object varObj = list.get(1);
                        if (varObj instanceof SourceExpr se) varObj = se.expr;
                        if (!(varObj instanceof String varName)) throw new EvalError("bad syntax: set!");
                        Object val = eval(list.get(2), env);
                        env.set(varName, val);
                        return VOID;
                    }
                    case "begin" -> {
                        if (list.size() == 1) return VOID;
                        for (int i = 1; i < list.size() - 1; i++) {
                            eval(list.get(i), env);
                        }
                        expr = list.get(list.size() - 1);
                        continue trampolineLoop;
                    }
                    case "cond" -> {
                        for (int i = 1; i < list.size(); i++) {
                            Object clauseObj = list.get(i);
                            if (clauseObj instanceof SourceExpr se) clauseObj = se.expr;
                            List<Object> clause = (List<Object>) clauseObj;
                            Object test = clause.get(0);
                            Object rawTest = test;
                            if (rawTest instanceof SourceExpr se) rawTest = se.expr;
                            if ("else".equals(rawTest)) {
                                for (int j = 1; j < clause.size() - 1; j++) {
                                    eval(clause.get(j), env);
                                }
                                expr = clause.get(clause.size() - 1);
                                continue trampolineLoop;
                            }
                            Object condVal = eval(test, env);
                            if (!isFalse(condVal)) {
                                if (clause.size() == 1) return condVal;
                                for (int j = 1; j < clause.size() - 1; j++) {
                                    eval(clause.get(j), env);
                                }
                                expr = clause.get(clause.size() - 1);
                                continue trampolineLoop;
                            }
                        }
                        return VOID;
                    }
                    case "and" -> {
                        if (list.size() == 1) return Boolean.TRUE;
                        for (int i = 1; i < list.size() - 1; i++) {
                            Object result = eval(list.get(i), env);
                            if (isFalse(result)) return result;
                        }
                        expr = list.get(list.size() - 1);
                        continue trampolineLoop;
                    }
                    case "or" -> {
                        if (list.size() == 1) return Boolean.FALSE;
                        for (int i = 1; i < list.size() - 1; i++) {
                            Object result = eval(list.get(i), env);
                            if (!isFalse(result)) return result;
                        }
                        expr = list.get(list.size() - 1);
                        continue trampolineLoop;
                    }
                    case "define-syntax" -> {
                        Object nameObj = list.get(1);
                        if (nameObj instanceof SourceExpr se) nameObj = se.expr;
                        String macroName = (String) nameObj;
                        Object srObj = list.get(2);
                        if (srObj instanceof SourceExpr se) srObj = se.expr;
                        @SuppressWarnings("unchecked")
                        List<Object> sr = (List<Object>) srObj;
                        Object litsObj = sr.get(1);
                        if (litsObj instanceof SourceExpr se) litsObj = se.expr;
                        List<?> litsRaw = (List<?>) litsObj;
                        List<String> lits = new ArrayList<>();
                        for (Object l : litsRaw) {
                            if (l instanceof SourceExpr se) l = se.expr;
                            lits.add((String) l);
                        }
                        List<Object[]> rules = new ArrayList<>();
                        for (int i = 2; i < sr.size(); i++) {
                            Object ruleObj = sr.get(i);
                            if (ruleObj instanceof SourceExpr se) ruleObj = se.expr;
                            @SuppressWarnings("unchecked")
                            List<Object> rule = (List<Object>) ruleObj;
                            rules.add(new Object[]{rule.get(0), rule.get(1)});
                        }
                        env.define(macroName, new SyntaxRulesMacro(lits, rules, env));
                        return VOID;
                    }
                    case "define-record-type" -> {
                        // (define-record-type <name> (constructor field ...) predicate (field accessor) ...)
                        // list.get(1) = type name (e.g. <point>)
                        // list.get(2) = (constructor-name field-names...)
                        // list.get(3) = predicate name
                        // list.get(4..) = (field-name accessor-name)
                        Object typeNameObj = list.get(1);
                        if (typeNameObj instanceof SourceExpr se) typeNameObj = se.expr;

                        Object ctorListObj = list.get(2);
                        if (ctorListObj instanceof SourceExpr se) ctorListObj = se.expr;
                        @SuppressWarnings("unchecked")
                        List<Object> ctorList = (List<Object>) ctorListObj;
                        Object ctorNameObj = ctorList.get(0);
                        if (ctorNameObj instanceof SourceExpr se) ctorNameObj = se.expr;
                        String ctorName = (String) ctorNameObj;
                        List<String> ctorFields = new ArrayList<>();
                        for (int i = 1; i < ctorList.size(); i++) {
                            Object f = ctorList.get(i);
                            if (f instanceof SourceExpr se) f = se.expr;
                            ctorFields.add((String) f);
                        }

                        Object predNameObj = list.get(3);
                        if (predNameObj instanceof SourceExpr se) predNameObj = se.expr;
                        String predName = (String) predNameObj;

                        // Collect field specs
                        List<String> fieldNames = new ArrayList<>();
                        List<String> accessorNames = new ArrayList<>();
                        for (int i = 4; i < list.size(); i++) {
                            Object fieldSpec = list.get(i);
                            if (fieldSpec instanceof SourceExpr se) fieldSpec = se.expr;
                            @SuppressWarnings("unchecked")
                            List<Object> spec = (List<Object>) fieldSpec;
                            Object fn = spec.get(0);
                            if (fn instanceof SourceExpr se) fn = se.expr;
                            fieldNames.add((String) fn);
                            Object an = spec.get(1);
                            if (an instanceof SourceExpr se) an = se.expr;
                            accessorNames.add((String) an);
                        }

                        RecordType recordType = new RecordType((String) typeNameObj, fieldNames);

                        // Define constructor
                        env.define(ctorName, (BuiltinProc) args -> {
                            if (args.size() != ctorFields.size()) {
                                throw new EvalError(ctorName + ": expected " + ctorFields.size() + " arguments");
                            }
                            Object[] fields = new Object[fieldNames.size()];
                            for (int i = 0; i < ctorFields.size(); i++) {
                                int idx = fieldNames.indexOf(ctorFields.get(i));
                                fields[idx] = args.get(i);
                            }
                            return new RecordInstance(recordType, fields);
                        });

                        // Define predicate
                        env.define(predName, (BuiltinProc) args -> {
                            return args.get(0) instanceof RecordInstance ri && ri.type == recordType;
                        });

                        // Define accessors
                        for (int i = 0; i < fieldNames.size(); i++) {
                            final int idx = i;
                            env.define(accessorNames.get(i), (BuiltinProc) args -> {
                                if (!(args.get(0) instanceof RecordInstance ri) || ri.type != recordType) {
                                    throw new EvalError(accessorNames.get(idx) + ": not a " + recordType.name);
                                }
                                return ri.fields[idx];
                            });
                        }

                        return VOID;
                    }
                }
            }

            // Function application
            Object proc = eval(head, env);
            if (proc instanceof SyntaxRulesMacro macro) {
                Object expanded = expandMacro(macro, list);
                return eval(expanded, env);
            }
            List<Object> args = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) {
                args.add(eval(list.get(i), env));
            }

            if (proc instanceof BuiltinProc builtin) {
                return builtin.apply(args);
            }
            Lambda lambda = null;
            if (proc instanceof Lambda l) {
                lambda = l;
            } else if (proc instanceof CaseLambda cl) {
                lambda = matchCaseLambda(cl, args.size());
            }
            if (lambda != null) {
                // Inline lambda application for TCO
                Env callEnv = bindLambdaArgs(lambda, args);
                for (int i = 0; i < lambda.body.size() - 1; i++) {
                    eval(lambda.body.get(i), callEnv);
                }
                expr = lambda.body.get(lambda.body.size() - 1);
                env = callEnv;
                continue trampolineLoop;
            }
            throw new EvalError("cannot apply: " + schemeToString(proc));
        }
        throw new EvalError("unknown expression type");
        } catch (EvalError e) {
            if (srcLine >= 0 && !e.getMessage().matches(".*\\d+:\\d+.*")) {
                throw new EvalError(e.getMessage() + " at " + srcLine + ":" + srcCol);
            }
            throw e;
        }
        } // end while(true)
    }

    private Object applyLambda(Lambda lambda, List<Object> args) throws EvalError {
        int required = lambda.params.size();
        if (lambda.restParam != null) {
            if (args.size() < required) {
                throw new EvalError("wrong number of arguments: expected at least " + required + ", got " + args.size());
            }
        } else {
            if (args.size() != required) {
                throw new EvalError("wrong number of arguments: expected " + required + ", got " + args.size());
            }
        }
        Env callEnv = new Env(lambda.closureEnv);
        for (int i = 0; i < required; i++) {
            callEnv.define(lambda.params.get(i), args.get(i));
        }
        if (lambda.restParam != null) {
            callEnv.define(lambda.restParam, javaListToScheme(args.subList(required, args.size())));
        }
        Object result = VOID;
        for (Object bodyExpr : lambda.body) {
            result = eval(bodyExpr, callEnv);
        }
        return result;
    }

    private Object applyCaseLambda(CaseLambda cl, List<Object> args) throws EvalError {
        for (Lambda clause : cl.clauses) {
            int required = clause.params.size();
            if (clause.restParam != null) {
                if (args.size() >= required) return applyLambda(clause, args);
            } else {
                if (args.size() == required) return applyLambda(clause, args);
            }
        }
        throw new EvalError("case-lambda: no matching clause for " + args.size() + " arguments");
    }

    private Env bindLambdaArgs(Lambda lambda, List<Object> args) throws EvalError {
        int required = lambda.params.size();
        if (lambda.restParam != null) {
            if (args.size() < required) {
                throw new EvalError("wrong number of arguments: expected at least " + required + ", got " + args.size());
            }
        } else {
            if (args.size() != required) {
                throw new EvalError("wrong number of arguments: expected " + required + ", got " + args.size());
            }
        }
        Env callEnv = new Env(lambda.closureEnv);
        for (int i = 0; i < required; i++) {
            callEnv.define(lambda.params.get(i), args.get(i));
        }
        if (lambda.restParam != null) {
            callEnv.define(lambda.restParam, javaListToScheme(args.subList(required, args.size())));
        }
        return callEnv;
    }

    private Lambda matchCaseLambda(CaseLambda cl, int argCount) throws EvalError {
        for (Lambda clause : cl.clauses) {
            int required = clause.params.size();
            if (clause.restParam != null) {
                if (argCount >= required) return clause;
            } else {
                if (argCount == required) return clause;
            }
        }
        throw new EvalError("case-lambda: no matching clause for " + argCount + " arguments");
    }

    private Object javaListToScheme(List<Object> items) {
        Object result = NIL;
        for (int i = items.size() - 1; i >= 0; i--) {
            result = new Pair(items.get(i), result);
        }
        return result;
    }

    // --- Macro expansion ---

    private Object unwrapSE(Object o) {
        while (o instanceof SourceExpr se) o = se.expr;
        return o;
    }

    private String gensym(String base) {
        return base + "__m" + (gensymCounter++);
    }

    @SuppressWarnings("unchecked")
    private Object expandMacro(SyntaxRulesMacro macro, List<Object> form) throws EvalError {
        for (Object[] rule : macro.rules) {
            Object pattern = unwrapSE(rule[0]);
            Object template = rule[1];
            List<?> patList = (List<?>) pattern;

            Map<String, Object> bindings = new HashMap<>();
            Set<String> patternVars = new HashSet<>();
            Set<String> ellipsisVars = new HashSet<>();

            if (matchPatternList(patList, form, 1, macro.literals, bindings, patternVars, ellipsisVars)) {
                Map<String, String> renames = new HashMap<>();
                return expandTemplate(template, bindings, patternVars, ellipsisVars, macro.defEnv, renames);
            }
        }
        throw new EvalError("no matching syntax-rules pattern");
    }

    private boolean matchPatternList(List<?> patList, List<?> inList, int startIdx,
                                      List<String> literals, Map<String, Object> bindings,
                                      Set<String> patternVars, Set<String> ellipsisVars) {
        int ellipsisIdx = -1;
        for (int i = startIdx; i < patList.size(); i++) {
            if ("...".equals(unwrapSE(patList.get(i)))) {
                ellipsisIdx = i;
                break;
            }
        }

        if (ellipsisIdx == -1) {
            if (patList.size() - startIdx != inList.size() - startIdx) return false;
            for (int i = startIdx; i < patList.size(); i++) {
                if (!matchPattern(patList.get(i), inList.get(i), literals, bindings, patternVars, ellipsisVars))
                    return false;
            }
            return true;
        }

        int repeatedPatIdx = ellipsisIdx - 1;
        int fixedBefore = repeatedPatIdx - startIdx;
        int fixedAfter = patList.size() - ellipsisIdx - 1;
        int minRequired = startIdx + fixedBefore + fixedAfter;

        if (inList.size() < minRequired) return false;

        for (int i = startIdx; i < repeatedPatIdx; i++) {
            if (!matchPattern(patList.get(i), inList.get(i), literals, bindings, patternVars, ellipsisVars))
                return false;
        }

        Object repeatedPat = unwrapSE(patList.get(repeatedPatIdx));
        int ellipsisCount = inList.size() - minRequired;

        if (repeatedPat instanceof String varName && !literals.contains(varName)) {
            patternVars.add(varName);
            ellipsisVars.add(varName);
            List<Object> matches = new ArrayList<>();
            for (int i = 0; i < ellipsisCount; i++) {
                matches.add(unwrapSE(inList.get(repeatedPatIdx + i)));
            }
            bindings.put(varName, matches);
        }

        for (int i = 0; i < fixedAfter; i++) {
            int patIdx = ellipsisIdx + 1 + i;
            int inIdx = inList.size() - fixedAfter + i;
            if (!matchPattern(patList.get(patIdx), inList.get(inIdx), literals, bindings, patternVars, ellipsisVars))
                return false;
        }

        return true;
    }

    private boolean matchPattern(Object pattern, Object input, List<String> literals,
                                  Map<String, Object> bindings, Set<String> patternVars,
                                  Set<String> ellipsisVars) {
        pattern = unwrapSE(pattern);
        input = unwrapSE(input);

        if (pattern instanceof String sym) {
            if ("_".equals(sym)) return true;
            if (literals.contains(sym)) {
                return sym.equals(input);
            }
            patternVars.add(sym);
            bindings.put(sym, input);
            return true;
        }

        if (pattern instanceof List<?> patList) {
            if (!(input instanceof List<?>)) return false;
            return matchPatternList(patList, (List<?>) input, 0, literals, bindings, patternVars, ellipsisVars);
        }

        if (pattern instanceof Long || pattern instanceof Boolean) {
            return pattern.equals(input);
        }

        return false;
    }

    @SuppressWarnings("unchecked")
    private Object expandTemplate(Object template, Map<String, Object> bindings,
                                   Set<String> patternVars, Set<String> ellipsisVars,
                                   Env defEnv, Map<String, String> renames) throws EvalError {
        template = unwrapSE(template);

        if (template instanceof String sym) {
            if (patternVars.contains(sym)) {
                return bindings.get(sym);
            }
            if (SPECIAL_FORMS.contains(sym)) {
                return sym;
            }
            if (renames.containsKey(sym)) {
                return renames.get(sym);
            }
            try {
                Object val = defEnv.lookup(sym);
                return new ResolvedValue(val);
            } catch (EvalError e) {
                String gs = gensym(sym);
                renames.put(sym, gs);
                return gs;
            }
        }

        if (template instanceof List<?> tmplList) {
            List<Object> result = new ArrayList<>();
            for (int i = 0; i < tmplList.size(); i++) {
                if ("...".equals(unwrapSE(tmplList.get(i)))) continue;

                if (i + 1 < tmplList.size() && "...".equals(unwrapSE(tmplList.get(i + 1)))) {
                    Object subTmpl = tmplList.get(i);
                    Set<String> usedEVars = findEllipsisVars(subTmpl, ellipsisVars);
                    if (!usedEVars.isEmpty()) {
                        String evar = usedEVars.iterator().next();
                        List<Object> eList = (List<Object>) bindings.get(evar);
                        for (Object elt : eList) {
                            Map<String, Object> newBindings = new HashMap<>(bindings);
                            newBindings.put(evar, elt);
                            Set<String> newEVars = new HashSet<>(ellipsisVars);
                            newEVars.remove(evar);
                            result.add(expandTemplate(subTmpl, newBindings, patternVars, newEVars, defEnv, renames));
                        }
                    }
                    i++; // skip ...
                } else {
                    result.add(expandTemplate(tmplList.get(i), bindings, patternVars, ellipsisVars, defEnv, renames));
                }
            }
            return result;
        }

        return template;
    }

    private Set<String> findEllipsisVars(Object template, Set<String> ellipsisVars) {
        template = unwrapSE(template);
        Set<String> found = new HashSet<>();
        if (template instanceof String sym && ellipsisVars.contains(sym)) {
            found.add(sym);
        } else if (template instanceof List<?> list) {
            for (Object elt : list) found.addAll(findEllipsisVars(elt, ellipsisVars));
        }
        return found;
    }

    // --- Numeric helpers for exact arithmetic ---

    private static long gcd(long a, long b) {
        while (b != 0) { long t = b; b = a % b; a = t; }
        return a;
    }

    private static Object makeRational(long num, long den) throws EvalError {
        if (den == 0) throw new EvalError("division by zero");
        if (den < 0) { num = -num; den = -den; }
        long g = gcd(Math.abs(num), den);
        num /= g; den /= g;
        if (den == 1) return num;
        return new SchemeRational(num, den);
    }

    private double toDouble(Object val) throws EvalError {
        if (val instanceof Long l) return (double) l;
        if (val instanceof Double d) return d;
        if (val instanceof SchemeRational r) return r.toDouble();
        throw new EvalError("expected number, got: " + schemeToString(val));
    }

    private long[] toRat(Object val) throws EvalError {
        if (val instanceof Long l) return new long[]{l, 1};
        if (val instanceof SchemeRational r) return new long[]{r.num, r.den};
        throw new EvalError("expected exact number, got: " + schemeToString(val));
    }

    private Object numAdd(Object a, Object b) throws EvalError {
        if (a instanceof Double || b instanceof Double) return toDouble(a) + toDouble(b);
        long[] ra = toRat(a), rb = toRat(b);
        return makeRational(ra[0] * rb[1] + rb[0] * ra[1], ra[1] * rb[1]);
    }

    private Object numSub(Object a, Object b) throws EvalError {
        if (a instanceof Double || b instanceof Double) return toDouble(a) - toDouble(b);
        long[] ra = toRat(a), rb = toRat(b);
        return makeRational(ra[0] * rb[1] - rb[0] * ra[1], ra[1] * rb[1]);
    }

    private Object numMul(Object a, Object b) throws EvalError {
        if (a instanceof Double || b instanceof Double) return toDouble(a) * toDouble(b);
        long[] ra = toRat(a), rb = toRat(b);
        return makeRational(ra[0] * rb[0], ra[1] * rb[1]);
    }

    private Object numDiv(Object a, Object b) throws EvalError {
        if (a instanceof Double || b instanceof Double) {
            double d = toDouble(b);
            if (d == 0) throw new EvalError("division by zero");
            return toDouble(a) / d;
        }
        long[] ra = toRat(a), rb = toRat(b);
        if (rb[0] == 0) throw new EvalError("division by zero");
        return makeRational(ra[0] * rb[1], ra[1] * rb[0]);
    }

    private Object numNeg(Object a) throws EvalError {
        if (a instanceof Long l) return -l;
        if (a instanceof Double d) return -d;
        if (a instanceof SchemeRational r) return new SchemeRational(-r.num, r.den);
        throw new EvalError("expected number");
    }

    private int numCompare(Object a, Object b) throws EvalError {
        if (a instanceof Double || b instanceof Double) return Double.compare(toDouble(a), toDouble(b));
        long[] ra = toRat(a), rb = toRat(b);
        return Long.compare(ra[0] * rb[1], rb[0] * ra[1]);
    }

    private boolean isSchemeNumber(Object val) {
        return val instanceof Long || val instanceof Double || val instanceof SchemeRational;
    }

    private static Object parseNumber(String tok) {
        try {
            return Long.parseLong(tok);
        } catch (NumberFormatException e) { /* fall through */ }
        // Try rational: num/den
        int slashIdx = tok.indexOf('/');
        if (slashIdx > 0 && slashIdx < tok.length() - 1) {
            try {
                long num = Long.parseLong(tok.substring(0, slashIdx));
                long den = Long.parseLong(tok.substring(slashIdx + 1));
                if (den == 0) return null; // let it be a symbol or error later
                if (den < 0) { num = -num; den = -den; }
                long g = gcd(Math.abs(num), den);
                num /= g; den /= g;
                if (den == 1) return num;
                return new SchemeRational(num, den);
            } catch (NumberFormatException e2) { /* fall through */ }
        }
        // Try double
        try {
            return Double.parseDouble(tok);
        } catch (NumberFormatException e2) { /* fall through */ }
        return null;
    }

    private String numberToString(Object val) throws EvalError {
        if (val instanceof Long l) return l.toString();
        if (val instanceof SchemeRational r) return r.num + "/" + r.den;
        if (val instanceof Double d) return formatDouble(d);
        throw new EvalError("number->string: not a number");
    }

    private static String formatDouble(double d) {
        if (d == Math.floor(d) && !Double.isInfinite(d) && Math.abs(d) < 1e15) {
            // Ensure trailing .0 for integer-valued doubles
            return String.valueOf(d);
        }
        return String.valueOf(d);
    }

    private boolean schemeEq(Object a, Object b) {
        if (a == b) return true;
        if (a instanceof Long && b instanceof Long) return a.equals(b);
        if (a instanceof Double && b instanceof Double) return a.equals(b);
        if (a instanceof SchemeRational && b instanceof SchemeRational) return a.equals(b);
        if (a instanceof Boolean && b instanceof Boolean) return a.equals(b);
        if (a instanceof SchemeChar && b instanceof SchemeChar) return a.equals(b);
        if (a instanceof String && b instanceof String) return a.equals(b);
        return false;
    }

    private boolean schemeEqual(Object a, Object b) {
        if (schemeEq(a, b)) return true;
        if (a instanceof Pair pa && b instanceof Pair pb) {
            return schemeEqual(pa.car, pb.car) && schemeEqual(pa.cdr, pb.cdr);
        }
        if (a instanceof SchemeString sa && b instanceof SchemeString sb) {
            return sa.value().equals(sb.value());
        }
        if (a instanceof SchemeVector va && b instanceof SchemeVector vb) {
            if (va.elements.length != vb.elements.length) return false;
            for (int i = 0; i < va.elements.length; i++) {
                if (!schemeEqual(va.elements[i], vb.elements[i])) return false;
            }
            return true;
        }
        return false;
    }

    private boolean isFalse(Object val) {
        return Boolean.FALSE.equals(val);
    }

    private long asLong(Object val) throws EvalError {
        if (val instanceof Long l) return l;
        throw new EvalError("expected number, got: " + schemeToString(val));
    }

    private SchemeString asSchemeString(Object val) throws EvalError {
        if (val instanceof SchemeString s) return s;
        throw new EvalError("expected string, got: " + schemeToString(val));
    }

    private String displayString(Object val) {
        if (val instanceof SchemeString s) return s.value();
        if (val instanceof SchemeChar c) return String.valueOf(c.value());
        if (val instanceof SchemeVector v) {
            StringBuilder sb = new StringBuilder("#(");
            for (int i = 0; i < v.elements.length; i++) {
                if (i > 0) sb.append(" ");
                sb.append(displayString(v.elements[i]));
            }
            sb.append(")");
            return sb.toString();
        }
        return schemeToString(val);
    }

    // --- Display ---

    private String schemeToString(Object val) {
        if (val instanceof Long l) return l.toString();
        if (val instanceof Double d) return formatDouble(d);
        if (val instanceof SchemeRational r) return r.num + "/" + r.den;
        if (val instanceof Boolean b) return b ? "#t" : "#f";
        if (val instanceof SchemeString s) return "\"" + s.value() + "\"";
        if (val instanceof SchemeChar c) {
            return switch (c.value()) {
                case ' ' -> "#\\space";
                case '\n' -> "#\\newline";
                case '\t' -> "#\\tab";
                default -> "#\\" + c.value();
            };
        }
        if (val instanceof String s) return s;
        if (val == NIL) return "()";
        if (val instanceof Pair) {
            StringBuilder sb = new StringBuilder("(");
            Object curr = val;
            boolean first = true;
            while (curr instanceof Pair p) {
                if (!first) sb.append(" ");
                first = false;
                sb.append(schemeToString(p.car));
                curr = p.cdr;
            }
            if (curr != NIL) {
                sb.append(" . ");
                sb.append(schemeToString(curr));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof List<?> list) {
            StringBuilder sb = new StringBuilder("(");
            for (int i = 0; i < list.size(); i++) {
                if (i > 0) sb.append(" ");
                sb.append(schemeToString(list.get(i)));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof SchemeVector v) {
            StringBuilder sb = new StringBuilder("#(");
            for (int i = 0; i < v.elements.length; i++) {
                if (i > 0) sb.append(" ");
                sb.append(schemeToString(v.elements[i]));
            }
            sb.append(")");
            return sb.toString();
        }
        if (val instanceof Lambda) return "#<procedure>";
        if (val instanceof CaseLambda) return "#<procedure>";
        if (val instanceof SyntaxRulesMacro) return "#<macro>";
        if (val instanceof ResolvedValue rv) return schemeToString(rv.value);
        return val.toString();
    }

    // Internal wrapper to distinguish strings from symbols (mutable for string-set!)
    static class SchemeString {
        private char[] chars;
        private boolean immutable;
        SchemeString(String value) { this.chars = value.toCharArray(); this.immutable = false; }
        SchemeString(String value, boolean immutable) { this.chars = value.toCharArray(); this.immutable = immutable; }
        String value() { return new String(chars); }
        char charAt(int i) { return chars[i]; }
        void setChar(int i, char c) throws EvalError {
            if (immutable) throw new EvalError("string-set!: strings are immutable");
            chars[i] = c;
        }
        int length() { return chars.length; }
        boolean isImmutable() { return immutable; }
        void markImmutable() { this.immutable = true; }
        @Override public boolean equals(Object o) {
            return o instanceof SchemeString s && java.util.Arrays.equals(chars, s.chars);
        }
        @Override public int hashCode() { return java.util.Arrays.hashCode(chars); }
    }

    // Internal wrapper for characters
    record SchemeChar(char value) {}

    // Exact rational number
    static class SchemeRational {
        final long num;
        final long den;
        SchemeRational(long num, long den) { this.num = num; this.den = den; }
        double toDouble() { return (double) num / den; }
        @Override public boolean equals(Object o) {
            return o instanceof SchemeRational r && num == r.num && den == r.den;
        }
        @Override public int hashCode() { return Long.hashCode(num) * 31 + Long.hashCode(den); }
    }
}
