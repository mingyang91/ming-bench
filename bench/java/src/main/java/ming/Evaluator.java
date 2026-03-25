package ming;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.IdentityHashMap;
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

    // --- Syntax-case transformer ---

    private static class SyntaxCaseTransformer {
        final Object transformer; // Lambda
        final Env defEnv;
        SyntaxCaseTransformer(Object transformer, Env defEnv) {
            this.transformer = transformer;
            this.defEnv = defEnv;
        }
    }

    // --- Syntax object (wrapper for syntax-case) ---

    private static class SyntaxObject {
        final Object datum;
        SyntaxObject(Object datum) { this.datum = datum; }
    }

    // --- Ellipsis binding for syntax-case ---

    private static class SyntaxEllipsis {
        final List<Object> elements;
        SyntaxEllipsis(List<Object> elements) { this.elements = elements; }
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
        "quote", "if", "define", "lambda", "let", "let*", "set!", "begin", "cond",
        "and", "or", "define-syntax", "syntax-rules", "else", "define-record-type",
        "letrec", "letrec*", "case", "do", "guard", "case-lambda",
        "syntax-case", "syntax", "with-syntax"
    );

    // --- Continuation types for CEK machine ---
    private interface Kont {}
    private record HaltK() implements Kont {}
    private record IfK(Object thenE, Object elseE, boolean hasElse, Env env, Kont k) implements Kont {}
    private record SeqK(List<Object> exprs, int nextIdx, Env env, Kont k) implements Kont {}
    private record DefineK(String name, Env env, Kont k) implements Kont {}
    private record SetK(String name, Env env, Kont k) implements Kont {}
    private record EvProcK(List<Object> list, Env env, Kont k) implements Kont {}
    private record EvArgK(Object proc, List<Object> list, int currentIdx, List<Object> evaluated, Env env, Kont k) implements Kont {}
    private record AndK(List<Object> exprs, int nextIdx, Env env, Kont k) implements Kont {}
    private record OrK(List<Object> exprs, int nextIdx, Env env, Kont k) implements Kont {}
    private record CondTestK(List<Object> clause, List<Object> fullList, int nextClauseIdx, Env env, Kont k) implements Kont {}
    private record LetBindK(List<String> params, List<Object> inits, int nextIdx, List<Object> evaluated, Env outerEnv, List<Object> body, String loopName, Kont k) implements Kont {}
    private record LetStarBindK(List<String> names, List<Object> inits, int nextIdx, Env letEnv, List<Object> body, Kont k) implements Kont {}
    private record LetrecBindK(List<String> names, List<Object> inits, int nextIdx, Env letEnv, List<Object> body, Kont k) implements Kont {}
    private record CaseKeyK(List<Object> list, Env env, Kont k) implements Kont {}
    private record DoInitK(List<String> varNames, List<Object> inits, int nextIdx, List<Object> evaluated, Env outerEnv, List<Object> testClause, List<Object> steps, List<Object> fullList, Kont k) implements Kont {}
    private record DoTestK(List<String> varNames, Env doEnv, List<Object> testClause, List<Object> steps, List<Object> fullList, Kont k) implements Kont {}
    private record DoAfterBodyK(List<String> varNames, Env doEnv, List<Object> steps, List<Object> testClause, List<Object> fullList, Kont k) implements Kont {}
    private record DoStepK(List<String> varNames, Env doEnv, List<Object> steps, int nextIdx, List<Object> newVals, List<Object> testClause, List<Object> fullList, Kont k) implements Kont {}

    // --- syntax-case / with-syntax continuations ---
    private record SyntaxCaseK(List<Object> fullList, Env env, Kont k) implements Kont {}
    private record WithSyntaxK(List<Object[]> bindings, int nextIdx, Env wsEnv, List<Object> body, Kont k) implements Kont {}

    // --- call-with-values continuation ---
    private record CallWithValuesK(Object consumer, Kont k) implements Kont {}

    // --- dynamic-wind continuation types ---
    private record DynWindAfterInK(Object inThunk, Object bodyThunk, Object outThunk, Kont k) implements Kont {}
    private record DynWindAfterBodyK(Object inThunk, Object outThunk, Kont k) implements Kont {}
    private record DynWindAfterOutK(Object bodyValue, Kont k) implements Kont {}
    // For unwinding/rewinding during continuation invocation
    private record DynWindDoThunksK(List<Object> thunks, int idx, List<WindEntry> targetWind, Kont targetK, Object targetValue, Kont k) implements Kont {}

    // --- guard / raise / with-exception-handler ---
    private record GuardBodyK(Kont k) implements Kont {} // pops handler on normal body completion
    private record GuardClauseK(String var, List<Object> clauses, Env guardEnv, Object exnValue, Kont k) implements Kont {}
    private record WehK(Kont k) implements Kont {} // pops handler on normal thunk completion

    // --- dynamic-wind entry ---
    private static class WindEntry {
        final Object inThunk;
        final Object outThunk;
        WindEntry(Object inThunk, Object outThunk) { this.inThunk = inThunk; this.outThunk = outThunk; }
    }

    // --- Scheme continuation (first-class value) ---
    private static class SchemeContinuation {
        final Kont k;
        final List<WindEntry> savedWind;
        SchemeContinuation(Kont k, List<WindEntry> savedWind) { this.k = k; this.savedWind = savedWind; }
    }

    // Thrown when raise is called in Scheme
    private static class SchemeRaise extends RuntimeException {
        final Object value;
        SchemeRaise(Object value) {
            super(null, null, true, false);
            this.value = value;
        }
    }

    // Exception handler stack entry
    private static class ExceptionHandlerEntry {
        // For guard:
        final String var;
        final List<Object> clauses;
        final Env guardEnv;
        final Kont guardK;
        final List<WindEntry> savedWind;
        // For with-exception-handler:
        final Object handlerProc;
        final boolean isGuard;

        // Guard constructor
        ExceptionHandlerEntry(String var, List<Object> clauses, Env guardEnv, Kont guardK, List<WindEntry> savedWind) {
            this.isGuard = true;
            this.var = var;
            this.clauses = clauses;
            this.guardEnv = guardEnv;
            this.guardK = guardK;
            this.savedWind = savedWind;
            this.handlerProc = null;
        }

        // with-exception-handler constructor
        ExceptionHandlerEntry(Object handlerProc) {
            this.isGuard = false;
            this.handlerProc = handlerProc;
            this.var = null;
            this.clauses = null;
            this.guardEnv = null;
            this.guardK = null;
            this.savedWind = null;
        }
    }

    // Thrown when a continuation is invoked to unwind back to the CEK loop
    private static class ContinuationReturn extends RuntimeException {
        final Kont k;
        final Object value;
        final List<WindEntry> targetWind;
        ContinuationReturn(Kont k, Object value, List<WindEntry> targetWind) {
            super(null, null, true, false); // no stack trace for performance
            this.k = k;
            this.value = value;
            this.targetWind = targetWind;
        }
    }

    // Sentinel for call/cc procedure
    private static final Object CALLCC_PROC = new Object() {
        @Override public String toString() { return "#<procedure:call/cc>"; }
    };

    // Sentinel for dynamic-wind procedure
    private static final Object DYNAMIC_WIND_PROC = new Object() {
        @Override public String toString() { return "#<procedure:dynamic-wind>"; }
    };

    // Sentinel for with-exception-handler procedure
    private static final Object WITH_EXCEPTION_HANDLER_PROC = new Object() {
        @Override public String toString() { return "#<procedure:with-exception-handler>"; }
    };

    // Sentinel for raise procedure
    private static final Object RAISE_PROC = new Object() {
        @Override public String toString() { return "#<procedure:raise>"; }
    };

    // Sentinel for values procedure
    private static final Object VALUES_PROC = new Object() {
        @Override public String toString() { return "#<procedure:values>"; }
    };

    // Sentinel for call-with-values procedure
    private static final Object CALL_WITH_VALUES_PROC = new Object() {
        @Override public String toString() { return "#<procedure:call-with-values>"; }
    };

    // Multiple values wrapper
    private static class MultipleValues {
        final List<Object> values;
        MultipleValues(List<Object> values) { this.values = values; }
    }

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
    private List<WindEntry> windStack = new ArrayList<>();
    private List<ExceptionHandlerEntry> handlerStack = new ArrayList<>();

    // CEK machine state (instance fields for helper method access)
    private Object cekExpr;
    private Env cekEnv;
    private Kont cekK;
    private Object cekValue;
    private boolean cekEval;
    private int cekSrcLine = -1;
    private int cekSrcCol = -1;

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
        globalEnv.define("set-car!", (BuiltinProc) args -> {
            if (!(args.get(0) instanceof Pair p)) throw new EvalError("set-car!: not a pair");
            p.car = args.get(1);
            return VOID;
        });
        globalEnv.define("set-cdr!", (BuiltinProc) args -> {
            if (!(args.get(0) instanceof Pair p)) throw new EvalError("set-cdr!: not a pair");
            p.cdr = args.get(1);
            return VOID;
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
            Object slow = args.get(0), fast = args.get(0);
            while (fast instanceof Pair pf) {
                fast = pf.cdr;
                count++;
                if (!(fast instanceof Pair pf2)) break;
                fast = pf2.cdr;
                count++;
                slow = ((Pair) slow).cdr;
                if (slow == fast) throw new EvalError("length: circular list");
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
        globalEnv.define("procedure?", (BuiltinProc) args -> args.get(0) instanceof Lambda || args.get(0) instanceof BuiltinProc || args.get(0) instanceof CaseLambda || args.get(0) instanceof SchemeContinuation || args.get(0) == CALLCC_PROC || args.get(0) == DYNAMIC_WIND_PROC || args.get(0) == VALUES_PROC || args.get(0) == CALL_WITH_VALUES_PROC);

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

        // --- syntax-case support builtins ---
        globalEnv.define("syntax->datum", (BuiltinProc) args -> {
            Object v = args.get(0);
            if (v instanceof SyntaxObject so) return so.datum;
            return v;
        });
        globalEnv.define("datum->syntax", (BuiltinProc) args -> {
            // (datum->syntax context-stx datum)
            Object datum = args.get(1);
            return new SyntaxObject(datum);
        });

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
            Object slow = args.get(0), fast = args.get(0);
            while (fast instanceof Pair pf) {
                fast = pf.cdr;
                if (!(fast instanceof Pair pf2)) return fast == NIL;
                fast = pf2.cdr;
                slow = ((Pair) slow).cdr;
                if (slow == fast) return false; // cycle
            }
            return fast == NIL;
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
                } else if (proc instanceof SchemeContinuation sc) {
                    throw new ContinuationReturn(sc.k, callArgs.get(0), sc.savedWind);
                } else {
                    throw new EvalError("map: not a procedure");
                }
            }
            Object result = NIL;
            for (int i = results.size() - 1; i >= 0; i--) result = new Pair(results.get(i), result);
            return result;
        });

        // for-each (like map but discards results)
        globalEnv.define("for-each", (BuiltinProc) args -> {
            if (args.size() < 2) throw new EvalError("for-each: requires at least 2 arguments");
            Object proc = args.get(0);
            int numLists = args.size() - 1;
            Object[] cursors = new Object[numLists];
            for (int i = 0; i < numLists; i++) cursors[i] = args.get(i + 1);
            while (true) {
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
                    builtin.apply(callArgs);
                } else if (proc instanceof Lambda lambda) {
                    applyLambda(lambda, callArgs);
                } else if (proc instanceof CaseLambda cl) {
                    applyCaseLambda(cl, callArgs);
                } else if (proc instanceof SchemeContinuation sc) {
                    throw new ContinuationReturn(sc.k, callArgs.get(0), sc.savedWind);
                } else {
                    throw new EvalError("for-each: not a procedure");
                }
            }
            return VOID;
        });

        // cxr helpers
        globalEnv.define("caar", (BuiltinProc) args -> { Pair p = asPair(args.get(0)); return asPair(p.car).car; });
        globalEnv.define("cadr", (BuiltinProc) args -> { Pair p = asPair(args.get(0)); return asPair(p.cdr).car; });
        globalEnv.define("cdar", (BuiltinProc) args -> { Pair p = asPair(args.get(0)); return asPair(p.car).cdr; });
        globalEnv.define("cddr", (BuiltinProc) args -> { Pair p = asPair(args.get(0)); return asPair(p.cdr).cdr; });
        globalEnv.define("caddr", (BuiltinProc) args -> { return asPair(asPair(asPair(args.get(0)).cdr).cdr).car; });
        globalEnv.define("cadddr", (BuiltinProc) args -> { return asPair(asPair(asPair(asPair(args.get(0)).cdr).cdr).cdr).car; });
        globalEnv.define("caaar", (BuiltinProc) args -> { return asPair(asPair(asPair(args.get(0)).car).car).car; });
        globalEnv.define("caadr", (BuiltinProc) args -> { return asPair(asPair(asPair(args.get(0)).cdr).car).car; });
        globalEnv.define("cadar", (BuiltinProc) args -> { return asPair(asPair(asPair(args.get(0)).car).cdr).car; });
        globalEnv.define("cdaar", (BuiltinProc) args -> { return asPair(asPair(asPair(args.get(0)).car).car).cdr; });
        globalEnv.define("cdadr", (BuiltinProc) args -> { return asPair(asPair(asPair(args.get(0)).cdr).car).cdr; });
        globalEnv.define("cddar", (BuiltinProc) args -> { return asPair(asPair(asPair(args.get(0)).car).cdr).cdr; });
        globalEnv.define("cdddr", (BuiltinProc) args -> { return asPair(asPair(asPair(args.get(0)).cdr).cdr).cdr; });
        globalEnv.define("caaaar", (BuiltinProc) args -> { return asPair(asPair(asPair(asPair(args.get(0)).car).car).car).car; });
        globalEnv.define("caaadr", (BuiltinProc) args -> { return asPair(asPair(asPair(asPair(args.get(0)).cdr).car).car).car; });
        globalEnv.define("caadar", (BuiltinProc) args -> { return asPair(asPair(asPair(asPair(args.get(0)).car).cdr).car).car; });
        globalEnv.define("caaddr", (BuiltinProc) args -> { return asPair(asPair(asPair(asPair(args.get(0)).cdr).cdr).car).car; });
        globalEnv.define("cadaar", (BuiltinProc) args -> { return asPair(asPair(asPair(asPair(args.get(0)).car).car).cdr).car; });
        globalEnv.define("cadadr", (BuiltinProc) args -> { return asPair(asPair(asPair(asPair(args.get(0)).cdr).car).cdr).car; });
        globalEnv.define("caddar", (BuiltinProc) args -> { return asPair(asPair(asPair(asPair(args.get(0)).car).cdr).cdr).car; });
        globalEnv.define("cdaaar", (BuiltinProc) args -> { return asPair(asPair(asPair(asPair(args.get(0)).car).car).car).cdr; });
        globalEnv.define("cdaadr", (BuiltinProc) args -> { return asPair(asPair(asPair(asPair(args.get(0)).cdr).car).car).cdr; });
        globalEnv.define("cdadar", (BuiltinProc) args -> { return asPair(asPair(asPair(asPair(args.get(0)).car).cdr).car).cdr; });
        globalEnv.define("cdaddr", (BuiltinProc) args -> { return asPair(asPair(asPair(asPair(args.get(0)).cdr).cdr).car).cdr; });
        globalEnv.define("cddaar", (BuiltinProc) args -> { return asPair(asPair(asPair(asPair(args.get(0)).car).car).cdr).cdr; });
        globalEnv.define("cddadr", (BuiltinProc) args -> { return asPair(asPair(asPair(asPair(args.get(0)).cdr).car).cdr).cdr; });
        globalEnv.define("cdddar", (BuiltinProc) args -> { return asPair(asPair(asPair(asPair(args.get(0)).car).cdr).cdr).cdr; });
        globalEnv.define("cddddr", (BuiltinProc) args -> { return asPair(asPair(asPair(asPair(args.get(0)).cdr).cdr).cdr).cdr; });

        // reverse
        globalEnv.define("reverse", (BuiltinProc) args -> {
            Object result = NIL;
            Object curr = args.get(0);
            while (curr instanceof Pair p) { result = new Pair(p.car, result); curr = p.cdr; }
            return result;
        });

        // Association list / membership
        globalEnv.define("memq", (BuiltinProc) args -> {
            Object obj = args.get(0);
            Object curr = args.get(1);
            while (curr instanceof Pair p) {
                if (schemeEq(obj, p.car)) return curr;
                curr = p.cdr;
            }
            return false;
        });
        globalEnv.define("memv", (BuiltinProc) args -> {
            Object obj = args.get(0);
            Object curr = args.get(1);
            while (curr instanceof Pair p) {
                if (schemeEq(obj, p.car)) return curr;
                curr = p.cdr;
            }
            return false;
        });
        globalEnv.define("member", (BuiltinProc) args -> {
            Object obj = args.get(0);
            Object curr = args.get(1);
            while (curr instanceof Pair p) {
                if (schemeEqual(obj, p.car)) return curr;
                curr = p.cdr;
            }
            return false;
        });
        globalEnv.define("assq", (BuiltinProc) args -> {
            Object key = args.get(0);
            Object curr = args.get(1);
            while (curr instanceof Pair p) {
                if (p.car instanceof Pair entry && schemeEq(key, entry.car)) return entry;
                curr = p.cdr;
            }
            return false;
        });
        globalEnv.define("assv", (BuiltinProc) args -> {
            Object key = args.get(0);
            Object curr = args.get(1);
            while (curr instanceof Pair p) {
                if (p.car instanceof Pair entry && schemeEq(key, entry.car)) return entry;
                curr = p.cdr;
            }
            return false;
        });

        // Math: gcd, lcm, truncate, round
        globalEnv.define("gcd", (BuiltinProc) args -> {
            if (args.isEmpty()) return 0L;
            long result = Math.abs(asLong(args.get(0)));
            for (int i = 1; i < args.size(); i++) {
                long b = Math.abs(asLong(args.get(i)));
                while (b != 0) { long t = b; b = result % b; result = t; }
            }
            return result;
        });
        globalEnv.define("lcm", (BuiltinProc) args -> {
            if (args.isEmpty()) return 1L;
            long result = Math.abs(asLong(args.get(0)));
            for (int i = 1; i < args.size(); i++) {
                long b = Math.abs(asLong(args.get(i)));
                if (result == 0 && b == 0) { result = 0; } else { result = result / gcdLong(result, b) * b; }
            }
            return result;
        });
        globalEnv.define("truncate", (BuiltinProc) args -> {
            Object v = args.get(0);
            if (v instanceof Long) return v;
            if (v instanceof Double d) return (long) d.doubleValue();
            if (v instanceof SchemeRational r) return r.num / r.den;
            throw new EvalError("truncate: expected number");
        });
        globalEnv.define("round", (BuiltinProc) args -> {
            Object v = args.get(0);
            if (v instanceof Long) return v;
            if (v instanceof Double d) return Math.round(d);
            if (v instanceof SchemeRational r) {
                long q = r.num / r.den;
                long rem = Math.abs(r.num % r.den);
                long halfDen = Math.abs(r.den) / 2;
                if (rem > halfDen || (rem == halfDen && Math.abs(r.den) % 2 == 0 && q % 2 != 0)) {
                    return r.num > 0 ? q + 1 : q - 1;
                }
                return q;
            }
            throw new EvalError("round: expected number");
        });

        // String constructors
        globalEnv.define("make-string", (BuiltinProc) args -> {
            int len = (int) asLong(args.get(0));
            char fill = args.size() > 1 ? ((SchemeChar) args.get(1)).value() : ' ';
            char[] chars = new char[len];
            java.util.Arrays.fill(chars, fill);
            return new SchemeString(new String(chars));
        });
        globalEnv.define("string", (BuiltinProc) args -> {
            StringBuilder sb = new StringBuilder();
            for (Object a : args) {
                if (!(a instanceof SchemeChar c)) throw new EvalError("string: expected char");
                sb.append(c.value());
            }
            return new SchemeString(sb.toString());
        });

        // Additional string comparisons
        globalEnv.define("string>?", (BuiltinProc) args ->
                asSchemeString(args.get(0)).value().compareTo(asSchemeString(args.get(1)).value()) > 0);
        globalEnv.define("string<=?", (BuiltinProc) args ->
                asSchemeString(args.get(0)).value().compareTo(asSchemeString(args.get(1)).value()) <= 0);
        globalEnv.define("string>=?", (BuiltinProc) args ->
                asSchemeString(args.get(0)).value().compareTo(asSchemeString(args.get(1)).value()) >= 0);

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
            if (proc instanceof SchemeContinuation sc) {
                throw new ContinuationReturn(sc.k, callArgs.get(0), sc.savedWind);
            }
            throw new EvalError("apply: not a procedure");
        });

        // First-class continuations
        globalEnv.define("call/cc", CALLCC_PROC);
        globalEnv.define("call-with-current-continuation", CALLCC_PROC);

        // dynamic-wind
        globalEnv.define("dynamic-wind", DYNAMIC_WIND_PROC);

        // exception handling
        globalEnv.define("with-exception-handler", WITH_EXCEPTION_HANDLER_PROC);
        globalEnv.define("raise", RAISE_PROC);

        // Multiple values
        globalEnv.define("values", VALUES_PROC);
        globalEnv.define("call-with-values", CALL_WITH_VALUES_PROC);
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
        List<Object> forms = new ArrayList<>();
        while (pos[0] < tokens.size()) {
            forms.add(parse(tokens, pos));
        }
        if (forms.isEmpty()) throw new EvalError("no expression");
        Object lastResult = evalForms(forms, globalEnv);
        if (lastResult == null || lastResult == VOID) throw new EvalError("no expression");
        return schemeToString(lastResult);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        outputBuffer.setLength(0);
        List<Token> tokens = tokenize(input);
        int[] pos = {0};
        List<Object> forms = new ArrayList<>();
        while (pos[0] < tokens.size()) {
            forms.add(parse(tokens, pos));
        }
        if (forms.isEmpty()) return new EvalResult("", outputBuffer.toString());
        Object lastResult = evalForms(forms, globalEnv);
        String result = (lastResult == null || lastResult == VOID) ? "" : schemeToString(lastResult);
        return new EvalResult(result, outputBuffer.toString());
    }

    private Object evalForms(List<Object> forms, Env env) throws EvalError {
        if (forms.size() == 1) {
            return evalCEK(forms.get(0), env, new HaltK());
        }
        Kont k = new SeqK(forms, 1, env, new HaltK());
        return evalCEK(forms.get(0), env, k);
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
                    if (next == '\'') {
                        // #' = syntax quote
                        tokens.add(new Token("#'", line, startCol));
                        i += 2;
                        col += 2;
                    } else if (next == 't') {
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
        if ("#'".equals(token.value)) {
            pos[0]++;
            Object syntaxed = parse(tokens, pos);
            List<Object> syntaxExpr = new ArrayList<>();
            syntaxExpr.add("syntax");
            syntaxExpr.add(syntaxed);
            return new SourceExpr(syntaxExpr, token.line, token.col);
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

    // --- CEK Machine Evaluator ---

    // Backward-compatible eval for builtins (map, for-each, apply) that need to call eval
    private Object eval(Object expr, Env env) throws EvalError {
        Object se = cekExpr; Env sn = cekEnv; Kont sk = cekK;
        Object sv = cekValue; boolean sl = cekEval;
        try {
            return evalCEK(expr, env, new HaltK());
        } finally {
            cekExpr = se; cekEnv = sn; cekK = sk;
            cekValue = sv; cekEval = sl;
        }
    }

    @SuppressWarnings("unchecked")
    private Object evalCEK(Object startExpr, Env startEnv, Kont startK) throws EvalError {
        cekExpr = startExpr;
        cekEnv = startEnv;
        cekK = startK;
        cekEval = true;

        while (true) {
            try {
                if (cekEval) {
                    cekEvalStep();
                } else {
                    if (cekK instanceof HaltK) return cekValue;
                    cekApplyKont();
                }
            } catch (ContinuationReturn cr) {
                List<WindEntry> targetWind = cr.targetWind != null ? cr.targetWind : List.of();
                // Compute common prefix
                int commonLen = 0;
                int minLen = Math.min(windStack.size(), targetWind.size());
                for (int i = 0; i < minLen; i++) {
                    if (windStack.get(i) == targetWind.get(i)) commonLen++;
                    else break;
                }
                // Build thunk list: out-thunks (innermost first), then in-thunks (outermost first)
                List<Object> thunks = new ArrayList<>();
                for (int i = windStack.size() - 1; i >= commonLen; i--) {
                    thunks.add(windStack.get(i).outThunk);
                }
                for (int i = commonLen; i < targetWind.size(); i++) {
                    thunks.add(targetWind.get(i).inThunk);
                }
                if (!thunks.isEmpty()) {
                    // Execute thunks, then restore continuation
                    cekK = new DynWindDoThunksK(thunks, 0, targetWind, cr.k, cr.value, cekK);
                    // Need to properly unwind current stack before running thunks
                    // Trim wind stack to common prefix before running out-thunks
                    while (windStack.size() > commonLen) windStack.remove(windStack.size() - 1);
                    try {
                        cekApplyProc(thunks.get(0), List.of(), cekK);
                    } catch (EvalError e) {
                        throw e;
                    }
                } else {
                    cekK = cr.k;
                    cekValue = cr.value;
                    cekEval = false;
                }
            } catch (SchemeRaise sr) {
                if (handlerStack.isEmpty()) {
                    throw new EvalError("unhandled exception: " + schemeToString(sr.value));
                }
                ExceptionHandlerEntry handler = handlerStack.remove(handlerStack.size() - 1);
                if (handler.isGuard) {
                    // Unwind dynamic-wind back to guard (same logic as ContinuationReturn handler)
                    Kont targetK = new GuardClauseK(handler.var, handler.clauses, handler.guardEnv, sr.value, handler.guardK);
                    List<WindEntry> targetWind = handler.savedWind != null ? handler.savedWind : List.of();
                    int commonLen = 0;
                    int minLen = Math.min(windStack.size(), targetWind.size());
                    for (int i = 0; i < minLen; i++) {
                        if (windStack.get(i) == targetWind.get(i)) commonLen++;
                        else break;
                    }
                    List<Object> thunks = new ArrayList<>();
                    for (int i = windStack.size() - 1; i >= commonLen; i--) {
                        thunks.add(windStack.get(i).outThunk);
                    }
                    for (int i = commonLen; i < targetWind.size(); i++) {
                        thunks.add(targetWind.get(i).inThunk);
                    }
                    if (!thunks.isEmpty()) {
                        cekK = new DynWindDoThunksK(thunks, 0, targetWind, targetK, sr.value, cekK);
                        while (windStack.size() > commonLen) windStack.remove(windStack.size() - 1);
                        cekApplyProc(thunks.get(0), List.of(), cekK);
                    } else {
                        cekK = targetK;
                        cekValue = sr.value;
                        cekEval = false;
                    }
                } else {
                    // with-exception-handler: call handler in raiser's dynamic extent
                    cekApplyProc(handler.handlerProc, List.of(sr.value), cekK);
                }
            } catch (EvalError e) {
                if (cekSrcLine >= 0 && !e.getMessage().matches(".*\\d+:\\d+.*")) {
                    throw new EvalError(e.getMessage() + " at " + cekSrcLine + ":" + cekSrcCol);
                }
                throw e;
            }
        }
    }

    @SuppressWarnings("unchecked")
    private void cekEvalStep() throws EvalError {
        Object expr = cekExpr;
        Env env = cekEnv;

        int srcLine = -1, srcCol = -1;
        while (expr instanceof SourceExpr se) {
            srcLine = se.line; srcCol = se.col;
            expr = se.expr;
        }
        if (srcLine >= 0) { cekSrcLine = srcLine; cekSrcCol = srcCol; }

        try {
            if (expr instanceof ResolvedValue rv) { cekValue = rv.value; cekEval = false; return; }
            if (expr instanceof Long || expr instanceof Double || expr instanceof SchemeRational ||
                expr instanceof Boolean || expr instanceof SchemeString || expr instanceof SchemeChar) {
                cekValue = expr; cekEval = false; return;
            }
            if (expr instanceof String sym) {
                cekValue = env.lookup(sym); cekEval = false; return;
            }
            if (expr instanceof List<?> rawList) {
                List<Object> list = (List<Object>) rawList;
                if (list.isEmpty()) throw new EvalError("empty application");

                Object head = list.get(0);
                Object rawHead = head;
                if (rawHead instanceof SourceExpr she) rawHead = she.expr;

                if (rawHead instanceof String op) {
                    switch (op) {
                        case "quote" -> {
                            if (list.size() < 2) throw new EvalError("bad syntax: quote");
                            cekValue = astToScheme(list.get(1)); cekEval = false; return;
                        }
                        case "if" -> {
                            if (list.size() < 3) throw new EvalError("bad syntax: if requires at least 2 parts");
                            cekK = new IfK(list.get(2), list.size() > 3 ? list.get(3) : null, list.size() > 3, env, cekK);
                            cekExpr = list.get(1); return;
                        }
                        case "define" -> {
                            if (list.size() < 2) throw new EvalError("bad syntax: define");
                            Object target = list.get(1);
                            if (target instanceof SourceExpr se) target = se.expr;
                            if (target instanceof String name) {
                                if (list.size() < 3) throw new EvalError("bad syntax: define");
                                cekK = new DefineK(name, env, cekK);
                                cekExpr = list.get(2); return;
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
                                cekValue = VOID; cekEval = false; return;
                            }
                            throw new EvalError("bad syntax: define");
                        }
                        case "lambda" -> {
                            if (list.size() < 3) throw new EvalError("bad syntax: lambda");
                            Object paramObj = list.get(1);
                            if (paramObj instanceof SourceExpr se) paramObj = se.expr;
                            List<Object> body = new ArrayList<>(list.subList(2, list.size()));
                            if (paramObj instanceof String restOnly) {
                                cekValue = new Lambda(List.of(), restOnly, body, env); cekEval = false; return;
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
                            cekValue = new Lambda(params, restParam, body, env); cekEval = false; return;
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
                            cekValue = new CaseLambda(clauses); cekEval = false; return;
                        }
                        case "set!" -> {
                            if (list.size() != 3) throw new EvalError("bad syntax: set!");
                            Object varObj = list.get(1);
                            if (varObj instanceof SourceExpr se) varObj = se.expr;
                            if (!(varObj instanceof String varName)) throw new EvalError("bad syntax: set!");
                            cekK = new SetK(varName, env, cekK);
                            cekExpr = list.get(2); return;
                        }
                        case "begin" -> {
                            if (list.size() == 1) { cekValue = VOID; cekEval = false; return; }
                            if (list.size() == 2) { cekExpr = list.get(1); return; }
                            cekK = new SeqK(list, 2, env, cekK);
                            cekExpr = list.get(1); return;
                        }
                        case "and" -> {
                            if (list.size() == 1) { cekValue = Boolean.TRUE; cekEval = false; return; }
                            if (list.size() == 2) { cekExpr = list.get(1); return; }
                            cekK = new AndK(list, 2, env, cekK);
                            cekExpr = list.get(1); return;
                        }
                        case "or" -> {
                            if (list.size() == 1) { cekValue = Boolean.FALSE; cekEval = false; return; }
                            if (list.size() == 2) { cekExpr = list.get(1); return; }
                            cekK = new OrK(list, 2, env, cekK);
                            cekExpr = list.get(1); return;
                        }
                        case "cond" -> {
                            cekHandleCond(list, env); return;
                        }
                        case "let" -> {
                            cekHandleLet(list, env); return;
                        }
                        case "let*" -> {
                            cekHandleLetStar(list, env); return;
                        }
                        case "letrec", "letrec*" -> {
                            cekHandleLetrec(list, env); return;
                        }
                        case "case" -> {
                            cekK = new CaseKeyK(list, env, cekK);
                            cekExpr = list.get(1); return;
                        }
                        case "do" -> {
                            cekHandleDo(list, env); return;
                        }
                        case "define-syntax" -> {
                            Object nameObj = list.get(1);
                            if (nameObj instanceof SourceExpr se) nameObj = se.expr;
                            String macroName = (String) nameObj;
                            Object srObj = list.get(2);
                            Object rawSr = srObj;
                            if (rawSr instanceof SourceExpr se) rawSr = se.expr;
                            // Check if body is (syntax-rules ...) or a transformer expression
                            boolean isSyntaxRules = false;
                            if (rawSr instanceof List<?> srList && !srList.isEmpty()) {
                                Object first = srList.get(0);
                                if (first instanceof SourceExpr se) first = se.expr;
                                if ("syntax-rules".equals(first)) isSyntaxRules = true;
                            }
                            if (isSyntaxRules) {
                                List<Object> sr = (List<Object>) rawSr;
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
                                    List<Object> rule = (List<Object>) ruleObj;
                                    rules.add(new Object[]{rule.get(0), rule.get(1)});
                                }
                                env.define(macroName, new SyntaxRulesMacro(lits, rules, env));
                            } else {
                                // Transformer expression (e.g., lambda)
                                Object transformer = eval(srObj, env);
                                env.define(macroName, new SyntaxCaseTransformer(transformer, env));
                            }
                            cekValue = VOID; cekEval = false; return;
                        }
                        case "define-record-type" -> {
                            cekHandleDefineRecordType(list, env);
                            cekValue = VOID; cekEval = false; return;
                        }
                        case "guard" -> {
                            cekHandleGuard(list, env); return;
                        }
                        case "syntax-case" -> {
                            // (syntax-case expr (lits ...) clause ...)
                            cekK = new SyntaxCaseK(list, env, cekK);
                            cekExpr = list.get(1); return;
                        }
                        case "syntax" -> {
                            // #'template - expand template with pattern variable substitution
                            Object template = list.get(1);
                            Map<String, String> renames = new HashMap<>();
                            Object result = expandSyntaxTemplate(template, env, renames);
                            cekValue = new SyntaxObject(result);
                            cekEval = false; return;
                        }
                        case "with-syntax" -> {
                            // (with-syntax ((pat expr) ...) body ...)
                            Object bindingsObj = unwrapSE(list.get(1));
                            List<?> bindingsList = (List<?>) bindingsObj;
                            List<Object[]> bindings = new ArrayList<>();
                            for (Object b : bindingsList) {
                                Object bu = unwrapSE(b);
                                List<?> bl = (List<?>) bu;
                                bindings.add(new Object[]{bl.get(0), bl.get(1)});
                            }
                            List<Object> body = new ArrayList<>(list.subList(2, list.size()));
                            Env wsEnv = new Env(env);
                            if (bindings.isEmpty()) {
                                cekEnv = wsEnv;
                                evalBodyExprs(body, 0, wsEnv); return;
                            }
                            cekK = new WithSyntaxK(bindings, 0, wsEnv, body, cekK);
                            cekExpr = bindings.get(0)[1];
                            cekEval = true; return;
                        }
                    }
                }

                // Function application: evaluate proc first
                cekK = new EvProcK(list, env, cekK);
                cekExpr = head; return;
            }
            throw new EvalError("unknown expression type");
        } catch (EvalError e) {
            if (srcLine >= 0 && !e.getMessage().matches(".*\\d+:\\d+.*")) {
                throw new EvalError(e.getMessage() + " at " + srcLine + ":" + srcCol);
            }
            throw e;
        }
    }

    @SuppressWarnings("unchecked")
    private void cekApplyKont() throws EvalError {
        Object value = cekValue;
        Kont k = cekK;

        if (k instanceof IfK ik) {
            cekEnv = ik.env; cekK = ik.k;
            if (!isFalse(value)) {
                cekExpr = ik.thenE; cekEval = true;
            } else if (ik.hasElse) {
                cekExpr = ik.elseE; cekEval = true;
            } else {
                cekValue = VOID;
            }
            return;
        }

        if (k instanceof SeqK sq) {
            cekEnv = sq.env;
            if (sq.nextIdx < sq.exprs.size() - 1) {
                cekK = new SeqK(sq.exprs, sq.nextIdx + 1, sq.env, sq.k);
            } else {
                cekK = sq.k;
            }
            cekExpr = sq.exprs.get(sq.nextIdx); cekEval = true;
            return;
        }

        if (k instanceof DefineK dk) {
            dk.env.define(dk.name, value);
            cekValue = VOID; cekK = dk.k; return;
        }

        if (k instanceof SetK sk) {
            sk.env.set(sk.name, value);
            cekValue = VOID; cekK = sk.k; return;
        }

        if (k instanceof EvProcK epk) {
            Object proc = value;
            List<Object> list = epk.list;
            Env env = epk.env;
            // Handle macros
            if (proc instanceof SyntaxRulesMacro macro) {
                cekExpr = expandMacro(macro, list);
                cekEnv = env; cekK = epk.k; cekEval = true; return;
            }
            if (proc instanceof SyntaxCaseTransformer sct) {
                SyntaxObject stxObj = new SyntaxObject(list);
                Env tempEnv = new Env(env);
                String tfVar = gensym("__tf");
                String stxVar = gensym("__sx");
                tempEnv.define(tfVar, sct.transformer);
                tempEnv.define(stxVar, stxObj);
                Object result = eval(List.of(tfVar, stxVar), tempEnv);
                if (result instanceof SyntaxObject so) result = so.datum;
                cekExpr = result;
                cekEnv = env; cekK = epk.k; cekEval = true; return;
            }
            int argCount = list.size() - 1;
            if (argCount == 0) {
                cekApplyProc(proc, List.of(), epk.k); return;
            }
            // Right-to-left argument evaluation: start from rightmost arg
            cekK = new EvArgK(proc, list, list.size() - 1, new ArrayList<>(), env, epk.k);
            cekExpr = list.get(list.size() - 1); cekEnv = env; cekEval = true; return;
        }

        if (k instanceof EvArgK eak) {
            List<Object> newEval = new ArrayList<>(eak.evaluated);
            newEval.add(value);
            int nextIdx = eak.currentIdx - 1;
            if (nextIdx >= 1) {
                cekK = new EvArgK(eak.proc, eak.list, nextIdx, newEval, eak.env, eak.k);
                cekExpr = eak.list.get(nextIdx); cekEnv = eak.env; cekEval = true; return;
            }
            // All args evaluated - reverse to get left-to-right order
            Collections.reverse(newEval);
            cekApplyProc(eak.proc, newEval, eak.k); return;
        }

        if (k instanceof AndK ak) {
            if (isFalse(value)) { cekK = ak.k; return; /* value stays false */ }
            cekEnv = ak.env;
            if (ak.nextIdx < ak.exprs.size() - 1) {
                cekK = new AndK(ak.exprs, ak.nextIdx + 1, ak.env, ak.k);
            } else {
                cekK = ak.k; // tail position
            }
            cekExpr = ak.exprs.get(ak.nextIdx); cekEval = true; return;
        }

        if (k instanceof OrK ok) {
            if (!isFalse(value)) { cekK = ok.k; return; /* value stays truthy */ }
            cekEnv = ok.env;
            if (ok.nextIdx < ok.exprs.size() - 1) {
                cekK = new OrK(ok.exprs, ok.nextIdx + 1, ok.env, ok.k);
            } else {
                cekK = ok.k; // tail position
            }
            cekExpr = ok.exprs.get(ok.nextIdx); cekEval = true; return;
        }

        if (k instanceof CondTestK ctk) {
            if (!isFalse(value)) {
                // Test passed - evaluate clause body
                if (ctk.clause.size() == 1) { cekK = ctk.k; return; /* return test value */ }
                cekEnv = ctk.env; cekK = ctk.k;
                evalBodyExprs(ctk.clause, 1, ctk.env);
                return;
            }
            // Test failed - try next clause
            cekHandleCondFrom(ctk.fullList, ctk.nextClauseIdx, ctk.env, ctk.k); return;
        }

        if (k instanceof LetBindK lbk) {
            List<Object> newEval = new ArrayList<>(lbk.evaluated);
            newEval.add(value);
            if (lbk.nextIdx < lbk.inits.size()) {
                cekK = new LetBindK(lbk.params, lbk.inits, lbk.nextIdx + 1, newEval, lbk.outerEnv, lbk.body, lbk.loopName, lbk.k);
                cekExpr = lbk.inits.get(lbk.nextIdx); cekEnv = lbk.outerEnv; cekEval = true; return;
            }
            // All inits evaluated
            if (lbk.loopName != null) {
                Env letEnv = new Env(lbk.outerEnv);
                Lambda loopLambda = new Lambda(lbk.params, null, lbk.body, letEnv);
                letEnv.define(lbk.loopName, loopLambda);
                Env callEnv = new Env(loopLambda.closureEnv);
                for (int i = 0; i < lbk.params.size(); i++) {
                    callEnv.define(lbk.params.get(i), newEval.get(i));
                }
                cekEnv = callEnv; cekK = lbk.k;
                evalBodyExprs(lbk.body, 0, callEnv);
            } else {
                Env letEnv = new Env(lbk.outerEnv);
                for (int i = 0; i < lbk.params.size(); i++) {
                    letEnv.define(lbk.params.get(i), newEval.get(i));
                }
                if (lbk.body.isEmpty()) { cekValue = VOID; cekK = lbk.k; return; }
                cekEnv = letEnv; cekK = lbk.k;
                evalBodyExprs(lbk.body, 0, letEnv);
            }
            return;
        }

        if (k instanceof LetStarBindK lsk) {
            lsk.letEnv.define(lsk.names.get(lsk.nextIdx - 1), value);
            if (lsk.nextIdx < lsk.inits.size()) {
                cekK = new LetStarBindK(lsk.names, lsk.inits, lsk.nextIdx + 1, lsk.letEnv, lsk.body, lsk.k);
                cekExpr = lsk.inits.get(lsk.nextIdx); cekEnv = lsk.letEnv; cekEval = true; return;
            }
            if (lsk.body.isEmpty()) { cekValue = VOID; cekK = lsk.k; return; }
            cekEnv = lsk.letEnv; cekK = lsk.k;
            evalBodyExprs(lsk.body, 0, lsk.letEnv);
            return;
        }

        if (k instanceof LetrecBindK lrk) {
            lrk.letEnv.define(lrk.names.get(lrk.nextIdx - 1), value);
            if (lrk.nextIdx < lrk.inits.size()) {
                cekK = new LetrecBindK(lrk.names, lrk.inits, lrk.nextIdx + 1, lrk.letEnv, lrk.body, lrk.k);
                cekExpr = lrk.inits.get(lrk.nextIdx); cekEnv = lrk.letEnv; cekEval = true; return;
            }
            if (lrk.body.isEmpty()) { cekValue = VOID; cekK = lrk.k; return; }
            cekEnv = lrk.letEnv; cekK = lrk.k;
            evalBodyExprs(lrk.body, 0, lrk.letEnv);
            return;
        }

        if (k instanceof CaseKeyK ck) {
            Object key = value;
            List<Object> list = ck.list;
            Env env = ck.env;
            for (int i = 2; i < list.size(); i++) {
                Object clauseObj = list.get(i);
                if (clauseObj instanceof SourceExpr se) clauseObj = se.expr;
                List<Object> clause = (List<Object>) clauseObj;
                Object datums = clause.get(0);
                if (datums instanceof SourceExpr se) datums = se.expr;
                if ("else".equals(datums)) {
                    cekEnv = env; cekK = ck.k;
                    evalBodyExprs(clause, 1, env); return;
                }
                List<?> datumList = (List<?>) datums;
                for (Object d : datumList) {
                    Object datum = d;
                    if (datum instanceof SourceExpr se) datum = se.expr;
                    if (schemeEq(key, datum)) {
                        cekEnv = env; cekK = ck.k;
                        evalBodyExprs(clause, 1, env); return;
                    }
                }
            }
            cekValue = VOID; cekK = ck.k; return;
        }

        if (k instanceof DoInitK dik) {
            List<Object> newEval = new ArrayList<>(dik.evaluated);
            newEval.add(value);
            if (dik.nextIdx < dik.inits.size()) {
                cekK = new DoInitK(dik.varNames, dik.inits, dik.nextIdx + 1, newEval, dik.outerEnv, dik.testClause, dik.steps, dik.fullList, dik.k);
                cekExpr = dik.inits.get(dik.nextIdx); cekEnv = dik.outerEnv; cekEval = true; return;
            }
            // All inits evaluated - create do env and start loop
            Env doEnv = new Env(dik.outerEnv);
            for (int i = 0; i < dik.varNames.size(); i++) {
                doEnv.define(dik.varNames.get(i), newEval.get(i));
            }
            // Evaluate test
            cekK = new DoTestK(dik.varNames, doEnv, dik.testClause, dik.steps, dik.fullList, dik.k);
            cekExpr = dik.testClause.get(0); cekEnv = doEnv; cekEval = true; return;
        }

        if (k instanceof DoTestK dtk) {
            if (!isFalse(value)) {
                // Test passed - evaluate result exprs
                if (dtk.testClause.size() > 1) {
                    cekEnv = dtk.doEnv; cekK = dtk.k;
                    evalBodyExprs(dtk.testClause, 1, dtk.doEnv); return;
                }
                cekValue = VOID; cekK = dtk.k; return;
            }
            // Test failed - evaluate body, then steps
            List<Object> bodyExprs = new ArrayList<>();
            for (int i = 3; i < dtk.fullList.size(); i++) bodyExprs.add(dtk.fullList.get(i));
            if (bodyExprs.isEmpty()) {
                // No body, go straight to steps
                cekStartDoSteps(dtk.varNames, dtk.doEnv, dtk.steps, dtk.testClause, dtk.fullList, dtk.k); return;
            }
            // Evaluate body, then steps
            Kont afterBody = new DoAfterBodyK(dtk.varNames, dtk.doEnv, dtk.steps, dtk.testClause, dtk.fullList, dtk.k);
            cekEnv = dtk.doEnv; cekK = afterBody;
            if (bodyExprs.size() == 1) {
                cekExpr = bodyExprs.get(0); cekEval = true;
            } else {
                cekK = new SeqK(bodyExprs, 1, dtk.doEnv, afterBody);
                cekExpr = bodyExprs.get(0); cekEval = true;
            }
            return;
        }

        if (k instanceof DoAfterBodyK dabk) {
            // Body done, start step evaluation
            cekStartDoSteps(dabk.varNames, dabk.doEnv, dabk.steps, dabk.testClause, dabk.fullList, dabk.k); return;
        }

        if (k instanceof DoStepK dsk) {
            List<Object> newVals = new ArrayList<>(dsk.newVals);
            newVals.add(value);
            if (dsk.nextIdx < dsk.steps.size()) {
                Object stepExpr = dsk.steps.get(dsk.nextIdx);
                if (stepExpr == null) {
                    // No step expression, keep current value
                    try { newVals.add(dsk.doEnv.lookup(dsk.varNames.get(dsk.nextIdx))); }
                    catch (EvalError e) { throw e; }
                    // Continue to next step
                    if (dsk.nextIdx + 1 < dsk.steps.size()) {
                        Object nextStep = dsk.steps.get(dsk.nextIdx + 1);
                        if (nextStep == null) {
                            // Also no step, add current val and continue
                            cekK = new DoStepK(dsk.varNames, dsk.doEnv, dsk.steps, dsk.nextIdx + 1, newVals, dsk.testClause, dsk.fullList, dsk.k);
                            // Trigger apply with dummy value
                            cekValue = VOID; return;
                        }
                        cekK = new DoStepK(dsk.varNames, dsk.doEnv, dsk.steps, dsk.nextIdx + 1, newVals, dsk.testClause, dsk.fullList, dsk.k);
                        cekExpr = nextStep; cekEnv = dsk.doEnv; cekEval = true; return;
                    }
                    // Fall through to update vars
                } else {
                    if (dsk.nextIdx + 1 < dsk.steps.size()) {
                        cekK = new DoStepK(dsk.varNames, dsk.doEnv, dsk.steps, dsk.nextIdx + 1, newVals, dsk.testClause, dsk.fullList, dsk.k);
                        Object nextStep = dsk.steps.get(dsk.nextIdx + 1);
                        if (nextStep != null) {
                            cekExpr = nextStep; cekEnv = dsk.doEnv; cekEval = true; return;
                        }
                        // No step for next var, use current value
                        try { newVals.add(dsk.doEnv.lookup(dsk.varNames.get(dsk.nextIdx + 1))); }
                        catch (EvalError e) { throw e; }
                        cekValue = VOID; return; // re-enter apply for DoStepK
                    }
                    // Fall through to update vars
                }
            }
            // All steps evaluated - update vars and loop back to test
            for (int i = 0; i < dsk.varNames.size(); i++) {
                dsk.doEnv.define(dsk.varNames.get(i), newVals.get(i));
            }
            cekK = new DoTestK(dsk.varNames, dsk.doEnv, dsk.testClause, dsk.steps, dsk.fullList, dsk.k);
            cekExpr = dsk.testClause.get(0); cekEnv = dsk.doEnv; cekEval = true; return;
        }

        if (k instanceof GuardBodyK gbk) {
            // Body completed normally; pop handler, pass value through
            if (!handlerStack.isEmpty()) handlerStack.remove(handlerStack.size() - 1);
            cekK = gbk.k;
            return;
        }

        if (k instanceof GuardClauseK gck) {
            // Exception caught; bind var and evaluate cond-like clauses
            Env clauseEnv = new Env(gck.guardEnv);
            clauseEnv.define(gck.var, gck.exnValue);
            cekHandleGuardClauses(gck.clauses, clauseEnv, gck.k, gck.exnValue);
            return;
        }

        if (k instanceof WehK wehk) {
            // Thunk completed normally; pop handler, pass value through
            if (!handlerStack.isEmpty()) handlerStack.remove(handlerStack.size() - 1);
            cekK = wehk.k;
            return;
        }

        if (k instanceof SyntaxCaseK sck) {
            Object scrutinee = value;
            if (scrutinee instanceof SyntaxObject so) scrutinee = so.datum;
            List<Object> fullList = sck.fullList;
            Env scEnv = sck.env;

            // Parse literals
            Object litsObj = unwrapSE(fullList.get(2));
            List<String> literals = new ArrayList<>();
            for (Object l : (List<?>) litsObj) {
                literals.add((String) unwrapSE(l));
            }

            // Try each clause
            for (int ci = 3; ci < fullList.size(); ci++) {
                Object clauseObj = unwrapSE(fullList.get(ci));
                List<Object> clause = (List<Object>) clauseObj;

                Object pattern = unwrapSE(clause.get(0));
                boolean hasFender = clause.size() == 3;
                Object body = hasFender ? clause.get(2) : clause.get(1);

                Map<String, Object> bindings = new HashMap<>();
                Set<String> patternVars = new HashSet<>();
                Set<String> ellipsisVars = new HashSet<>();

                boolean matched;
                if (pattern instanceof List<?> patList) {
                    if (!(scrutinee instanceof List<?>)) {
                        matched = false;
                    } else {
                        matched = matchPatternList(patList, (List<?>) scrutinee, 0, literals, bindings, patternVars, ellipsisVars);
                    }
                } else {
                    matched = matchPattern(pattern, scrutinee, literals, bindings, patternVars, ellipsisVars);
                }

                if (matched) {
                    // Bind pattern variables as SyntaxObjects / SyntaxEllipsis
                    Env bodyEnv = new Env(scEnv);
                    for (var entry : bindings.entrySet()) {
                        if (ellipsisVars.contains(entry.getKey())) {
                            bodyEnv.define(entry.getKey(), new SyntaxEllipsis((List<Object>) entry.getValue()));
                        } else {
                            bodyEnv.define(entry.getKey(), new SyntaxObject(entry.getValue()));
                        }
                    }

                    if (hasFender) {
                        Object fender = clause.get(1);
                        Object fResult = eval(fender, bodyEnv);
                        if (isFalse(fResult)) continue;
                    }

                    cekExpr = body; cekEnv = bodyEnv; cekK = sck.k; cekEval = true; return;
                }
            }
            throw new EvalError("no matching syntax-case pattern");
        }

        if (k instanceof WithSyntaxK wsk) {
            Object pattern = unwrapSE(wsk.bindings.get(wsk.nextIdx)[0]);
            // Simple pattern: just a symbol
            if (pattern instanceof String sym) {
                wsk.wsEnv.define(sym, value instanceof SyntaxObject ? value : new SyntaxObject(value));
            }
            int nextIdx = wsk.nextIdx + 1;
            if (nextIdx < wsk.bindings.size()) {
                cekK = new WithSyntaxK(wsk.bindings, nextIdx, wsk.wsEnv, wsk.body, wsk.k);
                cekExpr = wsk.bindings.get(nextIdx)[1];
                cekEnv = wsk.wsEnv;
                cekEval = true; return;
            }
            cekEnv = wsk.wsEnv; cekK = wsk.k;
            evalBodyExprs(wsk.body, 0, wsk.wsEnv);
            return;
        }

        if (k instanceof CallWithValuesK cwv) {
            // Producer completed; pass its values to consumer
            List<Object> vals;
            if (value instanceof MultipleValues mv) {
                vals = mv.values;
            } else {
                vals = List.of(value);
            }
            cekApplyProc(cwv.consumer, vals, cwv.k); return;
        }

        if (k instanceof DynWindAfterInK dwi) {
            // in-thunk done; push wind entry, call body-thunk
            WindEntry entry = new WindEntry(dwi.inThunk, dwi.outThunk);
            windStack.add(entry);
            cekK = new DynWindAfterBodyK(dwi.inThunk, dwi.outThunk, dwi.k);
            cekApplyProc(dwi.bodyThunk, List.of(), cekK); return;
        }

        if (k instanceof DynWindAfterBodyK dwb) {
            // body done; save result, pop wind entry, call out-thunk
            Object bodyValue = value;
            if (!windStack.isEmpty()) windStack.remove(windStack.size() - 1);
            cekK = new DynWindAfterOutK(bodyValue, dwb.k);
            cekApplyProc(dwb.outThunk, List.of(), cekK); return;
        }

        if (k instanceof DynWindAfterOutK dwo) {
            // out-thunk done; return body value
            cekValue = dwo.bodyValue; cekK = dwo.k; return;
        }

        if (k instanceof DynWindDoThunksK dwt) {
            // Just finished a thunk; advance to next
            if (dwt.idx + 1 < dwt.thunks.size()) {
                cekK = new DynWindDoThunksK(dwt.thunks, dwt.idx + 1, dwt.targetWind, dwt.targetK, dwt.targetValue, dwt.k);
                cekApplyProc(dwt.thunks.get(dwt.idx + 1), List.of(), cekK); return;
            }
            // All thunks done; restore target wind stack and continuation
            windStack = new ArrayList<>(dwt.targetWind);
            cekK = dwt.targetK;
            cekValue = dwt.targetValue;
            cekEval = false;
            return;
        }

        throw new EvalError("unknown continuation type: " + k.getClass().getSimpleName());
    }

    // Helper: apply a procedure to arguments
    private void cekApplyProc(Object proc, List<Object> args, Kont k) throws EvalError {
        if (proc instanceof BuiltinProc bp) {
            cekValue = bp.apply(args); cekK = k; cekEval = false; return;
        }
        if (proc instanceof Lambda lambda) {
            Env callEnv = bindLambdaArgs(lambda, args);
            cekEnv = callEnv; cekK = k;
            evalBodyExprs(lambda.body, 0, callEnv); return;
        }
        if (proc instanceof CaseLambda cl) {
            Lambda lambda = matchCaseLambda(cl, args.size());
            Env callEnv = bindLambdaArgs(lambda, args);
            cekEnv = callEnv; cekK = k;
            evalBodyExprs(lambda.body, 0, callEnv); return;
        }
        if (proc instanceof SchemeContinuation sc) {
            if (args.isEmpty()) throw new EvalError("continuation requires at least 1 argument");
            Object val = args.size() == 1 ? args.get(0) : new MultipleValues(args);
            throw new ContinuationReturn(sc.k, val, sc.savedWind);
        }
        if (proc == CALLCC_PROC) {
            if (args.size() != 1) throw new EvalError("call/cc requires 1 argument");
            SchemeContinuation sc = new SchemeContinuation(k, new ArrayList<>(windStack));
            cekApplyProc(args.get(0), List.of(sc), k); return;
        }
        if (proc == RAISE_PROC) {
            if (args.size() != 1) throw new EvalError("raise requires 1 argument");
            throw new SchemeRaise(args.get(0));
        }
        if (proc == WITH_EXCEPTION_HANDLER_PROC) {
            if (args.size() != 2) throw new EvalError("with-exception-handler requires 2 arguments");
            Object handler = args.get(0);
            Object thunk = args.get(1);
            handlerStack.add(new ExceptionHandlerEntry(handler));
            cekK = new WehK(k);
            cekApplyProc(thunk, List.of(), cekK); return;
        }
        if (proc == DYNAMIC_WIND_PROC) {
            if (args.size() != 3) throw new EvalError("dynamic-wind requires 3 arguments");
            Object inThunk = args.get(0);
            Object bodyThunk = args.get(1);
            Object outThunk = args.get(2);
            // Call in-thunk first, then DynWindAfterInK handles the rest
            cekK = new DynWindAfterInK(inThunk, bodyThunk, outThunk, k);
            cekApplyProc(inThunk, List.of(), cekK); return;
        }
        if (proc == VALUES_PROC) {
            if (args.size() == 1) {
                cekValue = args.get(0);
            } else {
                cekValue = new MultipleValues(args);
            }
            cekK = k;
            return;
        }
        if (proc == CALL_WITH_VALUES_PROC) {
            if (args.size() != 2) throw new EvalError("call-with-values requires 2 arguments");
            Object producer = args.get(0);
            Object consumer = args.get(1);
            cekK = new CallWithValuesK(consumer, k);
            cekApplyProc(producer, List.of(), cekK); return;
        }
        throw new EvalError("cannot apply: " + schemeToString(proc));
    }

    // Helper: set up evaluation of body expressions (handles TCO for last expr)
    private void evalBodyExprs(List<Object> body, int startIdx, Env env) {
        if (startIdx >= body.size()) { cekValue = VOID; cekEval = false; return; }
        if (startIdx == body.size() - 1) {
            cekExpr = body.get(startIdx); cekEval = true; return;
        }
        cekK = new SeqK(body, startIdx + 1, env, cekK);
        cekExpr = body.get(startIdx); cekEval = true;
    }

    // Helper: handle cond starting from a given clause index
    @SuppressWarnings("unchecked")
    private void cekHandleCondFrom(List<Object> list, int startIdx, Env env, Kont k) throws EvalError {
        for (int i = startIdx; i < list.size(); i++) {
            Object clauseObj = list.get(i);
            if (clauseObj instanceof SourceExpr se) clauseObj = se.expr;
            List<Object> clause = (List<Object>) clauseObj;
            Object test = clause.get(0);
            Object rawTest = test;
            if (rawTest instanceof SourceExpr se) rawTest = se.expr;
            if ("else".equals(rawTest)) {
                cekEnv = env; cekK = k;
                evalBodyExprs(clause, 1, env); return;
            }
            // Need to evaluate the test
            cekK = new CondTestK(clause, list, i + 1, env, k);
            cekExpr = test; cekEnv = env; cekEval = true; return;
        }
        cekValue = VOID; cekK = k; cekEval = false;
    }

    @SuppressWarnings("unchecked")
    private void cekHandleCond(List<Object> list, Env env) throws EvalError {
        cekHandleCondFrom(list, 1, env, cekK);
    }

    @SuppressWarnings("unchecked")
    private void cekHandleGuard(List<Object> list, Env env) throws EvalError {
        // (guard (var clause ...) body ...)
        if (list.size() < 3) throw new EvalError("bad syntax: guard");
        Object clauseSpec = list.get(1);
        if (clauseSpec instanceof SourceExpr se) clauseSpec = se.expr;
        List<Object> spec = (List<Object>) clauseSpec;
        if (spec.isEmpty()) throw new EvalError("bad syntax: guard");
        Object varObj = spec.get(0);
        if (varObj instanceof SourceExpr se) varObj = se.expr;
        String var = (String) varObj;
        List<Object> clauses = new ArrayList<>(spec.subList(1, spec.size()));
        List<Object> body = new ArrayList<>(list.subList(2, list.size()));

        // Push exception handler with current continuation and wind state
        handlerStack.add(new ExceptionHandlerEntry(var, clauses, env, cekK, new ArrayList<>(windStack)));
        // Set up body evaluation; GuardBodyK will pop handler on normal completion
        cekK = new GuardBodyK(cekK);
        evalBodyExprs(body, 0, env);
    }

    @SuppressWarnings("unchecked")
    private void cekHandleGuardClauses(List<Object> clauses, Env env, Kont k, Object exnValue) throws EvalError {
        // Evaluate cond-like clauses (like cekHandleCondFrom but with pre-built clause list)
        for (int i = 0; i < clauses.size(); i++) {
            Object clauseObj = clauses.get(i);
            if (clauseObj instanceof SourceExpr se) clauseObj = se.expr;
            List<Object> clause = (List<Object>) clauseObj;
            Object test = clause.get(0);
            Object rawTest = test;
            if (rawTest instanceof SourceExpr se) rawTest = se.expr;
            if ("else".equals(rawTest)) {
                cekEnv = env; cekK = k;
                evalBodyExprs(clause, 1, env); return;
            }
            // Need to evaluate the test — use CondTestK with remaining clauses
            // Build a fake "cond" list for CondTestK compatibility
            List<Object> fullList = new ArrayList<>();
            fullList.add("cond"); // placeholder at index 0
            fullList.addAll(clauses);
            cekK = new CondTestK(clause, fullList, i + 2, env, k);
            cekExpr = test; cekEnv = env; cekEval = true; return;
        }
        // No clause matched — re-raise the exception
        throw new SchemeRaise(exnValue);
    }

    @SuppressWarnings("unchecked")
    private void cekHandleLet(List<Object> list, Env env) throws EvalError {
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

        if (inits.isEmpty()) {
            if (loopName != null) {
                Env letEnv = new Env(env);
                Lambda loopLambda = new Lambda(params, null, body, letEnv);
                letEnv.define(loopName, loopLambda);
                Env callEnv = new Env(loopLambda.closureEnv);
                cekEnv = callEnv; cekK = cekK;
                evalBodyExprs(body, 0, callEnv);
            } else {
                Env letEnv = new Env(env);
                if (body.isEmpty()) { cekValue = VOID; cekEval = false; return; }
                cekEnv = letEnv;
                evalBodyExprs(body, 0, letEnv);
            }
            return;
        }
        cekK = new LetBindK(params, inits, 1, new ArrayList<>(), env, body, loopName, cekK);
        cekExpr = inits.get(0); cekEnv = env; cekEval = true;
    }

    @SuppressWarnings("unchecked")
    private void cekHandleLetStar(List<Object> list, Env env) throws EvalError {
        Object bindingsObj = list.get(1);
        if (bindingsObj instanceof SourceExpr se) bindingsObj = se.expr;
        List<?> bindings = (List<?>) bindingsObj;
        List<Object> body = new ArrayList<>(list.subList(2, list.size()));

        List<String> names = new ArrayList<>();
        List<Object> inits = new ArrayList<>();
        for (Object b : bindings) {
            if (b instanceof SourceExpr se) b = se.expr;
            List<?> binding = (List<?>) b;
            Object pname = binding.get(0);
            if (pname instanceof SourceExpr se) pname = se.expr;
            names.add((String) pname);
            inits.add(binding.get(1));
        }

        Env letEnv = new Env(env);
        if (inits.isEmpty()) {
            if (body.isEmpty()) { cekValue = VOID; cekEval = false; return; }
            cekEnv = letEnv;
            evalBodyExprs(body, 0, letEnv);
            return;
        }
        cekK = new LetStarBindK(names, inits, 1, letEnv, body, cekK);
        cekExpr = inits.get(0); cekEnv = letEnv; cekEval = true;
    }

    @SuppressWarnings("unchecked")
    private void cekHandleLetrec(List<Object> list, Env env) throws EvalError {
        Object bindingsObj = list.get(1);
        if (bindingsObj instanceof SourceExpr se) bindingsObj = se.expr;
        List<?> bindings = (List<?>) bindingsObj;
        List<Object> body = list.size() > 2 ? new ArrayList<>(list.subList(2, list.size())) : List.of();

        List<String> names = new ArrayList<>();
        List<Object> inits = new ArrayList<>();
        Env letEnv = new Env(env);
        for (Object b : bindings) {
            if (b instanceof SourceExpr se) b = se.expr;
            List<?> binding = (List<?>) b;
            Object pname = binding.get(0);
            if (pname instanceof SourceExpr se) pname = se.expr;
            names.add((String) pname);
            inits.add(binding.get(1));
            letEnv.define((String) pname, VOID);
        }

        if (inits.isEmpty()) {
            if (body.isEmpty()) { cekValue = VOID; cekEval = false; return; }
            cekEnv = letEnv;
            evalBodyExprs(body, 0, letEnv);
            return;
        }
        cekK = new LetrecBindK(names, inits, 1, letEnv, body, cekK);
        cekExpr = inits.get(0); cekEnv = letEnv; cekEval = true;
    }

    @SuppressWarnings("unchecked")
    private void cekHandleDo(List<Object> list, Env env) throws EvalError {
        Object varsObj = list.get(1);
        if (varsObj instanceof SourceExpr se) varsObj = se.expr;
        List<?> varSpecs = (List<?>) varsObj;
        Object testObj = list.get(2);
        if (testObj instanceof SourceExpr se) testObj = se.expr;
        List<Object> testClause = (List<Object>) testObj;

        List<String> varNames = new ArrayList<>();
        List<Object> inits = new ArrayList<>();
        List<Object> steps = new ArrayList<>();
        for (Object vs : varSpecs) {
            if (vs instanceof SourceExpr se) vs = se.expr;
            List<?> spec = (List<?>) vs;
            Object vn = spec.get(0);
            if (vn instanceof SourceExpr se) vn = se.expr;
            varNames.add((String) vn);
            inits.add(spec.get(1));
            steps.add(spec.size() > 2 ? spec.get(2) : null);
        }

        if (inits.isEmpty()) {
            Env doEnv = new Env(env);
            cekK = new DoTestK(varNames, doEnv, testClause, steps, list, cekK);
            cekExpr = testClause.get(0); cekEnv = doEnv; cekEval = true; return;
        }
        cekK = new DoInitK(varNames, inits, 1, new ArrayList<>(), env, testClause, steps, list, cekK);
        cekExpr = inits.get(0); cekEnv = env; cekEval = true;
    }

    @SuppressWarnings("unchecked")
    private void cekHandleDefineRecordType(List<Object> list, Env env) throws EvalError {
        Object typeNameObj = list.get(1);
        if (typeNameObj instanceof SourceExpr se) typeNameObj = se.expr;
        Object ctorListObj = list.get(2);
        if (ctorListObj instanceof SourceExpr se) ctorListObj = se.expr;
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
        List<String> fieldNames = new ArrayList<>();
        List<String> accessorNames = new ArrayList<>();
        for (int i = 4; i < list.size(); i++) {
            Object fieldSpec = list.get(i);
            if (fieldSpec instanceof SourceExpr se) fieldSpec = se.expr;
            List<Object> spec = (List<Object>) fieldSpec;
            Object fn = spec.get(0);
            if (fn instanceof SourceExpr se) fn = se.expr;
            fieldNames.add((String) fn);
            Object an = spec.get(1);
            if (an instanceof SourceExpr se) an = se.expr;
            accessorNames.add((String) an);
        }
        RecordType recordType = new RecordType((String) typeNameObj, fieldNames);
        env.define(ctorName, (BuiltinProc) args -> {
            if (args.size() != ctorFields.size()) throw new EvalError(ctorName + ": expected " + ctorFields.size() + " arguments");
            Object[] fields = new Object[fieldNames.size()];
            for (int i = 0; i < ctorFields.size(); i++) { int idx = fieldNames.indexOf(ctorFields.get(i)); fields[idx] = args.get(i); }
            return new RecordInstance(recordType, fields);
        });
        env.define(predName, (BuiltinProc) args -> args.get(0) instanceof RecordInstance ri && ri.type == recordType);
        for (int i = 0; i < fieldNames.size(); i++) {
            final int idx = i;
            env.define(accessorNames.get(i), (BuiltinProc) args -> {
                if (!(args.get(0) instanceof RecordInstance ri) || ri.type != recordType) throw new EvalError(accessorNames.get(idx) + ": not a " + recordType.name);
                return ri.fields[idx];
            });
        }
    }

    // Helper: start evaluating do step expressions
    private void cekStartDoSteps(List<String> varNames, Env doEnv, List<Object> steps, List<Object> testClause, List<Object> fullList, Kont k) throws EvalError {
        if (varNames.isEmpty()) {
            // No vars, just loop back to test
            cekK = new DoTestK(varNames, doEnv, testClause, steps, fullList, k);
            cekExpr = testClause.get(0); cekEnv = doEnv; cekEval = true; return;
        }
        // Evaluate all steps in parallel (collect values, then update)
        // Find first step to evaluate
        List<Object> newVals = new ArrayList<>();
        int firstStepIdx = -1;
        for (int i = 0; i < steps.size(); i++) {
            if (steps.get(i) != null) {
                firstStepIdx = i;
                break;
            } else {
                try { newVals.add(doEnv.lookup(varNames.get(i))); }
                catch (EvalError e) { throw e; }
            }
        }
        if (firstStepIdx < 0) {
            // No step expressions, all vars keep current value
            // Still need to re-define (for parallel semantics)
            for (int i = 0; i < varNames.size(); i++) {
                doEnv.define(varNames.get(i), newVals.get(i));
            }
            cekK = new DoTestK(varNames, doEnv, testClause, steps, fullList, k);
            cekExpr = testClause.get(0); cekEnv = doEnv; cekEval = true; return;
        }
        cekK = new DoStepK(varNames, doEnv, steps, firstStepIdx, newVals, testClause, fullList, k);
        cekExpr = steps.get(firstStepIdx); cekEnv = doEnv; cekEval = true;
    }

    private Object applyLambda(Lambda lambda, List<Object> args) throws EvalError {
        Env callEnv = bindLambdaArgs(lambda, args);
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

    // --- Syntax-case template expansion ---

    @SuppressWarnings("unchecked")
    private Object expandSyntaxTemplate(Object template, Env env, Map<String, String> renames) throws EvalError {
        template = unwrapSE(template);

        if (template instanceof String sym) {
            // Check if it's a pattern variable (bound as SyntaxObject/SyntaxEllipsis)
            try {
                Object val = env.lookup(sym);
                if (val instanceof SyntaxObject so) return so.datum;
                if (val instanceof SyntaxEllipsis) {
                    throw new EvalError("syntax: misuse of ellipsis variable: " + sym);
                }
            } catch (EvalError e) {
                // Not bound - leave as-is
            }
            // Non-pattern-variable: return as-is (symbol)
            return sym;
        }

        if (template instanceof List<?> tmplList) {
            List<Object> result = new ArrayList<>();
            for (int i = 0; i < tmplList.size(); i++) {
                if ("...".equals(unwrapSE(tmplList.get(i)))) continue;

                if (i + 1 < tmplList.size() && "...".equals(unwrapSE(tmplList.get(i + 1)))) {
                    // Ellipsis expansion
                    Object subTmpl = tmplList.get(i);
                    String evar = findSyntaxEllipsisVar(subTmpl, env);
                    if (evar != null) {
                        SyntaxEllipsis se = (SyntaxEllipsis) env.lookup(evar);
                        for (Object elt : se.elements) {
                            Env iterEnv = new Env(env);
                            iterEnv.define(evar, new SyntaxObject(elt));
                            result.add(expandSyntaxTemplate(subTmpl, iterEnv, renames));
                        }
                    }
                    i++; // skip ...
                } else {
                    result.add(expandSyntaxTemplate(tmplList.get(i), env, renames));
                }
            }
            return result;
        }

        return template;
    }

    private String findSyntaxEllipsisVar(Object template, Env env) {
        template = unwrapSE(template);
        if (template instanceof String sym) {
            try {
                Object val = env.lookup(sym);
                if (val instanceof SyntaxEllipsis) return sym;
            } catch (EvalError e) { /* not bound */ }
            return null;
        }
        if (template instanceof List<?> list) {
            for (Object elt : list) {
                String found = findSyntaxEllipsisVar(elt, env);
                if (found != null) return found;
            }
        }
        return null;
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

    private Pair asPair(Object val) throws EvalError {
        if (val instanceof Pair p) return p;
        throw new EvalError("not a pair: " + schemeToString(val));
    }

    private static long gcdLong(long a, long b) {
        a = Math.abs(a); b = Math.abs(b);
        while (b != 0) { long t = b; b = a % b; a = t; }
        return a;
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
        return schemeEqualRec(a, b, new IdentityHashMap<>());
    }

    private boolean schemeEqualRec(Object a, Object b, IdentityHashMap<Object, Set<Object>> seen) {
        if (schemeEq(a, b)) return true;
        if (a instanceof Pair pa && b instanceof Pair pb) {
            Set<Object> partners = seen.get(a);
            if (partners != null && partners.contains(b)) return true; // assume equal for cycles
            if (partners == null) { partners = java.util.Collections.newSetFromMap(new IdentityHashMap<>()); seen.put(a, partners); }
            partners.add(b);
            return schemeEqualRec(pa.car, pb.car, seen) && schemeEqualRec(pa.cdr, pb.cdr, seen);
        }
        if (a instanceof SchemeString sa && b instanceof SchemeString sb) {
            return sa.value().equals(sb.value());
        }
        if (a instanceof SchemeVector va && b instanceof SchemeVector vb) {
            if (va.elements.length != vb.elements.length) return false;
            for (int i = 0; i < va.elements.length; i++) {
                if (!schemeEqualRec(va.elements[i], vb.elements[i], seen)) return false;
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
            return pairToString(val, new IdentityHashMap<>(), true);
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
        if (val instanceof SchemeContinuation) return "#<continuation>";
        if (val == CALLCC_PROC) return "#<procedure:call/cc>";
        if (val == DYNAMIC_WIND_PROC) return "#<procedure:dynamic-wind>";
        if (val instanceof SyntaxRulesMacro) return "#<macro>";
        if (val instanceof SyntaxCaseTransformer) return "#<macro>";
        if (val instanceof SyntaxObject so) return schemeToString(so.datum);
        if (val instanceof ResolvedValue rv) return schemeToString(rv.value);
        return val.toString();
    }

    private String pairToString(Object val, IdentityHashMap<Object, Boolean> seen, boolean write) {
        StringBuilder sb = new StringBuilder("(");
        Object curr = val;
        boolean first = true;
        while (curr instanceof Pair p) {
            if (!first && seen.containsKey(curr)) {
                sb.append(" . ...");
                sb.append(")");
                return sb.toString();
            }
            seen.put(curr, Boolean.TRUE);
            if (!first) sb.append(" ");
            first = false;
            if (p.car instanceof Pair && seen.containsKey(p.car)) {
                sb.append("...");
            } else if (p.car instanceof Pair) {
                sb.append(pairToString(p.car, seen, write));
            } else {
                sb.append(write ? schemeToString(p.car) : displayString(p.car));
            }
            curr = p.cdr;
        }
        if (curr != NIL) {
            sb.append(" . ");
            sb.append(write ? schemeToString(curr) : displayString(curr));
        }
        sb.append(")");
        return sb.toString();
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
