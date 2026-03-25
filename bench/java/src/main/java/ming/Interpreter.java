package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class Interpreter {
    private final Environment globals = new Environment();
    private static final SchemeValue NIL = new SchemeValue.ListVal(List.of());
    private IdentityHashMap<SchemeValue, SourcePos> positions;
    private final StringBuilder outputBuffer = new StringBuilder();
    private int gensymCounter = 0;
    private static final Set<String> SPECIAL_FORMS = Set.of(
        "define", "if", "quote", "lambda", "and", "or", "let", "let*", "begin",
        "cond", "set!", "define-syntax", "syntax-rules", "define-record-type",
        "letrec", "letrec*", "case", "do", "when", "unless", "guard",
        "syntax-case", "syntax", "with-syntax"
    );

    // syntax-case bindings stack: pattern var bindings + defEnv for hygiene
    private record SyntaxFrame(Map<String, Object> bindings, Set<String> patternVars, Environment defEnv) {}
    private final List<SyntaxFrame> syntaxFrames = new ArrayList<>();
    // Accumulated hygiene pre-bindings from syntax templates (consumed by transformer expansion)
    private final Map<String, SchemeValue> syntaxPreBindings = new HashMap<>();

    // Sentinel for call/cc — identity-checked in applyProcCek
    private static final SchemeValue.BuiltinVal CALL_CC_MARKER =
        new SchemeValue.BuiltinVal("call/cc", null);
    private static final SchemeValue.BuiltinVal APPLY_MARKER =
        new SchemeValue.BuiltinVal("apply", null);
    private static final SchemeValue.BuiltinVal DYNAMIC_WIND_MARKER =
        new SchemeValue.BuiltinVal("dynamic-wind", null);
    private static final SchemeValue.BuiltinVal RAISE_MARKER =
        new SchemeValue.BuiltinVal("raise", null);
    private static final SchemeValue.BuiltinVal WITH_EXCEPTION_HANDLER_MARKER =
        new SchemeValue.BuiltinVal("with-exception-handler", null);
    private static final SchemeValue.BuiltinVal VALUES_MARKER =
        new SchemeValue.BuiltinVal("values", null);
    private static final SchemeValue.BuiltinVal CALL_WITH_VALUES_MARKER =
        new SchemeValue.BuiltinVal("call-with-values", null);

    // exception handler stack
    private final List<SchemeValue> exceptionHandlers = new ArrayList<>();

    // dynamic-wind support
    private record WindEntry(SchemeValue inThunk, SchemeValue outThunk) {}
    private record CapturedContinuation(Cont k, List<WindEntry> winds) {}
    private final List<WindEntry> windStack = new ArrayList<>();

    // ---- CEK machine types ----

    sealed interface Cont {
        record Halt() implements Cont {}
        record Frame(CekHandler handler) implements Cont {}
    }

    @FunctionalInterface
    interface CekHandler {
        void apply(SchemeValue value) throws EvalError;
    }

    // CEK machine state (instance variables for performance)
    private SchemeValue cControl;
    private Environment cEnv;
    private Cont cK;
    private boolean cIsValue;

    private void cekEval(SchemeValue expr, Environment env, Cont k) {
        cControl = expr; cEnv = env; cK = k; cIsValue = false;
    }
    private void cekReturn(SchemeValue value, Cont k) {
        cControl = value; cEnv = null; cK = k; cIsValue = true;
    }

    private record CekSaved(SchemeValue c, Environment e, Cont k, boolean v) {}
    private CekSaved saveCek() { return new CekSaved(cControl, cEnv, cK, cIsValue); }
    private void restoreCek(CekSaved s) { cControl = s.c; cEnv = s.e; cK = s.k; cIsValue = s.v; }

    // ---- Constructor & builtins ----

    public Interpreter() {
        registerBuiltins();
    }

    public void setPositions(IdentityHashMap<SchemeValue, SourcePos> positions) {
        this.positions = positions;
    }

    public String getOutput() {
        return outputBuffer.toString();
    }

    private void registerBuiltins() {
        globals.define("+", new SchemeValue.BuiltinVal("+", args -> {
            SchemeValue result = new SchemeValue.IntVal(0);
            for (var arg : args) result = numAdd(result, arg);
            return result;
        }));
        globals.define("*", new SchemeValue.BuiltinVal("*", args -> {
            SchemeValue result = new SchemeValue.IntVal(1);
            for (var arg : args) result = numMul(result, arg);
            return result;
        }));
        globals.define("-", new SchemeValue.BuiltinVal("-", args -> {
            if (args.length == 0) throw new EvalError("-: need at least one argument");
            if (args.length == 1) return numNeg(args[0]);
            SchemeValue result = args[0];
            for (int i = 1; i < args.length; i++) result = numSub(result, args[i]);
            return result;
        }));
        globals.define("/", new SchemeValue.BuiltinVal("/", args -> {
            if (args.length == 0) throw new EvalError("/: need at least one argument");
            if (args.length == 1) return numDiv(new SchemeValue.IntVal(1), args[0]);
            SchemeValue result = args[0];
            for (int i = 1; i < args.length; i++) result = numDiv(result, args[i]);
            return result;
        }));
        globals.define("<", new SchemeValue.BuiltinVal("<", args -> compareNum(args, (a, b) -> a < b)));
        globals.define(">", new SchemeValue.BuiltinVal(">", args -> compareNum(args, (a, b) -> a > b)));
        globals.define("=", new SchemeValue.BuiltinVal("=", args -> compareNum(args, (a, b) -> a == b)));
        globals.define("<=", new SchemeValue.BuiltinVal("<=", args -> compareNum(args, (a, b) -> a <= b)));
        globals.define(">=", new SchemeValue.BuiltinVal(">=", args -> compareNum(args, (a, b) -> a >= b)));
        globals.define("not", new SchemeValue.BuiltinVal("not", args -> {
            if (args.length != 1) throw new EvalError("not: expected 1 argument");
            return new SchemeValue.BoolVal(!args[0].isTruthy());
        }));

        // L03 builtins
        globals.define("cons", new SchemeValue.BuiltinVal("cons", args -> {
            if (args.length != 2) throw new EvalError("cons: expected 2 arguments");
            return new SchemeValue.PairVal(args[0], args[1]);
        }));
        globals.define("car", new SchemeValue.BuiltinVal("car", args -> {
            if (args.length != 1) throw new EvalError("car: expected 1 argument");
            if (args[0] instanceof SchemeValue.PairVal p) return p.car();
            throw new EvalError("car: not a pair: " + args[0].display());
        }));
        globals.define("cdr", new SchemeValue.BuiltinVal("cdr", args -> {
            if (args.length != 1) throw new EvalError("cdr: expected 1 argument");
            if (args[0] instanceof SchemeValue.PairVal p) return p.cdr();
            throw new EvalError("cdr: not a pair: " + args[0].display());
        }));
        globals.define("caar", new SchemeValue.BuiltinVal("caar", args -> {
            if (args.length != 1) throw new EvalError("caar: expected 1 argument");
            if (args[0] instanceof SchemeValue.PairVal p && p.car() instanceof SchemeValue.PairVal pp) return pp.car();
            throw new EvalError("caar: not a pair");
        }));
        globals.define("cadr", new SchemeValue.BuiltinVal("cadr", args -> {
            if (args.length != 1) throw new EvalError("cadr: expected 1 argument");
            if (args[0] instanceof SchemeValue.PairVal p && p.cdr() instanceof SchemeValue.PairVal pp) return pp.car();
            throw new EvalError("cadr: not a pair");
        }));
        globals.define("cdar", new SchemeValue.BuiltinVal("cdar", args -> {
            if (args.length != 1) throw new EvalError("cdar: expected 1 argument");
            if (args[0] instanceof SchemeValue.PairVal p && p.car() instanceof SchemeValue.PairVal pp) return pp.cdr();
            throw new EvalError("cdar: not a pair");
        }));
        globals.define("cddr", new SchemeValue.BuiltinVal("cddr", args -> {
            if (args.length != 1) throw new EvalError("cddr: expected 1 argument");
            if (args[0] instanceof SchemeValue.PairVal p && p.cdr() instanceof SchemeValue.PairVal pp) return pp.cdr();
            throw new EvalError("cddr: not a pair");
        }));
        globals.define("caaar", new SchemeValue.BuiltinVal("caaar", args -> {
            if (args.length != 1) throw new EvalError("caaar: expected 1 argument");
            if (args[0] instanceof SchemeValue.PairVal p && p.car() instanceof SchemeValue.PairVal p2 && p2.car() instanceof SchemeValue.PairVal p3) return p3.car();
            throw new EvalError("caaar: not a pair");
        }));
        globals.define("caddar", new SchemeValue.BuiltinVal("caddar", args -> {
            if (args.length != 1) throw new EvalError("caddar: expected 1 argument");
            if (args[0] instanceof SchemeValue.PairVal p && p.car() instanceof SchemeValue.PairVal p2 && p2.cdr() instanceof SchemeValue.PairVal p3 && p3.cdr() instanceof SchemeValue.PairVal p4) return p4.car();
            throw new EvalError("caddar: not a pair");
        }));
        globals.define("caddr", new SchemeValue.BuiltinVal("caddr", args -> {
            if (args.length != 1) throw new EvalError("caddr: expected 1 argument");
            if (args[0] instanceof SchemeValue.PairVal p && p.cdr() instanceof SchemeValue.PairVal p2 && p2.cdr() instanceof SchemeValue.PairVal p3) return p3.car();
            throw new EvalError("caddr: not a pair");
        }));
        globals.define("cdddr", new SchemeValue.BuiltinVal("cdddr", args -> {
            if (args.length != 1) throw new EvalError("cdddr: expected 1 argument");
            if (args[0] instanceof SchemeValue.PairVal p && p.cdr() instanceof SchemeValue.PairVal p2 && p2.cdr() instanceof SchemeValue.PairVal p3) return p3.cdr();
            throw new EvalError("cdddr: not a pair");
        }));
        globals.define("cadddr", new SchemeValue.BuiltinVal("cadddr", args -> {
            if (args.length != 1) throw new EvalError("cadddr: expected 1 argument");
            if (args[0] instanceof SchemeValue.PairVal p && p.cdr() instanceof SchemeValue.PairVal p2 && p2.cdr() instanceof SchemeValue.PairVal p3 && p3.cdr() instanceof SchemeValue.PairVal p4) return p4.car();
            throw new EvalError("cadddr: not a pair");
        }));
        globals.define("reverse", new SchemeValue.BuiltinVal("reverse", args -> {
            if (args.length != 1) throw new EvalError("reverse: expected 1 argument");
            SchemeValue result = NIL;
            SchemeValue cur = args[0];
            while (cur instanceof SchemeValue.PairVal p) {
                result = new SchemeValue.PairVal(p.car(), result);
                cur = p.cdr();
            }
            return result;
        }));
        globals.define("null?", new SchemeValue.BuiltinVal("null?", args -> {
            if (args.length != 1) throw new EvalError("null?: expected 1 argument");
            return new SchemeValue.BoolVal(args[0] instanceof SchemeValue.ListVal l && l.elements().isEmpty());
        }));
        globals.define("list", new SchemeValue.BuiltinVal("list", args -> {
            SchemeValue result = NIL;
            for (int i = args.length - 1; i >= 0; i--) {
                result = new SchemeValue.PairVal(args[i], result);
            }
            return result;
        }));
        globals.define("length", new SchemeValue.BuiltinVal("length", args -> {
            if (args.length != 1) throw new EvalError("length: expected 1 argument");
            long count = 0;
            SchemeValue cur = args[0];
            while (cur instanceof SchemeValue.PairVal p) {
                count++;
                cur = p.cdr();
            }
            if (!(cur instanceof SchemeValue.ListVal l && l.elements().isEmpty())) {
                throw new EvalError("length: not a proper list");
            }
            return new SchemeValue.IntVal(count);
        }));

        globals.define("append", new SchemeValue.BuiltinVal("append", args -> {
            if (args.length == 0) return NIL;
            if (args.length == 1) return args[0];
            SchemeValue result = args[args.length - 1];
            for (int i = args.length - 2; i >= 0; i--) {
                result = appendTwo(args[i], result);
            }
            return result;
        }));

        // Type predicates
        globals.define("number?", new SchemeValue.BuiltinVal("number?", args -> {
            if (args.length != 1) throw new EvalError("number?: expected 1 argument");
            return new SchemeValue.BoolVal(isNumber(args[0]));
        }));
        globals.define("string?", new SchemeValue.BuiltinVal("string?", args -> {
            if (args.length != 1) throw new EvalError("string?: expected 1 argument");
            return new SchemeValue.BoolVal(args[0] instanceof SchemeValue.StringVal);
        }));
        globals.define("boolean?", new SchemeValue.BuiltinVal("boolean?", args -> {
            if (args.length != 1) throw new EvalError("boolean?: expected 1 argument");
            return new SchemeValue.BoolVal(args[0] instanceof SchemeValue.BoolVal);
        }));
        globals.define("pair?", new SchemeValue.BuiltinVal("pair?", args -> {
            if (args.length != 1) throw new EvalError("pair?: expected 1 argument");
            return new SchemeValue.BoolVal(args[0] instanceof SchemeValue.PairVal);
        }));
        globals.define("symbol?", new SchemeValue.BuiltinVal("symbol?", args -> {
            if (args.length != 1) throw new EvalError("symbol?: expected 1 argument");
            return new SchemeValue.BoolVal(args[0] instanceof SchemeValue.SymbolVal);
        }));

        globals.define("procedure?", new SchemeValue.BuiltinVal("procedure?", args -> {
            if (args.length != 1) throw new EvalError("procedure?: expected 1 argument");
            return new SchemeValue.BoolVal(args[0] instanceof SchemeValue.LambdaVal
                || args[0] instanceof SchemeValue.BuiltinVal
                || args[0] instanceof SchemeValue.CaseLambdaVal
                || args[0] instanceof SchemeValue.ContinuationVal);
        }));

        // L05 builtins
        globals.define("display", new SchemeValue.BuiltinVal("display", args -> {
            if (args.length != 1) throw new EvalError("display: expected 1 argument");
            outputBuffer.append(args[0].displayOutput());
            return new SchemeValue.VoidVal();
        }));
        globals.define("write", new SchemeValue.BuiltinVal("write", args -> {
            if (args.length != 1) throw new EvalError("write: expected 1 argument");
            outputBuffer.append(args[0].display());
            return new SchemeValue.VoidVal();
        }));
        globals.define("newline", new SchemeValue.BuiltinVal("newline", args -> {
            if (args.length != 0) throw new EvalError("newline: expected 0 arguments");
            outputBuffer.append("\n");
            return new SchemeValue.VoidVal();
        }));
        globals.define("string-append", new SchemeValue.BuiltinVal("string-append", args -> {
            var sb = new StringBuilder();
            for (var arg : args) {
                if (!(arg instanceof SchemeValue.StringVal s))
                    throw new EvalError("string-append: expected string");
                sb.append(s.value());
            }
            return new SchemeValue.StringVal(sb.toString());
        }));
        globals.define("string-length", new SchemeValue.BuiltinVal("string-length", args -> {
            if (args.length != 1) throw new EvalError("string-length: expected 1 argument");
            if (!(args[0] instanceof SchemeValue.StringVal s))
                throw new EvalError("string-length: expected string");
            return new SchemeValue.IntVal(s.value().length());
        }));
        globals.define("substring", new SchemeValue.BuiltinVal("substring", args -> {
            if (args.length != 3) throw new EvalError("substring: expected 3 arguments");
            if (!(args[0] instanceof SchemeValue.StringVal s))
                throw new EvalError("substring: expected string");
            int start = (int) asLong(args[1]);
            int end = (int) asLong(args[2]);
            return new SchemeValue.StringVal(s.value().substring(start, end));
        }));
        globals.define("string->number", new SchemeValue.BuiltinVal("string->number", args -> {
            if (args.length != 1) throw new EvalError("string->number: expected 1 argument");
            if (!(args[0] instanceof SchemeValue.StringVal s))
                throw new EvalError("string->number: expected string");
            try {
                return new SchemeValue.IntVal(Long.parseLong(s.value()));
            } catch (NumberFormatException e) {
                return new SchemeValue.BoolVal(false);
            }
        }));
        globals.define("number->string", new SchemeValue.BuiltinVal("number->string", args -> {
            if (args.length != 1) throw new EvalError("number->string: expected 1 argument");
            return new SchemeValue.StringVal(Long.toString(asLong(args[0])));
        }));
        globals.define("symbol->string", new SchemeValue.BuiltinVal("symbol->string", args -> {
            if (args.length != 1) throw new EvalError("symbol->string: expected 1 argument");
            if (!(args[0] instanceof SchemeValue.SymbolVal s))
                throw new EvalError("symbol->string: expected symbol");
            return new SchemeValue.StringVal(s.name());
        }));
        globals.define("string->symbol", new SchemeValue.BuiltinVal("string->symbol", args -> {
            if (args.length != 1) throw new EvalError("string->symbol: expected 1 argument");
            if (!(args[0] instanceof SchemeValue.StringVal s))
                throw new EvalError("string->symbol: expected string");
            return new SchemeValue.SymbolVal(s.value());
        }));
        globals.define("syntax->datum", new SchemeValue.BuiltinVal("syntax->datum", args -> {
            if (args.length != 1) throw new EvalError("syntax->datum: expected 1 argument");
            return args[0]; // syntax objects are just values in our implementation
        }));
        globals.define("datum->syntax", new SchemeValue.BuiltinVal("datum->syntax", args -> {
            if (args.length != 2) throw new EvalError("datum->syntax: expected 2 arguments");
            return args[1]; // just return the datum — no wrapping needed
        }));
        globals.define("string-ref", new SchemeValue.BuiltinVal("string-ref", args -> {
            if (args.length != 2) throw new EvalError("string-ref: expected 2 arguments");
            if (!(args[0] instanceof SchemeValue.StringVal s))
                throw new EvalError("string-ref: expected string");
            int idx = (int) asLong(args[1]);
            return new SchemeValue.CharVal(s.charAt(idx));
        }));
        globals.define("char?", new SchemeValue.BuiltinVal("char?", args -> {
            if (args.length != 1) throw new EvalError("char?: expected 1 argument");
            return new SchemeValue.BoolVal(args[0] instanceof SchemeValue.CharVal);
        }));

        // L06 builtins
        globals.define("make-string", new SchemeValue.BuiltinVal("make-string", args -> {
            if (args.length < 1 || args.length > 2) throw new EvalError("make-string: expected 1-2 arguments");
            if (!(args[0] instanceof SchemeValue.IntVal iv)) throw new EvalError("make-string: expected integer");
            int len = (int) iv.value();
            char c = args.length == 2 && args[1] instanceof SchemeValue.CharVal cv ? cv.value() : '\0';
            char[] chars = new char[len];
            java.util.Arrays.fill(chars, c);
            return new SchemeValue.StringVal(new String(chars), true);
        }));
        globals.define("string", new SchemeValue.BuiltinVal("string", args -> {
            var sb = new StringBuilder();
            for (var arg : args) {
                if (!(arg instanceof SchemeValue.CharVal c)) throw new EvalError("string: expected char");
                sb.append(c.value());
            }
            return new SchemeValue.StringVal(sb.toString(), true);
        }));
        globals.define("string-copy", new SchemeValue.BuiltinVal("string-copy", args -> {
            if (args.length != 1) throw new EvalError("string-copy: expected 1 argument");
            if (!(args[0] instanceof SchemeValue.StringVal s))
                throw new EvalError("string-copy: expected string");
            return new SchemeValue.StringVal(s.value(), true);
        }));
        globals.define("string-set!", new SchemeValue.BuiltinVal("string-set!", args -> {
            if (args.length != 3) throw new EvalError("string-set!: expected 3 arguments");
            if (!(args[0] instanceof SchemeValue.StringVal s))
                throw new EvalError("string-set!: expected string");
            if (!s.isMutable())
                throw new EvalError("string-set!: strings are immutable");
            if (!(args[1] instanceof SchemeValue.IntVal idx))
                throw new EvalError("string-set!: expected integer index");
            if (!(args[2] instanceof SchemeValue.CharVal c))
                throw new EvalError("string-set!: expected character");
            int i = (int) idx.value();
            if (i < 0 || i >= s.length())
                throw new EvalError("string-set!: index out of bounds");
            s.setCharAt(i, c.value());
            return new SchemeValue.VoidVal();
        }));

        // L15 builtins
        globals.define("string->list", new SchemeValue.BuiltinVal("string->list", args -> {
            if (args.length != 1) throw new EvalError("string->list: expected 1 argument");
            if (!(args[0] instanceof SchemeValue.StringVal s))
                throw new EvalError("string->list: expected string");
            String str = s.value();
            SchemeValue result = new SchemeValue.ListVal(java.util.List.of());
            for (int i = str.length() - 1; i >= 0; i--) {
                result = new SchemeValue.PairVal(new SchemeValue.CharVal(str.charAt(i)), result);
            }
            return result;
        }));
        globals.define("list->string", new SchemeValue.BuiltinVal("list->string", args -> {
            if (args.length != 1) throw new EvalError("list->string: expected 1 argument");
            var sb = new StringBuilder();
            SchemeValue cur = args[0];
            while (cur instanceof SchemeValue.PairVal p) {
                if (!(p.car() instanceof SchemeValue.CharVal c))
                    throw new EvalError("list->string: expected list of characters");
                sb.append(c.value());
                cur = p.cdr();
            }
            if (cur instanceof SchemeValue.ListVal l && !l.elements().isEmpty()) {
                for (var elem : l.elements()) {
                    if (!(elem instanceof SchemeValue.CharVal c))
                        throw new EvalError("list->string: expected list of characters");
                    sb.append(c.value());
                }
            } else if (!(cur instanceof SchemeValue.ListVal)) {
                throw new EvalError("list->string: expected proper list");
            }
            return new SchemeValue.StringVal(sb.toString());
        }));
        globals.define("char->integer", new SchemeValue.BuiltinVal("char->integer", args -> {
            if (args.length != 1) throw new EvalError("char->integer: expected 1 argument");
            if (!(args[0] instanceof SchemeValue.CharVal c))
                throw new EvalError("char->integer: expected char");
            return new SchemeValue.IntVal(c.value());
        }));
        globals.define("integer->char", new SchemeValue.BuiltinVal("integer->char", args -> {
            if (args.length != 1) throw new EvalError("integer->char: expected 1 argument");
            long n = asLong(args[0]);
            return new SchemeValue.CharVal((char) n);
        }));

        // L08 builtins — apply is handled specially in CEK
        globals.define("apply", APPLY_MARKER);

        // L09 builtins
        globals.define("abs", new SchemeValue.BuiltinVal("abs", args -> {
            if (args.length != 1) throw new EvalError("abs: expected 1 argument");
            return new SchemeValue.IntVal(Math.abs(asLong(args[0])));
        }));
        globals.define("modulo", new SchemeValue.BuiltinVal("modulo", args -> {
            if (args.length != 2) throw new EvalError("modulo: expected 2 arguments");
            long a = asLong(args[0]), b = asLong(args[1]);
            if (b == 0) throw new EvalError("modulo: division by zero");
            return new SchemeValue.IntVal(Math.floorMod(a, b));
        }));
        globals.define("remainder", new SchemeValue.BuiltinVal("remainder", args -> {
            if (args.length != 2) throw new EvalError("remainder: expected 2 arguments");
            long a = asLong(args[0]), b = asLong(args[1]);
            if (b == 0) throw new EvalError("remainder: division by zero");
            return new SchemeValue.IntVal(a % b);
        }));
        globals.define("quotient", new SchemeValue.BuiltinVal("quotient", args -> {
            if (args.length != 2) throw new EvalError("quotient: expected 2 arguments");
            long a = asLong(args[0]), b = asLong(args[1]);
            if (b == 0) throw new EvalError("quotient: division by zero");
            return new SchemeValue.IntVal(a / b);
        }));
        globals.define("min", new SchemeValue.BuiltinVal("min", args -> {
            if (args.length == 0) throw new EvalError("min: need at least one argument");
            long result = asLong(args[0]);
            for (int i = 1; i < args.length; i++) {
                long v = asLong(args[i]);
                if (v < result) result = v;
            }
            return new SchemeValue.IntVal(result);
        }));
        globals.define("max", new SchemeValue.BuiltinVal("max", args -> {
            if (args.length == 0) throw new EvalError("max: need at least one argument");
            long result = asLong(args[0]);
            for (int i = 1; i < args.length; i++) {
                long v = asLong(args[i]);
                if (v > result) result = v;
            }
            return new SchemeValue.IntVal(result);
        }));
        globals.define("expt", new SchemeValue.BuiltinVal("expt", args -> {
            if (args.length != 2) throw new EvalError("expt: expected 2 arguments");
            long base = asLong(args[0]), exp = asLong(args[1]);
            long result = 1;
            for (long i = 0; i < exp; i++) result *= base;
            return new SchemeValue.IntVal(result);
        }));
        globals.define("zero?", new SchemeValue.BuiltinVal("zero?", args -> {
            if (args.length != 1) throw new EvalError("zero?: expected 1 argument");
            return new SchemeValue.BoolVal(asLong(args[0]) == 0);
        }));
        globals.define("positive?", new SchemeValue.BuiltinVal("positive?", args -> {
            if (args.length != 1) throw new EvalError("positive?: expected 1 argument");
            return new SchemeValue.BoolVal(asLong(args[0]) > 0);
        }));
        globals.define("negative?", new SchemeValue.BuiltinVal("negative?", args -> {
            if (args.length != 1) throw new EvalError("negative?: expected 1 argument");
            return new SchemeValue.BoolVal(asLong(args[0]) < 0);
        }));
        globals.define("odd?", new SchemeValue.BuiltinVal("odd?", args -> {
            if (args.length != 1) throw new EvalError("odd?: expected 1 argument");
            return new SchemeValue.BoolVal(asLong(args[0]) % 2 != 0);
        }));
        globals.define("even?", new SchemeValue.BuiltinVal("even?", args -> {
            if (args.length != 1) throw new EvalError("even?: expected 1 argument");
            return new SchemeValue.BoolVal(asLong(args[0]) % 2 == 0);
        }));
        globals.define("list-ref", new SchemeValue.BuiltinVal("list-ref", args -> {
            if (args.length != 2) throw new EvalError("list-ref: expected 2 arguments");
            int idx = (int) asLong(args[1]);
            SchemeValue cur = args[0];
            for (int i = 0; i < idx; i++) {
                if (!(cur instanceof SchemeValue.PairVal p))
                    throw new EvalError("list-ref: index out of bounds");
                cur = p.cdr();
            }
            if (cur instanceof SchemeValue.PairVal p) return p.car();
            throw new EvalError("list-ref: index out of bounds");
        }));
        globals.define("list-tail", new SchemeValue.BuiltinVal("list-tail", args -> {
            if (args.length != 2) throw new EvalError("list-tail: expected 2 arguments");
            int idx = (int) asLong(args[1]);
            SchemeValue cur = args[0];
            for (int i = 0; i < idx; i++) {
                if (!(cur instanceof SchemeValue.PairVal p))
                    throw new EvalError("list-tail: index out of bounds");
                cur = p.cdr();
            }
            return cur;
        }));
        globals.define("list?", new SchemeValue.BuiltinVal("list?", args -> {
            if (args.length != 1) throw new EvalError("list?: expected 1 argument");
            SchemeValue slow = args[0];
            SchemeValue fast = args[0];
            while (fast instanceof SchemeValue.PairVal fp) {
                fast = fp.cdr();
                if (fast instanceof SchemeValue.PairVal fp2) {
                    fast = fp2.cdr();
                } else {
                    return new SchemeValue.BoolVal(fast instanceof SchemeValue.ListVal l && l.elements().isEmpty());
                }
                slow = ((SchemeValue.PairVal) slow).cdr();
                if (slow == fast) return new SchemeValue.BoolVal(false);
            }
            return new SchemeValue.BoolVal(fast instanceof SchemeValue.ListVal l && l.elements().isEmpty());
        }));
        globals.define("eq?", new SchemeValue.BuiltinVal("eq?", args -> {
            if (args.length != 2) throw new EvalError("eq?: expected 2 arguments");
            return new SchemeValue.BoolVal(schemeEq(args[0], args[1]));
        }));
        globals.define("equal?", new SchemeValue.BuiltinVal("equal?", args -> {
            if (args.length != 2) throw new EvalError("equal?: expected 2 arguments");
            return new SchemeValue.BoolVal(schemeEqual(args[0], args[1]));
        }));
        globals.define("eqv?", new SchemeValue.BuiltinVal("eqv?", args -> {
            if (args.length != 2) throw new EvalError("eqv?: expected 2 arguments");
            return new SchemeValue.BoolVal(schemeEq(args[0], args[1]));
        }));

        // L14 vector builtins
        globals.define("vector", new SchemeValue.BuiltinVal("vector", args -> {
            return new SchemeValue.VectorVal(args.clone());
        }));
        globals.define("make-vector", new SchemeValue.BuiltinVal("make-vector", args -> {
            if (args.length < 1 || args.length > 2) throw new EvalError("make-vector: expected 1-2 arguments");
            int len = (int) asLong(args[0]);
            SchemeValue fill = args.length > 1 ? args[1] : new SchemeValue.IntVal(0);
            var elems = new SchemeValue[len];
            for (int i = 0; i < len; i++) elems[i] = fill;
            return new SchemeValue.VectorVal(elems);
        }));
        globals.define("vector-ref", new SchemeValue.BuiltinVal("vector-ref", args -> {
            if (args.length != 2) throw new EvalError("vector-ref: expected 2 arguments");
            if (!(args[0] instanceof SchemeValue.VectorVal v)) throw new EvalError("vector-ref: expected vector");
            int idx = (int) asLong(args[1]);
            return v.ref(idx);
        }));
        globals.define("vector-set!", new SchemeValue.BuiltinVal("vector-set!", args -> {
            if (args.length != 3) throw new EvalError("vector-set!: expected 3 arguments");
            if (!(args[0] instanceof SchemeValue.VectorVal v)) throw new EvalError("vector-set!: expected vector");
            int idx = (int) asLong(args[1]);
            v.set(idx, args[2]);
            return new SchemeValue.VoidVal();
        }));
        globals.define("vector-length", new SchemeValue.BuiltinVal("vector-length", args -> {
            if (args.length != 1) throw new EvalError("vector-length: expected 1 argument");
            if (!(args[0] instanceof SchemeValue.VectorVal v)) throw new EvalError("vector-length: expected vector");
            return new SchemeValue.IntVal(v.length());
        }));
        globals.define("vector?", new SchemeValue.BuiltinVal("vector?", args -> {
            if (args.length != 1) throw new EvalError("vector?: expected 1 argument");
            return new SchemeValue.BoolVal(args[0] instanceof SchemeValue.VectorVal);
        }));
        globals.define("vector->list", new SchemeValue.BuiltinVal("vector->list", args -> {
            if (args.length != 1) throw new EvalError("vector->list: expected 1 argument");
            if (!(args[0] instanceof SchemeValue.VectorVal v)) throw new EvalError("vector->list: expected vector");
            SchemeValue result = NIL;
            for (int i = v.length() - 1; i >= 0; i--) {
                result = new SchemeValue.PairVal(v.ref(i), result);
            }
            return result;
        }));
        globals.define("list->vector", new SchemeValue.BuiltinVal("list->vector", args -> {
            if (args.length != 1) throw new EvalError("list->vector: expected 1 argument");
            var elems = new ArrayList<SchemeValue>();
            SchemeValue cur = args[0];
            while (cur instanceof SchemeValue.PairVal p) {
                elems.add(p.car());
                cur = p.cdr();
            }
            return new SchemeValue.VectorVal(elems.toArray(new SchemeValue[0]));
        }));

        globals.define("assoc", new SchemeValue.BuiltinVal("assoc", args -> {
            if (args.length != 2) throw new EvalError("assoc: expected 2 arguments");
            SchemeValue key = args[0];
            SchemeValue alist = args[1];
            while (alist instanceof SchemeValue.PairVal p) {
                if (p.car() instanceof SchemeValue.PairVal entry) {
                    if (schemeEqual(entry.car(), key)) return entry;
                }
                alist = p.cdr();
            }
            return new SchemeValue.BoolVal(false);
        }));
        globals.define("assq", new SchemeValue.BuiltinVal("assq", args -> {
            if (args.length != 2) throw new EvalError("assq: expected 2 arguments");
            SchemeValue key = args[0];
            SchemeValue alist = args[1];
            while (alist instanceof SchemeValue.PairVal p) {
                if (p.car() instanceof SchemeValue.PairVal entry) {
                    if (schemeEq(entry.car(), key)) return entry;
                }
                alist = p.cdr();
            }
            return new SchemeValue.BoolVal(false);
        }));
        globals.define("assv", new SchemeValue.BuiltinVal("assv", args -> {
            if (args.length != 2) throw new EvalError("assv: expected 2 arguments");
            SchemeValue key = args[0];
            SchemeValue alist = args[1];
            while (alist instanceof SchemeValue.PairVal p) {
                if (p.car() instanceof SchemeValue.PairVal entry) {
                    if (schemeEq(entry.car(), key)) return entry;
                }
                alist = p.cdr();
            }
            return new SchemeValue.BoolVal(false);
        }));
        globals.define("memv", new SchemeValue.BuiltinVal("memv", args -> {
            if (args.length != 2) throw new EvalError("memv: expected 2 arguments");
            SchemeValue key = args[0];
            SchemeValue lst = args[1];
            while (lst instanceof SchemeValue.PairVal p) {
                if (schemeEq(p.car(), key)) return lst;
                lst = p.cdr();
            }
            return new SchemeValue.BoolVal(false);
        }));
        globals.define("memq", new SchemeValue.BuiltinVal("memq", args -> {
            if (args.length != 2) throw new EvalError("memq: expected 2 arguments");
            SchemeValue key = args[0];
            SchemeValue lst = args[1];
            while (lst instanceof SchemeValue.PairVal p) {
                if (schemeEq(p.car(), key)) return lst;
                lst = p.cdr();
            }
            return new SchemeValue.BoolVal(false);
        }));
        globals.define("member", new SchemeValue.BuiltinVal("member", args -> {
            if (args.length != 2) throw new EvalError("member: expected 2 arguments");
            SchemeValue key = args[0];
            SchemeValue lst = args[1];
            while (lst instanceof SchemeValue.PairVal p) {
                if (schemeEqual(p.car(), key)) return lst;
                lst = p.cdr();
            }
            return new SchemeValue.BoolVal(false);
        }));
        globals.define("truncate", new SchemeValue.BuiltinVal("truncate", args -> {
            if (args.length != 1) throw new EvalError("truncate: expected 1 argument");
            if (args[0] instanceof SchemeValue.IntVal) return args[0];
            if (args[0] instanceof SchemeValue.DoubleVal d)
                return new SchemeValue.DoubleVal((double)(long)d.value());
            if (args[0] instanceof SchemeValue.RationalVal r)
                return new SchemeValue.IntVal(r.num() / r.den());
            throw new EvalError("truncate: expected number");
        }));
        globals.define("floor", new SchemeValue.BuiltinVal("floor", args -> {
            if (args.length != 1) throw new EvalError("floor: expected 1 argument");
            if (args[0] instanceof SchemeValue.IntVal) return args[0];
            if (args[0] instanceof SchemeValue.DoubleVal d)
                return new SchemeValue.DoubleVal(Math.floor(d.value()));
            if (args[0] instanceof SchemeValue.RationalVal r) {
                long q = r.num() / r.den();
                if (r.num() < 0 && r.num() % r.den() != 0) q--;
                return new SchemeValue.IntVal(q);
            }
            throw new EvalError("floor: expected number");
        }));
        globals.define("ceiling", new SchemeValue.BuiltinVal("ceiling", args -> {
            if (args.length != 1) throw new EvalError("ceiling: expected 1 argument");
            if (args[0] instanceof SchemeValue.IntVal) return args[0];
            if (args[0] instanceof SchemeValue.DoubleVal d)
                return new SchemeValue.DoubleVal(Math.ceil(d.value()));
            if (args[0] instanceof SchemeValue.RationalVal r) {
                long q = r.num() / r.den();
                if (r.num() > 0 && r.num() % r.den() != 0) q++;
                return new SchemeValue.IntVal(q);
            }
            throw new EvalError("ceiling: expected number");
        }));
        globals.define("round", new SchemeValue.BuiltinVal("round", args -> {
            if (args.length != 1) throw new EvalError("round: expected 1 argument");
            if (args[0] instanceof SchemeValue.IntVal) return args[0];
            if (args[0] instanceof SchemeValue.DoubleVal d)
                return new SchemeValue.DoubleVal(Math.rint(d.value()));
            throw new EvalError("round: expected number");
        }));
        globals.define("sqrt", new SchemeValue.BuiltinVal("sqrt", args -> {
            if (args.length != 1) throw new EvalError("sqrt: expected 1 argument");
            return new SchemeValue.DoubleVal(Math.sqrt(toDouble(args[0])));
        }));
        globals.define("gcd", new SchemeValue.BuiltinVal("gcd", args -> {
            if (args.length == 0) return new SchemeValue.IntVal(0);
            long result = asLong(args[0]);
            if (result < 0) result = -result;
            for (int i = 1; i < args.length; i++) {
                long b = asLong(args[i]);
                if (b < 0) b = -b;
                result = gcd(result, b);
            }
            return new SchemeValue.IntVal(result);
        }));
        globals.define("lcm", new SchemeValue.BuiltinVal("lcm", args -> {
            if (args.length == 0) return new SchemeValue.IntVal(1);
            long result = asLong(args[0]);
            if (result < 0) result = -result;
            for (int i = 1; i < args.length; i++) {
                long b = asLong(args[i]);
                if (b < 0) b = -b;
                if (result == 0 || b == 0) { result = 0; } else {
                    result = result / gcd(result, b) * b;
                }
            }
            return new SchemeValue.IntVal(result);
        }));
        globals.define("set-car!", new SchemeValue.BuiltinVal("set-car!", args -> {
            if (args.length != 2) throw new EvalError("set-car!: expected 2 arguments");
            if (!(args[0] instanceof SchemeValue.PairVal p))
                throw new EvalError("set-car!: not a pair");
            p.setCar(args[1]);
            return new SchemeValue.VoidVal();
        }));
        globals.define("set-cdr!", new SchemeValue.BuiltinVal("set-cdr!", args -> {
            if (args.length != 2) throw new EvalError("set-cdr!: expected 2 arguments");
            if (!(args[0] instanceof SchemeValue.PairVal p))
                throw new EvalError("set-cdr!: not a pair");
            p.setCdr(args[1]);
            return new SchemeValue.VoidVal();
        }));
        globals.define("for-each", new SchemeValue.BuiltinVal("for-each", args -> {
            if (args.length < 2) throw new EvalError("for-each: expected at least 2 arguments");
            var proc = args[0];
            if (args.length == 2) {
                SchemeValue cur = args[1];
                while (cur instanceof SchemeValue.PairVal p) {
                    callProc(proc, new SchemeValue[]{p.car()});
                    cur = p.cdr();
                }
            } else {
                SchemeValue[] lists = new SchemeValue[args.length - 1];
                for (int i = 0; i < lists.length; i++) lists[i] = args[i + 1];
                while (true) {
                    boolean allPairs = true;
                    for (var l : lists) {
                        if (!(l instanceof SchemeValue.PairVal)) { allPairs = false; break; }
                    }
                    if (!allPairs) break;
                    var callArgs = new SchemeValue[lists.length];
                    for (int i = 0; i < lists.length; i++) {
                        callArgs[i] = ((SchemeValue.PairVal) lists[i]).car();
                        lists[i] = ((SchemeValue.PairVal) lists[i]).cdr();
                    }
                    callProc(proc, callArgs);
                }
            }
            return new SchemeValue.VoidVal();
        }));
        globals.define("map", new SchemeValue.BuiltinVal("map", args -> {
            if (args.length < 2) throw new EvalError("map: expected at least 2 arguments");
            var proc = args[0];
            if (args.length == 2) {
                var result = new ArrayList<SchemeValue>();
                SchemeValue cur = args[1];
                while (cur instanceof SchemeValue.PairVal p) {
                    result.add(callProc(proc, new SchemeValue[]{p.car()}));
                    cur = p.cdr();
                }
                SchemeValue out = NIL;
                for (int i = result.size() - 1; i >= 0; i--)
                    out = new SchemeValue.PairVal(result.get(i), out);
                return out;
            } else {
                SchemeValue[] lists = new SchemeValue[args.length - 1];
                for (int i = 0; i < lists.length; i++) lists[i] = args[i + 1];
                var result = new ArrayList<SchemeValue>();
                while (true) {
                    boolean allPairs = true;
                    for (var l : lists) {
                        if (!(l instanceof SchemeValue.PairVal)) { allPairs = false; break; }
                    }
                    if (!allPairs) break;
                    var callArgs = new SchemeValue[lists.length];
                    for (int i = 0; i < lists.length; i++) {
                        callArgs[i] = ((SchemeValue.PairVal) lists[i]).car();
                        lists[i] = ((SchemeValue.PairVal) lists[i]).cdr();
                    }
                    result.add(callProc(proc, callArgs));
                }
                SchemeValue out = NIL;
                for (int i = result.size() - 1; i >= 0; i--)
                    out = new SchemeValue.PairVal(result.get(i), out);
                return out;
            }
        }));
        // Char operations
        globals.define("char-alphabetic?", new SchemeValue.BuiltinVal("char-alphabetic?", args -> {
            if (args.length != 1) throw new EvalError("char-alphabetic?: expected 1 argument");
            if (!(args[0] instanceof SchemeValue.CharVal c)) throw new EvalError("char-alphabetic?: expected char");
            return new SchemeValue.BoolVal(Character.isLetter(c.value()));
        }));
        globals.define("char-numeric?", new SchemeValue.BuiltinVal("char-numeric?", args -> {
            if (args.length != 1) throw new EvalError("char-numeric?: expected 1 argument");
            if (!(args[0] instanceof SchemeValue.CharVal c)) throw new EvalError("char-numeric?: expected char");
            return new SchemeValue.BoolVal(Character.isDigit(c.value()));
        }));
        globals.define("char-upcase", new SchemeValue.BuiltinVal("char-upcase", args -> {
            if (args.length != 1) throw new EvalError("char-upcase: expected 1 argument");
            if (!(args[0] instanceof SchemeValue.CharVal c)) throw new EvalError("char-upcase: expected char");
            return new SchemeValue.CharVal(Character.toUpperCase(c.value()));
        }));
        globals.define("char-downcase", new SchemeValue.BuiltinVal("char-downcase", args -> {
            if (args.length != 1) throw new EvalError("char-downcase: expected 1 argument");
            if (!(args[0] instanceof SchemeValue.CharVal c)) throw new EvalError("char-downcase: expected char");
            return new SchemeValue.CharVal(Character.toLowerCase(c.value()));
        }));
        globals.define("char=?", new SchemeValue.BuiltinVal("char=?", args -> {
            if (args.length != 2) throw new EvalError("char=?: expected 2 arguments");
            if (!(args[0] instanceof SchemeValue.CharVal a) || !(args[1] instanceof SchemeValue.CharVal b))
                throw new EvalError("char=?: expected chars");
            return new SchemeValue.BoolVal(a.value() == b.value());
        }));
        globals.define("char<?", new SchemeValue.BuiltinVal("char<?", args -> {
            if (args.length != 2) throw new EvalError("char<?: expected 2 arguments");
            if (!(args[0] instanceof SchemeValue.CharVal a) || !(args[1] instanceof SchemeValue.CharVal b))
                throw new EvalError("char<?: expected chars");
            return new SchemeValue.BoolVal(a.value() < b.value());
        }));
        // String comparison/case operations
        globals.define("string=?", new SchemeValue.BuiltinVal("string=?", args -> {
            if (args.length != 2) throw new EvalError("string=?: expected 2 arguments");
            if (!(args[0] instanceof SchemeValue.StringVal a) || !(args[1] instanceof SchemeValue.StringVal b))
                throw new EvalError("string=?: expected strings");
            return new SchemeValue.BoolVal(a.value().equals(b.value()));
        }));
        globals.define("string<?", new SchemeValue.BuiltinVal("string<?", args -> {
            if (args.length != 2) throw new EvalError("string<?: expected 2 arguments");
            if (!(args[0] instanceof SchemeValue.StringVal a) || !(args[1] instanceof SchemeValue.StringVal b))
                throw new EvalError("string<?: expected strings");
            return new SchemeValue.BoolVal(a.value().compareTo(b.value()) < 0);
        }));
        globals.define("string>?", new SchemeValue.BuiltinVal("string>?", args -> {
            if (args.length != 2) throw new EvalError("string>?: expected 2 arguments");
            if (!(args[0] instanceof SchemeValue.StringVal a) || !(args[1] instanceof SchemeValue.StringVal b))
                throw new EvalError("string>?: expected strings");
            return new SchemeValue.BoolVal(a.value().compareTo(b.value()) > 0);
        }));
        globals.define("string<=?", new SchemeValue.BuiltinVal("string<=?", args -> {
            if (args.length != 2) throw new EvalError("string<=?: expected 2 arguments");
            if (!(args[0] instanceof SchemeValue.StringVal a) || !(args[1] instanceof SchemeValue.StringVal b))
                throw new EvalError("string<=?: expected strings");
            return new SchemeValue.BoolVal(a.value().compareTo(b.value()) <= 0);
        }));
        globals.define("string>=?", new SchemeValue.BuiltinVal("string>=?", args -> {
            if (args.length != 2) throw new EvalError("string>=?: expected 2 arguments");
            if (!(args[0] instanceof SchemeValue.StringVal a) || !(args[1] instanceof SchemeValue.StringVal b))
                throw new EvalError("string>=?: expected strings");
            return new SchemeValue.BoolVal(a.value().compareTo(b.value()) >= 0);
        }));
        globals.define("string-ci=?", new SchemeValue.BuiltinVal("string-ci=?", args -> {
            if (args.length != 2) throw new EvalError("string-ci=?: expected 2 arguments");
            if (!(args[0] instanceof SchemeValue.StringVal a) || !(args[1] instanceof SchemeValue.StringVal b))
                throw new EvalError("string-ci=?: expected strings");
            return new SchemeValue.BoolVal(a.value().equalsIgnoreCase(b.value()));
        }));
        globals.define("string-upcase", new SchemeValue.BuiltinVal("string-upcase", args -> {
            if (args.length != 1) throw new EvalError("string-upcase: expected 1 argument");
            if (!(args[0] instanceof SchemeValue.StringVal s)) throw new EvalError("string-upcase: expected string");
            return new SchemeValue.StringVal(s.value().toUpperCase());
        }));
        globals.define("string-downcase", new SchemeValue.BuiltinVal("string-downcase", args -> {
            if (args.length != 1) throw new EvalError("string-downcase: expected 1 argument");
            if (!(args[0] instanceof SchemeValue.StringVal s)) throw new EvalError("string-downcase: expected string");
            return new SchemeValue.StringVal(s.value().toLowerCase());
        }));

        // L11 builtins
        globals.define("exact?", new SchemeValue.BuiltinVal("exact?", args -> {
            if (args.length != 1) throw new EvalError("exact?: expected 1 argument");
            return new SchemeValue.BoolVal(args[0] instanceof SchemeValue.IntVal || args[0] instanceof SchemeValue.RationalVal);
        }));
        globals.define("inexact?", new SchemeValue.BuiltinVal("inexact?", args -> {
            if (args.length != 1) throw new EvalError("inexact?: expected 1 argument");
            return new SchemeValue.BoolVal(args[0] instanceof SchemeValue.DoubleVal);
        }));
        globals.define("integer?", new SchemeValue.BuiltinVal("integer?", args -> {
            if (args.length != 1) throw new EvalError("integer?: expected 1 argument");
            if (args[0] instanceof SchemeValue.IntVal) return new SchemeValue.BoolVal(true);
            if (args[0] instanceof SchemeValue.DoubleVal d) return new SchemeValue.BoolVal(d.value() == Math.floor(d.value()) && !Double.isInfinite(d.value()));
            return new SchemeValue.BoolVal(false);
        }));
        globals.define("rational?", new SchemeValue.BuiltinVal("rational?", args -> {
            if (args.length != 1) throw new EvalError("rational?: expected 1 argument");
            return new SchemeValue.BoolVal(args[0] instanceof SchemeValue.IntVal || args[0] instanceof SchemeValue.RationalVal);
        }));
        globals.define("exact->inexact", new SchemeValue.BuiltinVal("exact->inexact", args -> {
            if (args.length != 1) throw new EvalError("exact->inexact: expected 1 argument");
            return new SchemeValue.DoubleVal(toDouble(args[0]));
        }));
        globals.define("inexact->exact", new SchemeValue.BuiltinVal("inexact->exact", args -> {
            if (args.length != 1) throw new EvalError("inexact->exact: expected 1 argument");
            if (args[0] instanceof SchemeValue.IntVal) return args[0];
            if (args[0] instanceof SchemeValue.RationalVal) return args[0];
            if (args[0] instanceof SchemeValue.DoubleVal d) {
                java.math.BigDecimal bd = java.math.BigDecimal.valueOf(d.value());
                long unscaled = bd.unscaledValue().longValue();
                int scale = bd.scale();
                if (scale >= 0) {
                    long den = 1;
                    for (int i = 0; i < scale; i++) den *= 10;
                    return makeRational(unscaled, den);
                } else {
                    long num = unscaled;
                    for (int i = 0; i < -scale; i++) num *= 10;
                    return new SchemeValue.IntVal(num);
                }
            }
            throw new EvalError("inexact->exact: expected number");
        }));
        globals.define("numerator", new SchemeValue.BuiltinVal("numerator", args -> {
            if (args.length != 1) throw new EvalError("numerator: expected 1 argument");
            if (args[0] instanceof SchemeValue.IntVal iv) return iv;
            if (args[0] instanceof SchemeValue.RationalVal r) return new SchemeValue.IntVal(r.num());
            throw new EvalError("numerator: expected rational");
        }));
        globals.define("denominator", new SchemeValue.BuiltinVal("denominator", args -> {
            if (args.length != 1) throw new EvalError("denominator: expected 1 argument");
            if (args[0] instanceof SchemeValue.IntVal) return new SchemeValue.IntVal(1);
            if (args[0] instanceof SchemeValue.RationalVal r) return new SchemeValue.IntVal(r.den());
            throw new EvalError("denominator: expected rational");
        }));

        // L18: call/cc
        globals.define("call/cc", CALL_CC_MARKER);
        globals.define("call-with-current-continuation", CALL_CC_MARKER);

        // L19: dynamic-wind
        globals.define("dynamic-wind", DYNAMIC_WIND_MARKER);

        // L20: raise, with-exception-handler
        globals.define("raise", RAISE_MARKER);
        globals.define("with-exception-handler", WITH_EXCEPTION_HANDLER_MARKER);

        // L21: values, call-with-values
        globals.define("values", VALUES_MARKER);
        globals.define("call-with-values", CALL_WITH_VALUES_MARKER);
    }

    // ---- CEK evaluation engine ----

    // Track the last list expression being evaluated for error position reporting
    private SchemeValue lastExpr;

    // Step limit support (0 = unlimited)
    private int stepLimit = 0;
    private int stepCount = 0;

    public void setStepLimit(int limit) { this.stepLimit = limit; this.stepCount = 0; }

    private SchemeValue runCekLoop() throws EvalError {
        while (true) {
            if (stepLimit > 0 && ++stepCount > stepLimit) {
                throw new EvalError("step limit exceeded");
            }
            if (cIsValue) {
                if (cK instanceof Cont.Halt) return cControl;
                try {
                    ((Cont.Frame) cK).handler().apply(cControl);
                } catch (EvalError e) {
                    if (positions != null && lastExpr != null && !e.getMessage().matches(".*\\d+:\\d+.*")) {
                        var pos = positions.get(lastExpr);
                        if (pos != null) throw new EvalError(pos + ": " + e.getMessage());
                    }
                    throw e;
                }
            } else {
                step();
            }
        }
    }

    public SchemeValue eval(SchemeValue expr) throws EvalError {
        return eval(expr, globals);
    }

    public SchemeValue eval(SchemeValue expr, Environment env) throws EvalError {
        var saved = saveCek();
        cekEval(expr, env, new Cont.Halt());
        try {
            return runCekLoop();
        } finally {
            restoreCek(saved);
        }
    }

    /** Evaluate a sequence of top-level expressions in a single CEK loop (for call/cc support). */
    public SchemeValue evalAll(List<SchemeValue> exprs, Environment env) throws EvalError {
        if (exprs.isEmpty()) return new SchemeValue.VoidVal();
        if (exprs.size() == 1) return eval(exprs.get(0), env);
        var saved = saveCek();
        setupSequence(exprs, 0, env, new Cont.Halt());
        try {
            return runCekLoop();
        } finally {
            restoreCek(saved);
        }
    }

    // ---- CEK step: evaluate current control expression ----

    private void step() throws EvalError {
        SchemeValue expr = cControl;
        Environment env = cEnv;
        Cont k = cK;

        try {
            switch (expr) {
                case SchemeValue.IntVal v -> cekReturn(v, k);
                case SchemeValue.BoolVal v -> cekReturn(v, k);
                case SchemeValue.StringVal v -> cekReturn(v, k);
                case SchemeValue.VoidVal v -> cekReturn(v, k);
                case SchemeValue.LambdaVal v -> cekReturn(v, k);
                case SchemeValue.BuiltinVal v -> cekReturn(v, k);
                case SchemeValue.CharVal v -> cekReturn(v, k);
                case SchemeValue.DoubleVal v -> cekReturn(v, k);
                case SchemeValue.RationalVal v -> cekReturn(v, k);
                case SchemeValue.PairVal v -> cekReturn(v, k);
                case SchemeValue.MacroVal v -> cekReturn(v, k);
                case SchemeValue.SyntaxTransformerVal v -> cekReturn(v, k);
                case SchemeValue.RecordVal v -> cekReturn(v, k);
                case SchemeValue.CaseLambdaVal v -> cekReturn(v, k);
                case SchemeValue.VectorVal v -> cekReturn(v, k);
                case SchemeValue.ContinuationVal v -> cekReturn(v, k);
                case SchemeValue.ValuesVal v -> cekReturn(v, k);
                case SchemeValue.TailCall tc -> cekEval(tc.expr(), tc.env(), k);
                case SchemeValue.SymbolVal v -> cekReturn(env.get(v.name()), k);
                case SchemeValue.ListVal v -> stepList(v, env, k);
            }
        } catch (EvalError e) {
            if (positions != null && !e.getMessage().matches(".*\\d+:\\d+.*")) {
                var pos = positions.get(expr);
                if (pos != null) throw new EvalError(pos + ": " + e.getMessage());
            }
            throw e;
        }
    }

    private void stepList(SchemeValue.ListVal list, Environment env, Cont k) throws EvalError {
        lastExpr = list;
        if (list.elements().isEmpty()) throw new EvalError("empty application");

        var first = list.elements().get(0);
        if (first instanceof SchemeValue.SymbolVal sym) {
            switch (sym.name()) {
                case "define" -> { stepDefine(list.elements(), env, k); return; }
                case "define-syntax" -> { cekReturn(evalDefineSyntax(list.elements(), env), k); return; }
                case "define-record-type" -> { cekReturn(evalDefineRecordType(list.elements(), env), k); return; }
                case "if" -> { stepIf(list.elements(), env, k); return; }
                case "quote" -> { cekReturn(evalQuote(list.elements()), k); return; }
                case "quasiquote" -> { stepQuasiquote(list.elements(), env, k); return; }
                case "lambda" -> { cekReturn(evalLambda(list.elements(), env), k); return; }
                case "case-lambda" -> { cekReturn(evalCaseLambda(list.elements(), env), k); return; }
                case "and" -> { stepAnd(list.elements(), 1, env, k); return; }
                case "or" -> { stepOr(list.elements(), 1, env, k); return; }
                case "let" -> { stepLet(list.elements(), env, k); return; }
                case "let*" -> { stepLetStar(list.elements(), env, k); return; }
                case "when" -> { stepWhen(list.elements(), env, k); return; }
                case "unless" -> { stepUnless(list.elements(), env, k); return; }
                case "begin" -> { stepBegin(list.elements(), env, k); return; }
                case "cond" -> { stepCond(list.elements(), 1, env, k); return; }
                case "set!" -> { stepSet(list.elements(), env, k); return; }
                case "letrec" -> { stepLetrec(list.elements(), env, k); return; }
                case "letrec*" -> { stepLetrecStar(list.elements(), env, k); return; }
                case "case" -> { stepCase(list.elements(), env, k); return; }
                case "do" -> { stepDo(list.elements(), env, k); return; }
                case "guard" -> { stepGuard(list.elements(), env, k); return; }
                case "syntax-case" -> { stepSyntaxCase(list.elements(), env, k); return; }
                case "syntax" -> { stepSyntax(list.elements(), env, k); return; }
                case "with-syntax" -> { stepWithSyntax(list.elements(), env, k); return; }
                default -> {
                    SchemeValue resolved = null;
                    try { resolved = env.get(sym.name()); } catch (EvalError e) { /* not bound */ }
                    if (resolved instanceof SchemeValue.MacroVal macro) {
                        stepMacroExpansion(macro, list, env, k);
                        return;
                    }
                    if (resolved instanceof SchemeValue.SyntaxTransformerVal stv) {
                        stepSyntaxTransformerExpansion(stv, list, env, k);
                        return;
                    }
                }
            }
        }
        stepApplication(list.elements(), env, k);
    }

    // ---- Special forms (CEK-style) ----

    private void stepDefine(List<SchemeValue> elements, Environment env, Cont k) throws EvalError {
        if (elements.size() < 3) throw new EvalError("define: bad syntax");
        var target = elements.get(1);
        if (target instanceof SchemeValue.SymbolVal sym) {
            cekEval(elements.get(2), env, new Cont.Frame(val -> {
                env.define(sym.name(), val);
                cekReturn(new SchemeValue.VoidVal(), k);
            }));
        } else if (target instanceof SchemeValue.ListVal nameAndParams) {
            if (nameAndParams.elements().isEmpty()) throw new EvalError("define: bad syntax");
            if (!(nameAndParams.elements().get(0) instanceof SchemeValue.SymbolVal fnName))
                throw new EvalError("define: expected function name");
            var params = new ArrayList<String>();
            for (int i = 1; i < nameAndParams.elements().size(); i++) {
                if (!(nameAndParams.elements().get(i) instanceof SchemeValue.SymbolVal p))
                    throw new EvalError("define: expected parameter name");
                params.add(p.name());
            }
            var body = elements.subList(2, elements.size());
            env.define(fnName.name(), new SchemeValue.LambdaVal(params, null, body, env));
            cekReturn(new SchemeValue.VoidVal(), k);
        } else if (target instanceof SchemeValue.PairVal pair) {
            if (!(pair.car() instanceof SchemeValue.SymbolVal fnName))
                throw new EvalError("define: expected function name");
            var params = new ArrayList<String>();
            String restParam = null;
            SchemeValue cur = pair.cdr();
            while (cur instanceof SchemeValue.PairVal p) {
                if (!(p.car() instanceof SchemeValue.SymbolVal s))
                    throw new EvalError("define: expected parameter name");
                params.add(s.name());
                cur = p.cdr();
            }
            if (cur instanceof SchemeValue.SymbolVal rest) restParam = rest.name();
            else if (!(cur instanceof SchemeValue.ListVal l && l.elements().isEmpty()))
                throw new EvalError("define: bad syntax");
            var body = elements.subList(2, elements.size());
            env.define(fnName.name(), new SchemeValue.LambdaVal(params, restParam, body, env));
            cekReturn(new SchemeValue.VoidVal(), k);
        } else {
            throw new EvalError("define: bad syntax");
        }
    }

    private void stepIf(List<SchemeValue> elements, Environment env, Cont k) throws EvalError {
        if (elements.size() < 3) throw new EvalError("if: bad syntax");
        var thenExpr = elements.get(2);
        var hasElse = elements.size() > 3;
        var elseExpr = hasElse ? elements.get(3) : null;
        cekEval(elements.get(1), env, new Cont.Frame(condVal -> {
            if (condVal.isTruthy()) cekEval(thenExpr, env, k);
            else if (hasElse) cekEval(elseExpr, env, k);
            else cekReturn(new SchemeValue.VoidVal(), k);
        }));
    }

    private void stepAnd(List<SchemeValue> elements, int index, Environment env, Cont k) throws EvalError {
        if (index >= elements.size()) { cekReturn(new SchemeValue.BoolVal(true), k); return; }
        if (index == elements.size() - 1) { cekEval(elements.get(index), env, k); return; }
        cekEval(elements.get(index), env, new Cont.Frame(val -> {
            if (!val.isTruthy()) cekReturn(val, k);
            else stepAnd(elements, index + 1, env, k);
        }));
    }

    private void stepOr(List<SchemeValue> elements, int index, Environment env, Cont k) throws EvalError {
        if (index >= elements.size()) { cekReturn(new SchemeValue.BoolVal(false), k); return; }
        if (index == elements.size() - 1) { cekEval(elements.get(index), env, k); return; }
        cekEval(elements.get(index), env, new Cont.Frame(val -> {
            if (val.isTruthy()) cekReturn(val, k);
            else stepOr(elements, index + 1, env, k);
        }));
    }

    private void stepBegin(List<SchemeValue> elements, Environment env, Cont k) throws EvalError {
        if (elements.size() == 1) { cekReturn(new SchemeValue.VoidVal(), k); return; }
        setupSequence(elements, 1, env, k);
    }

    private void setupSequence(List<SchemeValue> exprs, int start, Environment env, Cont k) {
        if (start >= exprs.size()) { cekReturn(new SchemeValue.VoidVal(), k); return; }
        if (start == exprs.size() - 1) { cekEval(exprs.get(start), env, k); return; }
        cekEval(exprs.get(start), env, new Cont.Frame(val -> {
            setupSequence(exprs, start + 1, env, k);
        }));
    }

    private void stepSet(List<SchemeValue> elements, Environment env, Cont k) throws EvalError {
        if (elements.size() != 3) throw new EvalError("set!: bad syntax");
        if (!(elements.get(1) instanceof SchemeValue.SymbolVal sym))
            throw new EvalError("set!: expected symbol");
        cekEval(elements.get(2), env, new Cont.Frame(val -> {
            env.set(sym.name(), val);
            cekReturn(new SchemeValue.VoidVal(), k);
        }));
    }

    private void stepLet(List<SchemeValue> elements, Environment env, Cont k) throws EvalError {
        if (elements.size() < 3) throw new EvalError("let: bad syntax");
        if (elements.get(1) instanceof SchemeValue.SymbolVal loopName) {
            stepNamedLet(elements, loopName, env, k);
            return;
        }
        if (!(elements.get(1) instanceof SchemeValue.ListVal bl))
            throw new EvalError("let: expected bindings list");
        var names = new ArrayList<String>();
        var inits = new ArrayList<SchemeValue>();
        for (var binding : bl.elements()) {
            if (!(binding instanceof SchemeValue.ListVal b) || b.elements().size() != 2)
                throw new EvalError("let: bad binding");
            if (!(b.elements().get(0) instanceof SchemeValue.SymbolVal name))
                throw new EvalError("let: expected symbol in binding");
            names.add(name.name());
            inits.add(b.elements().get(1));
        }
        var body = elements.subList(2, elements.size());
        evalLetBindings(names, inits, 0, new ArrayList<>(), body, env, k);
    }

    private void evalLetBindings(List<String> names, List<SchemeValue> inits, int index,
                                  List<SchemeValue> vals, List<SchemeValue> body,
                                  Environment outerEnv, Cont k) throws EvalError {
        if (index >= inits.size()) {
            var letEnv = new Environment(outerEnv);
            for (int i = 0; i < names.size(); i++) letEnv.define(names.get(i), vals.get(i));
            setupSequence(body, 0, letEnv, k);
            return;
        }
        final var snapshot = List.copyOf(vals);
        cekEval(inits.get(index), outerEnv, new Cont.Frame(val -> {
            var newVals = new ArrayList<>(snapshot);
            newVals.add(val);
            evalLetBindings(names, inits, index + 1, newVals, body, outerEnv, k);
        }));
    }

    private void stepNamedLet(List<SchemeValue> elements, SchemeValue.SymbolVal loopName,
                               Environment env, Cont k) throws EvalError {
        if (elements.size() < 4) throw new EvalError("let: bad syntax");
        if (!(elements.get(2) instanceof SchemeValue.ListVal bl))
            throw new EvalError("let: expected bindings list");
        var params = new ArrayList<String>();
        var initExprs = new ArrayList<SchemeValue>();
        for (var binding : bl.elements()) {
            if (!(binding instanceof SchemeValue.ListVal b) || b.elements().size() != 2)
                throw new EvalError("let: bad binding");
            if (!(b.elements().get(0) instanceof SchemeValue.SymbolVal name))
                throw new EvalError("let: expected symbol in binding");
            params.add(name.name());
            initExprs.add(b.elements().get(1));
        }
        var body = elements.subList(3, elements.size());
        var letEnv = new Environment(env);
        var lambda = new SchemeValue.LambdaVal(params, null, body, letEnv);
        letEnv.define(loopName.name(), lambda);
        evalNamedLetInits(initExprs, 0, new ArrayList<>(), lambda, env, k);
    }

    private void evalNamedLetInits(List<SchemeValue> inits, int index, List<SchemeValue> vals,
                                    SchemeValue.LambdaVal lambda, Environment env, Cont k) throws EvalError {
        if (index >= inits.size()) {
            applyLambdaCek(lambda, vals.toArray(new SchemeValue[0]), k);
            return;
        }
        final var snapshot = List.copyOf(vals);
        cekEval(inits.get(index), env, new Cont.Frame(val -> {
            var newVals = new ArrayList<>(snapshot);
            newVals.add(val);
            evalNamedLetInits(inits, index + 1, newVals, lambda, env, k);
        }));
    }

    private void stepLetStar(List<SchemeValue> elements, Environment env, Cont k) throws EvalError {
        if (elements.size() < 3) throw new EvalError("let*: bad syntax");
        if (!(elements.get(1) instanceof SchemeValue.ListVal bl))
            throw new EvalError("let*: expected bindings list");
        var letEnv = new Environment(env);
        var body = elements.subList(2, elements.size());
        evalLetStarBindings(bl.elements(), 0, letEnv, body, k);
    }

    private void evalLetStarBindings(List<SchemeValue> bindings, int index, Environment letEnv,
                                      List<SchemeValue> body, Cont k) throws EvalError {
        if (index >= bindings.size()) {
            setupSequence(body, 0, letEnv, k);
            return;
        }
        if (!(bindings.get(index) instanceof SchemeValue.ListVal b) || b.elements().size() != 2)
            throw new EvalError("let*: bad binding");
        if (!(b.elements().get(0) instanceof SchemeValue.SymbolVal name))
            throw new EvalError("let*: expected symbol in binding");
        cekEval(b.elements().get(1), letEnv, new Cont.Frame(val -> {
            letEnv.define(name.name(), val);
            evalLetStarBindings(bindings, index + 1, letEnv, body, k);
        }));
    }

    private void stepLetrec(List<SchemeValue> elements, Environment env, Cont k) throws EvalError {
        if (elements.size() < 3) throw new EvalError("letrec: bad syntax");
        if (!(elements.get(1) instanceof SchemeValue.ListVal bl))
            throw new EvalError("letrec: expected bindings list");
        var letEnv = new Environment(env);
        var names = new ArrayList<String>();
        var initExprs = new ArrayList<SchemeValue>();
        for (var binding : bl.elements()) {
            if (!(binding instanceof SchemeValue.ListVal b) || b.elements().size() != 2)
                throw new EvalError("letrec: bad binding");
            if (!(b.elements().get(0) instanceof SchemeValue.SymbolVal name))
                throw new EvalError("letrec: expected symbol in binding");
            names.add(name.name());
            letEnv.define(name.name(), new SchemeValue.VoidVal());
            initExprs.add(b.elements().get(1));
        }
        var body = elements.subList(2, elements.size());
        evalLetrecBindings(names, initExprs, 0, letEnv, body, k);
    }

    private void evalLetrecBindings(List<String> names, List<SchemeValue> inits, int index,
                                     Environment letEnv, List<SchemeValue> body, Cont k) throws EvalError {
        if (index >= inits.size()) {
            setupSequence(body, 0, letEnv, k);
            return;
        }
        cekEval(inits.get(index), letEnv, new Cont.Frame(val -> {
            letEnv.set(names.get(index), val);
            evalLetrecBindings(names, inits, index + 1, letEnv, body, k);
        }));
    }

    private void stepLetrecStar(List<SchemeValue> elements, Environment env, Cont k) throws EvalError {
        if (elements.size() < 3) throw new EvalError("letrec*: bad syntax");
        if (!(elements.get(1) instanceof SchemeValue.ListVal bl))
            throw new EvalError("letrec*: expected bindings list");
        var letEnv = new Environment(env);
        for (var binding : bl.elements()) {
            if (!(binding instanceof SchemeValue.ListVal b) || b.elements().size() != 2)
                throw new EvalError("letrec*: bad binding");
            if (!(b.elements().get(0) instanceof SchemeValue.SymbolVal name))
                throw new EvalError("letrec*: expected symbol in binding");
            letEnv.define(name.name(), new SchemeValue.VoidVal());
        }
        var body = elements.subList(2, elements.size());
        evalLetrecStarBindings(bl.elements(), 0, letEnv, body, k);
    }

    private void evalLetrecStarBindings(List<SchemeValue> bindings, int index, Environment letEnv,
                                         List<SchemeValue> body, Cont k) throws EvalError {
        if (index >= bindings.size()) {
            setupSequence(body, 0, letEnv, k);
            return;
        }
        var b = (SchemeValue.ListVal) bindings.get(index);
        var name = ((SchemeValue.SymbolVal) b.elements().get(0)).name();
        cekEval(b.elements().get(1), letEnv, new Cont.Frame(val -> {
            letEnv.set(name, val);
            evalLetrecStarBindings(bindings, index + 1, letEnv, body, k);
        }));
    }

    private void stepWhen(List<SchemeValue> elements, Environment env, Cont k) throws EvalError {
        if (elements.size() < 3) throw new EvalError("when: bad syntax");
        var body = elements.subList(2, elements.size());
        cekEval(elements.get(1), env, new Cont.Frame(testVal -> {
            if (testVal.isTruthy()) setupSequence(body, 0, env, k);
            else cekReturn(new SchemeValue.VoidVal(), k);
        }));
    }

    private void stepUnless(List<SchemeValue> elements, Environment env, Cont k) throws EvalError {
        if (elements.size() < 3) throw new EvalError("unless: bad syntax");
        var body = elements.subList(2, elements.size());
        cekEval(elements.get(1), env, new Cont.Frame(testVal -> {
            if (!testVal.isTruthy()) setupSequence(body, 0, env, k);
            else cekReturn(new SchemeValue.VoidVal(), k);
        }));
    }

    private void stepCond(List<SchemeValue> elements, int index, Environment env, Cont k) throws EvalError {
        if (index >= elements.size()) { cekReturn(new SchemeValue.VoidVal(), k); return; }
        var clause = elements.get(index);
        if (!(clause instanceof SchemeValue.ListVal cl) || cl.elements().isEmpty())
            throw new EvalError("cond: bad clause");
        var test = cl.elements().get(0);
        if (test instanceof SchemeValue.SymbolVal sym && sym.name().equals("else")) {
            if (cl.elements().size() > 1) setupSequence(cl.elements(), 1, env, k);
            else cekReturn(new SchemeValue.VoidVal(), k);
            return;
        }
        var clauseBody = cl.elements().subList(1, cl.elements().size());
        // Check for => syntax: (test => proc)
        boolean hasArrow = clauseBody.size() == 2
            && clauseBody.get(0) instanceof SchemeValue.SymbolVal arrow
            && arrow.name().equals("=>");
        cekEval(test, env, new Cont.Frame(testVal -> {
            if (testVal.isTruthy()) {
                if (hasArrow) {
                    // (test => proc): evaluate proc then apply it to test value
                    cekEval(clauseBody.get(1), env, new Cont.Frame(proc -> {
                        applyProcCek(proc, new SchemeValue[]{testVal}, k);
                    }));
                } else if (clauseBody.isEmpty()) {
                    cekReturn(testVal, k);
                } else {
                    setupSequence(clauseBody, 0, env, k);
                }
            } else {
                stepCond(elements, index + 1, env, k);
            }
        }));
    }

    private void stepGuard(List<SchemeValue> elements, Environment env, Cont k) throws EvalError {
        // (guard (var clause1 clause2 ...) body ...)
        if (elements.size() < 3) throw new EvalError("guard: bad syntax");
        var spec = elements.get(1);
        if (!(spec instanceof SchemeValue.ListVal cl) || cl.elements().size() < 2)
            throw new EvalError("guard: bad syntax");
        if (!(cl.elements().get(0) instanceof SchemeValue.SymbolVal varSym))
            throw new EvalError("guard: expected variable name");

        String varName = varSym.name();
        var clauses = cl.elements().subList(1, cl.elements().size());
        var bodyExprs = elements.subList(2, elements.size());

        // Continuation that evaluates guard clauses after wind unwinding
        var guardClauseK = new Cont.Frame(exnValue -> {
            var clauseEnv = new Environment(env);
            clauseEnv.define(varName, exnValue);
            stepGuardClauses(clauses, 0, clauseEnv, k);
        });

        // Capture as a continuation value (enables wind transitions on raise)
        var guardCont = new SchemeValue.ContinuationVal(
            new CapturedContinuation(guardClauseK, new ArrayList<>(windStack)));

        // Push as exception handler
        exceptionHandlers.add(guardCont);

        // Evaluate body; on normal completion remove handler and return result
        setupSequence(bodyExprs, 0, env, new Cont.Frame(result -> {
            exceptionHandlers.remove(exceptionHandlers.size() - 1);
            cekReturn(result, k);
        }));
    }

    private void stepGuardClauses(List<SchemeValue> clauses, int index,
                                   Environment env, Cont k) throws EvalError {
        if (index >= clauses.size()) {
            // No clause matched, re-raise
            SchemeValue exn = env.get(((SchemeValue.SymbolVal) clauses.get(0)).name());
            applyProcCek(RAISE_MARKER, new SchemeValue[]{exn}, k);
            return;
        }
        var clause = clauses.get(index);
        if (!(clause instanceof SchemeValue.ListVal cl) || cl.elements().isEmpty())
            throw new EvalError("guard: bad clause");
        var test = cl.elements().get(0);
        if (test instanceof SchemeValue.SymbolVal sym && sym.name().equals("else")) {
            if (cl.elements().size() > 1) setupSequence(cl.elements(), 1, env, k);
            else cekReturn(new SchemeValue.VoidVal(), k);
            return;
        }
        cekEval(test, env, new Cont.Frame(testVal -> {
            if (testVal.isTruthy()) {
                if (cl.elements().size() > 1) setupSequence(cl.elements(), 1, env, k);
                else cekReturn(testVal, k);
            } else {
                stepGuardClauses(clauses, index + 1, env, k);
            }
        }));
    }

    private void stepCase(List<SchemeValue> elements, Environment env, Cont k) throws EvalError {
        if (elements.size() < 2) throw new EvalError("case: bad syntax");
        cekEval(elements.get(1), env, new Cont.Frame(key -> {
            stepCaseClauses(key, elements, 2, env, k);
        }));
    }

    private void stepCaseClauses(SchemeValue key, List<SchemeValue> elements, int index,
                                  Environment env, Cont k) throws EvalError {
        if (index >= elements.size()) { cekReturn(new SchemeValue.VoidVal(), k); return; }
        if (!(elements.get(index) instanceof SchemeValue.ListVal clause) || clause.elements().isEmpty())
            throw new EvalError("case: bad clause");
        var datums = clause.elements().get(0);
        if (datums instanceof SchemeValue.SymbolVal sym && sym.name().equals("else")) {
            if (clause.elements().size() > 1) setupSequence(clause.elements(), 1, env, k);
            else cekReturn(new SchemeValue.VoidVal(), k);
            return;
        }
        if (!(datums instanceof SchemeValue.ListVal dl)) throw new EvalError("case: expected datum list");
        for (var datum : dl.elements()) {
            if (schemeEq(key, quoteDatum(datum))) {
                if (clause.elements().size() > 1) setupSequence(clause.elements(), 1, env, k);
                else cekReturn(new SchemeValue.VoidVal(), k);
                return;
            }
        }
        stepCaseClauses(key, elements, index + 1, env, k);
    }

    private void stepDo(List<SchemeValue> elements, Environment env, Cont k) throws EvalError {
        if (elements.size() < 3) throw new EvalError("do: bad syntax");
        if (!(elements.get(1) instanceof SchemeValue.ListVal varSpecs))
            throw new EvalError("do: expected variable specs");
        if (!(elements.get(2) instanceof SchemeValue.ListVal testClause) || testClause.elements().isEmpty())
            throw new EvalError("do: expected test clause");
        var varNames = new ArrayList<String>();
        var initExprs = new ArrayList<SchemeValue>();
        var stepExprs = new ArrayList<SchemeValue>();
        for (var spec : varSpecs.elements()) {
            if (!(spec instanceof SchemeValue.ListVal sl) || sl.elements().size() < 2)
                throw new EvalError("do: bad variable spec");
            if (!(sl.elements().get(0) instanceof SchemeValue.SymbolVal name))
                throw new EvalError("do: expected variable name");
            varNames.add(name.name());
            initExprs.add(sl.elements().get(1));
            stepExprs.add(sl.elements().size() > 2 ? sl.elements().get(2) : null);
        }
        var bodyExprs = elements.subList(3, elements.size());
        evalDoInits(varNames, initExprs, stepExprs, testClause, bodyExprs, 0, new ArrayList<>(), env, k);
    }

    private void evalDoInits(List<String> varNames, List<SchemeValue> initExprs, List<SchemeValue> stepExprs,
                              SchemeValue.ListVal testClause, List<SchemeValue> bodyExprs,
                              int index, List<SchemeValue> vals, Environment env, Cont k) throws EvalError {
        if (index >= initExprs.size()) {
            var doEnv = new Environment(env);
            for (int i = 0; i < varNames.size(); i++) doEnv.define(varNames.get(i), vals.get(i));
            doIteration(varNames, stepExprs, testClause, bodyExprs, doEnv, k);
            return;
        }
        final var snapshot = List.copyOf(vals);
        cekEval(initExprs.get(index), env, new Cont.Frame(val -> {
            var newVals = new ArrayList<>(snapshot);
            newVals.add(val);
            evalDoInits(varNames, initExprs, stepExprs, testClause, bodyExprs, index + 1, newVals, env, k);
        }));
    }

    private void doIteration(List<String> varNames, List<SchemeValue> stepExprs,
                              SchemeValue.ListVal testClause, List<SchemeValue> bodyExprs,
                              Environment doEnv, Cont k) throws EvalError {
        cekEval(testClause.elements().get(0), doEnv, new Cont.Frame(testVal -> {
            if (testVal.isTruthy()) {
                if (testClause.elements().size() == 1) cekReturn(new SchemeValue.VoidVal(), k);
                else setupSequence(testClause.elements(), 1, doEnv, k);
            } else {
                doBody(varNames, stepExprs, testClause, bodyExprs, 0, doEnv, k);
            }
        }));
    }

    private void doBody(List<String> varNames, List<SchemeValue> stepExprs,
                         SchemeValue.ListVal testClause, List<SchemeValue> bodyExprs,
                         int index, Environment doEnv, Cont k) throws EvalError {
        if (index >= bodyExprs.size()) {
            doStep(varNames, stepExprs, testClause, bodyExprs, 0, new ArrayList<>(), doEnv, k);
            return;
        }
        cekEval(bodyExprs.get(index), doEnv, new Cont.Frame(val -> {
            doBody(varNames, stepExprs, testClause, bodyExprs, index + 1, doEnv, k);
        }));
    }

    private void doStep(List<String> varNames, List<SchemeValue> stepExprs,
                         SchemeValue.ListVal testClause, List<SchemeValue> bodyExprs,
                         int index, List<SchemeValue> newVals, Environment doEnv, Cont k) throws EvalError {
        if (index >= varNames.size()) {
            for (int i = 0; i < varNames.size(); i++) {
                if (newVals.get(i) != null) doEnv.set(varNames.get(i), newVals.get(i));
            }
            doIteration(varNames, stepExprs, testClause, bodyExprs, doEnv, k);
            return;
        }
        if (stepExprs.get(index) == null) {
            var v = new ArrayList<>(newVals);
            v.add(null);
            doStep(varNames, stepExprs, testClause, bodyExprs, index + 1, v, doEnv, k);
        } else {
            final var snapshot = new ArrayList<>(newVals);
            cekEval(stepExprs.get(index), doEnv, new Cont.Frame(val -> {
                var fresh = new ArrayList<>(snapshot);
                fresh.add(val);
                doStep(varNames, stepExprs, testClause, bodyExprs, index + 1, fresh, doEnv, k);
            }));
        }
    }

    // ---- Application (CEK) ----

    private void stepApplication(List<SchemeValue> elements, Environment env, Cont k) throws EvalError {
        var argExprs = elements.subList(1, elements.size());
        cekEval(elements.get(0), env, new Cont.Frame(proc -> {
            // Evaluate arguments right-to-left (matching Chez Scheme behavior)
            // This ensures call/cc in an argument position captures a continuation
            // that re-evaluates earlier arguments from the (possibly mutated) environment.
            evalArgsRTL(proc, argExprs, argExprs.size() - 1, new SchemeValue[argExprs.size()], env, k);
        }));
    }

    private void evalArgsRTL(SchemeValue proc, List<SchemeValue> argExprs, int index,
                              SchemeValue[] evaluatedArgs, Environment env, Cont k) throws EvalError {
        if (index < 0) {
            applyProcCek(proc, evaluatedArgs, k);
            return;
        }
        final var snapshot = evaluatedArgs.clone();
        cekEval(argExprs.get(index), env, new Cont.Frame(val -> {
            var newArgs = snapshot.clone();
            newArgs[index] = val;
            evalArgsRTL(proc, argExprs, index - 1, newArgs, env, k);
        }));
    }

    private void applyProcCek(SchemeValue proc, SchemeValue[] args, Cont k) throws EvalError {
        // call/cc
        if (proc == CALL_CC_MARKER) {
            if (args.length != 1) throw new EvalError("call/cc: expected 1 argument");
            SchemeValue f = args[0];
            SchemeValue capturedK = new SchemeValue.ContinuationVal(
                new CapturedContinuation(k, new ArrayList<>(windStack)));
            applyProcCek(f, new SchemeValue[]{capturedK}, k);
            return;
        }

        // dynamic-wind
        if (proc == DYNAMIC_WIND_MARKER) {
            if (args.length != 3) throw new EvalError("dynamic-wind: expected 3 arguments");
            var inThunk = args[0];
            var bodyThunk = args[1];
            var outThunk = args[2];
            var entry = new WindEntry(inThunk, outThunk);
            applyProcCek(inThunk, new SchemeValue[0], new Cont.Frame(inResult -> {
                windStack.add(entry);
                applyProcCek(bodyThunk, new SchemeValue[0], new Cont.Frame(bodyResult -> {
                    windStack.remove(windStack.size() - 1);
                    applyProcCek(outThunk, new SchemeValue[0], new Cont.Frame(outResult -> {
                        cekReturn(bodyResult, k);
                    }));
                }));
            }));
            return;
        }

        // raise
        if (proc == RAISE_MARKER) {
            if (args.length != 1) throw new EvalError("raise: expected 1 argument");
            SchemeValue value = args[0];
            if (exceptionHandlers.isEmpty()) {
                throw new EvalError("raise: unhandled exception: " + value.display());
            }
            SchemeValue handler = exceptionHandlers.remove(exceptionHandlers.size() - 1);
            // Apply handler; if it returns, that's an error
            applyProcCek(handler, new SchemeValue[]{value}, new Cont.Frame(result -> {
                throw new EvalError("raise: exception handler returned");
            }));
            return;
        }

        // with-exception-handler
        if (proc == WITH_EXCEPTION_HANDLER_MARKER) {
            if (args.length != 2) throw new EvalError("with-exception-handler: expected 2 arguments");
            SchemeValue handler = args[0];
            SchemeValue thunk = args[1];
            exceptionHandlers.add(handler);
            applyProcCek(thunk, new SchemeValue[0], new Cont.Frame(result -> {
                exceptionHandlers.remove(exceptionHandlers.size() - 1);
                cekReturn(result, k);
            }));
            return;
        }

        // values
        if (proc == VALUES_MARKER) {
            if (args.length == 1) {
                cekReturn(args[0], k);
            } else {
                cekReturn(new SchemeValue.ValuesVal(args), k);
            }
            return;
        }

        // call-with-values
        if (proc == CALL_WITH_VALUES_MARKER) {
            if (args.length != 2) throw new EvalError("call-with-values: expected 2 arguments");
            var producer = args[0];
            var consumer = args[1];
            applyProcCek(producer, new SchemeValue[0], new Cont.Frame(result -> {
                SchemeValue[] consumerArgs;
                if (result instanceof SchemeValue.ValuesVal mv) {
                    consumerArgs = mv.values();
                } else {
                    consumerArgs = new SchemeValue[]{result};
                }
                applyProcCek(consumer, consumerArgs, k);
            }));
            return;
        }

        // apply builtin
        if (proc == APPLY_MARKER) {
            if (args.length < 2) throw new EvalError("apply: expected at least 2 arguments");
            var proc2 = args[0];
            var lastArg = args[args.length - 1];
            var allArgs = new ArrayList<SchemeValue>();
            for (int i = 1; i < args.length - 1; i++) allArgs.add(args[i]);
            SchemeValue cur = lastArg;
            while (cur instanceof SchemeValue.PairVal p) {
                allArgs.add(p.car());
                cur = p.cdr();
            }
            if (!(cur instanceof SchemeValue.ListVal l && l.elements().isEmpty())) {
                if (!(cur instanceof SchemeValue.ListVal))
                    throw new EvalError("apply: last argument must be a list");
            }
            applyProcCek(proc2, allArgs.toArray(new SchemeValue[0]), k);
            return;
        }

        // Continuation invocation
        if (proc instanceof SchemeValue.ContinuationVal cv) {
            SchemeValue value;
            if (args.length == 0) value = new SchemeValue.VoidVal();
            else if (args.length == 1) value = args[0];
            else value = new SchemeValue.ValuesVal(args);
            var captured = (CapturedContinuation) cv.cont();
            var targetWinds = captured.winds();
            int common = commonWindPrefix(windStack, targetWinds);
            if (common == windStack.size() && common == targetWinds.size()) {
                // No wind transition needed
                cekReturn(value, captured.k());
            } else {
                doWindTransition(windStack.size(), common, targetWinds, common, value, captured.k());
            }
            return;
        }

        // Lambda
        if (proc instanceof SchemeValue.LambdaVal lambda) {
            applyLambdaCek(lambda, args, k);
            return;
        }

        // Case-lambda
        if (proc instanceof SchemeValue.CaseLambdaVal cl) {
            for (var clause : cl.clauses()) {
                boolean matches = clause.restParam() != null
                    ? args.length >= clause.params().size()
                    : args.length == clause.params().size();
                if (matches) { applyLambdaCek(clause, args, k); return; }
            }
            throw new EvalError("case-lambda: no matching clause for " + args.length + " arguments");
        }

        // Builtin
        if (proc instanceof SchemeValue.BuiltinVal builtin) {
            SchemeValue result = builtin.proc().apply(args);
            cekReturn(result, k);
            return;
        }

        throw new EvalError("not a procedure: " + proc.display());
    }

    // ---- dynamic-wind helpers ----

    private int commonWindPrefix(List<WindEntry> current, List<WindEntry> target) {
        int min = Math.min(current.size(), target.size());
        for (int i = 0; i < min; i++) {
            if (current.get(i) != target.get(i)) return i;
        }
        return min;
    }

    private void doWindTransition(int unwindFrom, int common, List<WindEntry> target,
                                  int rewindFrom, SchemeValue value, Cont k) throws EvalError {
        // Phase 1: Unwind — call out-thunks from innermost to common
        if (unwindFrom > common) {
            int idx = unwindFrom - 1;
            var entry = windStack.remove(idx);
            applyProcCek(entry.outThunk(), new SchemeValue[0], new Cont.Frame(ignored -> {
                doWindTransition(idx, common, target, rewindFrom, value, k);
            }));
            return;
        }
        // Phase 2: Rewind — call in-thunks from common to innermost
        if (rewindFrom < target.size()) {
            var entry = target.get(rewindFrom);
            windStack.add(entry);
            applyProcCek(entry.inThunk(), new SchemeValue[0], new Cont.Frame(ignored -> {
                doWindTransition(unwindFrom, common, target, rewindFrom + 1, value, k);
            }));
            return;
        }
        // Phase 3: Resume
        cekReturn(value, k);
    }

    private void applyLambdaCek(SchemeValue.LambdaVal lambda, SchemeValue[] args, Cont k) throws EvalError {
        var callEnv = bindLambdaArgs(lambda, args);
        setupSequence(lambda.body(), 0, callEnv, k);
    }

    private Environment bindLambdaArgs(SchemeValue.LambdaVal lambda, SchemeValue[] args) throws EvalError {
        var callEnv = new Environment(lambda.env());
        if (lambda.restParam() != null) {
            if (args.length < lambda.params().size())
                throw new EvalError("wrong number of arguments: expected at least " + lambda.params().size() + ", got " + args.length);
            for (int i = 0; i < lambda.params().size(); i++) {
                callEnv.define(lambda.params().get(i), args[i]);
            }
            SchemeValue rest = NIL;
            for (int i = args.length - 1; i >= lambda.params().size(); i--) {
                rest = new SchemeValue.PairVal(args[i], rest);
            }
            callEnv.define(lambda.restParam(), rest);
        } else {
            if (args.length != lambda.params().size())
                throw new EvalError("wrong number of arguments: expected " + lambda.params().size() + ", got " + args.length);
            for (int i = 0; i < lambda.params().size(); i++) {
                callEnv.define(lambda.params().get(i), args[i]);
            }
        }
        return callEnv;
    }

    /** Recursive callProc for builtins (map, for-each) that need to call procedures. */
    private SchemeValue callProc(SchemeValue proc, SchemeValue[] args) throws EvalError {
        if (proc instanceof SchemeValue.LambdaVal lambda) {
            var callEnv = bindLambdaArgs(lambda, args);
            SchemeValue result = new SchemeValue.VoidVal();
            for (var bodyExpr : lambda.body()) {
                result = eval(bodyExpr, callEnv);
            }
            return result;
        }
        if (proc instanceof SchemeValue.CaseLambdaVal cl) {
            for (var clause : cl.clauses()) {
                if (clause.restParam() != null) {
                    if (args.length >= clause.params().size()) return callProc(clause, args);
                } else {
                    if (args.length == clause.params().size()) return callProc(clause, args);
                }
            }
            throw new EvalError("case-lambda: no matching clause for " + args.length + " arguments");
        }
        if (proc instanceof SchemeValue.BuiltinVal builtin) {
            return builtin.proc().apply(args);
        }
        throw new EvalError("not a procedure: " + proc.display());
    }

    // ---- Lambda/quote parsing (no sub-expression evaluation) ----

    private void stepQuasiquote(List<SchemeValue> elements, Environment env, Cont k) throws EvalError {
        if (elements.size() != 2) throw new EvalError("quasiquote: expected 1 argument");
        var result = expandQuasiquote(elements.get(1), env, 0);
        cekReturn(result, k);
    }

    private SchemeValue expandQuasiquote(SchemeValue template, Environment env, int depth) throws EvalError {
        if (template instanceof SchemeValue.ListVal list) {
            var elems = list.elements();
            if (!elems.isEmpty() && elems.get(0) instanceof SchemeValue.SymbolVal sym) {
                if (sym.name().equals("unquote")) {
                    if (elems.size() != 2) throw new EvalError("unquote: expected 1 argument");
                    if (depth == 0) {
                        return eval(elems.get(1), env);
                    } else {
                        var expanded = expandQuasiquote(elems.get(1), env, depth - 1);
                        return new SchemeValue.PairVal(
                            new SchemeValue.SymbolVal("unquote"),
                            new SchemeValue.PairVal(expanded, NIL));
                    }
                }
                if (sym.name().equals("unquote-splicing")) {
                    if (depth == 0) {
                        throw new EvalError("unquote-splicing: not in list context");
                    } else {
                        if (elems.size() != 2) throw new EvalError("unquote-splicing: expected 1 argument");
                        var expanded = expandQuasiquote(elems.get(1), env, depth - 1);
                        return new SchemeValue.PairVal(
                            new SchemeValue.SymbolVal("unquote-splicing"),
                            new SchemeValue.PairVal(expanded, NIL));
                    }
                }
                if (sym.name().equals("quasiquote")) {
                    if (elems.size() != 2) throw new EvalError("quasiquote: expected 1 argument");
                    var expanded = expandQuasiquote(elems.get(1), env, depth + 1);
                    return new SchemeValue.PairVal(
                        new SchemeValue.SymbolVal("quasiquote"),
                        new SchemeValue.PairVal(expanded, NIL));
                }
            }
            // Process list elements, handling unquote-splicing
            return expandQuasiquoteList(elems, env, depth);
        }
        if (template instanceof SchemeValue.PairVal pair) {
            // Check if car is (unquote x) at depth 0
            var car = pair.car();
            if (car instanceof SchemeValue.ListVal carList && !carList.elements().isEmpty()
                && carList.elements().get(0) instanceof SchemeValue.SymbolVal sym
                && sym.name().equals("unquote-splicing") && depth == 0) {
                if (carList.elements().size() != 2) throw new EvalError("unquote-splicing: expected 1 argument");
                var spliced = eval(carList.elements().get(1), env);
                var cdrExpanded = expandQuasiquote(pair.cdr(), env, depth);
                return appendValues(spliced, cdrExpanded);
            }
            var expandedCar = expandQuasiquote(car, env, depth);
            var expandedCdr = expandQuasiquote(pair.cdr(), env, depth);
            return new SchemeValue.PairVal(expandedCar, expandedCdr);
        }
        if (template instanceof SchemeValue.VectorVal vec) {
            var resultElements = new ArrayList<SchemeValue>();
            for (var elem : vec.elements()) {
                if (elem instanceof SchemeValue.ListVal el && !el.elements().isEmpty()
                    && el.elements().get(0) instanceof SchemeValue.SymbolVal sym
                    && sym.name().equals("unquote-splicing") && depth == 0) {
                    if (el.elements().size() != 2) throw new EvalError("unquote-splicing: expected 1 argument");
                    var spliced = eval(el.elements().get(1), env);
                    var cur = spliced;
                    while (cur instanceof SchemeValue.PairVal p) {
                        resultElements.add(p.car());
                        cur = p.cdr();
                    }
                } else {
                    resultElements.add(expandQuasiquote(elem, env, depth));
                }
            }
            return new SchemeValue.VectorVal(resultElements.toArray(new SchemeValue[0]));
        }
        // Atoms are returned as-is (quoted)
        return template;
    }

    private SchemeValue expandQuasiquoteList(List<SchemeValue> elems, Environment env, int depth) throws EvalError {
        SchemeValue result = NIL;
        for (int i = elems.size() - 1; i >= 0; i--) {
            var elem = elems.get(i);
            if (elem instanceof SchemeValue.ListVal el && !el.elements().isEmpty()
                && el.elements().get(0) instanceof SchemeValue.SymbolVal sym
                && sym.name().equals("unquote-splicing") && depth == 0) {
                if (el.elements().size() != 2) throw new EvalError("unquote-splicing: expected 1 argument");
                var spliced = eval(el.elements().get(1), env);
                result = appendValues(spliced, result);
            } else {
                result = new SchemeValue.PairVal(expandQuasiquote(elem, env, depth), result);
            }
        }
        return result;
    }

    private SchemeValue appendValues(SchemeValue list, SchemeValue tail) {
        if (list instanceof SchemeValue.PairVal p) {
            return new SchemeValue.PairVal(p.car(), appendValues(p.cdr(), tail));
        }
        return tail; // list is nil
    }

    private SchemeValue evalQuote(List<SchemeValue> elements) throws EvalError {
        if (elements.size() != 2) throw new EvalError("quote: expected 1 argument");
        return quoteDatum(elements.get(1));
    }

    private SchemeValue quoteDatum(SchemeValue datum) {
        if (datum instanceof SchemeValue.ListVal l) {
            if (l.elements().isEmpty()) return NIL;
            SchemeValue result = NIL;
            for (int i = l.elements().size() - 1; i >= 0; i--) {
                result = new SchemeValue.PairVal(quoteDatum(l.elements().get(i)), result);
            }
            return result;
        }
        if (datum instanceof SchemeValue.PairVal p) {
            return new SchemeValue.PairVal(quoteDatum(p.car()), quoteDatum(p.cdr()));
        }
        return datum;
    }

    private SchemeValue evalLambda(List<SchemeValue> elements, Environment env) throws EvalError {
        if (elements.size() < 3) throw new EvalError("lambda: bad syntax");
        var paramList = elements.get(1);
        if (paramList instanceof SchemeValue.SymbolVal sym) {
            var body = elements.subList(2, elements.size());
            return new SchemeValue.LambdaVal(List.of(), sym.name(), body, env);
        }
        if (paramList instanceof SchemeValue.PairVal pair) {
            var params = new ArrayList<String>();
            String restParam = null;
            SchemeValue cur = pair;
            while (cur instanceof SchemeValue.PairVal p) {
                if (!(p.car() instanceof SchemeValue.SymbolVal s))
                    throw new EvalError("lambda: expected parameter name");
                params.add(s.name());
                cur = p.cdr();
            }
            if (cur instanceof SchemeValue.SymbolVal rest) restParam = rest.name();
            var body = elements.subList(2, elements.size());
            return new SchemeValue.LambdaVal(params, restParam, body, env);
        }
        if (!(paramList instanceof SchemeValue.ListVal pl))
            throw new EvalError("lambda: expected parameter list");
        var params = new ArrayList<String>();
        for (var p : pl.elements()) {
            if (!(p instanceof SchemeValue.SymbolVal sym))
                throw new EvalError("lambda: expected parameter name");
            params.add(sym.name());
        }
        var body = elements.subList(2, elements.size());
        return new SchemeValue.LambdaVal(params, null, body, env);
    }

    private SchemeValue evalCaseLambda(List<SchemeValue> elements, Environment env) throws EvalError {
        var clauses = new ArrayList<SchemeValue.LambdaVal>();
        for (int i = 1; i < elements.size(); i++) {
            if (!(elements.get(i) instanceof SchemeValue.ListVal clause) || clause.elements().size() < 2)
                throw new EvalError("case-lambda: bad clause");
            var lambdaElements = new ArrayList<SchemeValue>();
            lambdaElements.add(new SchemeValue.SymbolVal("lambda"));
            lambdaElements.addAll(clause.elements());
            var lambda = evalLambda(lambdaElements, env);
            clauses.add((SchemeValue.LambdaVal) lambda);
        }
        return new SchemeValue.CaseLambdaVal(clauses);
    }

    // ---- Macro support (Level 10) ----

    private void stepMacroExpansion(SchemeValue.MacroVal macro, SchemeValue.ListVal form,
                                    Environment env, Cont k) throws EvalError {
        var literalSet = new HashSet<>(macro.literals());
        for (int r = 0; r < macro.patterns().size(); r++) {
            var pattern = macro.patterns().get(r);
            var template = macro.templates().get(r);
            var patternVars = collectPatternVars(pattern, literalSet);
            var bindings = new HashMap<String, Object>();
            if (matchPattern(pattern, form, literalSet, patternVars, bindings)) {
                var renameMap = new HashMap<String, String>();
                var preBindings = new HashMap<String, SchemeValue>();
                collectTemplateRenames(template, patternVars, renameMap, preBindings, macro.defEnv());
                var expanded = instantiateTemplate(template, bindings, renameMap);
                if (!preBindings.isEmpty()) {
                    for (var entry : preBindings.entrySet()) {
                        env.define(entry.getKey(), entry.getValue());
                    }
                    cekEval(expanded, env, k);
                } else {
                    cekEval(expanded, env, k);
                }
                return;
            }
        }
        throw new EvalError("no matching pattern for macro");
    }

    private void stepSyntaxTransformerExpansion(SchemeValue.SyntaxTransformerVal stv,
                                                 SchemeValue.ListVal form,
                                                 Environment env, Cont k) throws EvalError {
        // Call the transformer lambda with the form as argument.
        // The result is expanded code to evaluate at the call site.
        var transformer = stv.transformer();
        var defEnv = stv.defEnv();
        // Push a frame so syntax/syntax-case inside the transformer know the defEnv
        syntaxFrames.add(new SyntaxFrame(new HashMap<>(), new HashSet<>(), defEnv));
        // Clear pre-bindings before running transformer
        syntaxPreBindings.clear();
        // Create continuation: when transformer returns, pop frame and eval result at call site
        Cont afterTransform = new Cont.Frame(result -> {
            syntaxFrames.remove(syntaxFrames.size() - 1);
            // Apply hygiene pre-bindings directly into call-site env (gensym names won't conflict)
            if (!syntaxPreBindings.isEmpty()) {
                for (var entry : syntaxPreBindings.entrySet()) {
                    env.define(entry.getKey(), entry.getValue());
                }
                syntaxPreBindings.clear();
            }
            cekEval(result, env, k);
        });
        // Apply the transformer
        applyProcCek(transformer, new SchemeValue[]{form}, afterTransform);
    }

    // (syntax-case expr (literals...) clause ...)
    private void stepSyntaxCase(List<SchemeValue> elements, Environment env, Cont k) throws EvalError {
        if (elements.size() < 4) throw new EvalError("syntax-case: bad syntax");
        // Evaluate the expression first
        cekEval(elements.get(1), env, new Cont.Frame(stxVal -> {
            if (!(elements.get(2) instanceof SchemeValue.ListVal litList))
                throw new EvalError("syntax-case: expected literals list");
            var literals = new HashSet<String>();
            for (var lit : litList.elements()) {
                if (lit instanceof SchemeValue.SymbolVal s) literals.add(s.name());
            }
            // Try each clause
            for (int i = 3; i < elements.size(); i++) {
                if (!(elements.get(i) instanceof SchemeValue.ListVal clause))
                    throw new EvalError("syntax-case: bad clause");
                var clauseElems = clause.elements();
                if (clauseElems.size() < 2 || clauseElems.size() > 3)
                    throw new EvalError("syntax-case: bad clause (expected 2 or 3 elements)");
                var pattern = clauseElems.get(0);
                SchemeValue fender = clauseElems.size() == 3 ? clauseElems.get(1) : null;
                SchemeValue body = clauseElems.get(clauseElems.size() - 1);

                var patternVars = collectPatternVars(pattern, literals);
                var bindings = new HashMap<String, Object>();
                if (matchSyntaxCasePattern(pattern, stxVal, literals, patternVars, bindings)) {
                    // Check fender if present
                    if (fender != null) {
                        // Push bindings, eval fender, check result
                        var frame = currentSyntaxFrame();
                        var merged = new HashMap<>(frame != null ? frame.bindings() : Map.<String, Object>of());
                        merged.putAll(bindings);
                        var mergedVars = new HashSet<>(frame != null ? frame.patternVars() : Set.<String>of());
                        mergedVars.addAll(patternVars);
                        var defEnv = frame != null ? frame.defEnv() : env;
                        syntaxFrames.add(new SyntaxFrame(merged, mergedVars, defEnv));
                        final var fBody = body;
                        final var fBindings = bindings;
                        final var fPatternVars = patternVars;
                        cekEval(fender, env, new Cont.Frame(fenderResult -> {
                            syntaxFrames.remove(syntaxFrames.size() - 1);
                            if (fenderResult.isTruthy()) {
                                evalSyntaxCaseBody(fBody, fBindings, fPatternVars, env, k);
                            } else {
                                throw new EvalError("syntax-case: fender failed (internal)");
                            }
                        }));
                        return;
                    }
                    evalSyntaxCaseBody(body, bindings, patternVars, env, k);
                    return;
                }
            }
            throw new EvalError("syntax-case: no matching pattern");
        }));
    }

    private void evalSyntaxCaseBody(SchemeValue body, Map<String, Object> bindings,
                                     Set<String> patternVars, Environment env, Cont k) throws EvalError {
        var frame = currentSyntaxFrame();
        var merged = new HashMap<>(frame != null ? frame.bindings() : Map.<String, Object>of());
        merged.putAll(bindings);
        var mergedVars = new HashSet<>(frame != null ? frame.patternVars() : Set.<String>of());
        mergedVars.addAll(patternVars);
        var defEnv = frame != null ? frame.defEnv() : env;
        syntaxFrames.add(new SyntaxFrame(merged, mergedVars, defEnv));
        cekEval(body, env, new Cont.Frame(result -> {
            syntaxFrames.remove(syntaxFrames.size() - 1);
            cekReturn(result, k);
        }));
    }

    private boolean matchSyntaxCasePattern(SchemeValue pattern, SchemeValue input,
                                            Set<String> literals, Set<String> patVars,
                                            Map<String, Object> bindings) {
        // Reuse existing matchPattern but handle the first element (don't skip it for non-list patterns)
        if (pattern instanceof SchemeValue.SymbolVal s) {
            if (s.name().equals("_")) return true;
            if (literals.contains(s.name()))
                return input instanceof SchemeValue.SymbolVal is && is.name().equals(s.name());
            if (patVars.contains(s.name())) { bindings.put(s.name(), input); return true; }
            return true;
        }
        if (pattern instanceof SchemeValue.ListVal pl && input instanceof SchemeValue.ListVal il)
            return matchListPattern(pl.elements(), il.elements(), literals, patVars, bindings);
        if (pattern instanceof SchemeValue.IntVal a && input instanceof SchemeValue.IntVal b)
            return a.value() == b.value();
        if (pattern instanceof SchemeValue.BoolVal a && input instanceof SchemeValue.BoolVal b)
            return a.value() == b.value();
        return false;
    }

    // (syntax template) — instantiate template with current syntax bindings + hygiene
    private void stepSyntax(List<SchemeValue> elements, Environment env, Cont k) throws EvalError {
        if (elements.size() != 2) throw new EvalError("syntax: bad syntax");
        var template = elements.get(1);
        var frame = currentSyntaxFrame();
        if (frame == null) throw new EvalError("syntax: not in a syntax-case context");
        var renameMap = new HashMap<String, String>();
        var preBindings = new HashMap<String, SchemeValue>();
        collectTemplateRenames(template, frame.patternVars(), renameMap, preBindings, frame.defEnv());
        var expanded = instantiateTemplate(template, frame.bindings(), renameMap);
        // Store pre-bindings for the transformer expansion to pick up
        syntaxPreBindings.putAll(preBindings);
        cekReturn(expanded, k);
    }

    // (with-syntax ((pattern expr) ...) body ...)
    private void stepWithSyntax(List<SchemeValue> elements, Environment env, Cont k) throws EvalError {
        if (elements.size() < 3) throw new EvalError("with-syntax: bad syntax");
        if (!(elements.get(1) instanceof SchemeValue.ListVal bindings))
            throw new EvalError("with-syntax: expected bindings list");
        // Evaluate binding expressions and match patterns
        evalWithSyntaxBindings(bindings.elements(), 0, new HashMap<>(), new HashSet<>(),
                               elements, env, k);
    }

    private void evalWithSyntaxBindings(List<SchemeValue> bindingClauses, int idx,
                                          Map<String, Object> accBindings, Set<String> accVars,
                                          List<SchemeValue> elements, Environment env, Cont k) throws EvalError {
        if (idx >= bindingClauses.size()) {
            // All bindings done, evaluate body with merged syntax frame
            var frame = currentSyntaxFrame();
            var merged = new HashMap<>(frame != null ? frame.bindings() : Map.<String, Object>of());
            merged.putAll(accBindings);
            var mergedVars = new HashSet<>(frame != null ? frame.patternVars() : Set.<String>of());
            mergedVars.addAll(accVars);
            var defEnv = frame != null ? frame.defEnv() : env;
            syntaxFrames.add(new SyntaxFrame(merged, mergedVars, defEnv));
            // Evaluate body expressions sequentially
            stepBeginRange(elements, 2, elements.size(), env, new Cont.Frame(result -> {
                syntaxFrames.remove(syntaxFrames.size() - 1);
                cekReturn(result, k);
            }));
            return;
        }
        if (!(bindingClauses.get(idx) instanceof SchemeValue.ListVal clause) || clause.elements().size() != 2)
            throw new EvalError("with-syntax: bad binding clause");
        var pattern = clause.elements().get(0);
        var expr = clause.elements().get(1);
        // Evaluate expression
        cekEval(expr, env, new Cont.Frame(val -> {
            // Match pattern against val
            var literals = new HashSet<String>();
            var patVars = new HashSet<String>();
            collectPatternVarsHelper2(pattern, patVars);
            var bindings = new HashMap<String, Object>();
            if (pattern instanceof SchemeValue.SymbolVal s) {
                bindings.put(s.name(), val);
                patVars.add(s.name());
            } else {
                matchSyntaxCasePattern(pattern, val, literals, patVars, bindings);
            }
            accBindings.putAll(bindings);
            accVars.addAll(patVars);
            evalWithSyntaxBindings(bindingClauses, idx + 1, accBindings, accVars, elements, env, k);
        }));
    }

    private void collectPatternVarsHelper2(SchemeValue pattern, Set<String> vars) {
        if (pattern instanceof SchemeValue.SymbolVal s) {
            if (!s.name().equals("...") && !s.name().equals("_")) vars.add(s.name());
        } else if (pattern instanceof SchemeValue.ListVal l) {
            for (var elem : l.elements()) collectPatternVarsHelper2(elem, vars);
        }
    }

    private SyntaxFrame currentSyntaxFrame() {
        return syntaxFrames.isEmpty() ? null : syntaxFrames.get(syntaxFrames.size() - 1);
    }

    private void stepBeginRange(List<SchemeValue> elements, int from, int to,
                                 Environment env, Cont k) throws EvalError {
        if (from >= to) { cekReturn(new SchemeValue.VoidVal(), k); return; }
        if (from == to - 1) { cekEval(elements.get(from), env, k); return; }
        cekEval(elements.get(from), env, new Cont.Frame(val -> {
            stepBeginRange(elements, from + 1, to, env, k);
        }));
    }

    private SchemeValue evalDefineSyntax(List<SchemeValue> elements, Environment env) throws EvalError {
        if (elements.size() != 3) throw new EvalError("define-syntax: bad syntax");
        if (!(elements.get(1) instanceof SchemeValue.SymbolVal name))
            throw new EvalError("define-syntax: expected name");
        var transformer = elements.get(2);
        if (!(transformer instanceof SchemeValue.ListVal tl) || tl.elements().isEmpty())
            throw new EvalError("define-syntax: expected transformer");
        // Check if transformer is (lambda ...) for syntax-case style macros
        if (tl.elements().get(0) instanceof SchemeValue.SymbolVal sr && sr.name().equals("lambda")) {
            var lambdaVal = evalLambda(tl.elements(), env);
            if (lambdaVal instanceof SchemeValue.LambdaVal lv) {
                env.define(name.name(), new SchemeValue.SyntaxTransformerVal(lv, env));
            }
            return new SchemeValue.VoidVal();
        }
        if (!(tl.elements().get(0) instanceof SchemeValue.SymbolVal sr2) || !sr2.name().equals("syntax-rules"))
            throw new EvalError("define-syntax: expected syntax-rules or lambda");
        var sr = sr2;
        if (tl.elements().size() < 2)
            throw new EvalError("syntax-rules: bad syntax");
        var literals = new ArrayList<String>();
        if (tl.elements().get(1) instanceof SchemeValue.ListVal ll) {
            for (var lit : ll.elements()) {
                if (lit instanceof SchemeValue.SymbolVal s) literals.add(s.name());
            }
        }
        var patterns = new ArrayList<SchemeValue>();
        var templates = new ArrayList<SchemeValue>();
        for (int i = 2; i < tl.elements().size(); i++) {
            if (!(tl.elements().get(i) instanceof SchemeValue.ListVal rule) || rule.elements().size() != 2)
                throw new EvalError("syntax-rules: bad rule");
            patterns.add(rule.elements().get(0));
            templates.add(rule.elements().get(1));
        }
        env.define(name.name(), new SchemeValue.MacroVal(literals, patterns, templates, env));
        return new SchemeValue.VoidVal();
    }

    private SchemeValue evalDefineRecordType(List<SchemeValue> elements, Environment env) throws EvalError {
        if (elements.size() < 4) throw new EvalError("define-record-type: bad syntax");
        if (!(elements.get(1) instanceof SchemeValue.SymbolVal typeName))
            throw new EvalError("define-record-type: expected type name");
        if (!(elements.get(2) instanceof SchemeValue.ListVal ctorList) || ctorList.elements().isEmpty())
            throw new EvalError("define-record-type: expected constructor");
        if (!(ctorList.elements().get(0) instanceof SchemeValue.SymbolVal ctorName))
            throw new EvalError("define-record-type: expected constructor name");
        var ctorFields = new ArrayList<String>();
        for (int i = 1; i < ctorList.elements().size(); i++) {
            if (!(ctorList.elements().get(i) instanceof SchemeValue.SymbolVal f))
                throw new EvalError("define-record-type: expected field name in constructor");
            ctorFields.add(f.name());
        }
        if (!(elements.get(3) instanceof SchemeValue.SymbolVal predName))
            throw new EvalError("define-record-type: expected predicate name");
        var fieldNames = new ArrayList<String>();
        var accessorNames = new ArrayList<String>();
        for (int i = 4; i < elements.size(); i++) {
            if (!(elements.get(i) instanceof SchemeValue.ListVal fieldSpec) || fieldSpec.elements().size() < 2)
                throw new EvalError("define-record-type: bad field spec");
            if (!(fieldSpec.elements().get(0) instanceof SchemeValue.SymbolVal fieldSym))
                throw new EvalError("define-record-type: expected field name");
            if (!(fieldSpec.elements().get(1) instanceof SchemeValue.SymbolVal accSym))
                throw new EvalError("define-record-type: expected accessor name");
            fieldNames.add(fieldSym.name());
            accessorNames.add(accSym.name());
        }
        var tag = new Object();
        var fieldNamesArr = fieldNames.toArray(new String[0]);
        var fieldIndexMap = new HashMap<String, Integer>();
        for (int i = 0; i < fieldNames.size(); i++) fieldIndexMap.put(fieldNames.get(i), i);

        env.define(ctorName.name(), new SchemeValue.BuiltinVal(ctorName.name(), args -> {
            if (args.length != ctorFields.size())
                throw new EvalError(ctorName.name() + ": expected " + ctorFields.size() + " arguments");
            var fields = new SchemeValue[fieldNames.size()];
            for (int i = 0; i < ctorFields.size(); i++) {
                int idx = fieldIndexMap.get(ctorFields.get(i));
                fields[idx] = args[i];
            }
            return new SchemeValue.RecordVal(tag, typeName.name(), fieldNamesArr, fields);
        }));
        env.define(predName.name(), new SchemeValue.BuiltinVal(predName.name(), args -> {
            if (args.length != 1) throw new EvalError(predName.name() + ": expected 1 argument");
            return new SchemeValue.BoolVal(args[0] instanceof SchemeValue.RecordVal r && r.tag() == tag);
        }));
        for (int i = 0; i < fieldNames.size(); i++) {
            final int idx = i;
            var accName = accessorNames.get(i);
            env.define(accName, new SchemeValue.BuiltinVal(accName, args -> {
                if (args.length != 1) throw new EvalError(accName + ": expected 1 argument");
                if (!(args[0] instanceof SchemeValue.RecordVal r) || r.tag() != tag)
                    throw new EvalError(accName + ": not a " + typeName.name());
                return r.fields()[idx];
            }));
        }
        return new SchemeValue.VoidVal();
    }

    private Set<String> collectPatternVars(SchemeValue pattern, Set<String> literals) {
        var vars = new HashSet<String>();
        if (pattern instanceof SchemeValue.ListVal l) {
            for (int i = 1; i < l.elements().size(); i++) {
                collectPatternVarsHelper(l.elements().get(i), literals, vars);
            }
        }
        return vars;
    }

    private void collectPatternVarsHelper(SchemeValue v, Set<String> literals, Set<String> vars) {
        if (v instanceof SchemeValue.SymbolVal s) {
            if (!s.name().equals("...") && !literals.contains(s.name())) vars.add(s.name());
        } else if (v instanceof SchemeValue.ListVal l) {
            for (var elem : l.elements()) collectPatternVarsHelper(elem, literals, vars);
        }
    }

    private boolean matchPattern(SchemeValue pattern, SchemeValue input, Set<String> literals,
                                  Set<String> patVars, Map<String, Object> bindings) {
        if (pattern instanceof SchemeValue.SymbolVal s) {
            if (s.name().equals("_")) return true;
            if (literals.contains(s.name()))
                return input instanceof SchemeValue.SymbolVal is && is.name().equals(s.name());
            if (patVars.contains(s.name())) { bindings.put(s.name(), input); return true; }
            return true;
        }
        if (pattern instanceof SchemeValue.ListVal pl && input instanceof SchemeValue.ListVal il)
            return matchListPattern(pl.elements(), il.elements(), literals, patVars, bindings);
        if (pattern instanceof SchemeValue.IntVal a && input instanceof SchemeValue.IntVal b)
            return a.value() == b.value();
        if (pattern instanceof SchemeValue.BoolVal a && input instanceof SchemeValue.BoolVal b)
            return a.value() == b.value();
        return false;
    }

    private boolean matchListPattern(List<SchemeValue> pat, List<SchemeValue> inp,
                                      Set<String> literals, Set<String> patVars,
                                      Map<String, Object> bindings) {
        int ellipsisIdx = -1;
        for (int i = 0; i < pat.size(); i++) {
            if (pat.get(i) instanceof SchemeValue.SymbolVal s && s.name().equals("...")) {
                ellipsisIdx = i; break;
            }
        }
        if (ellipsisIdx == -1) {
            if (pat.size() != inp.size()) return false;
            for (int i = 0; i < pat.size(); i++) {
                if (i == 0) continue;
                if (!matchPattern(pat.get(i), inp.get(i), literals, patVars, bindings)) return false;
            }
            return true;
        }
        int varStart = ellipsisIdx - 1;
        int afterCount = pat.size() - ellipsisIdx - 1;
        if (inp.size() < varStart + afterCount) return false;
        for (int i = 1; i < ellipsisIdx - 1; i++) {
            if (!matchPattern(pat.get(i), inp.get(i), literals, patVars, bindings)) return false;
        }
        int varCount = inp.size() - varStart - afterCount;
        SchemeValue varPat = pat.get(ellipsisIdx - 1);
        if (varPat instanceof SchemeValue.SymbolVal s && patVars.contains(s.name())) {
            var varBindings = new ArrayList<SchemeValue>();
            for (int i = varStart; i < varStart + varCount; i++) varBindings.add(inp.get(i));
            bindings.put(s.name(), varBindings);
        }
        for (int i = 0; i < afterCount; i++) {
            int patIdx = ellipsisIdx + 1 + i;
            int inpIdx = varStart + varCount + i;
            if (!matchPattern(pat.get(patIdx), inp.get(inpIdx), literals, patVars, bindings)) return false;
        }
        return true;
    }

    private void collectTemplateRenames(SchemeValue template, Set<String> patternVars,
                                         Map<String, String> renameMap,
                                         Map<String, SchemeValue> preBindings,
                                         Environment defEnv) {
        if (template instanceof SchemeValue.SymbolVal s) {
            String name = s.name();
            if (patternVars.contains(name) || name.equals("...") || SPECIAL_FORMS.contains(name)
                    || renameMap.containsKey(name)) return;
            try {
                SchemeValue val = defEnv.get(name);
                if (val instanceof SchemeValue.MacroVal) return;
                String gensym = name + "__m" + (gensymCounter++);
                renameMap.put(name, gensym);
                preBindings.put(gensym, val);
            } catch (EvalError e) {
                String gensym = name + "__m" + (gensymCounter++);
                renameMap.put(name, gensym);
            }
        } else if (template instanceof SchemeValue.ListVal l) {
            // Skip (quote ...) subforms — quoted data should not be renamed
            if (!l.elements().isEmpty() && l.elements().get(0) instanceof SchemeValue.SymbolVal qs
                    && qs.name().equals("quote")) return;
            for (var elem : l.elements()) collectTemplateRenames(elem, patternVars, renameMap, preBindings, defEnv);
        }
    }

    private SchemeValue instantiateTemplate(SchemeValue template, Map<String, Object> bindings,
                                             Map<String, String> renameMap) {
        if (template instanceof SchemeValue.SymbolVal s) {
            if (bindings.containsKey(s.name())) {
                Object val = bindings.get(s.name());
                if (val instanceof SchemeValue sv) return sv;
                return template;
            }
            if (renameMap.containsKey(s.name())) return new SchemeValue.SymbolVal(renameMap.get(s.name()));
            return template;
        }
        if (template instanceof SchemeValue.ListVal tl) {
            var result = new ArrayList<SchemeValue>();
            var elems = tl.elements();
            for (int i = 0; i < elems.size(); i++) {
                if (i + 1 < elems.size() && elems.get(i + 1) instanceof SchemeValue.SymbolVal dots
                        && dots.name().equals("...")) {
                    var subTemplate = elems.get(i);
                    var ellipsisVars = findEllipsisVars(subTemplate, bindings);
                    if (!ellipsisVars.isEmpty()) {
                        String firstVar = ellipsisVars.iterator().next();
                        @SuppressWarnings("unchecked")
                        var list = (List<SchemeValue>) bindings.get(firstVar);
                        int count = list.size();
                        for (int j = 0; j < count; j++) {
                            var singleBindings = new HashMap<>(bindings);
                            for (String var : ellipsisVars) {
                                @SuppressWarnings("unchecked")
                                var varList = (List<SchemeValue>) bindings.get(var);
                                singleBindings.put(var, varList.get(j));
                            }
                            result.add(instantiateTemplate(subTemplate, singleBindings, renameMap));
                        }
                    }
                    i++;
                } else {
                    result.add(instantiateTemplate(elems.get(i), bindings, renameMap));
                }
            }
            return new SchemeValue.ListVal(result);
        }
        return template;
    }

    private Set<String> findEllipsisVars(SchemeValue template, Map<String, Object> bindings) {
        var vars = new HashSet<String>();
        findEllipsisVarsHelper(template, bindings, vars);
        return vars;
    }

    private void findEllipsisVarsHelper(SchemeValue template, Map<String, Object> bindings, Set<String> vars) {
        if (template instanceof SchemeValue.SymbolVal s) {
            if (bindings.containsKey(s.name()) && bindings.get(s.name()) instanceof List) vars.add(s.name());
        } else if (template instanceof SchemeValue.ListVal l) {
            for (var elem : l.elements()) findEllipsisVarsHelper(elem, bindings, vars);
        }
    }

    // ---- Utility methods ----

    @FunctionalInterface
    interface DoubleCmp { boolean test(double a, double b); }

    private SchemeValue compareNum(SchemeValue[] args, DoubleCmp cmp) throws EvalError {
        if (args.length < 2) throw new EvalError("comparison needs at least 2 arguments");
        for (int i = 0; i < args.length - 1; i++) {
            if (!cmp.test(toDouble(args[i]), toDouble(args[i + 1]))) return new SchemeValue.BoolVal(false);
        }
        return new SchemeValue.BoolVal(true);
    }

    private static SchemeValue appendTwo(SchemeValue a, SchemeValue b) throws EvalError {
        if (a instanceof SchemeValue.ListVal l && l.elements().isEmpty()) return b;
        if (a instanceof SchemeValue.PairVal p) return new SchemeValue.PairVal(p.car(), appendTwo(p.cdr(), b));
        throw new EvalError("append: not a proper list");
    }

    private static long asLong(SchemeValue v) throws EvalError {
        if (v instanceof SchemeValue.IntVal iv) return iv.value();
        throw new EvalError("expected number, got: " + v.display());
    }

    private static long gcd(long a, long b) {
        while (b != 0) { long t = b; b = a % b; a = t; }
        return a;
    }

    private static SchemeValue makeRational(long num, long den) {
        if (den < 0) { num = -num; den = -den; }
        long g = gcd(Math.abs(num), den);
        num /= g; den /= g;
        if (den == 1) return new SchemeValue.IntVal(num);
        return new SchemeValue.RationalVal(num, den);
    }

    private static double toDouble(SchemeValue v) throws EvalError {
        if (v instanceof SchemeValue.IntVal iv) return (double) iv.value();
        if (v instanceof SchemeValue.RationalVal r) return (double) r.num() / r.den();
        if (v instanceof SchemeValue.DoubleVal d) return d.value();
        throw new EvalError("expected number, got: " + v.display());
    }

    private static long numOf(SchemeValue v) throws EvalError {
        if (v instanceof SchemeValue.IntVal iv) return iv.value();
        if (v instanceof SchemeValue.RationalVal r) return r.num();
        throw new EvalError("expected exact number");
    }

    private static long denOf(SchemeValue v) {
        if (v instanceof SchemeValue.IntVal) return 1;
        if (v instanceof SchemeValue.RationalVal r) return r.den();
        return 1;
    }

    private static boolean isInexact(SchemeValue v) {
        return v instanceof SchemeValue.DoubleVal;
    }

    private static boolean isNumber(SchemeValue v) {
        return v instanceof SchemeValue.IntVal || v instanceof SchemeValue.RationalVal || v instanceof SchemeValue.DoubleVal;
    }

    private static SchemeValue numAdd(SchemeValue a, SchemeValue b) throws EvalError {
        if (isInexact(a) || isInexact(b)) return new SchemeValue.DoubleVal(toDouble(a) + toDouble(b));
        long an = numOf(a), ad = denOf(a), bn = numOf(b), bd = denOf(b);
        return makeRational(an * bd + bn * ad, ad * bd);
    }

    private static SchemeValue numSub(SchemeValue a, SchemeValue b) throws EvalError {
        if (isInexact(a) || isInexact(b)) return new SchemeValue.DoubleVal(toDouble(a) - toDouble(b));
        long an = numOf(a), ad = denOf(a), bn = numOf(b), bd = denOf(b);
        return makeRational(an * bd - bn * ad, ad * bd);
    }

    private static SchemeValue numMul(SchemeValue a, SchemeValue b) throws EvalError {
        if (isInexact(a) || isInexact(b)) return new SchemeValue.DoubleVal(toDouble(a) * toDouble(b));
        long an = numOf(a), ad = denOf(a), bn = numOf(b), bd = denOf(b);
        return makeRational(an * bn, ad * bd);
    }

    private static SchemeValue numDiv(SchemeValue a, SchemeValue b) throws EvalError {
        if (isInexact(a) || isInexact(b)) {
            double d = toDouble(b);
            if (d == 0) throw new EvalError("division by zero");
            return new SchemeValue.DoubleVal(toDouble(a) / d);
        }
        long bn = numOf(b), bd = denOf(b);
        if (bn == 0) throw new EvalError("division by zero");
        long an = numOf(a), ad = denOf(a);
        return makeRational(an * bd, ad * bn);
    }

    private static SchemeValue numNeg(SchemeValue a) throws EvalError {
        if (isInexact(a)) return new SchemeValue.DoubleVal(-toDouble(a));
        return makeRational(-numOf(a), denOf(a));
    }

    private static boolean schemeEq(SchemeValue a, SchemeValue b) {
        if (a instanceof SchemeValue.IntVal ai && b instanceof SchemeValue.IntVal bi)
            return ai.value() == bi.value();
        if (a instanceof SchemeValue.RationalVal ar && b instanceof SchemeValue.RationalVal br)
            return ar.num() == br.num() && ar.den() == br.den();
        if (a instanceof SchemeValue.DoubleVal ad && b instanceof SchemeValue.DoubleVal bd)
            return ad.value() == bd.value();
        if (a instanceof SchemeValue.BoolVal ab && b instanceof SchemeValue.BoolVal bb)
            return ab.value() == bb.value();
        if (a instanceof SchemeValue.SymbolVal as && b instanceof SchemeValue.SymbolVal bs)
            return as.name().equals(bs.name());
        if (a instanceof SchemeValue.CharVal ac && b instanceof SchemeValue.CharVal bc)
            return ac.value() == bc.value();
        if (a instanceof SchemeValue.VoidVal && b instanceof SchemeValue.VoidVal)
            return true;
        if (a instanceof SchemeValue.ListVal la && la.elements().isEmpty()
                && b instanceof SchemeValue.ListVal lb && lb.elements().isEmpty())
            return true;
        return a == b;
    }

    private static boolean schemeEqual(SchemeValue a, SchemeValue b) {
        return schemeEqualSafe(a, b, java.util.Collections.newSetFromMap(new IdentityHashMap<>()));
    }

    private static boolean schemeEqualSafe(SchemeValue a, SchemeValue b, Set<Object> visited) {
        if (schemeEq(a, b)) return true;
        if (a instanceof SchemeValue.StringVal sa && b instanceof SchemeValue.StringVal sb)
            return sa.value().equals(sb.value());
        if (a instanceof SchemeValue.PairVal pa && b instanceof SchemeValue.PairVal pb) {
            long key = System.identityHashCode(pa) * 31L + System.identityHashCode(pb);
            Long boxedKey = key;
            if (!visited.add(boxedKey)) return true;
            return schemeEqualSafe(pa.car(), pb.car(), visited) && schemeEqualSafe(pa.cdr(), pb.cdr(), visited);
        }
        if (a instanceof SchemeValue.ListVal la && b instanceof SchemeValue.ListVal lb)
            return la.elements().isEmpty() && lb.elements().isEmpty();
        if (a instanceof SchemeValue.VectorVal va && b instanceof SchemeValue.VectorVal vb) {
            if (va.length() != vb.length()) return false;
            for (int i = 0; i < va.length(); i++) {
                if (!schemeEqualSafe(va.ref(i), vb.ref(i), visited)) return false;
            }
            return true;
        }
        return false;
    }
}
