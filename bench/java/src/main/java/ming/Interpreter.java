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
        "define", "if", "quote", "lambda", "and", "or", "let", "begin",
        "cond", "set!", "define-syntax", "syntax-rules"
    );

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
            // Append all lists
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
        globals.define("string-copy", new SchemeValue.BuiltinVal("string-copy", args -> {
            if (args.length != 1) throw new EvalError("string-copy: expected 1 argument");
            if (!(args[0] instanceof SchemeValue.StringVal s))
                throw new EvalError("string-copy: expected string");
            return new SchemeValue.StringVal(s.value());
        }));
        globals.define("string-set!", new SchemeValue.BuiltinVal("string-set!", args -> {
            if (args.length != 3) throw new EvalError("string-set!: expected 3 arguments");
            if (!(args[0] instanceof SchemeValue.StringVal s))
                throw new EvalError("string-set!: expected string");
            int idx = (int) asLong(args[1]);
            if (!(args[2] instanceof SchemeValue.CharVal c))
                throw new EvalError("string-set!: expected char");
            s.setCharAt(idx, c.value());
            return new SchemeValue.VoidVal();
        }));

        // L08 builtins
        globals.define("apply", new SchemeValue.BuiltinVal("apply", this::applyProc));

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
            long q = a / b;
            // Truncate toward zero (Java's default for long division)
            return new SchemeValue.IntVal(q);
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
            SchemeValue cur = args[0];
            while (cur instanceof SchemeValue.PairVal p) {
                cur = p.cdr();
            }
            return new SchemeValue.BoolVal(cur instanceof SchemeValue.ListVal l && l.elements().isEmpty());
        }));
        globals.define("eq?", new SchemeValue.BuiltinVal("eq?", args -> {
            if (args.length != 2) throw new EvalError("eq?: expected 2 arguments");
            return new SchemeValue.BoolVal(schemeEq(args[0], args[1]));
        }));
        globals.define("equal?", new SchemeValue.BuiltinVal("equal?", args -> {
            if (args.length != 2) throw new EvalError("equal?: expected 2 arguments");
            return new SchemeValue.BoolVal(schemeEqual(args[0], args[1]));
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
        globals.define("map", new SchemeValue.BuiltinVal("map", args -> {
            if (args.length < 2) throw new EvalError("map: expected at least 2 arguments");
            var proc = args[0];
            if (args.length == 2) {
                // Single list
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
                // Multi-list
                SchemeValue[] lists = new SchemeValue[args.length - 1];
                for (int i = 0; i < lists.length; i++) lists[i] = args[i + 1];
                var result = new ArrayList<SchemeValue>();
                while (true) {
                    // Check if any list is exhausted
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

        // L11 builtins — exact arithmetic & rationals
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
    }

    public SchemeValue eval(SchemeValue expr) throws EvalError {
        return eval(expr, globals);
    }

    public SchemeValue eval(SchemeValue expr, Environment env) throws EvalError {
        try {
            return switch (expr) {
                case SchemeValue.IntVal v -> v;
                case SchemeValue.BoolVal v -> v;
                case SchemeValue.StringVal v -> v;
                case SchemeValue.VoidVal v -> v;
                case SchemeValue.LambdaVal v -> v;
                case SchemeValue.BuiltinVal v -> v;
                case SchemeValue.CharVal v -> v;
                case SchemeValue.DoubleVal v -> v;
                case SchemeValue.RationalVal v -> v;
                case SchemeValue.PairVal v -> v;
                case SchemeValue.MacroVal v -> v;
                case SchemeValue.SymbolVal v -> env.get(v.name());
                case SchemeValue.ListVal v -> evalList(v, env);
            };
        } catch (EvalError e) {
            if (positions != null && !e.getMessage().matches(".*\\d+:\\d+.*")) {
                var pos = positions.get(expr);
                if (pos != null) {
                    throw new EvalError(pos + ": " + e.getMessage());
                }
            }
            throw e;
        }
    }

    private SchemeValue evalList(SchemeValue.ListVal list, Environment env) throws EvalError {
        if (list.elements().isEmpty()) throw new EvalError("empty application");

        var first = list.elements().get(0);
        if (first instanceof SchemeValue.SymbolVal sym) {
            return switch (sym.name()) {
                case "define" -> evalDefine(list.elements(), env);
                case "define-syntax" -> evalDefineSyntax(list.elements(), env);
                case "if" -> evalIf(list.elements(), env);
                case "quote" -> evalQuote(list.elements());
                case "lambda" -> evalLambda(list.elements(), env);
                case "and" -> evalAnd(list.elements(), env);
                case "or" -> evalOr(list.elements(), env);
                case "let" -> evalLet(list.elements(), env);
                case "begin" -> evalBegin(list.elements(), env);
                case "cond" -> evalCond(list.elements(), env);
                case "set!" -> evalSet(list.elements(), env);
                default -> {
                    SchemeValue resolved = null;
                    try { resolved = env.get(sym.name()); } catch (EvalError e) { /* not bound */ }
                    if (resolved instanceof SchemeValue.MacroVal macro) {
                        yield evalMacroExpansion(macro, list, env);
                    }
                    yield evalApplication(list.elements(), env);
                }
            };
        }
        return evalApplication(list.elements(), env);
    }

    private SchemeValue evalDefine(List<SchemeValue> elements, Environment env) throws EvalError {
        if (elements.size() < 3) throw new EvalError("define: bad syntax");
        var target = elements.get(1);
        if (target instanceof SchemeValue.SymbolVal sym) {
            var val = eval(elements.get(2), env);
            env.define(sym.name(), val);
            return new SchemeValue.VoidVal();
        } else if (target instanceof SchemeValue.ListVal nameAndParams) {
            if (nameAndParams.elements().isEmpty())
                throw new EvalError("define: bad syntax");
            if (!(nameAndParams.elements().get(0) instanceof SchemeValue.SymbolVal fnName))
                throw new EvalError("define: expected function name");
            var params = new ArrayList<String>();
            for (int i = 1; i < nameAndParams.elements().size(); i++) {
                if (!(nameAndParams.elements().get(i) instanceof SchemeValue.SymbolVal p))
                    throw new EvalError("define: expected parameter name");
                params.add(p.name());
            }
            var body = elements.subList(2, elements.size());
            var lambda = new SchemeValue.LambdaVal(params, null, body, env);
            env.define(fnName.name(), lambda);
            return new SchemeValue.VoidVal();
        } else if (target instanceof SchemeValue.PairVal pair) {
            // Dotted pair: (define (name p1 p2 . rest) body)
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
            if (cur instanceof SchemeValue.SymbolVal rest) {
                restParam = rest.name();
            } else if (!(cur instanceof SchemeValue.ListVal l && l.elements().isEmpty())) {
                throw new EvalError("define: bad syntax");
            }
            var body = elements.subList(2, elements.size());
            var lambda = new SchemeValue.LambdaVal(params, restParam, body, env);
            env.define(fnName.name(), lambda);
            return new SchemeValue.VoidVal();
        }
        throw new EvalError("define: bad syntax");
    }

    private SchemeValue evalIf(List<SchemeValue> elements, Environment env) throws EvalError {
        if (elements.size() < 3) throw new EvalError("if: bad syntax");
        var cond = eval(elements.get(1), env);
        if (cond.isTruthy()) {
            return eval(elements.get(2), env);
        } else if (elements.size() > 3) {
            return eval(elements.get(3), env);
        }
        return new SchemeValue.VoidVal();
    }

    private SchemeValue evalQuote(List<SchemeValue> elements) throws EvalError {
        if (elements.size() != 2) throw new EvalError("quote: expected 1 argument");
        return quoteDatum(elements.get(1));
    }

    /** Convert reader ListVal to pair chains for quoted data. */
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
            // (lambda args body) — all args collected into single rest param
            var body = elements.subList(2, elements.size());
            return new SchemeValue.LambdaVal(List.of(), sym.name(), body, env);
        }
        if (paramList instanceof SchemeValue.PairVal pair) {
            // Dotted pair: (lambda (p1 p2 . rest) body)
            var params = new ArrayList<String>();
            String restParam = null;
            SchemeValue cur = pair;
            while (cur instanceof SchemeValue.PairVal p) {
                if (!(p.car() instanceof SchemeValue.SymbolVal s))
                    throw new EvalError("lambda: expected parameter name");
                params.add(s.name());
                cur = p.cdr();
            }
            if (cur instanceof SchemeValue.SymbolVal rest) {
                restParam = rest.name();
            }
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

    private SchemeValue evalAnd(List<SchemeValue> elements, Environment env) throws EvalError {
        if (elements.size() == 1) return new SchemeValue.BoolVal(true);
        SchemeValue result = new SchemeValue.BoolVal(true);
        for (int i = 1; i < elements.size(); i++) {
            result = eval(elements.get(i), env);
            if (!result.isTruthy()) return result;
        }
        return result;
    }

    private SchemeValue evalOr(List<SchemeValue> elements, Environment env) throws EvalError {
        if (elements.size() == 1) return new SchemeValue.BoolVal(false);
        for (int i = 1; i < elements.size(); i++) {
            var result = eval(elements.get(i), env);
            if (result.isTruthy()) return result;
        }
        return new SchemeValue.BoolVal(false);
    }

    private SchemeValue evalLet(List<SchemeValue> elements, Environment env) throws EvalError {
        if (elements.size() < 3) throw new EvalError("let: bad syntax");

        // Named let: (let name ((var init) ...) body ...)
        if (elements.get(1) instanceof SchemeValue.SymbolVal loopName) {
            if (elements.size() < 4) throw new EvalError("let: bad syntax");
            if (!(elements.get(2) instanceof SchemeValue.ListVal bl))
                throw new EvalError("let: expected bindings list");
            var params = new ArrayList<String>();
            var initVals = new ArrayList<SchemeValue>();
            for (var binding : bl.elements()) {
                if (!(binding instanceof SchemeValue.ListVal b) || b.elements().size() != 2)
                    throw new EvalError("let: bad binding");
                if (!(b.elements().get(0) instanceof SchemeValue.SymbolVal name))
                    throw new EvalError("let: expected symbol in binding");
                params.add(name.name());
                initVals.add(eval(b.elements().get(1), env));
            }
            var body = elements.subList(3, elements.size());
            var letEnv = new Environment(env);
            var lambda = new SchemeValue.LambdaVal(params, null, body, letEnv);
            letEnv.define(loopName.name(), lambda);
            // Call with initial values
            var callEnv = new Environment(letEnv);
            for (int i = 0; i < params.size(); i++) {
                callEnv.define(params.get(i), initVals.get(i));
            }
            SchemeValue result = new SchemeValue.VoidVal();
            for (var bodyExpr : body) {
                result = eval(bodyExpr, callEnv);
            }
            return result;
        }

        // Regular let: (let ((var init) ...) body ...)
        var bindings = elements.get(1);
        if (!(bindings instanceof SchemeValue.ListVal bl))
            throw new EvalError("let: expected bindings list");
        var letEnv = new Environment(env);
        for (var binding : bl.elements()) {
            if (!(binding instanceof SchemeValue.ListVal b) || b.elements().size() != 2)
                throw new EvalError("let: bad binding");
            if (!(b.elements().get(0) instanceof SchemeValue.SymbolVal name))
                throw new EvalError("let: expected symbol in binding");
            var val = eval(b.elements().get(1), env);
            letEnv.define(name.name(), val);
        }
        SchemeValue result = new SchemeValue.VoidVal();
        for (int i = 2; i < elements.size(); i++) {
            result = eval(elements.get(i), letEnv);
        }
        return result;
    }

    private SchemeValue evalBegin(List<SchemeValue> elements, Environment env) throws EvalError {
        SchemeValue result = new SchemeValue.VoidVal();
        for (int i = 1; i < elements.size(); i++) {
            result = eval(elements.get(i), env);
        }
        return result;
    }

    private SchemeValue evalCond(List<SchemeValue> elements, Environment env) throws EvalError {
        for (int i = 1; i < elements.size(); i++) {
            var clause = elements.get(i);
            if (!(clause instanceof SchemeValue.ListVal cl) || cl.elements().isEmpty())
                throw new EvalError("cond: bad clause");
            var test = cl.elements().get(0);
            if (test instanceof SchemeValue.SymbolVal sym && sym.name().equals("else")) {
                SchemeValue result = new SchemeValue.VoidVal();
                for (int j = 1; j < cl.elements().size(); j++) {
                    result = eval(cl.elements().get(j), env);
                }
                return result;
            }
            var testVal = eval(test, env);
            if (testVal.isTruthy()) {
                SchemeValue result = testVal;
                for (int j = 1; j < cl.elements().size(); j++) {
                    result = eval(cl.elements().get(j), env);
                }
                return result;
            }
        }
        return new SchemeValue.VoidVal();
    }

    private SchemeValue evalSet(List<SchemeValue> elements, Environment env) throws EvalError {
        if (elements.size() != 3) throw new EvalError("set!: bad syntax");
        if (!(elements.get(1) instanceof SchemeValue.SymbolVal sym))
            throw new EvalError("set!: expected symbol");
        var val = eval(elements.get(2), env);
        env.set(sym.name(), val);
        return new SchemeValue.VoidVal();
    }

    private SchemeValue evalApplication(List<SchemeValue> elements, Environment env) throws EvalError {
        var proc = eval(elements.get(0), env);
        var args = new SchemeValue[elements.size() - 1];
        for (int i = 1; i < elements.size(); i++) {
            args[i - 1] = eval(elements.get(i), env);
        }
        return callProc(proc, args);
    }

    private SchemeValue callProc(SchemeValue proc, SchemeValue[] args) throws EvalError {
        if (proc instanceof SchemeValue.LambdaVal lambda) {
            var callEnv = new Environment(lambda.env());
            if (lambda.restParam() != null) {
                if (args.length < lambda.params().size())
                    throw new EvalError("wrong number of arguments: expected at least " + lambda.params().size() + ", got " + args.length);
                for (int i = 0; i < lambda.params().size(); i++) {
                    callEnv.define(lambda.params().get(i), args[i]);
                }
                // Collect remaining args into rest list
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
            SchemeValue result = new SchemeValue.VoidVal();
            for (var bodyExpr : lambda.body()) {
                result = eval(bodyExpr, callEnv);
            }
            return result;
        }

        if (proc instanceof SchemeValue.BuiltinVal builtin) {
            return builtin.proc().apply(args);
        }

        throw new EvalError("not a procedure: " + proc.display());
    }

    // ---- Macro support (Level 10) ----

    private SchemeValue evalDefineSyntax(List<SchemeValue> elements, Environment env) throws EvalError {
        if (elements.size() != 3) throw new EvalError("define-syntax: bad syntax");
        if (!(elements.get(1) instanceof SchemeValue.SymbolVal name))
            throw new EvalError("define-syntax: expected name");
        var transformer = elements.get(2);
        if (!(transformer instanceof SchemeValue.ListVal tl) || tl.elements().isEmpty())
            throw new EvalError("define-syntax: expected syntax-rules");
        if (!(tl.elements().get(0) instanceof SchemeValue.SymbolVal sr) || !sr.name().equals("syntax-rules"))
            throw new EvalError("define-syntax: expected syntax-rules");
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

    private SchemeValue evalMacroExpansion(SchemeValue.MacroVal macro, SchemeValue.ListVal form, Environment env) throws EvalError {
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
                    var macroEnv = new Environment(env);
                    for (var entry : preBindings.entrySet()) {
                        macroEnv.define(entry.getKey(), entry.getValue());
                    }
                    return eval(expanded, macroEnv);
                }
                return eval(expanded, env);
            }
        }
        throw new EvalError("no matching pattern for macro");
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
            if (!s.name().equals("...") && !literals.contains(s.name())) {
                vars.add(s.name());
            }
        } else if (v instanceof SchemeValue.ListVal l) {
            for (var elem : l.elements()) {
                collectPatternVarsHelper(elem, literals, vars);
            }
        }
    }

    private boolean matchPattern(SchemeValue pattern, SchemeValue input, Set<String> literals,
                                  Set<String> patVars, Map<String, Object> bindings) {
        if (pattern instanceof SchemeValue.SymbolVal s) {
            if (s.name().equals("_")) return true;
            if (literals.contains(s.name())) {
                return input instanceof SchemeValue.SymbolVal is && is.name().equals(s.name());
            }
            if (patVars.contains(s.name())) {
                bindings.put(s.name(), input);
                return true;
            }
            return true;
        }
        if (pattern instanceof SchemeValue.ListVal pl && input instanceof SchemeValue.ListVal il) {
            return matchListPattern(pl.elements(), il.elements(), literals, patVars, bindings);
        }
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
                ellipsisIdx = i;
                break;
            }
        }

        if (ellipsisIdx == -1) {
            if (pat.size() != inp.size()) return false;
            for (int i = 0; i < pat.size(); i++) {
                if (i == 0) continue; // skip keyword position
                if (!matchPattern(pat.get(i), inp.get(i), literals, patVars, bindings)) return false;
            }
            return true;
        }

        // Has ellipsis at ellipsisIdx; variadic pattern is at ellipsisIdx-1
        int varStart = ellipsisIdx - 1; // input index where variadic matching starts
        int afterCount = pat.size() - ellipsisIdx - 1;
        if (inp.size() < varStart + afterCount) return false;

        // Match fixed elements before variadic (indices 1..ellipsisIdx-2)
        for (int i = 1; i < ellipsisIdx - 1; i++) {
            if (!matchPattern(pat.get(i), inp.get(i), literals, patVars, bindings)) return false;
        }

        int varCount = inp.size() - varStart - afterCount;

        SchemeValue varPat = pat.get(ellipsisIdx - 1);
        if (varPat instanceof SchemeValue.SymbolVal s && patVars.contains(s.name())) {
            var varBindings = new ArrayList<SchemeValue>();
            for (int i = varStart; i < varStart + varCount; i++) {
                varBindings.add(inp.get(i));
            }
            bindings.put(s.name(), varBindings);
        }

        // Match fixed elements after ellipsis
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
                    || renameMap.containsKey(name)) {
                return;
            }
            // Don't rename macros (needed for recursive macro calls)
            try {
                SchemeValue val = defEnv.get(name);
                if (val instanceof SchemeValue.MacroVal) return;
                String gensym = name + "__m" + (gensymCounter++);
                renameMap.put(name, gensym);
                preBindings.put(gensym, val);
            } catch (EvalError e) {
                // Not bound in defEnv - rename for hygiene (e.g. tmp in swap!)
                String gensym = name + "__m" + (gensymCounter++);
                renameMap.put(name, gensym);
            }
        } else if (template instanceof SchemeValue.ListVal l) {
            for (var elem : l.elements()) {
                collectTemplateRenames(elem, patternVars, renameMap, preBindings, defEnv);
            }
        }
    }

    private SchemeValue instantiateTemplate(SchemeValue template, Map<String, Object> bindings,
                                             Map<String, String> renameMap) {
        if (template instanceof SchemeValue.SymbolVal s) {
            if (bindings.containsKey(s.name())) {
                Object val = bindings.get(s.name());
                if (val instanceof SchemeValue sv) return sv;
                return template; // ellipsis var used without ..., shouldn't happen
            }
            if (renameMap.containsKey(s.name())) {
                return new SchemeValue.SymbolVal(renameMap.get(s.name()));
            }
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
                    i++; // skip the ...
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
            if (bindings.containsKey(s.name()) && bindings.get(s.name()) instanceof List) {
                vars.add(s.name());
            }
        } else if (template instanceof SchemeValue.ListVal l) {
            for (var elem : l.elements()) {
                findEllipsisVarsHelper(elem, bindings, vars);
            }
        }
    }

    // ---- End macro support ----

    private SchemeValue applyProc(SchemeValue[] args) throws EvalError {
        if (args.length < 2) throw new EvalError("apply: expected at least 2 arguments");
        var proc = args[0];
        // Last arg must be a list; prefix args are prepended
        var lastArg = args[args.length - 1];
        var allArgs = new ArrayList<SchemeValue>();
        // Add prefix args
        for (int i = 1; i < args.length - 1; i++) {
            allArgs.add(args[i]);
        }
        // Flatten last arg (must be a list)
        SchemeValue cur = lastArg;
        while (cur instanceof SchemeValue.PairVal p) {
            allArgs.add(p.car());
            cur = p.cdr();
        }
        if (!(cur instanceof SchemeValue.ListVal l && l.elements().isEmpty())) {
            if (!(cur instanceof SchemeValue.ListVal)) {
                throw new EvalError("apply: last argument must be a list");
            }
        }
        return callProc(proc, allArgs.toArray(new SchemeValue[0]));
    }

    @FunctionalInterface
    interface DoubleCmp { boolean test(double a, double b); }

    private SchemeValue compareNum(SchemeValue[] args, DoubleCmp cmp) throws EvalError {
        if (args.length < 2) throw new EvalError("comparison needs at least 2 arguments");
        for (int i = 0; i < args.length - 1; i++) {
            if (!cmp.test(toDouble(args[i]), toDouble(args[i + 1]))) {
                return new SchemeValue.BoolVal(false);
            }
        }
        return new SchemeValue.BoolVal(true);
    }

    private static SchemeValue appendTwo(SchemeValue a, SchemeValue b) throws EvalError {
        if (a instanceof SchemeValue.ListVal l && l.elements().isEmpty()) return b;
        if (a instanceof SchemeValue.PairVal p) {
            return new SchemeValue.PairVal(p.car(), appendTwo(p.cdr(), b));
        }
        throw new EvalError("append: not a proper list");
    }

    private static long asLong(SchemeValue v) throws EvalError {
        if (v instanceof SchemeValue.IntVal iv) return iv.value();
        throw new EvalError("expected number, got: " + v.display());
    }

    // ---- Numeric tower helpers ----

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
        if (a instanceof SchemeValue.ListVal la && la.elements().isEmpty()
                && b instanceof SchemeValue.ListVal lb && lb.elements().isEmpty())
            return true;
        return a == b;
    }

    private static boolean schemeEqual(SchemeValue a, SchemeValue b) {
        if (schemeEq(a, b)) return true;
        if (a instanceof SchemeValue.StringVal sa && b instanceof SchemeValue.StringVal sb)
            return sa.value().equals(sb.value());
        if (a instanceof SchemeValue.PairVal pa && b instanceof SchemeValue.PairVal pb)
            return schemeEqual(pa.car(), pb.car()) && schemeEqual(pa.cdr(), pb.cdr());
        if (a instanceof SchemeValue.ListVal la && b instanceof SchemeValue.ListVal lb)
            return la.elements().isEmpty() && lb.elements().isEmpty();
        return false;
    }
}
