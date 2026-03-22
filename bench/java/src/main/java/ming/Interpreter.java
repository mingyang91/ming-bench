package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

public class Interpreter {
    private final Environment globalEnv = new Environment();
    private final StringBuilder outputBuffer = new StringBuilder();

    // Continuation support
    private int nextContId = 0;
    final Map<Integer, ContData> continuationData = new HashMap<>();
    final IdentityHashMap<SchemeValue, SchemeValue> pendingReturns = new IdentityHashMap<>();
    int topLevelIndex = 0;
    // syntax-case bindings stack
    MacroExpander.Bindings currentSyntaxBindings;
    Environment currentSyntaxDefEnv;
    Environment currentTransformerDefEnv;
    java.util.Set<String> currentTransformerBoundNames;

    // Tracks the outermost let body for continuation restart.
    // Only set once (first let encountered); inner lets don't overwrite it.
    private List<SchemeValue> outerLetBody;
    private int outerLetBodyIndex;
    private Environment outerLetEnv;

    static class ContData {
        final SchemeValue callccExpr;
        final List<SchemeValue> letBody;
        final int letBodyIndex;
        final Environment letBodyEnv;
        final int topLevelIndex;

        ContData(SchemeValue callccExpr, List<SchemeValue> letBody, int letBodyIndex, Environment letBodyEnv, int topLevelIndex) {
            this.callccExpr = callccExpr;
            this.letBody = letBody;
            this.letBodyIndex = letBodyIndex;
            this.letBodyEnv = letBodyEnv;
            this.topLevelIndex = topLevelIndex;
        }
    }

    public Interpreter() {
        registerBuiltins();
    }

    public String getOutput() {
        return outputBuffer.toString();
    }

    private void registerBuiltins() {
        builtin("+", args -> arithPlus(args));
        builtin("-", args -> arithMinus(args));
        builtin("*", args -> arithMul(args));
        builtin("/", args -> arithDiv(args));
        builtin("<", args -> compare(args, (a, b) -> a < b));
        builtin(">", args -> compare(args, (a, b) -> a > b));
        builtin("=", args -> compare(args, (a, b) -> a == b));
        builtin("<=", args -> compare(args, (a, b) -> a <= b));
        builtin(">=", args -> compare(args, (a, b) -> a >= b));
        builtin("not", args -> {
            if (args.size() != 1) throw new EvalError("not: expected 1 argument, got " + args.size());
            return new SchemeValue.BoolVal(!args.getFirst().isTruthy());
        });
        // cxr accessors
        builtin("caar", args -> { if (args.size()!=1) throw new EvalError("caar: expected 1 argument"); return schemeCar(schemeCar(args.getFirst())); });
        builtin("cadr", args -> { if (args.size()!=1) throw new EvalError("cadr: expected 1 argument"); return schemeCar(schemeCdr(args.getFirst())); });
        builtin("cdar", args -> { if (args.size()!=1) throw new EvalError("cdar: expected 1 argument"); return schemeCdr(schemeCar(args.getFirst())); });
        builtin("cddr", args -> { if (args.size()!=1) throw new EvalError("cddr: expected 1 argument"); return schemeCdr(schemeCdr(args.getFirst())); });
        builtin("caddr", args -> { if (args.size()!=1) throw new EvalError("caddr: expected 1 argument"); return schemeCar(schemeCdr(schemeCdr(args.getFirst()))); });
        builtin("cdddr", args -> { if (args.size()!=1) throw new EvalError("cdddr: expected 1 argument"); return schemeCdr(schemeCdr(schemeCdr(args.getFirst()))); });
        builtin("cadddr", args -> { if (args.size()!=1) throw new EvalError("cadddr: expected 1 argument"); return schemeCar(schemeCdr(schemeCdr(schemeCdr(args.getFirst())))); });
        builtin("cons", args -> {
            if (args.size() != 2) throw new EvalError("cons: expected 2 arguments");
            return new SchemeValue.PairVal(args.get(0), args.get(1));
        });
        builtin("car", args -> {
            if (args.size() != 1) throw new EvalError("car: expected 1 argument");
            SchemeValue v = args.getFirst();
            if (v instanceof SchemeValue.PairVal p) return p.car();
            if (v instanceof SchemeValue.ListVal l && !l.elements().isEmpty()) return l.elements().getFirst();
            throw new EvalError("car: expected pair, got: " + v.display());
        });
        builtin("cdr", args -> {
            if (args.size() != 1) throw new EvalError("cdr: expected 1 argument");
            SchemeValue v = args.getFirst();
            if (v instanceof SchemeValue.PairVal p) return p.cdr();
            if (v instanceof SchemeValue.ListVal l && !l.elements().isEmpty()) {
                return new SchemeValue.ListVal(l.elements().subList(1, l.elements().size()));
            }
            throw new EvalError("cdr: expected pair, got: " + v.display());
        });
        builtin("null?", args -> {
            if (args.size() != 1) throw new EvalError("null?: expected 1 argument");
            SchemeValue v = args.getFirst();
            return new SchemeValue.BoolVal(v instanceof SchemeValue.ListVal l && l.elements().isEmpty());
        });
        builtin("list", args -> {
            if (args.isEmpty()) return new SchemeValue.ListVal(List.of());
            SchemeValue result = new SchemeValue.ListVal(List.of());
            for (int i = args.size() - 1; i >= 0; i--) {
                result = new SchemeValue.PairVal(args.get(i), result);
            }
            return result;
        });
        builtin("length", args -> {
            if (args.size() != 1) throw new EvalError("length: expected 1 argument");
            SchemeValue v = args.getFirst();
            long count = 0;
            while (true) {
                if (v instanceof SchemeValue.ListVal l) {
                    count += l.elements().size();
                    break;
                } else if (v instanceof SchemeValue.PairVal p) {
                    count++;
                    v = p.cdr();
                } else {
                    throw new EvalError("length: expected list");
                }
            }
            return new SchemeValue.IntVal(count);
        });
        builtin("append", args -> {
            SchemeValue result = new SchemeValue.ListVal(List.of());
            for (int i = args.size() - 1; i >= 0; i--) {
                SchemeValue lst = args.get(i);
                var elems = new ArrayList<SchemeValue>();
                while (true) {
                    if (lst instanceof SchemeValue.PairVal p) {
                        elems.add(p.car());
                        lst = p.cdr();
                    } else if (lst instanceof SchemeValue.ListVal l) {
                        elems.addAll(l.elements());
                        break;
                    } else {
                        break;
                    }
                }
                for (int j = elems.size() - 1; j >= 0; j--) {
                    result = new SchemeValue.PairVal(elems.get(j), result);
                }
            }
            return result;
        });
        builtin("string?", args -> {
            if (args.size() != 1) throw new EvalError("string?: expected 1 argument");
            return new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.StringVal || args.getFirst() instanceof SchemeValue.MutableStringVal);
        });
        builtin("number?", args -> {
            if (args.size() != 1) throw new EvalError("number?: expected 1 argument");
            return new SchemeValue.BoolVal(isNumber(args.getFirst()));
        });
        builtin("boolean?", args -> {
            if (args.size() != 1) throw new EvalError("boolean?: expected 1 argument");
            return new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.BoolVal);
        });
        builtin("pair?", args -> {
            if (args.size() != 1) throw new EvalError("pair?: expected 1 argument");
            SchemeValue v = args.getFirst();
            return new SchemeValue.BoolVal(v instanceof SchemeValue.PairVal ||
                (v instanceof SchemeValue.ListVal l && !l.elements().isEmpty()));
        });
        builtin("symbol?", args -> {
            if (args.size() != 1) throw new EvalError("symbol?: expected 1 argument");
            return new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.SymbolVal);
        });
        builtin("char?", args -> {
            if (args.size() != 1) throw new EvalError("char?: expected 1 argument");
            return new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.CharVal);
        });
        // Display/Write/Newline
        builtin("display", args -> {
            if (args.size() != 1) throw new EvalError("display: expected 1 argument");
            outputBuffer.append(args.getFirst().displayStr());
            return new SchemeValue.VoidVal();
        });
        builtin("write", args -> {
            if (args.size() != 1) throw new EvalError("write: expected 1 argument");
            outputBuffer.append(args.getFirst().display());
            return new SchemeValue.VoidVal();
        });
        builtin("newline", args -> {
            if (!args.isEmpty()) throw new EvalError("newline: expected 0 arguments");
            outputBuffer.append('\n');
            return new SchemeValue.VoidVal();
        });
        // String operations
        builtin("string-append", args -> {
            var sb = new StringBuilder();
            for (var arg : args) {
                sb.append(requireString(arg, "string-append"));
            }
            return new SchemeValue.StringVal(sb.toString());
        });
        builtin("string-length", args -> {
            if (args.size() != 1) throw new EvalError("string-length: expected 1 argument");
            return new SchemeValue.IntVal(requireString(args.getFirst(), "string-length").length());
        });
        builtin("substring", args -> {
            if (args.size() != 3) throw new EvalError("substring: expected 3 arguments");
            String str = requireString(args.get(0), "substring");
            long start = requireInt(args.get(1));
            long end = requireInt(args.get(2));
            return new SchemeValue.StringVal(str.substring((int) start, (int) end));
        });
        builtin("string->number", args -> {
            if (args.size() != 1) throw new EvalError("string->number: expected 1 argument");
            String str = requireString(args.getFirst(), "string->number");
            try {
                return new SchemeValue.IntVal(Long.parseLong(str));
            } catch (NumberFormatException e) {
                return new SchemeValue.BoolVal(false);
            }
        });
        builtin("number->string", args -> {
            if (args.size() != 1) throw new EvalError("number->string: expected 1 argument");
            return new SchemeValue.StringVal(args.getFirst().display());
        });
        builtin("symbol->string", args -> {
            if (args.size() != 1) throw new EvalError("symbol->string: expected 1 argument");
            if (!(args.getFirst() instanceof SchemeValue.SymbolVal s)) throw new EvalError("symbol->string: expected symbol");
            return new SchemeValue.StringVal(s.name());
        });
        builtin("string->symbol", args -> {
            if (args.size() != 1) throw new EvalError("string->symbol: expected 1 argument");
            return new SchemeValue.SymbolVal(requireString(args.getFirst(), "string->symbol"));
        });
        builtin("syntax->datum", args -> {
            if (args.size() != 1) throw new EvalError("syntax->datum: expected 1 argument");
            return syntaxToDatum(args.getFirst());
        });
        builtin("datum->syntax", args -> {
            if (args.size() != 2) throw new EvalError("datum->syntax: expected 2 arguments");
            // (datum->syntax template-id datum) — datum is already a scheme value
            return args.get(1);
        });
        builtin("string-ref", args -> {
            if (args.size() != 2) throw new EvalError("string-ref: expected 2 arguments");
            String str = requireString(args.get(0), "string-ref");
            long idx = requireInt(args.get(1));
            return new SchemeValue.CharVal(str.charAt((int) idx));
        });
        builtin("string-copy", args -> {
            if (args.size() != 1) throw new EvalError("string-copy: expected 1 argument");
            String str = requireString(args.getFirst(), "string-copy");
            return new SchemeValue.MutableStringVal(new StringBuilder(str));
        });
        builtin("apply", args -> {
            if (args.size() < 2) throw new EvalError("apply: expected at least 2 arguments");
            SchemeValue proc = args.getFirst();
            // Last arg must be a list; prefix args are prepended
            List<SchemeValue> lastList = toJavaList(args.getLast());
            var allArgs = new ArrayList<SchemeValue>();
            for (int i = 1; i < args.size() - 1; i++) {
                allArgs.add(args.get(i));
            }
            allArgs.addAll(lastList);
            if (proc instanceof SchemeValue.ContinuationVal cont) {
                SchemeValue val = allArgs.size() == 1 ? allArgs.getFirst() : new SchemeValue.ValuesVal(new ArrayList<>(allArgs));
                throw new ContinuationException(cont.id(), val);
            }
            if (proc instanceof SchemeValue.LambdaVal lambda) {
                var localEnv = applyLambda(lambda, allArgs, "");
                SchemeValue result = null;
                for (var bodyExpr : lambda.body()) {
                    result = eval(bodyExpr, localEnv);
                }
                return result;
            } else if (proc instanceof SchemeValue.BuiltinVal builtin) {
                return builtin.fn().apply(allArgs);
            }
            throw new EvalError("apply: not a procedure: " + proc.display());
        });
        // call/cc registered as builtins for first-class usage; actual logic handled specially in eval
        globalEnv.define("call/cc", new SchemeValue.BuiltinVal("call/cc", args -> { throw new RuntimeException("call/cc: internal error"); }));
        globalEnv.define("call-with-current-continuation", new SchemeValue.BuiltinVal("call-with-current-continuation", args -> { throw new RuntimeException("internal error"); }));
        // eq? — identity/symbol equality
        builtin("eq?", args -> {
            if (args.size() != 2) throw new EvalError("eq?: expected 2 arguments");
            SchemeValue a = args.get(0), b = args.get(1);
            if (a instanceof SchemeValue.BoolVal ba && b instanceof SchemeValue.BoolVal bb)
                return new SchemeValue.BoolVal(ba.value() == bb.value());
            if (a instanceof SchemeValue.SymbolVal sa && b instanceof SchemeValue.SymbolVal sb)
                return new SchemeValue.BoolVal(sa.name().equals(sb.name()));
            if (a instanceof SchemeValue.IntVal ia && b instanceof SchemeValue.IntVal ib)
                return new SchemeValue.BoolVal(ia.value() == ib.value());
            if (a instanceof SchemeValue.CharVal ca && b instanceof SchemeValue.CharVal cb)
                return new SchemeValue.BoolVal(ca.value() == cb.value());
            if (a instanceof SchemeValue.ListVal la && la.elements().isEmpty() &&
                b instanceof SchemeValue.ListVal lb && lb.elements().isEmpty())
                return new SchemeValue.BoolVal(true);
            if (a instanceof SchemeValue.VoidVal && b instanceof SchemeValue.VoidVal)
                return new SchemeValue.BoolVal(true);
            return new SchemeValue.BoolVal(a == b);
        });
        // equal? — deep structural equality
        builtin("equal?", args -> {
            if (args.size() != 2) throw new EvalError("equal?: expected 2 arguments");
            return new SchemeValue.BoolVal(schemeEqual(args.get(0), args.get(1)));
        });
        // Numeric utilities
        builtin("abs", args -> {
            if (args.size() != 1) throw new EvalError("abs: expected 1 argument");
            return new SchemeValue.IntVal(Math.abs(requireInt(args.getFirst())));
        });
        builtin("modulo", args -> {
            if (args.size() != 2) throw new EvalError("modulo: expected 2 arguments");
            long a = requireInt(args.get(0)), b = requireInt(args.get(1));
            if (b == 0) throw new EvalError("modulo: division by zero");
            return new SchemeValue.IntVal(Math.floorMod(a, b));
        });
        builtin("remainder", args -> {
            if (args.size() != 2) throw new EvalError("remainder: expected 2 arguments");
            long a = requireInt(args.get(0)), b = requireInt(args.get(1));
            if (b == 0) throw new EvalError("remainder: division by zero");
            return new SchemeValue.IntVal(a % b);
        });
        builtin("quotient", args -> {
            if (args.size() != 2) throw new EvalError("quotient: expected 2 arguments");
            long a = requireInt(args.get(0)), b = requireInt(args.get(1));
            if (b == 0) throw new EvalError("quotient: division by zero");
            // truncate toward zero (Java's default integer division)
            return new SchemeValue.IntVal(a / b);
        });
        builtin("min", args -> {
            if (args.isEmpty()) throw new EvalError("min: expected at least 1 argument");
            long result = requireInt(args.getFirst());
            for (int i = 1; i < args.size(); i++) result = Math.min(result, requireInt(args.get(i)));
            return new SchemeValue.IntVal(result);
        });
        builtin("max", args -> {
            if (args.isEmpty()) throw new EvalError("max: expected at least 1 argument");
            long result = requireInt(args.getFirst());
            for (int i = 1; i < args.size(); i++) result = Math.max(result, requireInt(args.get(i)));
            return new SchemeValue.IntVal(result);
        });
        builtin("expt", args -> {
            if (args.size() != 2) throw new EvalError("expt: expected 2 arguments");
            long base = requireInt(args.get(0)), exp = requireInt(args.get(1));
            long result = 1;
            for (long i = 0; i < exp; i++) result *= base;
            return new SchemeValue.IntVal(result);
        });
        // Numeric predicates
        builtin("zero?", args -> {
            if (args.size() != 1) throw new EvalError("zero?: expected 1 argument");
            return new SchemeValue.BoolVal(requireInt(args.getFirst()) == 0);
        });
        builtin("positive?", args -> {
            if (args.size() != 1) throw new EvalError("positive?: expected 1 argument");
            return new SchemeValue.BoolVal(requireInt(args.getFirst()) > 0);
        });
        builtin("negative?", args -> {
            if (args.size() != 1) throw new EvalError("negative?: expected 1 argument");
            return new SchemeValue.BoolVal(requireInt(args.getFirst()) < 0);
        });
        builtin("odd?", args -> {
            if (args.size() != 1) throw new EvalError("odd?: expected 1 argument");
            return new SchemeValue.BoolVal(requireInt(args.getFirst()) % 2 != 0);
        });
        builtin("even?", args -> {
            if (args.size() != 1) throw new EvalError("even?: expected 1 argument");
            return new SchemeValue.BoolVal(requireInt(args.getFirst()) % 2 == 0);
        });
        // List utilities
        builtin("list-ref", args -> {
            if (args.size() != 2) throw new EvalError("list-ref: expected 2 arguments");
            SchemeValue lst = args.get(0);
            long idx = requireInt(args.get(1));
            for (long i = 0; i < idx; i++) {
                if (lst instanceof SchemeValue.PairVal p) lst = p.cdr();
                else if (lst instanceof SchemeValue.ListVal l && !l.elements().isEmpty())
                    lst = new SchemeValue.ListVal(l.elements().subList(1, l.elements().size()));
                else throw new EvalError("list-ref: index out of range");
            }
            if (lst instanceof SchemeValue.PairVal p) return p.car();
            if (lst instanceof SchemeValue.ListVal l && !l.elements().isEmpty()) return l.elements().getFirst();
            throw new EvalError("list-ref: index out of range");
        });
        builtin("list-tail", args -> {
            if (args.size() != 2) throw new EvalError("list-tail: expected 2 arguments");
            SchemeValue lst = args.get(0);
            long idx = requireInt(args.get(1));
            for (long i = 0; i < idx; i++) {
                if (lst instanceof SchemeValue.PairVal p) lst = p.cdr();
                else if (lst instanceof SchemeValue.ListVal l && !l.elements().isEmpty())
                    lst = new SchemeValue.ListVal(l.elements().subList(1, l.elements().size()));
                else throw new EvalError("list-tail: index out of range");
            }
            return lst;
        });
        builtin("list?", args -> {
            if (args.size() != 1) throw new EvalError("list?: expected 1 argument");
            return new SchemeValue.BoolVal(isProperList(args.getFirst()));
        });
        builtin("assoc", args -> {
            if (args.size() != 2) throw new EvalError("assoc: expected 2 arguments");
            SchemeValue key = args.get(0);
            SchemeValue alist = args.get(1);
            while (true) {
                if (alist instanceof SchemeValue.ListVal l && l.elements().isEmpty()) return new SchemeValue.BoolVal(false);
                SchemeValue pair;
                if (alist instanceof SchemeValue.PairVal p) { pair = p.car(); alist = p.cdr(); }
                else if (alist instanceof SchemeValue.ListVal l && !l.elements().isEmpty()) {
                    pair = l.elements().getFirst();
                    alist = new SchemeValue.ListVal(l.elements().subList(1, l.elements().size()));
                } else throw new EvalError("assoc: expected proper list");
                SchemeValue pairKey;
                if (pair instanceof SchemeValue.PairVal pp) pairKey = pp.car();
                else if (pair instanceof SchemeValue.ListVal pl && !pl.elements().isEmpty()) pairKey = pl.elements().getFirst();
                else throw new EvalError("assoc: expected pair in alist");
                if (schemeEqual(key, pairKey)) return pair;
            }
        });
        // map — supports multiple list arguments
        builtin("map", args -> {
            if (args.size() < 2) throw new EvalError("map: expected at least 2 arguments");
            SchemeValue proc = args.getFirst();
            // Convert all list args to java lists
            var lists = new ArrayList<List<SchemeValue>>();
            for (int i = 1; i < args.size(); i++) {
                lists.add(toJavaList(args.get(i)));
            }
            int len = lists.getFirst().size();
            SchemeValue result = new SchemeValue.ListVal(List.of());
            var resultElems = new ArrayList<SchemeValue>();
            for (int i = 0; i < len; i++) {
                var callArgs = new ArrayList<SchemeValue>();
                for (var list : lists) callArgs.add(list.get(i));
                if (proc instanceof SchemeValue.LambdaVal lambda) {
                    var localEnv = applyLambda(lambda, callArgs, "map: ");
                    SchemeValue r = null;
                    for (var bodyExpr : lambda.body()) r = eval(bodyExpr, localEnv);
                    resultElems.add(r);
                } else if (proc instanceof SchemeValue.BuiltinVal builtin) {
                    resultElems.add(builtin.fn().apply(callArgs));
                } else throw new EvalError("map: not a procedure");
            }
            // Build proper list from results
            for (int i = resultElems.size() - 1; i >= 0; i--) {
                result = new SchemeValue.PairVal(resultElems.get(i), result);
            }
            return result;
        });
        // Character utilities
        builtin("char-alphabetic?", args -> {
            if (args.size() != 1) throw new EvalError("char-alphabetic?: expected 1 argument");
            if (!(args.getFirst() instanceof SchemeValue.CharVal c)) throw new EvalError("char-alphabetic?: expected char");
            return new SchemeValue.BoolVal(Character.isLetter(c.value()));
        });
        builtin("char-numeric?", args -> {
            if (args.size() != 1) throw new EvalError("char-numeric?: expected 1 argument");
            if (!(args.getFirst() instanceof SchemeValue.CharVal c)) throw new EvalError("char-numeric?: expected char");
            return new SchemeValue.BoolVal(Character.isDigit(c.value()));
        });
        builtin("char-upcase", args -> {
            if (args.size() != 1) throw new EvalError("char-upcase: expected 1 argument");
            if (!(args.getFirst() instanceof SchemeValue.CharVal c)) throw new EvalError("char-upcase: expected char");
            return new SchemeValue.CharVal(Character.toUpperCase(c.value()));
        });
        builtin("char-downcase", args -> {
            if (args.size() != 1) throw new EvalError("char-downcase: expected 1 argument");
            if (!(args.getFirst() instanceof SchemeValue.CharVal c)) throw new EvalError("char-downcase: expected char");
            return new SchemeValue.CharVal(Character.toLowerCase(c.value()));
        });
        builtin("char=?", args -> {
            if (args.size() != 2) throw new EvalError("char=?: expected 2 arguments");
            if (!(args.get(0) instanceof SchemeValue.CharVal a) || !(args.get(1) instanceof SchemeValue.CharVal b))
                throw new EvalError("char=?: expected chars");
            return new SchemeValue.BoolVal(a.value() == b.value());
        });
        builtin("char<?", args -> {
            if (args.size() != 2) throw new EvalError("char<?: expected 2 arguments");
            if (!(args.get(0) instanceof SchemeValue.CharVal a) || !(args.get(1) instanceof SchemeValue.CharVal b))
                throw new EvalError("char<?: expected chars");
            return new SchemeValue.BoolVal(a.value() < b.value());
        });
        // String comparison/conversion
        builtin("string=?", args -> {
            if (args.size() != 2) throw new EvalError("string=?: expected 2 arguments");
            return new SchemeValue.BoolVal(requireString(args.get(0), "string=?").equals(requireString(args.get(1), "string=?")));
        });
        builtin("string<?", args -> {
            if (args.size() != 2) throw new EvalError("string<?: expected 2 arguments");
            return new SchemeValue.BoolVal(requireString(args.get(0), "string<?").compareTo(requireString(args.get(1), "string<?")) < 0);
        });
        builtin("string-ci=?", args -> {
            if (args.size() != 2) throw new EvalError("string-ci=?: expected 2 arguments");
            return new SchemeValue.BoolVal(requireString(args.get(0), "string-ci=?").equalsIgnoreCase(requireString(args.get(1), "string-ci=?")));
        });
        builtin("string-upcase", args -> {
            if (args.size() != 1) throw new EvalError("string-upcase: expected 1 argument");
            return new SchemeValue.StringVal(requireString(args.getFirst(), "string-upcase").toUpperCase());
        });
        builtin("string-downcase", args -> {
            if (args.size() != 1) throw new EvalError("string-downcase: expected 1 argument");
            return new SchemeValue.StringVal(requireString(args.getFirst(), "string-downcase").toLowerCase());
        });
        builtin("string-set!", args -> {
            if (args.size() != 3) throw new EvalError("string-set!: expected 3 arguments");
            if (args.getFirst() instanceof SchemeValue.MutableStringVal ms) {
                int idx = (int) requireInt(args.get(1));
                if (!(args.get(2) instanceof SchemeValue.CharVal ch))
                    throw new EvalError("string-set!: third argument must be a character");
                ms.chars().setCharAt(idx, ch.value());
                return new SchemeValue.VoidVal();
            } else if (args.getFirst() instanceof SchemeValue.StringVal) {
                throw new EvalError("string-set!: strings are immutable");
            } else {
                throw new EvalError("string-set!: first argument must be a string");
            }
        });
        builtin("string->list", args -> {
            if (args.size() != 1) throw new EvalError("string->list: expected 1 argument");
            String str = requireString(args.getFirst(), "string->list");
            SchemeValue result = new SchemeValue.ListVal(List.of());
            for (int i = str.length() - 1; i >= 0; i--) {
                result = new SchemeValue.PairVal(new SchemeValue.CharVal(str.charAt(i)), result);
            }
            return result;
        });
        builtin("list->string", args -> {
            if (args.size() != 1) throw new EvalError("list->string: expected 1 argument");
            StringBuilder sb = new StringBuilder();
            SchemeValue cur = args.getFirst();
            while (cur instanceof SchemeValue.PairVal p) {
                if (!(p.car() instanceof SchemeValue.CharVal ch)) {
                    throw new EvalError("list->string: expected list of characters");
                }
                sb.append(ch.value());
                cur = p.cdr();
            }
            return new SchemeValue.StringVal(sb.toString());
        });
        builtin("char->integer", args -> {
            if (args.size() != 1) throw new EvalError("char->integer: expected 1 argument");
            if (!(args.getFirst() instanceof SchemeValue.CharVal ch)) {
                throw new EvalError("char->integer: expected char");
            }
            return new SchemeValue.IntVal((long) ch.value());
        });
        builtin("integer->char", args -> {
            if (args.size() != 1) throw new EvalError("integer->char: expected 1 argument");
            long n = requireInt(args.getFirst());
            return new SchemeValue.CharVal((char) n);
        });
        // eqv?
        builtin("eqv?", args -> {
            if (args.size() != 2) throw new EvalError("eqv?: expected 2 arguments");
            return new SchemeValue.BoolVal(schemeEqv(args.get(0), args.get(1)));
        });
        // Vector builtins
        builtin("vector", args -> {
            return new SchemeValue.VectorVal(args.toArray(new SchemeValue[0]));
        });
        builtin("make-vector", args -> {
            if (args.size() < 1 || args.size() > 2) throw new EvalError("make-vector: expected 1-2 arguments");
            int len = (int) requireInt(args.getFirst());
            SchemeValue fill = args.size() == 2 ? args.get(1) : new SchemeValue.IntVal(0);
            var elems = new SchemeValue[len];
            java.util.Arrays.fill(elems, fill);
            return new SchemeValue.VectorVal(elems);
        });
        builtin("vector-ref", args -> {
            if (args.size() != 2) throw new EvalError("vector-ref: expected 2 arguments");
            if (!(args.getFirst() instanceof SchemeValue.VectorVal v)) throw new EvalError("vector-ref: expected vector");
            int idx = (int) requireInt(args.get(1));
            return v.elements()[idx];
        });
        builtin("vector-set!", args -> {
            if (args.size() != 3) throw new EvalError("vector-set!: expected 3 arguments");
            if (!(args.getFirst() instanceof SchemeValue.VectorVal v)) throw new EvalError("vector-set!: expected vector");
            int idx = (int) requireInt(args.get(1));
            v.elements()[idx] = args.get(2);
            return new SchemeValue.VoidVal();
        });
        builtin("vector-length", args -> {
            if (args.size() != 1) throw new EvalError("vector-length: expected 1 argument");
            if (!(args.getFirst() instanceof SchemeValue.VectorVal v)) throw new EvalError("vector-length: expected vector");
            return new SchemeValue.IntVal(v.elements().length);
        });
        builtin("vector?", args -> {
            if (args.size() != 1) throw new EvalError("vector?: expected 1 argument");
            return new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.VectorVal);
        });
        builtin("vector->list", args -> {
            if (args.size() != 1) throw new EvalError("vector->list: expected 1 argument");
            if (!(args.getFirst() instanceof SchemeValue.VectorVal v)) throw new EvalError("vector->list: expected vector");
            return schemeList(java.util.Arrays.asList(v.elements()));
        });
        builtin("list->vector", args -> {
            if (args.size() != 1) throw new EvalError("list->vector: expected 1 argument");
            var elems = toJavaList(args.getFirst());
            return new SchemeValue.VectorVal(elems.toArray(new SchemeValue[0]));
        });
        // Additional builtins needed by L15 realworld tests
        builtin("integer?", args -> {
            if (args.size() != 1) throw new EvalError("integer?: expected 1 argument");
            SchemeValue v = args.getFirst();
            if (v instanceof SchemeValue.IntVal) return new SchemeValue.BoolVal(true);
            if (v instanceof SchemeValue.RationalVal rv) return new SchemeValue.BoolVal(rv.isInteger());
            if (v instanceof SchemeValue.DoubleVal dv) {
                double d = dv.value();
                return new SchemeValue.BoolVal(d == Math.floor(d) && !Double.isInfinite(d));
            }
            return new SchemeValue.BoolVal(false);
        });
        builtin("exact?", args -> {
            if (args.size() != 1) throw new EvalError("exact?: expected 1 argument");
            return new SchemeValue.BoolVal(isExact(args.getFirst()));
        });
        builtin("inexact?", args -> {
            if (args.size() != 1) throw new EvalError("inexact?: expected 1 argument");
            return new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.DoubleVal);
        });
        builtin("rational?", args -> {
            if (args.size() != 1) throw new EvalError("rational?: expected 1 argument");
            return new SchemeValue.BoolVal(isExact(args.getFirst()));
        });
        builtin("exact->inexact", args -> {
            if (args.size() != 1) throw new EvalError("exact->inexact: expected 1 argument");
            return new SchemeValue.DoubleVal(toDouble(args.getFirst()));
        });
        builtin("inexact->exact", args -> {
            if (args.size() != 1) throw new EvalError("inexact->exact: expected 1 argument");
            SchemeValue v = args.getFirst();
            if (isExact(v)) return v;
            if (v instanceof SchemeValue.DoubleVal dv) {
                double d = dv.value();
                // Convert to rational via continued fraction / simple approach
                // Use the fact that doubles are rationals: multiply by power of 2
                if (d == Math.floor(d) && !Double.isInfinite(d)) {
                    return new SchemeValue.IntVal((long) d);
                }
                // Convert double to exact rational
                long bits = Double.doubleToLongBits(d);
                boolean negative = (bits >>> 63) != 0;
                int exp = (int)((bits >>> 52) & 0x7FFL) - 1023;
                long mantissa = (bits & 0xFFFFFFFFFFFFFL) | (1L << 52);
                // value = mantissa * 2^(exp - 52)
                int shift = exp - 52;
                long num, den;
                if (shift >= 0) {
                    num = mantissa << shift;
                    den = 1;
                } else {
                    num = mantissa;
                    den = 1L << (-shift);
                }
                if (negative) num = -num;
                return makeExact(num, den);
            }
            throw new EvalError("inexact->exact: expected number, got: " + v.display());
        });
        builtin("numerator", args -> {
            if (args.size() != 1) throw new EvalError("numerator: expected 1 argument");
            SchemeValue v = args.getFirst();
            if (v instanceof SchemeValue.IntVal iv) return new SchemeValue.IntVal(iv.value());
            if (v instanceof SchemeValue.RationalVal rv) return new SchemeValue.IntVal(rv.num());
            throw new EvalError("numerator: expected exact number, got: " + v.display());
        });
        builtin("denominator", args -> {
            if (args.size() != 1) throw new EvalError("denominator: expected 1 argument");
            SchemeValue v = args.getFirst();
            if (v instanceof SchemeValue.IntVal) return new SchemeValue.IntVal(1);
            if (v instanceof SchemeValue.RationalVal rv) return new SchemeValue.IntVal(rv.den());
            throw new EvalError("denominator: expected exact number, got: " + v.display());
        });
        builtin("procedure?", args -> {
            if (args.size() != 1) throw new EvalError("procedure?: expected 1 argument");
            SchemeValue v = args.getFirst();
            return new SchemeValue.BoolVal(v instanceof SchemeValue.LambdaVal || v instanceof SchemeValue.BuiltinVal || v instanceof SchemeValue.ContinuationVal);
        });
        builtin("for-each", args -> {
            if (args.size() < 2) throw new EvalError("for-each: expected at least 2 arguments");
            SchemeValue proc = args.getFirst();
            var lists = new ArrayList<List<SchemeValue>>();
            for (int i = 1; i < args.size(); i++) {
                lists.add(toJavaList(args.get(i)));
            }
            int len = lists.getFirst().size();
            for (int i = 0; i < len; i++) {
                var callArgs = new ArrayList<SchemeValue>();
                for (var list : lists) callArgs.add(list.get(i));
                if (proc instanceof SchemeValue.LambdaVal lambda) {
                    var localEnv = applyLambda(lambda, callArgs, "for-each: ");
                    for (var bodyExpr : lambda.body()) eval(bodyExpr, localEnv);
                } else if (proc instanceof SchemeValue.BuiltinVal builtin) {
                    builtin.fn().apply(callArgs);
                } else throw new EvalError("for-each: not a procedure");
            }
            return new SchemeValue.VoidVal();
        });
        builtin("member", args -> {
            if (args.size() != 2) throw new EvalError("member: expected 2 arguments");
            SchemeValue key = args.get(0);
            SchemeValue lst = args.get(1);
            while (true) {
                if (lst instanceof SchemeValue.ListVal l && l.elements().isEmpty()) return new SchemeValue.BoolVal(false);
                if (lst instanceof SchemeValue.PairVal p) {
                    if (schemeEqual(key, p.car())) return lst;
                    lst = p.cdr();
                } else if (lst instanceof SchemeValue.ListVal l && !l.elements().isEmpty()) {
                    if (schemeEqual(key, l.elements().getFirst())) return lst;
                    lst = new SchemeValue.ListVal(l.elements().subList(1, l.elements().size()));
                } else return new SchemeValue.BoolVal(false);
            }
        });
        builtin("assv", args -> {
            if (args.size() != 2) throw new EvalError("assv: expected 2 arguments");
            SchemeValue key = args.get(0);
            SchemeValue alist = args.get(1);
            while (true) {
                if (alist instanceof SchemeValue.ListVal l && l.elements().isEmpty()) return new SchemeValue.BoolVal(false);
                SchemeValue pair;
                if (alist instanceof SchemeValue.PairVal p) { pair = p.car(); alist = p.cdr(); }
                else if (alist instanceof SchemeValue.ListVal l && !l.elements().isEmpty()) {
                    pair = l.elements().getFirst();
                    alist = new SchemeValue.ListVal(l.elements().subList(1, l.elements().size()));
                } else throw new EvalError("assv: expected proper list");
                SchemeValue pairKey;
                if (pair instanceof SchemeValue.PairVal pp) pairKey = pp.car();
                else if (pair instanceof SchemeValue.ListVal pl && !pl.elements().isEmpty()) pairKey = pl.elements().getFirst();
                else throw new EvalError("assv: expected pair in alist");
                if (schemeEqv(key, pairKey)) return pair;
            }
        });
        builtin("memq", args -> {
            if (args.size() != 2) throw new EvalError("memq: expected 2 arguments");
            SchemeValue key = args.get(0);
            SchemeValue lst = args.get(1);
            while (true) {
                if (lst instanceof SchemeValue.ListVal l && l.elements().isEmpty()) return new SchemeValue.BoolVal(false);
                if (lst instanceof SchemeValue.PairVal p) {
                    if (schemeEq(key, p.car())) return lst;
                    lst = p.cdr();
                } else if (lst instanceof SchemeValue.ListVal l && !l.elements().isEmpty()) {
                    if (schemeEq(key, l.elements().getFirst())) return lst;
                    lst = new SchemeValue.ListVal(l.elements().subList(1, l.elements().size()));
                } else return new SchemeValue.BoolVal(false);
            }
        });
        builtin("assq", args -> {
            if (args.size() != 2) throw new EvalError("assq: expected 2 arguments");
            SchemeValue key = args.get(0);
            SchemeValue alist = args.get(1);
            while (true) {
                if (alist instanceof SchemeValue.ListVal l && l.elements().isEmpty()) return new SchemeValue.BoolVal(false);
                SchemeValue pair;
                if (alist instanceof SchemeValue.PairVal p) { pair = p.car(); alist = p.cdr(); }
                else if (alist instanceof SchemeValue.ListVal l && !l.elements().isEmpty()) {
                    pair = l.elements().getFirst();
                    alist = new SchemeValue.ListVal(l.elements().subList(1, l.elements().size()));
                } else throw new EvalError("assq: expected proper list");
                SchemeValue pairKey;
                if (pair instanceof SchemeValue.PairVal pp) pairKey = pp.car();
                else if (pair instanceof SchemeValue.ListVal pl && !pl.elements().isEmpty()) pairKey = pl.elements().getFirst();
                else throw new EvalError("assq: expected pair in alist");
                if (schemeEq(key, pairKey)) return pair;
            }
        });
        builtin("gcd", args -> {
            if (args.isEmpty()) return new SchemeValue.IntVal(0);
            long result = Math.abs(requireInt(args.getFirst()));
            for (int i = 1; i < args.size(); i++) {
                long b = Math.abs(requireInt(args.get(i)));
                while (b != 0) { long t = b; b = result % b; result = t; }
            }
            return new SchemeValue.IntVal(result);
        });
        builtin("lcm", args -> {
            if (args.isEmpty()) return new SchemeValue.IntVal(1);
            long result = Math.abs(requireInt(args.getFirst()));
            for (int i = 1; i < args.size(); i++) {
                long b = Math.abs(requireInt(args.get(i)));
                if (result == 0 && b == 0) { result = 0; } else { result = result / gcd(result, b) * b; }
            }
            return new SchemeValue.IntVal(result);
        });
        builtin("truncate", args -> {
            if (args.size() != 1) throw new EvalError("truncate: expected 1 argument");
            return new SchemeValue.IntVal(requireInt(args.getFirst()));
        });
        builtin("round", args -> {
            if (args.size() != 1) throw new EvalError("round: expected 1 argument");
            return new SchemeValue.IntVal(requireInt(args.getFirst()));
        });
        builtin("make-string", args -> {
            if (args.size() < 1 || args.size() > 2) throw new EvalError("make-string: expected 1-2 arguments");
            int len = (int) requireInt(args.getFirst());
            char fill = args.size() == 2 && args.get(1) instanceof SchemeValue.CharVal ch ? ch.value() : ' ';
            return new SchemeValue.MutableStringVal(new StringBuilder(String.valueOf(fill).repeat(len)));
        });
        builtin("string", args -> {
            var sb = new StringBuilder();
            for (var arg : args) {
                if (!(arg instanceof SchemeValue.CharVal ch)) throw new EvalError("string: expected character");
                sb.append(ch.value());
            }
            return new SchemeValue.StringVal(sb.toString());
        });
        builtin("string>?", args -> {
            if (args.size() != 2) throw new EvalError("string>?: expected 2 arguments");
            return new SchemeValue.BoolVal(requireString(args.get(0), "string>?").compareTo(requireString(args.get(1), "string>?")) > 0);
        });
        builtin("string<=?", args -> {
            if (args.size() != 2) throw new EvalError("string<=?: expected 2 arguments");
            return new SchemeValue.BoolVal(requireString(args.get(0), "string<=?").compareTo(requireString(args.get(1), "string<=?")) <= 0);
        });
        builtin("string>=?", args -> {
            if (args.size() != 2) throw new EvalError("string>=?: expected 2 arguments");
            return new SchemeValue.BoolVal(requireString(args.get(0), "string>=?").compareTo(requireString(args.get(1), "string>=?")) >= 0);
        });
        builtin("reverse", args -> {
            if (args.size() != 1) throw new EvalError("reverse: expected 1 argument");
            var elems = toJavaList(args.getFirst());
            java.util.Collections.reverse(elems);
            return schemeList(elems);
        });
        builtin("set-car!", args -> {
            if (args.size() != 2) throw new EvalError("set-car!: expected 2 arguments");
            if (!(args.getFirst() instanceof SchemeValue.PairVal p)) throw new EvalError("set-car!: expected pair");
            p.car = args.get(1);
            return new SchemeValue.VoidVal();
        });
        builtin("set-cdr!", args -> {
            if (args.size() != 2) throw new EvalError("set-cdr!: expected 2 arguments");
            if (!(args.getFirst() instanceof SchemeValue.PairVal p)) throw new EvalError("set-cdr!: expected pair");
            p.cdr = args.get(1);
            return new SchemeValue.VoidVal();
        });
        builtin("dynamic-wind", args -> {
            if (args.size() != 3) throw new EvalError("dynamic-wind: expected 3 arguments");
            SchemeValue inThunk = args.get(0);
            SchemeValue bodyThunk = args.get(1);
            SchemeValue outThunk = args.get(2);
            callThunk(inThunk, "dynamic-wind");
            SchemeValue result;
            try {
                result = callThunk(bodyThunk, "dynamic-wind");
            } catch (ContinuationException e) {
                callThunk(outThunk, "dynamic-wind");
                throw e;
            } catch (SchemeException e) {
                callThunk(outThunk, "dynamic-wind");
                throw e;
            }
            callThunk(outThunk, "dynamic-wind");
            return result;
        });
        builtin("values", args -> {
            if (args.size() == 1) return args.getFirst();
            return new SchemeValue.ValuesVal(args);
        });
        builtin("call-with-values", args -> {
            if (args.size() != 2) throw new EvalError("call-with-values: expected 2 arguments");
            SchemeValue producer = args.get(0);
            SchemeValue consumer = args.get(1);
            SchemeValue produced = callThunk(producer, "call-with-values");
            List<SchemeValue> vals;
            if (produced instanceof SchemeValue.ValuesVal mv) {
                vals = mv.values();
            } else {
                vals = List.of(produced);
            }
            if (consumer instanceof SchemeValue.LambdaVal lambda) {
                var localEnv = applyLambda(lambda, vals, "call-with-values: ");
                SchemeValue r = null;
                for (var bodyExpr : lambda.body()) r = eval(bodyExpr, localEnv);
                return r;
            } else if (consumer instanceof SchemeValue.BuiltinVal builtin) {
                try {
                    return builtin.fn().apply(vals);
                } catch (RuntimeException re) {
                    if (re.getCause() instanceof EvalError ee) throw ee;
                    throw re;
                }
            }
            throw new EvalError("call-with-values: consumer is not a procedure");
        });
        globalEnv.define("raise", new SchemeValue.BuiltinVal("raise", args -> {
            if (args.size() != 1) throw new RuntimeException(new EvalError("raise: expected 1 argument"));
            throw new SchemeException(args.getFirst());
        }));
        builtin("with-exception-handler", args -> {
            if (args.size() != 2) throw new EvalError("with-exception-handler: expected 2 arguments");
            SchemeValue handler = args.get(0);
            SchemeValue thunk = args.get(1);
            try {
                return callThunk(thunk, "with-exception-handler");
            } catch (SchemeException e) {
                if (handler instanceof SchemeValue.LambdaVal lambda) {
                    var localEnv = applyLambda(lambda, List.of(e.value), "with-exception-handler: ");
                    SchemeValue r = null;
                    for (var bodyExpr : lambda.body()) r = eval(bodyExpr, localEnv);
                    return r;
                } else if (handler instanceof SchemeValue.BuiltinVal builtin) {
                    try {
                        return builtin.fn().apply(List.of(e.value));
                    } catch (RuntimeException re) {
                        if (re.getCause() instanceof EvalError ee) throw ee;
                        throw re;
                    }
                }
                throw new EvalError("with-exception-handler: handler is not a procedure");
            }
        });
    }

    @FunctionalInterface
    interface CheckedFunction {
        SchemeValue apply(List<SchemeValue> args) throws EvalError;
    }

    private void builtin(String name, CheckedFunction fn) {
        globalEnv.define(name, new SchemeValue.BuiltinVal(name, args -> {
            try {
                return fn.apply(args);
            } catch (EvalError e) {
                throw new RuntimeException(e);
            }
        }));
    }

    private static String posPrefix(SchemeValue expr) {
        SourcePos p = expr.pos();
        return p != null ? p + ": " : "";
    }

    public SchemeValue eval(SchemeValue expr) throws EvalError {
        return eval(expr, globalEnv);
    }

    private record GuardContext(String varName, List<SchemeValue> clauses, Environment env) {}

    public SchemeValue eval(SchemeValue expr, Environment env) throws EvalError {
        java.util.ArrayDeque<GuardContext> guardStack = new java.util.ArrayDeque<>();
        while (true) { try {
            switch (expr) {
                case SchemeValue.IntVal v -> { return v; }
                case SchemeValue.DoubleVal v -> { return v; }
                case SchemeValue.RationalVal v -> { return v; }
                case SchemeValue.BoolVal v -> { return v; }
                case SchemeValue.StringVal v -> { return v; }
                case SchemeValue.LambdaVal v -> { return v; }
                case SchemeValue.BuiltinVal v -> { return v; }
                case SchemeValue.PairVal v -> { return v; }
                case SchemeValue.VoidVal v -> { return v; }
                case SchemeValue.CharVal v -> { return v; }
                case SchemeValue.MutableStringVal v -> { return v; }
                case SchemeValue.ContinuationVal v -> { return v; }
                case SchemeValue.SyntaxRulesVal v -> { return v; }
                case SchemeValue.TransformerVal v -> { return v; }
                case SchemeValue.VectorVal v -> { return v; }
                case SchemeValue.ValuesVal v -> { return v; }
                case SchemeValue.RecordVal v -> { return v; }
                case SchemeValue.SymbolVal v -> {
                    try {
                        return env.get(v.name());
                    } catch (EvalError e) {
                        throw new EvalError(posPrefix(expr) + e.getMessage());
                    }
                }
                case SchemeValue.ListVal listVal -> {
                    List<SchemeValue> elements = listVal.elements();
                    if (elements.isEmpty()) {
                        throw new EvalError(posPrefix(listVal) + "empty application");
                    }
                    SchemeValue head = elements.getFirst();
                    if (head instanceof SchemeValue.SymbolVal sym) {
                        String name = sym.name();
                        switch (name) {
                            case "and" -> {
                                if (elements.size() == 1) return new SchemeValue.BoolVal(true);
                                for (int i = 1; i < elements.size() - 1; i++) {
                                    SchemeValue result = eval(elements.get(i), env);
                                    if (!result.isTruthy()) return result;
                                }
                                expr = elements.getLast();
                                continue;
                            }
                            case "or" -> {
                                if (elements.size() == 1) return new SchemeValue.BoolVal(false);
                                for (int i = 1; i < elements.size() - 1; i++) {
                                    SchemeValue result = eval(elements.get(i), env);
                                    if (result.isTruthy()) return result;
                                }
                                expr = elements.getLast();
                                continue;
                            }
                            case "define" -> { return evalDefine(listVal, elements, env); }
                            case "set!" -> {
                                if (elements.size() != 3) throw new EvalError(posPrefix(listVal) + "set!: bad syntax");
                                if (!(elements.get(1) instanceof SchemeValue.SymbolVal sym2))
                                    throw new EvalError(posPrefix(listVal) + "set!: expected symbol");
                                SchemeValue val = eval(elements.get(2), env);
                                env.set(sym2.name(), val);
                                return new SchemeValue.VoidVal();
                            }
                            case "if" -> {
                                if (elements.size() < 3 || elements.size() > 4) throw new EvalError(posPrefix(listVal) + "if: bad syntax");
                                SchemeValue cond = eval(elements.get(1), env);
                                if (cond.isTruthy()) {
                                    expr = elements.get(2);
                                } else if (elements.size() == 4) {
                                    expr = elements.get(3);
                                } else {
                                    return new SchemeValue.VoidVal();
                                }
                                continue;
                            }
                            case "quote" -> {
                                if (elements.size() != 2) throw new EvalError(posPrefix(listVal) + "quote: expected 1 argument");
                                return elements.get(1);
                            }
                            case "lambda" -> { return evalLambda(listVal, elements, env); }
                            case "let" -> {
                                if (elements.size() < 3) throw new EvalError("let: bad syntax");
                                if (elements.get(1) instanceof SchemeValue.SymbolVal nameSym) {
                                    // Named let
                                    if (elements.size() < 4) throw new EvalError("let: bad syntax");
                                    if (!(elements.get(2) instanceof SchemeValue.ListVal bl)) throw new EvalError("let: expected bindings list");
                                    var params = new ArrayList<String>();
                                    var inits = new ArrayList<SchemeValue>();
                                    for (var binding : bl.elements()) {
                                        if (!(binding instanceof SchemeValue.ListVal b) || b.elements().size() != 2)
                                            throw new EvalError("let: bad binding");
                                        if (!(b.elements().getFirst() instanceof SchemeValue.SymbolVal s))
                                            throw new EvalError("let: expected symbol in binding");
                                        params.add(s.name());
                                        inits.add(eval(b.elements().get(1), env));
                                    }
                                    var body = elements.subList(3, elements.size());
                                    var localEnv = new Environment(env);
                                    var lambda = new SchemeValue.LambdaVal(params, null, body, localEnv);
                                    localEnv.define(nameSym.name(), lambda);
                                    // TCO: set up apply inline
                                    for (int i = 0; i < params.size(); i++) {
                                        localEnv.define(params.get(i), inits.get(i));
                                    }
                                    for (int i = 0; i < body.size() - 1; i++) {
                                        eval(body.get(i), localEnv);
                                    }
                                    expr = body.getLast();
                                    env = localEnv;
                                    continue;
                                }
                                // Regular let
                                if (!(elements.get(1) instanceof SchemeValue.ListVal bl)) throw new EvalError("let: expected bindings list");
                                var localEnv = new Environment(env);
                                for (var binding : bl.elements()) {
                                    if (!(binding instanceof SchemeValue.ListVal b) || b.elements().size() != 2)
                                        throw new EvalError("let: bad binding");
                                    if (!(b.elements().getFirst() instanceof SchemeValue.SymbolVal s))
                                        throw new EvalError("let: expected symbol in binding");
                                    localEnv.define(s.name(), eval(b.elements().get(1), env));
                                }
                                var letBody = elements.subList(2, elements.size());
                                boolean isOuterLet = (outerLetBody == null);
                                if (isOuterLet) {
                                    outerLetBody = letBody;
                                    outerLetEnv = localEnv;
                                }
                                for (int i = 0; i < letBody.size() - 1; i++) {
                                    if (isOuterLet) outerLetBodyIndex = i;
                                    eval(letBody.get(i), localEnv);
                                }
                                if (isOuterLet) outerLetBodyIndex = letBody.size() - 1;
                                expr = letBody.getLast();
                                env = localEnv;
                                continue;
                            }
                            case "begin" -> {
                                if (elements.size() < 2) throw new EvalError("begin: empty body");
                                for (int i = 1; i < elements.size() - 1; i++) {
                                    eval(elements.get(i), env);
                                }
                                expr = elements.getLast();
                                continue;
                            }
                            case "call/cc", "call-with-current-continuation" -> {
                                SchemeValue pending = pendingReturns.remove(listVal);
                                if (pending != null) return pending;
                                if (elements.size() != 2) throw new EvalError(posPrefix(listVal) + "call/cc: expected 1 argument");
                                SchemeValue proc = eval(elements.get(1), env);
                                return handleCallCC(proc, listVal);
                            }
                            case "define-record-type" -> {
                                return evalDefineRecordType(elements, env);
                            }
                            case "define-syntax" -> {
                                if (elements.size() != 3) throw new EvalError(posPrefix(listVal) + "define-syntax: bad syntax");
                                if (!(elements.get(1) instanceof SchemeValue.SymbolVal nameSym))
                                    throw new EvalError(posPrefix(listVal) + "define-syntax: expected symbol");
                                SchemeValue transformerExpr = elements.get(2);
                                SchemeValue transformer;
                                if (transformerExpr instanceof SchemeValue.ListVal tl
                                    && !tl.elements().isEmpty()
                                    && tl.elements().getFirst() instanceof SchemeValue.SymbolVal ts
                                    && ts.name().equals("syntax-rules")) {
                                    transformer = evalSyntaxRules(transformerExpr, env);
                                } else {
                                    // Evaluate as expression (e.g., lambda transformer for syntax-case)
                                    SchemeValue proc = eval(transformerExpr, env);
                                    transformer = new SchemeValue.TransformerVal(proc, env, env.boundNames());
                                }
                                env.define(nameSym.name(), transformer);
                                return new SchemeValue.VoidVal();
                            }
                            case "let*" -> {
                                if (elements.size() < 3) throw new EvalError("let*: bad syntax");
                                if (!(elements.get(1) instanceof SchemeValue.ListVal bl)) throw new EvalError("let*: expected bindings list");
                                var localEnv = new Environment(env);
                                for (var binding : bl.elements()) {
                                    if (!(binding instanceof SchemeValue.ListVal b) || b.elements().size() != 2)
                                        throw new EvalError("let*: bad binding");
                                    if (!(b.elements().getFirst() instanceof SchemeValue.SymbolVal s))
                                        throw new EvalError("let*: expected symbol in binding");
                                    localEnv.define(s.name(), eval(b.elements().get(1), localEnv));
                                }
                                for (int i = 2; i < elements.size() - 1; i++) {
                                    eval(elements.get(i), localEnv);
                                }
                                expr = elements.getLast();
                                env = localEnv;
                                continue;
                            }
                            case "letrec" -> {
                                if (elements.size() < 3) throw new EvalError("letrec: bad syntax");
                                if (!(elements.get(1) instanceof SchemeValue.ListVal bl)) throw new EvalError("letrec: expected bindings list");
                                var localEnv = new Environment(env);
                                // First pass: define all names with undefined placeholder
                                var names = new ArrayList<String>();
                                var initExprs = new ArrayList<SchemeValue>();
                                for (var binding : bl.elements()) {
                                    if (!(binding instanceof SchemeValue.ListVal b) || b.elements().size() != 2)
                                        throw new EvalError("letrec: bad binding");
                                    if (!(b.elements().getFirst() instanceof SchemeValue.SymbolVal s))
                                        throw new EvalError("letrec: expected symbol in binding");
                                    names.add(s.name());
                                    initExprs.add(b.elements().get(1));
                                    localEnv.define(s.name(), new SchemeValue.VoidVal());
                                }
                                // Second pass: evaluate all inits and assign
                                for (int i = 0; i < names.size(); i++) {
                                    localEnv.set(names.get(i), eval(initExprs.get(i), localEnv));
                                }
                                for (int i = 2; i < elements.size() - 1; i++) {
                                    eval(elements.get(i), localEnv);
                                }
                                expr = elements.getLast();
                                env = localEnv;
                                continue;
                            }
                            case "letrec*" -> {
                                if (elements.size() < 3) throw new EvalError("letrec*: bad syntax");
                                if (!(elements.get(1) instanceof SchemeValue.ListVal bl)) throw new EvalError("letrec*: expected bindings list");
                                var localEnv = new Environment(env);
                                for (var binding : bl.elements()) {
                                    if (!(binding instanceof SchemeValue.ListVal b) || b.elements().size() != 2)
                                        throw new EvalError("letrec*: bad binding");
                                    if (!(b.elements().getFirst() instanceof SchemeValue.SymbolVal s))
                                        throw new EvalError("letrec*: expected symbol in binding");
                                    localEnv.define(s.name(), eval(b.elements().get(1), localEnv));
                                }
                                for (int i = 2; i < elements.size() - 1; i++) {
                                    eval(elements.get(i), localEnv);
                                }
                                expr = elements.getLast();
                                env = localEnv;
                                continue;
                            }
                            case "case" -> {
                                if (elements.size() < 3) throw new EvalError("case: bad syntax");
                                SchemeValue key = eval(elements.get(1), env);
                                boolean matched = false;
                                for (int i = 2; i < elements.size(); i++) {
                                    if (!(elements.get(i) instanceof SchemeValue.ListVal clause) || clause.elements().isEmpty())
                                        throw new EvalError("case: bad clause");
                                    SchemeValue datums = clause.elements().getFirst();
                                    if (datums instanceof SchemeValue.SymbolVal s && s.name().equals("else")) {
                                        for (int j = 1; j < clause.elements().size() - 1; j++) {
                                            eval(clause.elements().get(j), env);
                                        }
                                        expr = clause.elements().getLast();
                                        matched = true;
                                        break;
                                    }
                                    if (!(datums instanceof SchemeValue.ListVal dl)) throw new EvalError("case: bad datum list");
                                    boolean found = false;
                                    for (var datum : dl.elements()) {
                                        if (schemeEqv(key, datum)) { found = true; break; }
                                    }
                                    if (found) {
                                        if (clause.elements().size() == 1) { return new SchemeValue.VoidVal(); }
                                        for (int j = 1; j < clause.elements().size() - 1; j++) {
                                            eval(clause.elements().get(j), env);
                                        }
                                        expr = clause.elements().getLast();
                                        matched = true;
                                        break;
                                    }
                                }
                                if (matched) continue;
                                return new SchemeValue.VoidVal();
                            }
                            case "do" -> {
                                // (do ((var init step) ...) (test expr ...) body ...)
                                if (elements.size() < 3) throw new EvalError("do: bad syntax");
                                if (!(elements.get(1) instanceof SchemeValue.ListVal bindings)) throw new EvalError("do: expected bindings list");
                                if (!(elements.get(2) instanceof SchemeValue.ListVal testClause) || testClause.elements().isEmpty())
                                    throw new EvalError("do: expected test clause");
                                var localEnv = new Environment(env);
                                var varNames = new ArrayList<String>();
                                var stepExprs = new ArrayList<SchemeValue>(); // null = no step
                                for (var binding : bindings.elements()) {
                                    if (!(binding instanceof SchemeValue.ListVal b) || b.elements().size() < 2)
                                        throw new EvalError("do: bad binding");
                                    if (!(b.elements().getFirst() instanceof SchemeValue.SymbolVal s))
                                        throw new EvalError("do: expected symbol");
                                    varNames.add(s.name());
                                    localEnv.define(s.name(), eval(b.elements().get(1), env));
                                    stepExprs.add(b.elements().size() >= 3 ? b.elements().get(2) : null);
                                }
                                SchemeValue testExpr = testClause.elements().getFirst();
                                var resultExprs = testClause.elements().subList(1, testClause.elements().size());
                                while (true) {
                                    SchemeValue testResult = eval(testExpr, localEnv);
                                    if (testResult.isTruthy()) {
                                        if (resultExprs.isEmpty()) return new SchemeValue.VoidVal();
                                        for (int i = 0; i < resultExprs.size() - 1; i++) {
                                            eval(resultExprs.get(i), localEnv);
                                        }
                                        expr = resultExprs.getLast();
                                        env = localEnv;
                                        break;
                                    }
                                    // Execute body
                                    for (int i = 3; i < elements.size(); i++) {
                                        eval(elements.get(i), localEnv);
                                    }
                                    // Compute steps
                                    var newVals = new ArrayList<SchemeValue>();
                                    for (int i = 0; i < varNames.size(); i++) {
                                        if (stepExprs.get(i) != null) {
                                            newVals.add(eval(stepExprs.get(i), localEnv));
                                        } else {
                                            newVals.add(localEnv.get(varNames.get(i)));
                                        }
                                    }
                                    for (int i = 0; i < varNames.size(); i++) {
                                        localEnv.set(varNames.get(i), newVals.get(i));
                                    }
                                }
                                continue;
                            }
                            case "guard" -> {
                                // (guard (var clause ...) body ...)
                                if (elements.size() < 3) throw new EvalError("guard: bad syntax");
                                if (!(elements.get(1) instanceof SchemeValue.ListVal clauseList) || clauseList.elements().isEmpty())
                                    throw new EvalError("guard: bad syntax");
                                if (!(clauseList.elements().getFirst() instanceof SchemeValue.SymbolVal varSym))
                                    throw new EvalError("guard: expected symbol");
                                String varName = varSym.name();
                                var clauses = clauseList.elements().subList(1, clauseList.elements().size());
                                var bodyExprs = elements.subList(2, elements.size());
                                // Evaluate non-last body exprs in try/catch (not in tail position)
                                boolean raised = false;
                                SchemeValue raisedValue = null;
                                try {
                                    for (int i = 0; i < bodyExprs.size() - 1; i++) {
                                        eval(bodyExprs.get(i), env);
                                    }
                                } catch (SchemeException e) {
                                    raised = true;
                                    raisedValue = e.value;
                                }
                                if (raised) {
                                    // Match clauses for non-tail exception
                                    var guardEnv = new Environment(env);
                                    guardEnv.define(varName, raisedValue);
                                    boolean clauseMatched = false;
                                    for (var clause : clauses) {
                                        if (!(clause instanceof SchemeValue.ListVal cl) || cl.elements().isEmpty())
                                            throw new EvalError("guard: bad clause");
                                        SchemeValue test = cl.elements().getFirst();
                                        if (test instanceof SchemeValue.SymbolVal s && s.name().equals("else")) {
                                            for (int j = 1; j < cl.elements().size() - 1; j++) {
                                                eval(cl.elements().get(j), guardEnv);
                                            }
                                            expr = cl.elements().getLast();
                                            env = guardEnv;
                                            clauseMatched = true;
                                            break;
                                        }
                                        SchemeValue testResult = eval(test, guardEnv);
                                        if (testResult.isTruthy()) {
                                            if (cl.elements().size() == 1) return testResult;
                                            for (int j = 1; j < cl.elements().size() - 1; j++) {
                                                eval(cl.elements().get(j), guardEnv);
                                            }
                                            expr = cl.elements().getLast();
                                            env = guardEnv;
                                            clauseMatched = true;
                                            break;
                                        }
                                    }
                                    if (!clauseMatched) throw new SchemeException(raisedValue);
                                    continue;
                                }
                                // Last body expr: TCO with guard context on stack
                                guardStack.push(new GuardContext(varName, clauses, env));
                                expr = bodyExprs.getLast();
                                continue;
                            }
                            case "when" -> {
                                if (elements.size() < 3) throw new EvalError("when: bad syntax");
                                SchemeValue test = eval(elements.get(1), env);
                                if (test.isTruthy()) {
                                    for (int i = 2; i < elements.size() - 1; i++) {
                                        eval(elements.get(i), env);
                                    }
                                    expr = elements.getLast();
                                    continue;
                                }
                                return new SchemeValue.VoidVal();
                            }
                            case "cond" -> {
                                boolean matched = false;
                                for (int i = 1; i < elements.size(); i++) {
                                    if (!(elements.get(i) instanceof SchemeValue.ListVal clause) || clause.elements().isEmpty())
                                        throw new EvalError("cond: bad clause");
                                    SchemeValue test = clause.elements().getFirst();
                                    if (test instanceof SchemeValue.SymbolVal s && s.name().equals("else")) {
                                        for (int j = 1; j < clause.elements().size() - 1; j++) {
                                            eval(clause.elements().get(j), env);
                                        }
                                        expr = clause.elements().getLast();
                                        matched = true;
                                        break;
                                    }
                                    SchemeValue testResult = eval(test, env);
                                    if (testResult.isTruthy()) {
                                        if (clause.elements().size() == 1) return testResult;
                                        for (int j = 1; j < clause.elements().size() - 1; j++) {
                                            eval(clause.elements().get(j), env);
                                        }
                                        expr = clause.elements().getLast();
                                        matched = true;
                                        break;
                                    }
                                }
                                if (matched) continue;
                                return new SchemeValue.BoolVal(false);
                            }
                            case "syntax-case" -> {
                                // (syntax-case stx-expr (literal ...) clause ...)
                                // clause = (pattern body) or (pattern fender body)
                                if (elements.size() < 4) throw new EvalError("syntax-case: bad syntax");
                                SchemeValue stxObj = eval(elements.get(1), env);
                                var literals = new ArrayList<String>();
                                if (elements.get(2) instanceof SchemeValue.ListVal litList) {
                                    for (var lit : litList.elements()) {
                                        if (lit instanceof SchemeValue.SymbolVal ls) literals.add(ls.name());
                                    }
                                }
                                for (int ci = 3; ci < elements.size(); ci++) {
                                    if (!(elements.get(ci) instanceof SchemeValue.ListVal clause))
                                        throw new EvalError("syntax-case: bad clause");
                                    var clauseElems = clause.elements();
                                    if (clauseElems.size() < 2) throw new EvalError("syntax-case: bad clause");
                                    SchemeValue pattern = clauseElems.get(0);
                                    SchemeValue fender = clauseElems.size() == 3 ? clauseElems.get(1) : null;
                                    SchemeValue body = clauseElems.getLast();

                                    var bindings = new MacroExpander.Bindings();
                                    if (MacroExpander.matchPattern(pattern, stxObj, literals, bindings)) {
                                        // Check fender if present
                                        if (fender != null) {
                                            var fenderEnv = new Environment(env);
                                            bindSyntaxVars(fenderEnv, bindings);
                                            SchemeValue fenderResult = eval(fender, fenderEnv);
                                            if (!fenderResult.isTruthy()) continue;
                                        }
                                        // Push syntax bindings and evaluate body
                                        var prevBindings = currentSyntaxBindings;
                                        var prevDefEnv = currentSyntaxDefEnv;
                                        var merged = mergeSyntaxBindings(prevBindings, bindings);
                                        currentSyntaxBindings = merged;
                                        currentSyntaxDefEnv = currentTransformerDefEnv != null ? currentTransformerDefEnv : env;
                                        try {
                                            return eval(body, env);
                                        } finally {
                                            currentSyntaxBindings = prevBindings;
                                            currentSyntaxDefEnv = prevDefEnv;
                                        }
                                    }
                                }
                                throw new EvalError("syntax-case: no matching pattern");
                            }
                            case "syntax-quote" -> {
                                // #'template — expand template with current syntax-case bindings
                                if (elements.size() != 2) throw new EvalError("syntax-quote: expected 1 argument");
                                if (currentSyntaxBindings == null)
                                    throw new EvalError("syntax-quote: not in syntax-case context");
                                int mark = MacroExpander.nextMark();
                                var renames = new java.util.HashMap<String, String>();
                                return MacroExpander.expandTemplate(elements.get(1), currentSyntaxBindings,
                                    currentSyntaxDefEnv != null ? currentSyntaxDefEnv : env, mark, renames,
                                    currentTransformerBoundNames);
                            }
                            case "with-syntax" -> {
                                // (with-syntax ((pat expr) ...) body ...)
                                if (elements.size() < 3) throw new EvalError("with-syntax: bad syntax");
                                if (!(elements.get(1) instanceof SchemeValue.ListVal bindingsList))
                                    throw new EvalError("with-syntax: expected bindings list");
                                var extraBindings = new MacroExpander.Bindings();
                                for (var binding : bindingsList.elements()) {
                                    if (!(binding instanceof SchemeValue.ListVal bl) || bl.elements().size() != 2)
                                        throw new EvalError("with-syntax: bad binding");
                                    SchemeValue pat = bl.elements().get(0);
                                    SchemeValue val = eval(bl.elements().get(1), env);
                                    if (pat instanceof SchemeValue.SymbolVal sv) {
                                        extraBindings.regular.put(sv.name(), val);
                                    }
                                }
                                var prevBindings = currentSyntaxBindings;
                                var prevDefEnv = currentSyntaxDefEnv;
                                currentSyntaxBindings = mergeSyntaxBindings(prevBindings, extraBindings);
                                if (currentSyntaxDefEnv == null) currentSyntaxDefEnv = env;
                                try {
                                    SchemeValue result = null;
                                    for (int i = 2; i < elements.size(); i++) {
                                        result = eval(elements.get(i), env);
                                    }
                                    return result;
                                } finally {
                                    currentSyntaxBindings = prevBindings;
                                    currentSyntaxDefEnv = prevDefEnv;
                                }
                            }
                            default -> {
                                // fall through to procedure call below
                            }
                        }
                    }
                    // Evaluate head and call as procedure
                    SchemeValue proc = eval(head, env);
                    // Macro expansion
                    if (proc instanceof SchemeValue.SyntaxRulesVal macro) {
                        expr = MacroExpander.expand(macro, listVal);
                        continue;
                    }
                    if (proc instanceof SchemeValue.TransformerVal transformer) {
                        // Call the transformer procedure with the syntax object (the original form)
                        var prevTransformerDefEnv = currentTransformerDefEnv;
                        var prevTransformerBoundNames = currentTransformerBoundNames;
                        currentTransformerDefEnv = transformer.defEnv();
                        currentTransformerBoundNames = transformer.defBoundNames();
                        try {
                            expr = callProc(transformer.proc(), List.of(listVal), posPrefix(listVal));
                        } finally {
                            currentTransformerDefEnv = prevTransformerDefEnv;
                            currentTransformerBoundNames = prevTransformerBoundNames;
                        }
                        continue;
                    }
                    var args = new ArrayList<SchemeValue>();
                    for (int i = 1; i < elements.size(); i++) {
                        args.add(eval(elements.get(i), env));
                    }
                    // Handle continuation invocation
                    if (proc instanceof SchemeValue.ContinuationVal cont) {
                        SchemeValue val = args.size() == 1 ? args.getFirst() : new SchemeValue.ValuesVal(new ArrayList<>(args));
                        throw new ContinuationException(cont.id(), val);
                    }
                    // Handle call/cc used as first-class value
                    if (proc instanceof SchemeValue.BuiltinVal bv &&
                            (bv.name().equals("call/cc") || bv.name().equals("call-with-current-continuation"))) {
                        if (args.size() != 1) throw new EvalError(posPrefix(listVal) + "call/cc: expected 1 argument");
                        return handleCallCC(args.getFirst(), listVal);
                    }
                    // TCO for lambda calls
                    if (proc instanceof SchemeValue.LambdaVal lambda) {
                        var localEnv = applyLambda(lambda, args, posPrefix(listVal));
                        for (int i = 0; i < lambda.body().size() - 1; i++) {
                            eval(lambda.body().get(i), localEnv);
                        }
                        expr = lambda.body().getLast();
                        env = localEnv;
                        continue;
                    }
                    if (proc instanceof SchemeValue.BuiltinVal builtin) {
                        try {
                            return builtin.fn().apply(args);
                        } catch (RuntimeException e) {
                            if (e.getCause() instanceof EvalError ee) {
                                String msg = ee.getMessage();
                                if (listVal.pos() != null && !msg.matches(".*\\d+:\\d+.*")) {
                                    throw new EvalError(posPrefix(listVal) + msg);
                                }
                                throw ee;
                            }
                            throw e;
                        }
                    }
                    throw new EvalError(posPrefix(listVal) + "not a procedure: " + proc.display());
                }
            }
        } catch (SchemeException e) {
            if (!guardStack.isEmpty()) {
                var ctx = guardStack.pop();
                var guardEnv = new Environment(ctx.env());
                guardEnv.define(ctx.varName(), e.value);
                boolean clauseMatched = false;
                for (var clause : ctx.clauses()) {
                    if (!(clause instanceof SchemeValue.ListVal cl) || cl.elements().isEmpty())
                        throw new EvalError("guard: bad clause");
                    SchemeValue test = cl.elements().getFirst();
                    if (test instanceof SchemeValue.SymbolVal s && s.name().equals("else")) {
                        for (int j = 1; j < cl.elements().size() - 1; j++) {
                            eval(cl.elements().get(j), guardEnv);
                        }
                        expr = cl.elements().getLast();
                        env = guardEnv;
                        clauseMatched = true;
                        break;
                    }
                    SchemeValue testResult = eval(test, guardEnv);
                    if (testResult.isTruthy()) {
                        if (cl.elements().size() == 1) { expr = test; env = guardEnv; clauseMatched = true; break; }
                        for (int j = 1; j < cl.elements().size() - 1; j++) {
                            eval(cl.elements().get(j), guardEnv);
                        }
                        expr = cl.elements().getLast();
                        env = guardEnv;
                        clauseMatched = true;
                        break;
                    }
                }
                if (!clauseMatched) throw e;
                continue;
            }
            throw e;
        } }
    }


    private SchemeValue evalSyntaxRules(SchemeValue srExpr, Environment env) throws EvalError {
        if (!(srExpr instanceof SchemeValue.ListVal list)) throw new EvalError("syntax-rules: bad syntax");
        var elems = list.elements();
        if (elems.isEmpty() || !(elems.getFirst() instanceof SchemeValue.SymbolVal s) || !s.name().equals("syntax-rules"))
            throw new EvalError("syntax-rules: bad syntax");
        if (elems.size() < 2) throw new EvalError("syntax-rules: bad syntax");
        var literals = new ArrayList<String>();
        if (elems.get(1) instanceof SchemeValue.ListVal litList) {
            for (var lit : litList.elements()) {
                if (lit instanceof SchemeValue.SymbolVal ls) literals.add(ls.name());
            }
        }
        var patterns = new ArrayList<SchemeValue>();
        var templates = new ArrayList<SchemeValue>();
        for (int i = 2; i < elems.size(); i++) {
            if (!(elems.get(i) instanceof SchemeValue.ListVal rule) || rule.elements().size() != 2)
                throw new EvalError("syntax-rules: bad rule");
            patterns.add(rule.elements().get(0));
            templates.add(rule.elements().get(1));
        }
        return new SchemeValue.SyntaxRulesVal(literals, patterns, templates, env);
    }

    private static SchemeValue syntaxToDatum(SchemeValue stx) {
        // syntax->datum strips syntax information, returning bare data
        // In our representation, syntax objects ARE data, so this is identity
        // except for symbols which may have gensym marks — we return as-is
        return stx;
    }

    private SchemeValue callProc(SchemeValue proc, List<SchemeValue> args, String posPrefix) throws EvalError {
        if (proc instanceof SchemeValue.LambdaVal lambda) {
            var localEnv = applyLambda(lambda, args, posPrefix);
            SchemeValue result = null;
            for (var bodyExpr : lambda.body()) {
                result = eval(bodyExpr, localEnv);
            }
            return result;
        }
        if (proc instanceof SchemeValue.BuiltinVal builtin) {
            return builtin.fn().apply(args);
        }
        throw new EvalError("not a procedure: " + proc.display());
    }

    private void bindSyntaxVars(Environment env, MacroExpander.Bindings bindings) {
        for (var entry : bindings.regular.entrySet()) {
            env.define(entry.getKey(), entry.getValue());
        }
    }

    private MacroExpander.Bindings mergeSyntaxBindings(MacroExpander.Bindings prev, MacroExpander.Bindings next) {
        var merged = new MacroExpander.Bindings();
        if (prev != null) {
            merged.regular.putAll(prev.regular);
            merged.ellipsis.putAll(prev.ellipsis);
        }
        merged.regular.putAll(next.regular);
        merged.ellipsis.putAll(next.ellipsis);
        return merged;
    }

    private SchemeValue evalDefine(SchemeValue.ListVal listVal, List<SchemeValue> elements, Environment env) throws EvalError {
        if (elements.size() < 3) throw new EvalError(posPrefix(listVal) + "define: bad syntax");
        SchemeValue target = elements.get(1);
        if (target instanceof SchemeValue.SymbolVal sym) {
            SchemeValue val = eval(elements.get(2), env);
            env.define(sym.name(), val);
            return new SchemeValue.VoidVal();
        } else if (target instanceof SchemeValue.ListVal nameAndParams) {
            if (nameAndParams.elements().isEmpty()) throw new EvalError(posPrefix(listVal) + "define: bad syntax");
            SchemeValue nameVal = nameAndParams.elements().getFirst();
            if (!(nameVal instanceof SchemeValue.SymbolVal nameSym)) {
                throw new EvalError(posPrefix(listVal) + "define: expected symbol as function name");
            }
            var params = new ArrayList<String>();
            String restParam = null;
            var pElems = nameAndParams.elements();
            for (int i = 1; i < pElems.size(); i++) {
                if (pElems.get(i) instanceof SchemeValue.SymbolVal s && s.name().equals(".")) {
                    if (i + 1 >= pElems.size()) throw new EvalError(posPrefix(listVal) + "define: bad syntax");
                    if (!(pElems.get(i + 1) instanceof SchemeValue.SymbolVal rp))
                        throw new EvalError(posPrefix(listVal) + "define: expected symbol after dot");
                    restParam = rp.name();
                    break;
                }
                if (!(pElems.get(i) instanceof SchemeValue.SymbolVal p)) {
                    throw new EvalError(posPrefix(listVal) + "define: expected symbol as parameter");
                }
                params.add(p.name());
            }
            var body = elements.subList(2, elements.size());
            var lambda = new SchemeValue.LambdaVal(params, restParam, body, env);
            env.define(nameSym.name(), lambda);
            return new SchemeValue.VoidVal();
        }
        throw new EvalError(posPrefix(listVal) + "define: bad syntax");
    }


    private SchemeValue evalDefineRecordType(List<SchemeValue> elements, Environment env) throws EvalError {
        // (define-record-type <name> (constructor field ...) predicate (field accessor) ...)
        if (elements.size() < 4) throw new EvalError("define-record-type: bad syntax");
        // Type name
        if (!(elements.get(1) instanceof SchemeValue.SymbolVal typeSym))
            throw new EvalError("define-record-type: expected type name symbol");
        String typeName = typeSym.name();
        // Constructor: (make-xxx field1 field2 ...)
        if (!(elements.get(2) instanceof SchemeValue.ListVal ctorList) || ctorList.elements().isEmpty())
            throw new EvalError("define-record-type: expected constructor spec");
        if (!(ctorList.elements().getFirst() instanceof SchemeValue.SymbolVal ctorSym))
            throw new EvalError("define-record-type: expected constructor name");
        String ctorName = ctorSym.name();
        var ctorFields = new ArrayList<String>();
        for (int i = 1; i < ctorList.elements().size(); i++) {
            if (!(ctorList.elements().get(i) instanceof SchemeValue.SymbolVal f))
                throw new EvalError("define-record-type: expected field name in constructor");
            ctorFields.add(f.name());
        }
        // Predicate
        if (!(elements.get(3) instanceof SchemeValue.SymbolVal predSym))
            throw new EvalError("define-record-type: expected predicate name");
        String predName = predSym.name();
        // Field specs: (field accessor) ...
        var allFieldNames = new ArrayList<String>();
        var accessorMap = new java.util.LinkedHashMap<String, String>(); // field -> accessor name
        for (int i = 4; i < elements.size(); i++) {
            if (!(elements.get(i) instanceof SchemeValue.ListVal fspec) || fspec.elements().size() < 2)
                throw new EvalError("define-record-type: bad field spec");
            if (!(fspec.elements().get(0) instanceof SchemeValue.SymbolVal fName))
                throw new EvalError("define-record-type: expected field name");
            if (!(fspec.elements().get(1) instanceof SchemeValue.SymbolVal accName))
                throw new EvalError("define-record-type: expected accessor name");
            allFieldNames.add(fName.name());
            accessorMap.put(fName.name(), accName.name());
        }
        // Build field name array (ordered by field spec order)
        String[] fieldNames = allFieldNames.toArray(new String[0]);
        // Map constructor arg positions to field indices
        int[] ctorFieldIndices = new int[ctorFields.size()];
        for (int i = 0; i < ctorFields.size(); i++) {
            int idx = allFieldNames.indexOf(ctorFields.get(i));
            if (idx < 0) throw new EvalError("define-record-type: constructor field " + ctorFields.get(i) + " not in field list");
            ctorFieldIndices[i] = idx;
        }
        // Define constructor
        final String tn = typeName;
        final String[] fn = fieldNames;
        final int[] cfi = ctorFieldIndices;
        env.define(ctorName, new SchemeValue.BuiltinVal(ctorName, args -> {
            if (args.size() != cfi.length) throw new RuntimeException(ctorName + ": expected " + cfi.length + " arguments, got " + args.size());
            SchemeValue[] fields = new SchemeValue[fn.length];
            for (int i = 0; i < cfi.length; i++) {
                fields[cfi[i]] = args.get(i);
            }
            return new SchemeValue.RecordVal(tn, fn, fields);
        }));
        // Define predicate
        env.define(predName, new SchemeValue.BuiltinVal(predName, args -> {
            if (args.size() != 1) throw new RuntimeException(predName + ": expected 1 argument");
            return new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.RecordVal r && r.typeName.equals(tn));
        }));
        // Define accessors
        for (int i = 0; i < fieldNames.length; i++) {
            String accName = accessorMap.get(fieldNames[i]);
            final int fi = i;
            env.define(accName, new SchemeValue.BuiltinVal(accName, args -> {
                if (args.size() != 1) throw new RuntimeException(accName + ": expected 1 argument");
                if (!(args.getFirst() instanceof SchemeValue.RecordVal r) || !r.typeName.equals(tn))
                    throw new RuntimeException(accName + ": expected " + tn + " record");
                return r.fields[fi];
            }));
        }
        return new SchemeValue.VoidVal();
    }

    private SchemeValue evalLambda(SchemeValue.ListVal listVal, List<SchemeValue> elements, Environment env) throws EvalError {
        if (elements.size() < 3) throw new EvalError(posPrefix(listVal) + "lambda: bad syntax");
        SchemeValue paramList = elements.get(1);
        // Single symbol = all-rest parameter: (lambda args ...)
        if (paramList instanceof SchemeValue.SymbolVal restSym) {
            var body = elements.subList(2, elements.size());
            return new SchemeValue.LambdaVal(List.of(), restSym.name(), body, env);
        }
        if (!(paramList instanceof SchemeValue.ListVal pList)) {
            throw new EvalError(posPrefix(listVal) + "lambda: expected parameter list");
        }
        var params = new ArrayList<String>();
        String restParam = null;
        for (int i = 0; i < pList.elements().size(); i++) {
            var p = pList.elements().get(i);
            if (p instanceof SchemeValue.SymbolVal s && s.name().equals(".")) {
                if (i + 1 >= pList.elements().size()) throw new EvalError(posPrefix(listVal) + "lambda: bad syntax");
                if (!(pList.elements().get(i + 1) instanceof SchemeValue.SymbolVal rp))
                    throw new EvalError(posPrefix(listVal) + "lambda: expected symbol after dot");
                restParam = rp.name();
                break;
            }
            if (!(p instanceof SchemeValue.SymbolVal sym)) {
                throw new EvalError(posPrefix(listVal) + "lambda: expected symbol as parameter");
            }
            params.add(sym.name());
        }
        var body = elements.subList(2, elements.size());
        return new SchemeValue.LambdaVal(params, restParam, body, env);
    }


    private Environment applyLambda(SchemeValue.LambdaVal lambda, List<SchemeValue> args, String errPrefix) throws EvalError {
        int nFixed = lambda.params().size();
        if (lambda.restParam() != null) {
            if (args.size() < nFixed) {
                throw new EvalError(errPrefix + "expected at least " + nFixed + " arguments, got " + args.size());
            }
        } else {
            if (args.size() != nFixed) {
                throw new EvalError(errPrefix + "expected " + nFixed + " arguments, got " + args.size());
            }
        }
        var localEnv = new Environment(lambda.env());
        for (int i = 0; i < nFixed; i++) {
            localEnv.define(lambda.params().get(i), args.get(i));
        }
        if (lambda.restParam() != null) {
            localEnv.define(lambda.restParam(), schemeList(args.subList(nFixed, args.size())));
        }
        return localEnv;
    }

    private SchemeValue schemeList(List<SchemeValue> elems) {
        SchemeValue result = new SchemeValue.ListVal(List.of());
        for (int i = elems.size() - 1; i >= 0; i--) {
            result = new SchemeValue.PairVal(elems.get(i), result);
        }
        return result;
    }

    private List<SchemeValue> toJavaList(SchemeValue v) throws EvalError {
        var result = new ArrayList<SchemeValue>();
        while (true) {
            if (v instanceof SchemeValue.ListVal l) {
                result.addAll(l.elements());
                return result;
            } else if (v instanceof SchemeValue.PairVal p) {
                result.add(p.car());
                v = p.cdr();
            } else {
                throw new EvalError("apply: expected proper list");
            }
        }
    }

    // --- Number helpers ---
    private boolean isNumber(SchemeValue v) {
        return v instanceof SchemeValue.IntVal || v instanceof SchemeValue.DoubleVal || v instanceof SchemeValue.RationalVal;
    }

    private boolean isExact(SchemeValue v) {
        return v instanceof SchemeValue.IntVal || v instanceof SchemeValue.RationalVal;
    }

    private boolean hasInexact(List<SchemeValue> args) {
        for (var a : args) if (a instanceof SchemeValue.DoubleVal) return true;
        return false;
    }

    private double toDouble(SchemeValue v) throws EvalError {
        if (v instanceof SchemeValue.IntVal iv) return (double) iv.value();
        if (v instanceof SchemeValue.DoubleVal dv) return dv.value();
        if (v instanceof SchemeValue.RationalVal rv) return (double) rv.num() / rv.den();
        throw new EvalError("expected number, got: " + v.display());
    }

    // Return as [num, den] for exact values
    private long[] toRational(SchemeValue v) throws EvalError {
        if (v instanceof SchemeValue.IntVal iv) return new long[]{iv.value(), 1};
        if (v instanceof SchemeValue.RationalVal rv) return new long[]{rv.num(), rv.den()};
        throw new EvalError("expected exact number, got: " + v.display());
    }

    private SchemeValue makeExact(long num, long den) {
        if (den == 0) throw new ArithmeticException("division by zero");
        var r = new SchemeValue.RationalVal(num, den);
        return r.simplify();
    }

    private SchemeValue arithPlus(List<SchemeValue> args) throws EvalError {
        if (hasInexact(args)) {
            double result = 0;
            for (var arg : args) result += toDouble(arg);
            return new SchemeValue.DoubleVal(result);
        }
        long num = 0, den = 1;
        for (var arg : args) {
            long[] r = toRational(arg);
            num = num * r[1] + r[0] * den;
            den = den * r[1];
        }
        return makeExact(num, den);
    }

    private SchemeValue arithMinus(List<SchemeValue> args) throws EvalError {
        if (args.isEmpty()) throw new EvalError("-: expected at least 1 argument");
        if (hasInexact(args)) {
            if (args.size() == 1) return new SchemeValue.DoubleVal(-toDouble(args.getFirst()));
            double result = toDouble(args.getFirst());
            for (int i = 1; i < args.size(); i++) result -= toDouble(args.get(i));
            return new SchemeValue.DoubleVal(result);
        }
        if (args.size() == 1) {
            long[] r = toRational(args.getFirst());
            return makeExact(-r[0], r[1]);
        }
        long[] acc = toRational(args.getFirst());
        long num = acc[0], den = acc[1];
        for (int i = 1; i < args.size(); i++) {
            long[] r = toRational(args.get(i));
            num = num * r[1] - r[0] * den;
            den = den * r[1];
        }
        return makeExact(num, den);
    }

    private SchemeValue arithMul(List<SchemeValue> args) throws EvalError {
        if (hasInexact(args)) {
            double result = 1;
            for (var arg : args) result *= toDouble(arg);
            return new SchemeValue.DoubleVal(result);
        }
        long num = 1, den = 1;
        for (var arg : args) {
            long[] r = toRational(arg);
            num *= r[0];
            den *= r[1];
        }
        return makeExact(num, den);
    }

    private SchemeValue arithDiv(List<SchemeValue> args) throws EvalError {
        if (args.isEmpty()) throw new EvalError("/: expected at least 1 argument");
        if (hasInexact(args)) {
            if (args.size() == 1) {
                double d = toDouble(args.getFirst());
                if (d == 0) throw new EvalError("division by zero");
                return new SchemeValue.DoubleVal(1.0 / d);
            }
            double result = toDouble(args.getFirst());
            for (int i = 1; i < args.size(); i++) {
                double d = toDouble(args.get(i));
                if (d == 0) throw new EvalError("division by zero");
                result /= d;
            }
            return new SchemeValue.DoubleVal(result);
        }
        long[] acc = toRational(args.getFirst());
        long num = acc[0], den = acc[1];
        if (args.size() == 1) {
            if (num == 0) throw new EvalError("division by zero");
            return makeExact(den, num);
        }
        for (int i = 1; i < args.size(); i++) {
            long[] r = toRational(args.get(i));
            if (r[0] == 0) throw new EvalError("division by zero");
            num *= r[1];
            den *= r[0];
        }
        return makeExact(num, den);
    }

    @FunctionalInterface
    interface DoubleBiPredicate {
        boolean test(double a, double b);
    }

    private SchemeValue compare(List<SchemeValue> args, DoubleBiPredicate pred) throws EvalError {
        if (args.size() < 2) throw new EvalError("comparison: expected at least 2 arguments");
        for (int i = 0; i < args.size() - 1; i++) {
            if (!pred.test(toDouble(args.get(i)), toDouble(args.get(i + 1)))) {
                return new SchemeValue.BoolVal(false);
            }
        }
        return new SchemeValue.BoolVal(true);
    }

    private static long gcd(long a, long b) {
        while (b != 0) { long t = b; b = a % b; a = t; }
        return a;
    }

    private long requireInt(SchemeValue v) throws EvalError {
        if (v instanceof SchemeValue.IntVal iv) return iv.value();
        if (v instanceof SchemeValue.RationalVal rv && rv.isInteger()) return rv.num();
        if (v instanceof SchemeValue.DoubleVal dv) {
            double d = dv.value();
            if (d == Math.floor(d) && !Double.isInfinite(d)) return (long) d;
        }
        throw new EvalError("expected integer, got: " + v.display());
    }

    private SchemeValue schemeCar(SchemeValue v) throws EvalError {
        if (v instanceof SchemeValue.PairVal p) return p.car();
        if (v instanceof SchemeValue.ListVal l && !l.elements().isEmpty()) return l.elements().getFirst();
        throw new EvalError("car: expected pair, got: " + v.display());
    }

    private SchemeValue schemeCdr(SchemeValue v) throws EvalError {
        if (v instanceof SchemeValue.PairVal p) return p.cdr();
        if (v instanceof SchemeValue.ListVal l && !l.elements().isEmpty())
            return new SchemeValue.ListVal(l.elements().subList(1, l.elements().size()));
        throw new EvalError("cdr: expected pair, got: " + v.display());
    }

    private boolean schemeEq(SchemeValue a, SchemeValue b) {
        if (a instanceof SchemeValue.BoolVal ba && b instanceof SchemeValue.BoolVal bb) return ba.value() == bb.value();
        if (a instanceof SchemeValue.SymbolVal sa && b instanceof SchemeValue.SymbolVal sb) return sa.name().equals(sb.name());
        if (a instanceof SchemeValue.IntVal ia && b instanceof SchemeValue.IntVal ib) return ia.value() == ib.value();
        if (a instanceof SchemeValue.CharVal ca && b instanceof SchemeValue.CharVal cb) return ca.value() == cb.value();
        if (a instanceof SchemeValue.ListVal la && la.elements().isEmpty() &&
            b instanceof SchemeValue.ListVal lb && lb.elements().isEmpty()) return true;
        return a == b;
    }

    private boolean schemeEqv(SchemeValue a, SchemeValue b) {
        if (a instanceof SchemeValue.IntVal ia && b instanceof SchemeValue.IntVal ib) return ia.value() == ib.value();
        if (a instanceof SchemeValue.DoubleVal da && b instanceof SchemeValue.DoubleVal db) return da.value() == db.value();
        if (a instanceof SchemeValue.RationalVal ra && b instanceof SchemeValue.RationalVal rb) return ra.num() == rb.num() && ra.den() == rb.den();
        if (a instanceof SchemeValue.BoolVal ba && b instanceof SchemeValue.BoolVal bb) return ba.value() == bb.value();
        if (a instanceof SchemeValue.SymbolVal sa && b instanceof SchemeValue.SymbolVal sb) return sa.name().equals(sb.name());
        if (a instanceof SchemeValue.CharVal ca && b instanceof SchemeValue.CharVal cb) return ca.value() == cb.value();
        if (a instanceof SchemeValue.ListVal la && la.elements().isEmpty() &&
            b instanceof SchemeValue.ListVal lb && lb.elements().isEmpty()) return true;
        return a == b;
    }

    private boolean schemeEqual(SchemeValue a, SchemeValue b) {
        return schemeEqualRec(a, b, java.util.Collections.newSetFromMap(new IdentityHashMap<>()));
    }

    private boolean schemeEqualRec(SchemeValue a, SchemeValue b, java.util.Set<Long> seen) {
        if (a == b) return true;
        if (isNumber(a) && isNumber(b)) {
            try { return toDouble(a) == toDouble(b); } catch (EvalError e) { return false; }
        }
        if (a instanceof SchemeValue.BoolVal ba && b instanceof SchemeValue.BoolVal bb) return ba.value() == bb.value();
        if (a instanceof SchemeValue.SymbolVal sa && b instanceof SchemeValue.SymbolVal sb) return sa.name().equals(sb.name());
        if (a instanceof SchemeValue.CharVal ca && b instanceof SchemeValue.CharVal cb) return ca.value() == cb.value();
        if ((a instanceof SchemeValue.StringVal || a instanceof SchemeValue.MutableStringVal) &&
            (b instanceof SchemeValue.StringVal || b instanceof SchemeValue.MutableStringVal)) {
            String sa = a instanceof SchemeValue.StringVal s ? s.value() : ((SchemeValue.MutableStringVal)a).value();
            String sb = b instanceof SchemeValue.StringVal s ? s.value() : ((SchemeValue.MutableStringVal)b).value();
            return sa.equals(sb);
        }
        // Both empty lists
        if (a instanceof SchemeValue.ListVal la && la.elements().isEmpty() &&
            b instanceof SchemeValue.ListVal lb && lb.elements().isEmpty()) return true;
        // Pair-like comparisons
        SchemeValue aCar = null, aCdr = null, bCar = null, bCdr = null;
        if (a instanceof SchemeValue.PairVal pa) { aCar = pa.car(); aCdr = pa.cdr(); }
        else if (a instanceof SchemeValue.ListVal la && !la.elements().isEmpty()) {
            aCar = la.elements().getFirst();
            aCdr = la.elements().size() == 1 ? new SchemeValue.ListVal(List.of()) :
                new SchemeValue.ListVal(la.elements().subList(1, la.elements().size()));
        }
        if (b instanceof SchemeValue.PairVal pb) { bCar = pb.car(); bCdr = pb.cdr(); }
        else if (b instanceof SchemeValue.ListVal lb && !lb.elements().isEmpty()) {
            bCar = lb.elements().getFirst();
            bCdr = lb.elements().size() == 1 ? new SchemeValue.ListVal(List.of()) :
                new SchemeValue.ListVal(lb.elements().subList(1, lb.elements().size()));
        }
        if (aCar != null && bCar != null) {
            // Cycle detection using identity pair encoding
            long key = ((long) System.identityHashCode(a) << 32) | (System.identityHashCode(b) & 0xFFFFFFFFL);
            if (!seen.add(key)) return true; // already comparing these — assume equal
            return schemeEqualRec(aCar, bCar, seen) && schemeEqualRec(aCdr, bCdr, seen);
        }
        if (a instanceof SchemeValue.VectorVal va && b instanceof SchemeValue.VectorVal vb) {
            if (va.elements().length != vb.elements().length) return false;
            for (int i = 0; i < va.elements().length; i++) {
                if (!schemeEqualRec(va.elements()[i], vb.elements()[i], seen)) return false;
            }
            return true;
        }
        return a == b;
    }

    private boolean isProperList(SchemeValue v) {
        // Tortoise-and-hare cycle detection
        SchemeValue slow = v, fast = v;
        while (true) {
            if (slow instanceof SchemeValue.ListVal) return true;
            if (!(slow instanceof SchemeValue.PairVal ps)) return false;
            slow = ps.cdr();
            // Advance fast twice
            for (int i = 0; i < 2; i++) {
                if (fast instanceof SchemeValue.ListVal) return true;
                if (!(fast instanceof SchemeValue.PairVal pf)) return false;
                fast = pf.cdr();
            }
            if (slow == fast) return false; // cycle detected
        }
    }

    private String requireString(SchemeValue v, String caller) throws EvalError {
        if (v instanceof SchemeValue.StringVal s) return s.value();
        if (v instanceof SchemeValue.MutableStringVal s) return s.value();
        throw new EvalError(caller + ": expected string");
    }

    private SchemeValue handleCallCC(SchemeValue proc, SchemeValue callccExpr) throws EvalError {
        int contId = nextContId++;
        var contVal = new SchemeValue.ContinuationVal(contId);

        // Store continuation data — capture outermost let body context
        continuationData.put(contId, new ContData(callccExpr, outerLetBody, outerLetBodyIndex, outerLetEnv, topLevelIndex));

        // Call the thunk with the continuation
        try {
            if (proc instanceof SchemeValue.LambdaVal lambda) {
                var localEnv = applyLambda(lambda, List.of(contVal), "");
                SchemeValue result = null;
                for (var bodyExpr : lambda.body()) {
                    result = eval(bodyExpr, localEnv);
                }
                return result;
            } else if (proc instanceof SchemeValue.BuiltinVal builtin) {
                return builtin.fn().apply(List.of(contVal));
            }
            throw new EvalError("call/cc: expected procedure, got: " + proc.display());
        } catch (ContinuationException e) {
            if (e.contId == contId) {
                return e.value; // escape continuation
            }
            throw e;
        }
    }

    private SchemeValue callThunk(SchemeValue thunk, String context) throws EvalError {
        if (thunk instanceof SchemeValue.LambdaVal lambda) {
            var localEnv = applyLambda(lambda, List.of(), context + ": ");
            SchemeValue result = null;
            for (var bodyExpr : lambda.body()) {
                result = eval(bodyExpr, localEnv);
            }
            return result != null ? result : new SchemeValue.VoidVal();
        } else if (thunk instanceof SchemeValue.BuiltinVal builtin) {
            try {
                return builtin.fn().apply(List.of());
            } catch (RuntimeException e) {
                if (e.getCause() instanceof EvalError ee) throw ee;
                throw e;
            }
        }
        throw new EvalError(context + ": expected procedure");
    }

    /**
     * Restart a let body from a given index (used for reentrant continuations).
     * Called from Evaluator when a ContinuationException targets a let body.
     */
    SchemeValue restartLetBody(List<SchemeValue> body, int startIndex, Environment env) throws EvalError {
        int idx = startIndex;
        while (true) {
            try {
                SchemeValue result = null;
                for (int i = idx; i < body.size(); i++) {
                    outerLetBody = body;
                    outerLetBodyIndex = i;
                    outerLetEnv = env;
                    result = eval(body.get(i), env);
                }
                return result != null ? result : new SchemeValue.VoidVal();
            } catch (ContinuationException e) {
                var cont = continuationData.get(e.contId);
                if (cont != null && cont.letBody == body) {
                    pendingReturns.put(cont.callccExpr, e.value);
                    idx = cont.letBodyIndex;
                } else {
                    throw e;
                }
            }
        }
    }
}
