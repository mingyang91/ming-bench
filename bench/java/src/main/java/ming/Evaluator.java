package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class Evaluator {
    private final Environment globalEnv = new Environment();
    private StringBuilder outputBuffer;
    private int gensymCounter = 0;

    private static final Set<String> SPECIAL_FORMS = Set.of(
        "if", "define", "lambda", "quote", "begin", "let", "set!",
        "cond", "and", "or", "call/cc", "call-with-current-continuation",
        "define-syntax", "else", "syntax-rules"
    );

    // --- CPS + Trampoline infrastructure ---

    sealed interface Bounce {
        record Done(SchemeValue value) implements Bounce {}
        record More(ThunkFn thunk) implements Bounce {}
    }

    @FunctionalInterface
    interface ThunkFn { Bounce run(); }

    @FunctionalInterface
    interface Cont { Bounce apply(SchemeValue value); }

    @FunctionalInterface
    interface ArgsK { Bounce apply(List<SchemeValue> args); }

    private static final class ContinuationReturn extends RuntimeException {
        final Bounce bounce;
        ContinuationReturn(Bounce bounce) {
            super(null, null, true, false);
            this.bounce = bounce;
        }
    }

    private SchemeValue trampoline(Bounce b) {
        while (true) {
            try {
                switch (b) {
                    case Bounce.Done d -> { return d.value(); }
                    case Bounce.More m -> { b = m.thunk().run(); }
                }
            } catch (ContinuationReturn cr) {
                b = cr.bounce;
            }
        }
    }

    // --- Public API ---

    public String evalStr(String input) throws EvalError {
        var parser = new Parser(input);
        List<SchemeValue> exprs = parser.parseAll();
        if (exprs.isEmpty()) throw new EvalError("empty input");
        Bounce b = evalSeq(exprs, globalEnv, v -> new Bounce.Done(v));
        return trampoline(b).display();
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        outputBuffer = new StringBuilder();
        try {
            var parser = new Parser(input);
            List<SchemeValue> exprs = parser.parseAll();
            if (exprs.isEmpty()) throw new EvalError("empty input");
            Bounce b = evalSeq(exprs, globalEnv, v -> new Bounce.Done(v));
            SchemeValue result = trampoline(b);
            String output = outputBuffer.toString();
            outputBuffer = null;
            return new EvalResult(result.display(), output);
        } catch (RuntimeException e) {
            outputBuffer = null;
            throw e;
        }
    }

    // --- Core eval (CPS) ---

    private Bounce eval(SchemeValue expr, Environment env, Cont k) {
        return switch (expr) {
            case SchemeValue.IntVal v -> k.apply(v);
            case SchemeValue.BoolVal v -> k.apply(v);
            case SchemeValue.StringVal v -> k.apply(v);
            case SchemeValue.VoidVal v -> k.apply(v);
            case SchemeValue.NilVal v -> k.apply(v);
            case SchemeValue.PairVal v -> k.apply(v);
            case SchemeValue.CharVal v -> k.apply(v);
            case SchemeValue.LambdaVal v -> k.apply(v);
            case SchemeValue.ContinuationVal v -> k.apply(v);
            case SchemeValue.MacroVal v -> k.apply(v);
            case SchemeValue.SymbolVal v -> {
                try {
                    yield k.apply(env.get(v.name()));
                } catch (EvalError e) {
                    if (isBuiltin(v.name())) yield k.apply(new SchemeValue.SymbolVal(v.name()));
                    throw withPos(e, v.line(), v.col());
                }
            }
            case SchemeValue.ListVal list -> evalList(list, env, k);
        };
    }

    private Bounce evalList(SchemeValue.ListVal list, Environment env, Cont k) {
        if (list.elements().isEmpty())
            throw withPos(new EvalError("empty application"), list.line(), list.col());
        var first = list.elements().getFirst();
        var args = list.elements().subList(1, list.elements().size());

        if (first instanceof SchemeValue.SymbolVal sym) {
            try {
                return switch (sym.name()) {
                    case "if" -> evalIf(args, env, k);
                    case "define" -> evalDefine(args, env, k);
                    case "lambda" -> k.apply(makeLambda(args, env));
                    case "quote" -> {
                        if (args.size() != 1) throw new EvalError("quote: needs exactly 1 argument");
                        yield k.apply(quoteDatum(args.getFirst()));
                    }
                    case "begin" -> {
                        if (args.isEmpty()) yield k.apply(new SchemeValue.VoidVal());
                        yield evalSeq(args, env, k);
                    }
                    case "let" -> evalLet(args, env, k);
                    case "set!" -> evalSet(args, env, k);
                    case "cond" -> evalCond(args, env, k);
                    case "and" -> evalAnd(args, 0, env, k);
                    case "or" -> evalOr(args, 0, env, k);
                    case "call/cc", "call-with-current-continuation" -> evalCallCC(args, env, k);
                    case "define-syntax" -> evalDefineSyntax(args, env, k);
                    default -> {
                        // Check if this is a macro call
                        SchemeValue macroVal = null;
                        try { macroVal = env.get(sym.name()); } catch (EvalError ignored) {}
                        if (macroVal instanceof SchemeValue.MacroVal macro) {
                            yield expandAndEval(macro, list, env, k);
                        }
                        yield evalCall(first, args, env, k, list.line(), list.col());
                    }
                };
            } catch (EvalError e) {
                throw withPos(e, list.line(), list.col());
            }
        }
        return evalCall(first, args, env, k, list.line(), list.col());
    }

    // --- Sequence & args ---

    private Bounce evalSeq(List<SchemeValue> exprs, Environment env, Cont k) {
        if (exprs.isEmpty()) return k.apply(new SchemeValue.VoidVal());
        if (exprs.size() == 1) return eval(exprs.getFirst(), env, k);
        return new Bounce.More(() -> eval(exprs.getFirst(), env,
            _v -> evalSeq(exprs.subList(1, exprs.size()), env, k)));
    }

    private Bounce evalArgsList(List<SchemeValue> exprs, Environment env, ArgsK then) {
        if (exprs.isEmpty()) return then.apply(List.of());
        return evalArgsRTL(exprs, env, exprs.size() - 1, List.of(), then);
    }

    private Bounce evalArgsRTL(List<SchemeValue> exprs, Environment env, int i,
                                List<SchemeValue> acc, ArgsK then) {
        if (i < 0) return then.apply(acc);
        return new Bounce.More(() -> eval(exprs.get(i), env, v -> {
            var next = new ArrayList<SchemeValue>(acc.size() + 1);
            next.add(v);
            next.addAll(acc);
            return evalArgsRTL(exprs, env, i - 1, next, then);
        }));
    }

    // --- Special forms ---

    private Bounce evalIf(List<SchemeValue> args, Environment env, Cont k) {
        if (args.size() < 2 || args.size() > 3) throw new EvalError("if: needs 2 or 3 arguments");
        return new Bounce.More(() -> eval(args.get(0), env, cond -> {
            if (cond.isTruthy()) return new Bounce.More(() -> eval(args.get(1), env, k));
            if (args.size() == 3) return new Bounce.More(() -> eval(args.get(2), env, k));
            return k.apply(new SchemeValue.VoidVal());
        }));
    }

    private Bounce evalDefine(List<SchemeValue> args, Environment env, Cont k) {
        if (args.size() < 2) throw new EvalError("define: needs at least 2 arguments");
        var target = args.getFirst();
        if (target instanceof SchemeValue.SymbolVal sym) {
            return new Bounce.More(() -> eval(args.get(1), env, value -> {
                env.define(sym.name(), value);
                return k.apply(new SchemeValue.VoidVal());
            }));
        }
        if (target instanceof SchemeValue.ListVal nameAndParams) {
            if (nameAndParams.elements().isEmpty()) throw new EvalError("define: empty name list");
            var nameVal = nameAndParams.elements().getFirst();
            if (!(nameVal instanceof SchemeValue.SymbolVal nameSym))
                throw new EvalError("define: name must be a symbol");
            List<String> params = new ArrayList<>();
            String restParam = null;
            var elems = nameAndParams.elements();
            for (int i = 1; i < elems.size(); i++) {
                if (elems.get(i) instanceof SchemeValue.SymbolVal p && p.name().equals(".")) {
                    if (i + 1 >= elems.size() || i + 2 < elems.size())
                        throw new EvalError("define: invalid dot syntax");
                    if (!(elems.get(i + 1) instanceof SchemeValue.SymbolVal restSym))
                        throw new EvalError("define: rest parameter must be a symbol");
                    restParam = restSym.name();
                    break;
                }
                if (!(elems.get(i) instanceof SchemeValue.SymbolVal p))
                    throw new EvalError("define: parameter must be a symbol");
                params.add(p.name());
            }
            var lambda = new SchemeValue.LambdaVal(params, restParam, args.subList(1, args.size()), env);
            env.define(nameSym.name(), lambda);
            return k.apply(new SchemeValue.VoidVal());
        }
        throw new EvalError("define: invalid syntax");
    }

    private SchemeValue makeLambda(List<SchemeValue> args, Environment env) {
        if (args.size() < 2) throw new EvalError("lambda: needs params and body");
        var paramList = args.getFirst();
        if (!(paramList instanceof SchemeValue.ListVal plist))
            throw new EvalError("lambda: params must be a list");
        List<String> params = new ArrayList<>();
        String restParam = null;
        var elems = plist.elements();
        for (int i = 0; i < elems.size(); i++) {
            if (elems.get(i) instanceof SchemeValue.SymbolVal sym && sym.name().equals(".")) {
                if (i + 1 >= elems.size() || i + 2 < elems.size())
                    throw new EvalError("lambda: invalid dot syntax");
                if (!(elems.get(i + 1) instanceof SchemeValue.SymbolVal restSym))
                    throw new EvalError("lambda: rest parameter must be a symbol");
                restParam = restSym.name();
                break;
            }
            if (!(elems.get(i) instanceof SchemeValue.SymbolVal sym))
                throw new EvalError("lambda: parameter must be a symbol");
            params.add(sym.name());
        }
        return new SchemeValue.LambdaVal(params, restParam, args.subList(1, args.size()), env);
    }

    private Bounce evalSet(List<SchemeValue> args, Environment env, Cont k) {
        if (args.size() != 2) throw new EvalError("set!: needs exactly 2 arguments");
        if (!(args.getFirst() instanceof SchemeValue.SymbolVal sym))
            throw new EvalError("set!: first argument must be a symbol");
        return new Bounce.More(() -> eval(args.get(1), env, value -> {
            env.set(sym.name(), value);
            return k.apply(new SchemeValue.VoidVal());
        }));
    }

    private Bounce evalLet(List<SchemeValue> args, Environment env, Cont k) {
        if (args.size() < 2) throw new EvalError("let: needs bindings and body");

        // Named let
        if (args.getFirst() instanceof SchemeValue.SymbolVal nameSym) {
            if (args.size() < 3) throw new EvalError("let: named let needs bindings and body");
            if (!(args.get(1) instanceof SchemeValue.ListVal bl))
                throw new EvalError("let: bindings must be a list");
            List<String> params = new ArrayList<>();
            List<SchemeValue> initExprs = new ArrayList<>();
            for (var binding : bl.elements()) {
                if (!(binding instanceof SchemeValue.ListVal pair) || pair.elements().size() != 2)
                    throw new EvalError("let: invalid binding");
                if (!(pair.elements().getFirst() instanceof SchemeValue.SymbolVal sym))
                    throw new EvalError("let: binding name must be a symbol");
                params.add(sym.name());
                initExprs.add(pair.elements().get(1));
            }
            List<SchemeValue> body = args.subList(2, args.size());
            Environment letEnv = new Environment(env);
            var lambda = new SchemeValue.LambdaVal(params, body, letEnv);
            letEnv.define(nameSym.name(), lambda);
            return evalArgsList(initExprs, env, vals -> {
                Environment callEnv = new Environment(letEnv);
                for (int i = 0; i < params.size(); i++) callEnv.define(params.get(i), vals.get(i));
                return evalSeq(body, callEnv, k);
            });
        }

        // Regular let
        if (!(args.getFirst() instanceof SchemeValue.ListVal bl))
            throw new EvalError("let: bindings must be a list");
        List<String> names = new ArrayList<>();
        List<SchemeValue> initExprs = new ArrayList<>();
        for (var binding : bl.elements()) {
            if (!(binding instanceof SchemeValue.ListVal pair) || pair.elements().size() != 2)
                throw new EvalError("let: invalid binding");
            if (!(pair.elements().getFirst() instanceof SchemeValue.SymbolVal sym))
                throw new EvalError("let: binding name must be a symbol");
            names.add(sym.name());
            initExprs.add(pair.elements().get(1));
        }
        List<SchemeValue> body = args.subList(1, args.size());
        return evalArgsList(initExprs, env, vals -> {
            Environment letEnv = new Environment(env);
            for (int i = 0; i < names.size(); i++) letEnv.define(names.get(i), vals.get(i));
            return evalSeq(body, letEnv, k);
        });
    }

    private Bounce evalCond(List<SchemeValue> clauses, Environment env, Cont k) {
        if (clauses.isEmpty()) return k.apply(new SchemeValue.VoidVal());
        var clause = clauses.getFirst();
        if (!(clause instanceof SchemeValue.ListVal cl) || cl.elements().isEmpty())
            throw new EvalError("cond: invalid clause");
        var test = cl.elements().getFirst();
        if (test instanceof SchemeValue.SymbolVal sym && sym.name().equals("else")) {
            if (cl.elements().size() == 1) return k.apply(new SchemeValue.VoidVal());
            return evalSeq(cl.elements().subList(1, cl.elements().size()), env, k);
        }
        var rest = clauses.subList(1, clauses.size());
        return new Bounce.More(() -> eval(test, env, testVal -> {
            if (testVal.isTruthy()) {
                if (cl.elements().size() == 1) return k.apply(testVal);
                return evalSeq(cl.elements().subList(1, cl.elements().size()), env, k);
            }
            return evalCond(rest, env, k);
        }));
    }

    private Bounce evalAnd(List<SchemeValue> args, int i, Environment env, Cont k) {
        if (args.isEmpty()) return k.apply(new SchemeValue.BoolVal(true));
        if (i == args.size() - 1) return new Bounce.More(() -> eval(args.get(i), env, k));
        return new Bounce.More(() -> eval(args.get(i), env, v -> {
            if (!v.isTruthy()) return k.apply(v);
            return evalAnd(args, i + 1, env, k);
        }));
    }

    private Bounce evalOr(List<SchemeValue> args, int i, Environment env, Cont k) {
        if (args.isEmpty()) return k.apply(new SchemeValue.BoolVal(false));
        if (i == args.size() - 1) return new Bounce.More(() -> eval(args.get(i), env, k));
        return new Bounce.More(() -> eval(args.get(i), env, v -> {
            if (v.isTruthy()) return k.apply(v);
            return evalOr(args, i + 1, env, k);
        }));
    }

    private Bounce evalCallCC(List<SchemeValue> args, Environment env, Cont k) {
        if (args.size() != 1) throw new EvalError("call/cc: needs exactly 1 argument");
        return new Bounce.More(() -> eval(args.get(0), env, proc -> {
            var contVal = new SchemeValue.ContinuationVal(k);
            return applyProc(proc, List.of(contVal), k);
        }));
    }

    // --- Procedure call ---

    private Bounce evalCall(SchemeValue first, List<SchemeValue> argExprs,
                            Environment env, Cont k, int line, int col) {
        Cont withProc = proc -> evalArgsList(argExprs, env, eArgs -> {
            try {
                return applyProc(proc, eArgs, k);
            } catch (EvalError e) {
                throw withPos(e, line, col);
            }
        });

        if (first instanceof SchemeValue.SymbolVal sym) {
            try {
                return withProc.apply(env.get(sym.name()));
            } catch (EvalError e) {
                if (isBuiltin(sym.name())) return withProc.apply(new SchemeValue.SymbolVal(sym.name()));
                throw withPos(e, line, col);
            }
        }
        return new Bounce.More(() -> eval(first, env, withProc));
    }

    private Bounce applyProc(SchemeValue proc, List<SchemeValue> args, Cont k) {
        if (proc instanceof SchemeValue.ContinuationVal cont) {
            if (args.size() != 1) throw new EvalError("continuation: needs exactly 1 argument");
            throw new ContinuationReturn(((Cont) cont.cont()).apply(args.getFirst()));
        }
        if (proc instanceof SchemeValue.LambdaVal lambda) {
            return applyLambda(lambda, args, k);
        }
        if (proc instanceof SchemeValue.SymbolVal sym && isBuiltin(sym.name())) {
            return applyBuiltin(sym.name(), args, k);
        }
        throw new EvalError("not a procedure: " + proc.display());
    }

    private Bounce applyLambda(SchemeValue.LambdaVal lambda, List<SchemeValue> args, Cont k) {
        int nParams = lambda.params().size();
        boolean hasRest = lambda.restParam() != null;
        if (hasRest) {
            if (args.size() < nParams)
                throw new EvalError("wrong number of arguments: expected at least " + nParams + ", got " + args.size());
        } else {
            if (args.size() != nParams)
                throw new EvalError("wrong number of arguments: expected " + nParams + ", got " + args.size());
        }
        Environment callEnv = new Environment(lambda.env());
        for (int i = 0; i < nParams; i++) {
            callEnv.define(lambda.params().get(i), args.get(i));
        }
        if (hasRest) {
            SchemeValue restList = new SchemeValue.NilVal();
            for (int i = args.size() - 1; i >= nParams; i--) {
                restList = new SchemeValue.PairVal(args.get(i), restList);
            }
            callEnv.define(lambda.restParam(), restList);
        }
        return evalSeq(lambda.body(), callEnv, k);
    }

    // --- Builtins ---

    private static final java.util.Set<String> BUILTINS = java.util.Set.of(
        "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
        "cons", "car", "cdr", "null?", "list", "length", "append",
        "string?", "number?", "boolean?", "pair?", "symbol?", "char?",
        "zero?", "positive?", "negative?", "abs", "min", "max",
        "equal?", "eq?", "modulo", "remainder", "even?", "odd?",
        "display", "write", "newline",
        "string-append", "string-length", "substring",
        "string->number", "number->string",
        "symbol->string", "string->symbol", "string-ref",
        "string-set!", "string-copy",
        "apply", "call/cc", "call-with-current-continuation"
    );

    private boolean isBuiltin(String name) { return BUILTINS.contains(name); }

    private Bounce applyBuiltin(String name, List<SchemeValue> a, Cont k) {
        return switch (name) {
            case "+" -> {
                long s = 0; for (var x : a) s += asInt(x);
                yield k.apply(new SchemeValue.IntVal(s));
            }
            case "-" -> {
                if (a.isEmpty()) throw new EvalError("-: needs at least 1 argument");
                if (a.size() == 1) yield k.apply(new SchemeValue.IntVal(-asInt(a.getFirst())));
                long r = asInt(a.getFirst());
                for (int i = 1; i < a.size(); i++) r -= asInt(a.get(i));
                yield k.apply(new SchemeValue.IntVal(r));
            }
            case "*" -> {
                long r = 1; for (var x : a) r *= asInt(x);
                yield k.apply(new SchemeValue.IntVal(r));
            }
            case "/" -> {
                if (a.isEmpty()) throw new EvalError("/: needs at least 1 argument");
                long r = asInt(a.getFirst());
                for (int i = 1; i < a.size(); i++) {
                    long d = asInt(a.get(i)); if (d == 0) throw new EvalError("division by zero");
                    r /= d;
                }
                yield k.apply(new SchemeValue.IntVal(r));
            }
            case "<" -> cmpOp(a, (x, y) -> x < y, k);
            case ">" -> cmpOp(a, (x, y) -> x > y, k);
            case "=" -> cmpOp(a, (x, y) -> x == y, k);
            case "<=" -> cmpOp(a, (x, y) -> x <= y, k);
            case ">=" -> cmpOp(a, (x, y) -> x >= y, k);
            case "not" -> {
                if (a.size() != 1) throw new EvalError("not: needs exactly 1 argument");
                yield k.apply(new SchemeValue.BoolVal(!a.getFirst().isTruthy()));
            }
            case "cons" -> {
                if (a.size() != 2) throw new EvalError("cons: needs exactly 2 arguments");
                yield k.apply(new SchemeValue.PairVal(a.get(0), a.get(1)));
            }
            case "car" -> {
                if (a.size() != 1) throw new EvalError("car: needs exactly 1 argument");
                if (!(a.getFirst() instanceof SchemeValue.PairVal p)) throw new EvalError("car: not a pair");
                yield k.apply(p.car());
            }
            case "cdr" -> {
                if (a.size() != 1) throw new EvalError("cdr: needs exactly 1 argument");
                if (!(a.getFirst() instanceof SchemeValue.PairVal p)) throw new EvalError("cdr: not a pair");
                yield k.apply(p.cdr());
            }
            case "null?" -> {
                if (a.size() != 1) throw new EvalError("null?: needs exactly 1 argument");
                yield k.apply(new SchemeValue.BoolVal(a.getFirst() instanceof SchemeValue.NilVal));
            }
            case "list" -> {
                SchemeValue r = new SchemeValue.NilVal();
                for (int i = a.size() - 1; i >= 0; i--) r = new SchemeValue.PairVal(a.get(i), r);
                yield k.apply(r);
            }
            case "length" -> {
                if (a.size() != 1) throw new EvalError("length: needs exactly 1 argument");
                var v = a.getFirst(); long len = 0;
                while (v instanceof SchemeValue.PairVal p) { len++; v = p.cdr(); }
                yield k.apply(new SchemeValue.IntVal(len));
            }
            case "append" -> {
                if (a.isEmpty()) yield k.apply(new SchemeValue.NilVal());
                SchemeValue r = a.getLast();
                for (int i = a.size() - 2; i >= 0; i--) r = appendTwo(a.get(i), r);
                yield k.apply(r);
            }
            case "string?" -> typePred(a, SchemeValue.StringVal.class, k);
            case "number?" -> typePred(a, SchemeValue.IntVal.class, k);
            case "boolean?" -> typePred(a, SchemeValue.BoolVal.class, k);
            case "pair?" -> typePred(a, SchemeValue.PairVal.class, k);
            case "symbol?" -> typePred(a, SchemeValue.SymbolVal.class, k);
            case "char?" -> typePred(a, SchemeValue.CharVal.class, k);
            case "zero?" -> {
                if (a.size() != 1) throw new EvalError("zero?: needs exactly 1 argument");
                yield k.apply(new SchemeValue.BoolVal(asInt(a.getFirst()) == 0));
            }
            case "positive?" -> {
                if (a.size() != 1) throw new EvalError("positive?: needs exactly 1 argument");
                yield k.apply(new SchemeValue.BoolVal(asInt(a.getFirst()) > 0));
            }
            case "negative?" -> {
                if (a.size() != 1) throw new EvalError("negative?: needs exactly 1 argument");
                yield k.apply(new SchemeValue.BoolVal(asInt(a.getFirst()) < 0));
            }
            case "abs" -> {
                if (a.size() != 1) throw new EvalError("abs: needs exactly 1 argument");
                yield k.apply(new SchemeValue.IntVal(Math.abs(asInt(a.getFirst()))));
            }
            case "min" -> {
                if (a.isEmpty()) throw new EvalError("min: needs at least 1 argument");
                long m = asInt(a.getFirst());
                for (int i = 1; i < a.size(); i++) m = Math.min(m, asInt(a.get(i)));
                yield k.apply(new SchemeValue.IntVal(m));
            }
            case "max" -> {
                if (a.isEmpty()) throw new EvalError("max: needs at least 1 argument");
                long m = asInt(a.getFirst());
                for (int i = 1; i < a.size(); i++) m = Math.max(m, asInt(a.get(i)));
                yield k.apply(new SchemeValue.IntVal(m));
            }
            case "equal?", "eq?" -> {
                if (a.size() != 2) throw new EvalError(name + ": needs exactly 2 arguments");
                yield k.apply(new SchemeValue.BoolVal(schemeEqual(a.get(0), a.get(1))));
            }
            case "modulo" -> {
                if (a.size() != 2) throw new EvalError("modulo: needs exactly 2 arguments");
                long x = asInt(a.get(0)), y = asInt(a.get(1));
                if (y == 0) throw new EvalError("division by zero");
                yield k.apply(new SchemeValue.IntVal(Math.floorMod(x, y)));
            }
            case "remainder" -> {
                if (a.size() != 2) throw new EvalError("remainder: needs exactly 2 arguments");
                long x = asInt(a.get(0)), y = asInt(a.get(1));
                if (y == 0) throw new EvalError("division by zero");
                yield k.apply(new SchemeValue.IntVal(x % y));
            }
            case "even?" -> {
                if (a.size() != 1) throw new EvalError("even?: needs exactly 1 argument");
                yield k.apply(new SchemeValue.BoolVal(asInt(a.getFirst()) % 2 == 0));
            }
            case "odd?" -> {
                if (a.size() != 1) throw new EvalError("odd?: needs exactly 1 argument");
                yield k.apply(new SchemeValue.BoolVal(asInt(a.getFirst()) % 2 != 0));
            }
            case "display" -> {
                if (a.size() != 1) throw new EvalError("display: needs exactly 1 argument");
                if (outputBuffer != null) outputBuffer.append(a.getFirst().displayOutput());
                yield k.apply(new SchemeValue.VoidVal());
            }
            case "write" -> {
                if (a.size() != 1) throw new EvalError("write: needs exactly 1 argument");
                if (outputBuffer != null) outputBuffer.append(a.getFirst().display());
                yield k.apply(new SchemeValue.VoidVal());
            }
            case "newline" -> {
                if (outputBuffer != null) outputBuffer.append('\n');
                yield k.apply(new SchemeValue.VoidVal());
            }
            case "string-append" -> {
                var sb = new StringBuilder();
                for (var x : a) {
                    if (!(x instanceof SchemeValue.StringVal sv)) throw new EvalError("string-append: not a string");
                    sb.append(sv.value());
                }
                yield k.apply(new SchemeValue.StringVal(sb.toString()));
            }
            case "string-length" -> {
                if (a.size() != 1) throw new EvalError("string-length: needs exactly 1 argument");
                if (!(a.getFirst() instanceof SchemeValue.StringVal sv)) throw new EvalError("string-length: not a string");
                yield k.apply(new SchemeValue.IntVal(sv.length()));
            }
            case "substring" -> {
                if (a.size() != 3) throw new EvalError("substring: needs exactly 3 arguments");
                if (!(a.get(0) instanceof SchemeValue.StringVal sv)) throw new EvalError("substring: not a string");
                yield k.apply(new SchemeValue.StringVal(sv.value().substring((int) asInt(a.get(1)), (int) asInt(a.get(2)))));
            }
            case "string->number" -> {
                if (a.size() != 1) throw new EvalError("string->number: needs exactly 1 argument");
                if (!(a.getFirst() instanceof SchemeValue.StringVal sv)) throw new EvalError("string->number: not a string");
                SchemeValue r2;
                try { r2 = new SchemeValue.IntVal(Long.parseLong(sv.value())); }
                catch (NumberFormatException e) { r2 = new SchemeValue.BoolVal(false); }
                yield k.apply(r2);
            }
            case "number->string" -> {
                if (a.size() != 1) throw new EvalError("number->string: needs exactly 1 argument");
                yield k.apply(new SchemeValue.StringVal(Long.toString(asInt(a.getFirst()))));
            }
            case "symbol->string" -> {
                if (a.size() != 1) throw new EvalError("symbol->string: needs exactly 1 argument");
                if (!(a.getFirst() instanceof SchemeValue.SymbolVal sv)) throw new EvalError("symbol->string: not a symbol");
                yield k.apply(new SchemeValue.StringVal(sv.name()));
            }
            case "string->symbol" -> {
                if (a.size() != 1) throw new EvalError("string->symbol: needs exactly 1 argument");
                if (!(a.getFirst() instanceof SchemeValue.StringVal sv)) throw new EvalError("string->symbol: not a string");
                yield k.apply(new SchemeValue.SymbolVal(sv.value()));
            }
            case "string-ref" -> {
                if (a.size() != 2) throw new EvalError("string-ref: needs exactly 2 arguments");
                if (!(a.get(0) instanceof SchemeValue.StringVal sv)) throw new EvalError("string-ref: not a string");
                yield k.apply(new SchemeValue.CharVal(sv.charAt((int) asInt(a.get(1)))));
            }
            case "string-set!" -> {
                if (a.size() != 3) throw new EvalError("string-set!: needs exactly 3 arguments");
                if (!(a.get(0) instanceof SchemeValue.StringVal sv)) throw new EvalError("string-set!: not a string");
                if (!(a.get(2) instanceof SchemeValue.CharVal c)) throw new EvalError("string-set!: not a character");
                sv.setChar((int) asInt(a.get(1)), c.value());
                yield k.apply(new SchemeValue.VoidVal());
            }
            case "string-copy" -> {
                if (a.size() != 1) throw new EvalError("string-copy: needs exactly 1 argument");
                if (!(a.getFirst() instanceof SchemeValue.StringVal sv)) throw new EvalError("string-copy: not a string");
                yield k.apply(sv.copy());
            }
            case "apply" -> {
                if (a.size() < 2) throw new EvalError("apply: needs at least 2 arguments");
                SchemeValue proc = a.getFirst();
                List<SchemeValue> allArgs = new ArrayList<>();
                for (int i = 1; i < a.size() - 1; i++) allArgs.add(a.get(i));
                SchemeValue cur = a.getLast();
                while (cur instanceof SchemeValue.PairVal p) { allArgs.add(p.car()); cur = p.cdr(); }
                yield applyProc(proc, allArgs, k);
            }
            case "call/cc", "call-with-current-continuation" -> {
                if (a.size() != 1) throw new EvalError("call/cc: needs exactly 1 argument");
                var contVal = new SchemeValue.ContinuationVal(k);
                yield applyProc(a.getFirst(), List.of(contVal), k);
            }
            default -> throw new EvalError("unknown procedure: " + name);
        };
    }

    // --- Macros ---

    private Bounce evalDefineSyntax(List<SchemeValue> args, Environment env, Cont k) {
        if (args.size() != 2) throw new EvalError("define-syntax: needs name and transformer");
        if (!(args.get(0) instanceof SchemeValue.SymbolVal nameSym))
            throw new EvalError("define-syntax: name must be a symbol");
        var transformer = args.get(1);
        if (!(transformer instanceof SchemeValue.ListVal srList) || srList.elements().size() < 2)
            throw new EvalError("define-syntax: invalid transformer");
        if (!(srList.elements().getFirst() instanceof SchemeValue.SymbolVal sr)
                || !sr.name().equals("syntax-rules"))
            throw new EvalError("define-syntax: expected syntax-rules");

        // Parse literals
        List<String> literals = new ArrayList<>();
        if (srList.elements().get(1) instanceof SchemeValue.ListVal ll) {
            for (var el : ll.elements()) {
                if (!(el instanceof SchemeValue.SymbolVal s))
                    throw new EvalError("syntax-rules: literal must be a symbol");
                literals.add(s.name());
            }
        }

        // Parse rules: each is (pattern template)
        List<List<SchemeValue>> rules = new ArrayList<>();
        for (int i = 2; i < srList.elements().size(); i++) {
            var rule = srList.elements().get(i);
            if (!(rule instanceof SchemeValue.ListVal rl) || rl.elements().size() != 2)
                throw new EvalError("syntax-rules: rule must be (pattern template)");
            rules.add(rl.elements());
        }

        env.define(nameSym.name(), new SchemeValue.MacroVal(literals, rules, env));
        return k.apply(new SchemeValue.VoidVal());
    }

    private Bounce expandAndEval(SchemeValue.MacroVal macro, SchemeValue.ListVal form,
                                  Environment env, Cont k) {
        for (var rule : macro.rules()) {
            SchemeValue pattern = rule.get(0);
            SchemeValue template = rule.get(1);
            Map<String, Object> bindings = new HashMap<>();
            if (matchMacroPattern(pattern, form, macro.literals(), bindings)) {
                Set<String> patternVars = new HashSet<>(bindings.keySet());
                Map<String, String> gensymMap = new HashMap<>();
                collectIntroducedSymbols(template, patternVars, gensymMap);

                SchemeValue expanded = expandTemplate(template, bindings, gensymMap);

                // Pre-bind gensyms to definition-site values for hygiene
                Environment macroEnv = new Environment(env);
                for (var entry : gensymMap.entrySet()) {
                    try {
                        SchemeValue defVal = macro.defEnv().get(entry.getKey());
                        macroEnv.define(entry.getValue(), defVal);
                    } catch (EvalError ignored) {}
                }
                return new Bounce.More(() -> eval(expanded, macroEnv, k));
            }
        }
        throw new EvalError("no matching pattern for macro: " + form.display());
    }

    // --- Pattern matching ---

    private boolean matchMacroPattern(SchemeValue pattern, SchemeValue form,
                                       List<String> literals, Map<String, Object> bindings) {
        if (!(pattern instanceof SchemeValue.ListVal patList)
                || !(form instanceof SchemeValue.ListVal formList))
            return false;
        // Skip first element (macro name), match from index 1
        return matchElems(patList.elements(), 1, formList.elements(), 1, literals, bindings);
    }

    private boolean matchElems(List<SchemeValue> pat, int pi, List<SchemeValue> form, int fi,
                                List<String> literals, Map<String, Object> bindings) {
        while (pi < pat.size()) {
            if (pi + 1 < pat.size() && isEllipsis(pat.get(pi + 1))) {
                SchemeValue subPat = pat.get(pi);
                int remaining = 0;
                for (int j = pi + 2; j < pat.size(); j++)
                    if (!isEllipsis(pat.get(j))) remaining++;
                int limit = form.size() - remaining;
                List<SchemeValue> matches = new ArrayList<>();
                while (fi < limit) {
                    matches.add(form.get(fi));
                    fi++;
                }
                if (subPat instanceof SchemeValue.SymbolVal sym)
                    bindings.put(sym.name(), matches);
                pi += 2;
            } else {
                if (fi >= form.size()) return false;
                if (!matchElem(pat.get(pi), form.get(fi), literals, bindings)) return false;
                pi++;
                fi++;
            }
        }
        return fi == form.size();
    }

    private boolean matchElem(SchemeValue pattern, SchemeValue form,
                               List<String> literals, Map<String, Object> bindings) {
        if (pattern instanceof SchemeValue.SymbolVal sym) {
            if (literals.contains(sym.name()))
                return form instanceof SchemeValue.SymbolVal fs && fs.name().equals(sym.name());
            bindings.put(sym.name(), form);
            return true;
        }
        if (pattern instanceof SchemeValue.ListVal patList && form instanceof SchemeValue.ListVal formList)
            return matchElems(patList.elements(), 0, formList.elements(), 0, literals, bindings);
        return false;
    }

    private boolean isEllipsis(SchemeValue v) {
        return v instanceof SchemeValue.SymbolVal s && s.name().equals("...");
    }

    // --- Template expansion ---

    private SchemeValue expandTemplate(SchemeValue template, Map<String, Object> bindings,
                                        Map<String, String> gensymMap) {
        if (template instanceof SchemeValue.SymbolVal sym) {
            String name = sym.name();
            if (bindings.containsKey(name)) {
                Object val = bindings.get(name);
                if (val instanceof SchemeValue sv) return sv;
                throw new EvalError("macro: ellipsis variable outside ellipsis context: " + name);
            }
            if (gensymMap.containsKey(name))
                return new SchemeValue.SymbolVal(gensymMap.get(name));
            return template;
        }
        if (template instanceof SchemeValue.ListVal listT) {
            List<SchemeValue> result = new ArrayList<>();
            var elems = listT.elements();
            for (int i = 0; i < elems.size(); i++) {
                if (i + 1 < elems.size() && isEllipsis(elems.get(i + 1))) {
                    String eVar = findEllipsisVar(elems.get(i), bindings);
                    if (eVar != null) {
                        @SuppressWarnings("unchecked")
                        List<SchemeValue> values = (List<SchemeValue>) bindings.get(eVar);
                        for (SchemeValue val : values) {
                            Map<String, Object> newB = new HashMap<>(bindings);
                            newB.put(eVar, val);
                            result.add(expandTemplate(elems.get(i), newB, gensymMap));
                        }
                    }
                    i++; // skip ...
                } else {
                    result.add(expandTemplate(elems.get(i), bindings, gensymMap));
                }
            }
            return new SchemeValue.ListVal(result);
        }
        return template;
    }

    private String findEllipsisVar(SchemeValue template, Map<String, Object> bindings) {
        if (template instanceof SchemeValue.SymbolVal sym) {
            Object val = bindings.get(sym.name());
            if (val instanceof List) return sym.name();
            return null;
        }
        if (template instanceof SchemeValue.ListVal list) {
            for (var elem : list.elements()) {
                String found = findEllipsisVar(elem, bindings);
                if (found != null) return found;
            }
        }
        return null;
    }

    private void collectIntroducedSymbols(SchemeValue template, Set<String> patternVars,
                                           Map<String, String> gensymMap) {
        if (template instanceof SchemeValue.SymbolVal sym) {
            String name = sym.name();
            if (!patternVars.contains(name) && !name.equals("...")
                    && !SPECIAL_FORMS.contains(name) && !BUILTINS.contains(name)
                    && !gensymMap.containsKey(name)) {
                gensymMap.put(name, name + "__g" + (++gensymCounter));
            }
        }
        if (template instanceof SchemeValue.ListVal list) {
            for (var elem : list.elements())
                collectIntroducedSymbols(elem, patternVars, gensymMap);
        }
    }

    // --- Helpers ---

    private long asInt(SchemeValue v) {
        if (v instanceof SchemeValue.IntVal i) return i.value();
        throw new EvalError("expected number, got " + v.display());
    }

    @FunctionalInterface interface LongCmp { boolean test(long a, long b); }

    private Bounce cmpOp(List<SchemeValue> a, LongCmp cmp, Cont k) {
        if (a.size() < 2) throw new EvalError("comparison needs at least 2 arguments");
        long prev = asInt(a.getFirst());
        for (int i = 1; i < a.size(); i++) {
            long cur = asInt(a.get(i));
            if (!cmp.test(prev, cur)) return k.apply(new SchemeValue.BoolVal(false));
            prev = cur;
        }
        return k.apply(new SchemeValue.BoolVal(true));
    }

    private Bounce typePred(List<SchemeValue> a, Class<? extends SchemeValue> type, Cont k) {
        if (a.size() != 1) throw new EvalError("type predicate: needs exactly 1 argument");
        return k.apply(new SchemeValue.BoolVal(type.isInstance(a.getFirst())));
    }

    private SchemeValue quoteDatum(SchemeValue v) {
        if (v instanceof SchemeValue.ListVal list) {
            SchemeValue r = new SchemeValue.NilVal();
            for (int i = list.elements().size() - 1; i >= 0; i--)
                r = new SchemeValue.PairVal(quoteDatum(list.elements().get(i)), r);
            return r;
        }
        return v;
    }

    private SchemeValue appendTwo(SchemeValue a, SchemeValue b) {
        if (a instanceof SchemeValue.NilVal) return b;
        if (a instanceof SchemeValue.PairVal p)
            return new SchemeValue.PairVal(p.car(), appendTwo(p.cdr(), b));
        throw new EvalError("append: not a proper list");
    }

    private boolean schemeEqual(SchemeValue a, SchemeValue b) {
        if (a instanceof SchemeValue.IntVal ia && b instanceof SchemeValue.IntVal ib) return ia.value() == ib.value();
        if (a instanceof SchemeValue.BoolVal ba && b instanceof SchemeValue.BoolVal bb) return ba.value() == bb.value();
        if (a instanceof SchemeValue.StringVal sa && b instanceof SchemeValue.StringVal sb) return sa.value().equals(sb.value());
        if (a instanceof SchemeValue.SymbolVal sa && b instanceof SchemeValue.SymbolVal sb) return sa.name().equals(sb.name());
        if (a instanceof SchemeValue.NilVal && b instanceof SchemeValue.NilVal) return true;
        if (a instanceof SchemeValue.PairVal pa && b instanceof SchemeValue.PairVal pb)
            return schemeEqual(pa.car(), pb.car()) && schemeEqual(pa.cdr(), pb.cdr());
        return false;
    }

    private static EvalError withPos(EvalError e, int line, int col) {
        if (line == 0 && col == 0) return e;
        String msg = e.getMessage();
        if (msg != null && msg.matches(".*\\d+:\\d+.*")) return e;
        return new EvalError(msg + " at " + line + ":" + col);
    }
}
