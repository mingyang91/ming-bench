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

    // --- dynamic-wind infrastructure ---
    record WindFrame(SchemeValue inThunk, SchemeValue outThunk) {}
    private final List<WindFrame> windStack = new ArrayList<>();

    private static final Set<String> SPECIAL_FORMS = Set.of(
        "if", "define", "lambda", "quote", "begin", "let", "let*", "set!",
        "cond", "and", "or", "call/cc", "call-with-current-continuation",
        "define-syntax", "else", "syntax-rules",
        "letrec", "letrec*", "case", "do", "guard"
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

    private static final class SchemeRaise extends RuntimeException {
        final SchemeValue value;
        SchemeRaise(SchemeValue value) {
            super(null, null, true, false);
            this.value = value;
        }
    }

    // --- exception handler infrastructure ---
    record HandlerFrame(Cont onRaise, List<WindFrame> savedWind) {}
    private final List<HandlerFrame> handlerStack = new ArrayList<>();

    private SchemeValue trampoline(Bounce b) {
        while (true) {
            try {
                switch (b) {
                    case Bounce.Done d -> { return d.value(); }
                    case Bounce.More m -> { b = m.thunk().run(); }
                }
            } catch (ContinuationReturn cr) {
                b = cr.bounce;
            } catch (SchemeRaise sr) {
                if (handlerStack.isEmpty()) {
                    throw new EvalError("unhandled exception: " + sr.value.display());
                }
                HandlerFrame frame = handlerStack.removeLast();
                // Wind transition from current context to handler's saved context
                int common = 0;
                int minLen = Math.min(windStack.size(), frame.savedWind().size());
                while (common < minLen && windStack.get(common) == frame.savedWind().get(common)) common++;
                List<SchemeValue> thunks = new ArrayList<>();
                for (int i = windStack.size() - 1; i >= common; i--)
                    thunks.add(windStack.get(i).outThunk());
                for (int i = common; i < frame.savedWind().size(); i++)
                    thunks.add(frame.savedWind().get(i).inThunk());
                windStack.clear();
                windStack.addAll(frame.savedWind());
                b = runWindThenHandler(thunks, 0, sr.value, frame.onRaise());
            }
        }
    }

    private Bounce runWindThenHandler(List<SchemeValue> thunks, int idx,
                                       SchemeValue exnVal, Cont handler) {
        if (idx >= thunks.size()) {
            return handler.apply(exnVal);
        }
        return new Bounce.More(() -> applyProc(thunks.get(idx), List.of(),
            _v -> runWindThenHandler(thunks, idx + 1, exnVal, handler)));
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
            case SchemeValue.RatVal v -> k.apply(v);
            case SchemeValue.DoubleVal v -> k.apply(v);
            case SchemeValue.BoolVal v -> k.apply(v);
            case SchemeValue.StringVal v -> k.apply(v);
            case SchemeValue.VoidVal v -> k.apply(v);
            case SchemeValue.NilVal v -> k.apply(v);
            case SchemeValue.PairVal v -> k.apply(v);
            case SchemeValue.CharVal v -> k.apply(v);
            case SchemeValue.LambdaVal v -> k.apply(v);
            case SchemeValue.ContinuationVal v -> k.apply(v);
            case SchemeValue.MacroVal v -> k.apply(v);
            case SchemeValue.VectorVal v -> k.apply(v);
            case SchemeValue.ValuesVal v -> k.apply(v);
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
                    case "let*" -> evalLetStar(args, env, k);
                    case "letrec" -> evalLetrec(args, env, k);
                    case "letrec*" -> evalLetrecStar(args, env, k);
                    case "case" -> evalCase(args, env, k);
                    case "do" -> evalDo(args, env, k);
                    case "set!" -> evalSet(args, env, k);
                    case "cond" -> evalCond(args, env, k);
                    case "and" -> evalAnd(args, 0, env, k);
                    case "or" -> evalOr(args, 0, env, k);
                    case "call/cc", "call-with-current-continuation" -> evalCallCC(args, env, k);
                    case "define-syntax" -> evalDefineSyntax(args, env, k);
                    case "guard" -> evalGuard(args, env, k);
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

    private Bounce evalLetStar(List<SchemeValue> args, Environment env, Cont k) {
        if (args.size() < 2) throw new EvalError("let*: needs bindings and body");
        if (!(args.getFirst() instanceof SchemeValue.ListVal bl))
            throw new EvalError("let*: bindings must be a list");
        List<SchemeValue> body = args.subList(1, args.size());
        Environment letEnv = new Environment(env);
        return evalLetStarBindings(bl.elements(), 0, letEnv, body, k);
    }

    private Bounce evalLetStarBindings(List<SchemeValue> bindings, int i, Environment env,
                                        List<SchemeValue> body, Cont k) {
        if (i >= bindings.size()) return evalSeq(body, env, k);
        if (!(bindings.get(i) instanceof SchemeValue.ListVal pair) || pair.elements().size() != 2)
            throw new EvalError("let*: invalid binding");
        if (!(pair.elements().getFirst() instanceof SchemeValue.SymbolVal sym))
            throw new EvalError("let*: binding name must be a symbol");
        return new Bounce.More(() -> eval(pair.elements().get(1), env, val -> {
            env.define(sym.name(), val);
            return evalLetStarBindings(bindings, i + 1, env, body, k);
        }));
    }

    private Bounce evalLetrec(List<SchemeValue> args, Environment env, Cont k) {
        if (args.size() < 2) throw new EvalError("letrec: needs bindings and body");
        if (!(args.getFirst() instanceof SchemeValue.ListVal bl))
            throw new EvalError("letrec: bindings must be a list");
        List<String> names = new ArrayList<>();
        List<SchemeValue> initExprs = new ArrayList<>();
        for (var binding : bl.elements()) {
            if (!(binding instanceof SchemeValue.ListVal pair) || pair.elements().size() != 2)
                throw new EvalError("letrec: invalid binding");
            if (!(pair.elements().getFirst() instanceof SchemeValue.SymbolVal sym))
                throw new EvalError("letrec: binding name must be a symbol");
            names.add(sym.name());
            initExprs.add(pair.elements().get(1));
        }
        Environment letEnv = new Environment(env);
        for (String name : names) letEnv.define(name, new SchemeValue.VoidVal());
        List<SchemeValue> body = args.subList(1, args.size());
        return evalArgsList(initExprs, letEnv, vals -> {
            for (int i = 0; i < names.size(); i++) letEnv.define(names.get(i), vals.get(i));
            return evalSeq(body, letEnv, k);
        });
    }

    private Bounce evalLetrecStar(List<SchemeValue> args, Environment env, Cont k) {
        if (args.size() < 2) throw new EvalError("letrec*: needs bindings and body");
        if (!(args.getFirst() instanceof SchemeValue.ListVal bl))
            throw new EvalError("letrec*: bindings must be a list");
        Environment letEnv = new Environment(env);
        List<SchemeValue> body = args.subList(1, args.size());
        return evalLetrecStarBindings(bl.elements(), 0, letEnv, body, k);
    }

    private Bounce evalLetrecStarBindings(List<SchemeValue> bindings, int i, Environment env,
                                            List<SchemeValue> body, Cont k) {
        if (i >= bindings.size()) return evalSeq(body, env, k);
        if (!(bindings.get(i) instanceof SchemeValue.ListVal pair) || pair.elements().size() != 2)
            throw new EvalError("letrec*: invalid binding");
        if (!(pair.elements().getFirst() instanceof SchemeValue.SymbolVal sym))
            throw new EvalError("letrec*: binding name must be a symbol");
        env.define(sym.name(), new SchemeValue.VoidVal());
        return new Bounce.More(() -> eval(pair.elements().get(1), env, val -> {
            env.define(sym.name(), val);
            return evalLetrecStarBindings(bindings, i + 1, env, body, k);
        }));
    }

    private Bounce evalCase(List<SchemeValue> args, Environment env, Cont k) {
        if (args.size() < 2) throw new EvalError("case: needs key and clauses");
        return new Bounce.More(() -> eval(args.get(0), env, key ->
            evalCaseClauses(key, args.subList(1, args.size()), env, k)));
    }

    private Bounce evalCaseClauses(SchemeValue key, List<SchemeValue> clauses,
                                     Environment env, Cont k) {
        if (clauses.isEmpty()) return k.apply(new SchemeValue.VoidVal());
        var clause = clauses.getFirst();
        if (!(clause instanceof SchemeValue.ListVal cl) || cl.elements().isEmpty())
            throw new EvalError("case: invalid clause");
        var datums = cl.elements().getFirst();
        if (datums instanceof SchemeValue.SymbolVal sym && sym.name().equals("else")) {
            if (cl.elements().size() == 1) return k.apply(new SchemeValue.VoidVal());
            return evalSeq(cl.elements().subList(1, cl.elements().size()), env, k);
        }
        if (!(datums instanceof SchemeValue.ListVal datumList))
            throw new EvalError("case: datums must be a list");
        for (var datum : datumList.elements()) {
            SchemeValue d = quoteDatum(datum);
            if (schemeEqv(key, d)) {
                if (cl.elements().size() == 1) return k.apply(new SchemeValue.VoidVal());
                return evalSeq(cl.elements().subList(1, cl.elements().size()), env, k);
            }
        }
        return evalCaseClauses(key, clauses.subList(1, clauses.size()), env, k);
    }

    private Bounce evalDo(List<SchemeValue> args, Environment env, Cont k) {
        if (args.size() < 2) throw new EvalError("do: needs variables, test, and optional body");
        if (!(args.get(0) instanceof SchemeValue.ListVal varSpecs))
            throw new EvalError("do: variable specs must be a list");
        if (!(args.get(1) instanceof SchemeValue.ListVal testClause) || testClause.elements().isEmpty())
            throw new EvalError("do: test clause must be a non-empty list");

        List<String> varNames = new ArrayList<>();
        List<SchemeValue> initExprs = new ArrayList<>();
        List<SchemeValue> stepExprs = new ArrayList<>(); // null means no step
        for (var spec : varSpecs.elements()) {
            if (!(spec instanceof SchemeValue.ListVal sv) || sv.elements().size() < 2 || sv.elements().size() > 3)
                throw new EvalError("do: invalid variable spec");
            if (!(sv.elements().get(0) instanceof SchemeValue.SymbolVal sym))
                throw new EvalError("do: variable name must be a symbol");
            varNames.add(sym.name());
            initExprs.add(sv.elements().get(1));
            stepExprs.add(sv.elements().size() == 3 ? sv.elements().get(2) : null);
        }

        SchemeValue testExpr = testClause.elements().get(0);
        List<SchemeValue> resultExprs = testClause.elements().subList(1, testClause.elements().size());
        List<SchemeValue> body = args.subList(2, args.size());

        return evalArgsList(initExprs, env, initVals -> {
            Environment doEnv = new Environment(env);
            for (int i = 0; i < varNames.size(); i++) doEnv.define(varNames.get(i), initVals.get(i));
            return doLoop(varNames, stepExprs, testExpr, resultExprs, body, doEnv, k);
        });
    }

    private Bounce doLoop(List<String> varNames, List<SchemeValue> stepExprs,
                           SchemeValue testExpr, List<SchemeValue> resultExprs,
                           List<SchemeValue> body, Environment doEnv, Cont k) {
        return new Bounce.More(() -> eval(testExpr, doEnv, testVal -> {
            if (testVal.isTruthy()) {
                if (resultExprs.isEmpty()) return k.apply(new SchemeValue.VoidVal());
                return evalSeq(resultExprs, doEnv, k);
            }
            // Execute body (for side effects), then step
            Cont afterBody = _v -> {
                // Evaluate all step expressions using current env (parallel)
                List<SchemeValue> stepExprList = new ArrayList<>();
                List<Integer> stepIndices = new ArrayList<>();
                for (int i = 0; i < stepExprs.size(); i++) {
                    if (stepExprs.get(i) != null) {
                        stepExprList.add(stepExprs.get(i));
                        stepIndices.add(i);
                    }
                }
                if (stepExprList.isEmpty()) {
                    return doLoop(varNames, stepExprs, testExpr, resultExprs, body, doEnv, k);
                }
                return evalArgsList(stepExprList, doEnv, stepVals -> {
                    for (int j = 0; j < stepIndices.size(); j++) {
                        doEnv.define(varNames.get(stepIndices.get(j)), stepVals.get(j));
                    }
                    return doLoop(varNames, stepExprs, testExpr, resultExprs, body, doEnv, k);
                });
            };
            if (body.isEmpty()) return afterBody.apply(new SchemeValue.VoidVal());
            return evalSeq(body, doEnv, afterBody);
        }));
    }

    private Bounce evalCallCC(List<SchemeValue> args, Environment env, Cont k) {
        if (args.size() != 1) throw new EvalError("call/cc: needs exactly 1 argument");
        return new Bounce.More(() -> eval(args.get(0), env, proc -> {
            var savedWind = new ArrayList<>(windStack);
            var contVal = new SchemeValue.ContinuationVal(k, savedWind);
            return applyProc(proc, List.of(contVal), k);
        }));
    }

    private Bounce evalGuard(List<SchemeValue> args, Environment env, Cont k) {
        if (args.size() < 2) throw new EvalError("guard: needs variable/clauses and body");
        if (!(args.get(0) instanceof SchemeValue.ListVal clauseList) || clauseList.elements().size() < 2)
            throw new EvalError("guard: invalid clause list");
        if (!(clauseList.elements().get(0) instanceof SchemeValue.SymbolVal varSym))
            throw new EvalError("guard: variable must be a symbol");
        String varName = varSym.name();
        List<SchemeValue> clauses = clauseList.elements().subList(1, clauseList.elements().size());
        List<SchemeValue> body = args.subList(1, args.size());

        List<WindFrame> savedWind = new ArrayList<>(windStack);
        handlerStack.add(new HandlerFrame(
            exnVal -> {
                Environment guardEnv = new Environment(env);
                guardEnv.define(varName, exnVal);
                return evalGuardClauses(clauses, guardEnv, k, exnVal);
            },
            savedWind
        ));
        return evalSeq(body, env, result -> {
            handlerStack.removeLast();
            return k.apply(result);
        });
    }

    private Bounce evalGuardClauses(List<SchemeValue> clauses, Environment env,
                                     Cont k, SchemeValue exnVal) {
        if (clauses.isEmpty()) throw new SchemeRaise(exnVal);
        var clause = clauses.getFirst();
        if (!(clause instanceof SchemeValue.ListVal cl) || cl.elements().isEmpty())
            throw new EvalError("guard: invalid clause");
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
            return evalGuardClauses(rest, env, k, exnVal);
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

    @SuppressWarnings("unchecked")
    private Bounce applyProc(SchemeValue proc, List<SchemeValue> args, Cont k) {
        if (proc instanceof SchemeValue.ContinuationVal cont) {
            if (args.size() != 1) throw new EvalError("continuation: needs exactly 1 argument");
            SchemeValue value = args.getFirst();
            Cont capturedK = (Cont) cont.cont();
            List<WindFrame> targetWind = (List<WindFrame>) cont.windStack();
            return doWindTransition(targetWind, value, capturedK);
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
        "equal?", "eq?", "eqv?", "modulo", "remainder", "even?", "odd?",
        "quotient", "expt",
        "list-ref", "list-tail", "list?", "assoc", "map",
        "display", "write", "newline",
        "string-append", "string-length", "substring",
        "string->number", "number->string",
        "symbol->string", "string->symbol", "string-ref",
        "string-set!", "string-copy",
        "string->list", "list->string",
        "char->integer", "integer->char",
        "char-alphabetic?", "char-numeric?",
        "char-upcase", "char-downcase", "char=?", "char<?",
        "string=?", "string<?", "string-ci=?",
        "string-upcase", "string-downcase",
        "vector", "make-vector", "vector-ref", "vector-set!",
        "vector-length", "vector?", "vector->list", "list->vector",
        "apply", "call/cc", "call-with-current-continuation",
        "dynamic-wind", "reverse",
        "raise", "with-exception-handler",
        "values", "call-with-values",
        "exact?", "inexact?", "exact->inexact", "inexact->exact",
        "numerator", "denominator", "rational?", "integer?"
    );

    private boolean isBuiltin(String name) { return BUILTINS.contains(name); }

    private Bounce applyBuiltin(String name, List<SchemeValue> a, Cont k) {
        return switch (name) {
            case "+" -> {
                if (a.isEmpty()) yield k.apply(new SchemeValue.IntVal(0));
                if (allInts(a)) { long s = 0; for (var x : a) s += ((SchemeValue.IntVal)x).value(); yield k.apply(new SchemeValue.IntVal(s)); }
                if (hasDouble(a)) { double s = 0; for (var x : a) s += SchemeValue.toDouble(x); yield k.apply(new SchemeValue.DoubleVal(s)); }
                long num = 0, den = 1;
                for (var x : a) { long[] r = toRat(x); num = num * r[1] + r[0] * den; den = den * r[1]; long g = SchemeValue.gcd(Math.abs(num), den); num /= g; den /= g; }
                yield k.apply(SchemeValue.makeRational(num, den));
            }
            case "-" -> {
                if (a.isEmpty()) throw new EvalError("-: needs at least 1 argument");
                if (allInts(a)) { long r = ((SchemeValue.IntVal)a.getFirst()).value(); if (a.size()==1) yield k.apply(new SchemeValue.IntVal(-r)); for (int i=1;i<a.size();i++) r -= ((SchemeValue.IntVal)a.get(i)).value(); yield k.apply(new SchemeValue.IntVal(r)); }
                if (hasDouble(a)) { double r = SchemeValue.toDouble(a.getFirst()); if (a.size()==1) yield k.apply(new SchemeValue.DoubleVal(-r)); for (int i=1;i<a.size();i++) r -= SchemeValue.toDouble(a.get(i)); yield k.apply(new SchemeValue.DoubleVal(r)); }
                long[] first = toRat(a.getFirst());
                long num = first[0], den = first[1];
                if (a.size() == 1) yield k.apply(SchemeValue.makeRational(-num, den));
                for (int i = 1; i < a.size(); i++) { long[] r = toRat(a.get(i)); num = num * r[1] - r[0] * den; den = den * r[1]; long g = SchemeValue.gcd(Math.abs(num), den); num /= g; den /= g; }
                yield k.apply(SchemeValue.makeRational(num, den));
            }
            case "*" -> {
                if (allInts(a)) { long r = 1; for (var x : a) r *= ((SchemeValue.IntVal)x).value(); yield k.apply(new SchemeValue.IntVal(r)); }
                if (hasDouble(a)) { double r = 1; for (var x : a) r *= SchemeValue.toDouble(x); yield k.apply(new SchemeValue.DoubleVal(r)); }
                long num = 1, den = 1;
                for (var x : a) { long[] r = toRat(x); num *= r[0]; den *= r[1]; long g = SchemeValue.gcd(Math.abs(num), den); num /= g; den /= g; }
                yield k.apply(SchemeValue.makeRational(num, den));
            }
            case "/" -> {
                if (a.isEmpty()) throw new EvalError("/: needs at least 1 argument");
                if (hasDouble(a)) {
                    double r = SchemeValue.toDouble(a.getFirst());
                    if (a.size() == 1) { if (r == 0) throw new EvalError("division by zero"); yield k.apply(new SchemeValue.DoubleVal(1.0 / r)); }
                    for (int i = 1; i < a.size(); i++) { double d = SchemeValue.toDouble(a.get(i)); if (d == 0) throw new EvalError("division by zero"); r /= d; }
                    yield k.apply(new SchemeValue.DoubleVal(r));
                }
                long[] first = toRat(a.getFirst());
                long num = first[0], den = first[1];
                if (a.size() == 1) { if (num == 0) throw new EvalError("division by zero"); yield k.apply(SchemeValue.makeRational(den, num)); }
                for (int i = 1; i < a.size(); i++) { long[] r = toRat(a.get(i)); if (r[0] == 0) throw new EvalError("division by zero"); num *= r[1]; den *= r[0]; long g = SchemeValue.gcd(Math.abs(num), Math.abs(den)); num /= g; den /= g; }
                yield k.apply(SchemeValue.makeRational(num, den));
            }
            case "<" -> numCmpOp(a, (x, y) -> x < y, k);
            case ">" -> numCmpOp(a, (x, y) -> x > y, k);
            case "=" -> numCmpOp(a, (x, y) -> x == y, k);
            case "<=" -> numCmpOp(a, (x, y) -> x <= y, k);
            case ">=" -> numCmpOp(a, (x, y) -> x >= y, k);
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
            case "reverse" -> {
                if (a.size() != 1) throw new EvalError("reverse: needs exactly 1 argument");
                SchemeValue cur = a.getFirst();
                SchemeValue rev = new SchemeValue.NilVal();
                while (cur instanceof SchemeValue.PairVal p) {
                    rev = new SchemeValue.PairVal(p.car(), rev);
                    cur = p.cdr();
                }
                if (!(cur instanceof SchemeValue.NilVal))
                    throw new EvalError("reverse: not a proper list");
                yield k.apply(rev);
            }
            case "string?" -> typePred(a, SchemeValue.StringVal.class, k);
            case "number?" -> {
                if (a.size() != 1) throw new EvalError("number?: needs exactly 1 argument");
                yield k.apply(new SchemeValue.BoolVal(SchemeValue.isNumber(a.getFirst())));
            }
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
            case "equal?" -> {
                if (a.size() != 2) throw new EvalError("equal?: needs exactly 2 arguments");
                yield k.apply(new SchemeValue.BoolVal(schemeEqual(a.get(0), a.get(1))));
            }
            case "eq?" -> {
                if (a.size() != 2) throw new EvalError("eq?: needs exactly 2 arguments");
                yield k.apply(new SchemeValue.BoolVal(schemeEqv(a.get(0), a.get(1))));
            }
            case "eqv?" -> {
                if (a.size() != 2) throw new EvalError("eqv?: needs exactly 2 arguments");
                yield k.apply(new SchemeValue.BoolVal(schemeEqv(a.get(0), a.get(1))));
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
            case "quotient" -> {
                if (a.size() != 2) throw new EvalError("quotient: needs exactly 2 arguments");
                long x = asInt(a.get(0)), y = asInt(a.get(1));
                if (y == 0) throw new EvalError("division by zero");
                long q = x / y; // Java truncates toward zero
                yield k.apply(new SchemeValue.IntVal(q));
            }
            case "expt" -> {
                if (a.size() != 2) throw new EvalError("expt: needs exactly 2 arguments");
                long base = asInt(a.get(0)), exp = asInt(a.get(1));
                long result = 1;
                for (long i = 0; i < exp; i++) result *= base;
                yield k.apply(new SchemeValue.IntVal(result));
            }
            case "list-ref" -> {
                if (a.size() != 2) throw new EvalError("list-ref: needs exactly 2 arguments");
                long idx = asInt(a.get(1));
                SchemeValue cur = a.get(0);
                for (long i = 0; i < idx; i++) {
                    if (!(cur instanceof SchemeValue.PairVal p)) throw new EvalError("list-ref: index out of range");
                    cur = p.cdr();
                }
                if (!(cur instanceof SchemeValue.PairVal p)) throw new EvalError("list-ref: index out of range");
                yield k.apply(p.car());
            }
            case "list-tail" -> {
                if (a.size() != 2) throw new EvalError("list-tail: needs exactly 2 arguments");
                long idx = asInt(a.get(1));
                SchemeValue cur = a.get(0);
                for (long i = 0; i < idx; i++) {
                    if (!(cur instanceof SchemeValue.PairVal p)) throw new EvalError("list-tail: index out of range");
                    cur = p.cdr();
                }
                yield k.apply(cur);
            }
            case "list?" -> {
                if (a.size() != 1) throw new EvalError("list?: needs exactly 1 argument");
                SchemeValue cur = a.getFirst();
                boolean proper = true;
                while (cur instanceof SchemeValue.PairVal p) cur = p.cdr();
                if (!(cur instanceof SchemeValue.NilVal)) proper = false;
                yield k.apply(new SchemeValue.BoolVal(proper));
            }
            case "assoc" -> {
                if (a.size() != 2) throw new EvalError("assoc: needs exactly 2 arguments");
                SchemeValue key = a.get(0);
                SchemeValue lst = a.get(1);
                SchemeValue result = new SchemeValue.BoolVal(false);
                while (lst instanceof SchemeValue.PairVal p) {
                    if (p.car() instanceof SchemeValue.PairVal entry && schemeEqual(entry.car(), key)) {
                        result = entry;
                        break;
                    }
                    lst = p.cdr();
                }
                yield k.apply(result);
            }
            case "map" -> {
                if (a.size() < 2) throw new EvalError("map: needs at least 2 arguments");
                SchemeValue proc = a.get(0);
                List<SchemeValue> lists = a.subList(1, a.size());
                yield mapLoop(proc, lists, k);
            }
            case "char-alphabetic?" -> {
                if (a.size() != 1) throw new EvalError("char-alphabetic?: needs exactly 1 argument");
                if (!(a.getFirst() instanceof SchemeValue.CharVal c)) throw new EvalError("char-alphabetic?: not a character");
                yield k.apply(new SchemeValue.BoolVal(Character.isLetter(c.value())));
            }
            case "char-numeric?" -> {
                if (a.size() != 1) throw new EvalError("char-numeric?: needs exactly 1 argument");
                if (!(a.getFirst() instanceof SchemeValue.CharVal c)) throw new EvalError("char-numeric?: not a character");
                yield k.apply(new SchemeValue.BoolVal(Character.isDigit(c.value())));
            }
            case "char-upcase" -> {
                if (a.size() != 1) throw new EvalError("char-upcase: needs exactly 1 argument");
                if (!(a.getFirst() instanceof SchemeValue.CharVal c)) throw new EvalError("char-upcase: not a character");
                yield k.apply(new SchemeValue.CharVal(Character.toUpperCase(c.value())));
            }
            case "char-downcase" -> {
                if (a.size() != 1) throw new EvalError("char-downcase: needs exactly 1 argument");
                if (!(a.getFirst() instanceof SchemeValue.CharVal c)) throw new EvalError("char-downcase: not a character");
                yield k.apply(new SchemeValue.CharVal(Character.toLowerCase(c.value())));
            }
            case "char=?" -> {
                if (a.size() != 2) throw new EvalError("char=?: needs exactly 2 arguments");
                if (!(a.get(0) instanceof SchemeValue.CharVal c1)) throw new EvalError("char=?: not a character");
                if (!(a.get(1) instanceof SchemeValue.CharVal c2)) throw new EvalError("char=?: not a character");
                yield k.apply(new SchemeValue.BoolVal(c1.value() == c2.value()));
            }
            case "char<?" -> {
                if (a.size() != 2) throw new EvalError("char<?: needs exactly 2 arguments");
                if (!(a.get(0) instanceof SchemeValue.CharVal c1)) throw new EvalError("char<?: not a character");
                if (!(a.get(1) instanceof SchemeValue.CharVal c2)) throw new EvalError("char<?: not a character");
                yield k.apply(new SchemeValue.BoolVal(c1.value() < c2.value()));
            }
            case "string=?" -> {
                if (a.size() != 2) throw new EvalError("string=?: needs exactly 2 arguments");
                if (!(a.get(0) instanceof SchemeValue.StringVal s1)) throw new EvalError("string=?: not a string");
                if (!(a.get(1) instanceof SchemeValue.StringVal s2)) throw new EvalError("string=?: not a string");
                yield k.apply(new SchemeValue.BoolVal(s1.value().equals(s2.value())));
            }
            case "string<?" -> {
                if (a.size() != 2) throw new EvalError("string<?: needs exactly 2 arguments");
                if (!(a.get(0) instanceof SchemeValue.StringVal s1)) throw new EvalError("string<?: not a string");
                if (!(a.get(1) instanceof SchemeValue.StringVal s2)) throw new EvalError("string<?: not a string");
                yield k.apply(new SchemeValue.BoolVal(s1.value().compareTo(s2.value()) < 0));
            }
            case "string-ci=?" -> {
                if (a.size() != 2) throw new EvalError("string-ci=?: needs exactly 2 arguments");
                if (!(a.get(0) instanceof SchemeValue.StringVal s1)) throw new EvalError("string-ci=?: not a string");
                if (!(a.get(1) instanceof SchemeValue.StringVal s2)) throw new EvalError("string-ci=?: not a string");
                yield k.apply(new SchemeValue.BoolVal(s1.value().equalsIgnoreCase(s2.value())));
            }
            case "string-upcase" -> {
                if (a.size() != 1) throw new EvalError("string-upcase: needs exactly 1 argument");
                if (!(a.getFirst() instanceof SchemeValue.StringVal s)) throw new EvalError("string-upcase: not a string");
                yield k.apply(new SchemeValue.StringVal(s.value().toUpperCase()));
            }
            case "string-downcase" -> {
                if (a.size() != 1) throw new EvalError("string-downcase: needs exactly 1 argument");
                if (!(a.getFirst() instanceof SchemeValue.StringVal s)) throw new EvalError("string-downcase: not a string");
                yield k.apply(new SchemeValue.StringVal(s.value().toLowerCase()));
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
                SchemeValue r2 = new SchemeValue.BoolVal(false);
                String str = sv.value();
                try { r2 = new SchemeValue.IntVal(Long.parseLong(str)); } catch (NumberFormatException e) {
                    try { r2 = new SchemeValue.DoubleVal(Double.parseDouble(str)); } catch (NumberFormatException e2) {}
                }
                yield k.apply(r2);
            }
            case "number->string" -> {
                if (a.size() != 1) throw new EvalError("number->string: needs exactly 1 argument");
                yield k.apply(new SchemeValue.StringVal(a.getFirst().display()));
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
                if (sv.isImmutable()) throw new EvalError("string-set!: string is immutable");
                int idx = (int) asInt(a.get(1));
                if (!(a.get(2) instanceof SchemeValue.CharVal cv)) throw new EvalError("string-set!: not a char");
                sv.setChar(idx, cv.value());
                yield k.apply(new SchemeValue.VoidVal());
            }
            case "string-copy" -> {
                if (a.size() != 1) throw new EvalError("string-copy: needs exactly 1 argument");
                if (!(a.getFirst() instanceof SchemeValue.StringVal sv)) throw new EvalError("string-copy: not a string");
                yield k.apply(sv.copy());
            }
            case "string->list" -> {
                if (a.size() != 1) throw new EvalError("string->list: needs exactly 1 argument");
                if (!(a.getFirst() instanceof SchemeValue.StringVal sv)) throw new EvalError("string->list: not a string");
                String s = sv.value();
                SchemeValue result = new SchemeValue.NilVal();
                for (int i = s.length() - 1; i >= 0; i--) {
                    result = new SchemeValue.PairVal(new SchemeValue.CharVal(s.charAt(i)), result);
                }
                yield k.apply(result);
            }
            case "list->string" -> {
                if (a.size() != 1) throw new EvalError("list->string: needs exactly 1 argument");
                var sb = new StringBuilder();
                SchemeValue cur = a.getFirst();
                while (cur instanceof SchemeValue.PairVal p) {
                    if (!(p.car() instanceof SchemeValue.CharVal c)) throw new EvalError("list->string: not a character");
                    sb.append(c.value());
                    cur = p.cdr();
                }
                yield k.apply(new SchemeValue.StringVal(sb.toString()));
            }
            case "char->integer" -> {
                if (a.size() != 1) throw new EvalError("char->integer: needs exactly 1 argument");
                if (!(a.getFirst() instanceof SchemeValue.CharVal c)) throw new EvalError("char->integer: not a character");
                yield k.apply(new SchemeValue.IntVal(c.value()));
            }
            case "integer->char" -> {
                if (a.size() != 1) throw new EvalError("integer->char: needs exactly 1 argument");
                yield k.apply(new SchemeValue.CharVal((char) asInt(a.getFirst())));
            }
            case "vector" -> {
                var elems = new SchemeValue[a.size()];
                for (int i = 0; i < a.size(); i++) elems[i] = a.get(i);
                yield k.apply(new SchemeValue.VectorVal(elems));
            }
            case "make-vector" -> {
                if (a.size() < 1 || a.size() > 2) throw new EvalError("make-vector: needs 1 or 2 arguments");
                int sz = (int) asInt(a.get(0));
                SchemeValue fill = a.size() == 2 ? a.get(1) : new SchemeValue.IntVal(0);
                yield k.apply(new SchemeValue.VectorVal(sz, fill));
            }
            case "vector-ref" -> {
                if (a.size() != 2) throw new EvalError("vector-ref: needs exactly 2 arguments");
                if (!(a.get(0) instanceof SchemeValue.VectorVal vec)) throw new EvalError("vector-ref: not a vector");
                yield k.apply(vec.get((int) asInt(a.get(1))));
            }
            case "vector-set!" -> {
                if (a.size() != 3) throw new EvalError("vector-set!: needs exactly 3 arguments");
                if (!(a.get(0) instanceof SchemeValue.VectorVal vec)) throw new EvalError("vector-set!: not a vector");
                vec.set((int) asInt(a.get(1)), a.get(2));
                yield k.apply(new SchemeValue.VoidVal());
            }
            case "vector-length" -> {
                if (a.size() != 1) throw new EvalError("vector-length: needs exactly 1 argument");
                if (!(a.getFirst() instanceof SchemeValue.VectorVal vec)) throw new EvalError("vector-length: not a vector");
                yield k.apply(new SchemeValue.IntVal(vec.length()));
            }
            case "vector?" -> {
                if (a.size() != 1) throw new EvalError("vector?: needs exactly 1 argument");
                yield k.apply(new SchemeValue.BoolVal(a.getFirst() instanceof SchemeValue.VectorVal));
            }
            case "vector->list" -> {
                if (a.size() != 1) throw new EvalError("vector->list: needs exactly 1 argument");
                if (!(a.getFirst() instanceof SchemeValue.VectorVal vec)) throw new EvalError("vector->list: not a vector");
                SchemeValue result = new SchemeValue.NilVal();
                for (int i = vec.length() - 1; i >= 0; i--) result = new SchemeValue.PairVal(vec.get(i), result);
                yield k.apply(result);
            }
            case "list->vector" -> {
                if (a.size() != 1) throw new EvalError("list->vector: needs exactly 1 argument");
                List<SchemeValue> elems = new ArrayList<>();
                SchemeValue cur = a.getFirst();
                while (cur instanceof SchemeValue.PairVal p) { elems.add(p.car()); cur = p.cdr(); }
                yield k.apply(new SchemeValue.VectorVal(elems.toArray(new SchemeValue[0])));
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
                var savedWind = new ArrayList<>(windStack);
                var contVal = new SchemeValue.ContinuationVal(k, savedWind);
                yield applyProc(a.getFirst(), List.of(contVal), k);
            }
            case "raise" -> {
                if (a.size() != 1) throw new EvalError("raise: needs exactly 1 argument");
                throw new SchemeRaise(a.getFirst());
            }
            case "with-exception-handler" -> {
                if (a.size() != 2) throw new EvalError("with-exception-handler: needs exactly 2 arguments");
                SchemeValue handlerProc = a.get(0);
                SchemeValue thunk = a.get(1);
                var frame = new HandlerFrame(
                    exnVal -> applyProc(handlerProc, List.of(exnVal), result -> {
                        throw new EvalError("exception handler returned from raise");
                    }),
                    new ArrayList<>(windStack)
                );
                handlerStack.add(frame);
                yield applyProc(thunk, List.of(), result -> {
                    handlerStack.removeLast();
                    return k.apply(result);
                });
            }
            case "values" -> {
                if (a.size() == 1) yield k.apply(a.getFirst());
                yield k.apply(new SchemeValue.ValuesVal(a));
            }
            case "call-with-values" -> {
                if (a.size() != 2) throw new EvalError("call-with-values: needs exactly 2 arguments");
                SchemeValue producer = a.get(0);
                SchemeValue consumer = a.get(1);
                yield applyProc(producer, List.of(), produced -> {
                    if (produced instanceof SchemeValue.ValuesVal mv) {
                        return applyProc(consumer, mv.values(), k);
                    }
                    return applyProc(consumer, List.of(produced), k);
                });
            }
            case "exact?" -> {
                if (a.size() != 1) throw new EvalError("exact?: needs exactly 1 argument");
                var v = a.getFirst();
                yield k.apply(new SchemeValue.BoolVal(v instanceof SchemeValue.IntVal || v instanceof SchemeValue.RatVal));
            }
            case "inexact?" -> {
                if (a.size() != 1) throw new EvalError("inexact?: needs exactly 1 argument");
                yield k.apply(new SchemeValue.BoolVal(a.getFirst() instanceof SchemeValue.DoubleVal));
            }
            case "exact->inexact" -> {
                if (a.size() != 1) throw new EvalError("exact->inexact: needs exactly 1 argument");
                yield k.apply(new SchemeValue.DoubleVal(SchemeValue.toDouble(a.getFirst())));
            }
            case "inexact->exact" -> {
                if (a.size() != 1) throw new EvalError("inexact->exact: needs exactly 1 argument");
                var v = a.getFirst();
                if (v instanceof SchemeValue.IntVal || v instanceof SchemeValue.RatVal) yield k.apply(v);
                if (v instanceof SchemeValue.DoubleVal dv) {
                    double d = dv.value();
                    if (d == Math.floor(d) && !Double.isInfinite(d)) yield k.apply(new SchemeValue.IntVal((long) d));
                    // Convert binary fraction to rational
                    long bits = Double.doubleToLongBits(Math.abs(d));
                    long significand = (bits & 0x000fffffffffffffL) | 0x0010000000000000L;
                    int biasedExp = (int) ((bits >> 52) & 0x7ff);
                    int exp = biasedExp - 1023 - 52;
                    long rNum = d < 0 ? -significand : significand;
                    if (exp >= 0) yield k.apply(new SchemeValue.IntVal(rNum << exp));
                    yield k.apply(SchemeValue.makeRational(rNum, 1L << (-exp)));
                }
                throw new EvalError("inexact->exact: not a number");
            }
            case "numerator" -> {
                if (a.size() != 1) throw new EvalError("numerator: needs exactly 1 argument");
                var v = a.getFirst();
                if (v instanceof SchemeValue.IntVal i) yield k.apply(new SchemeValue.IntVal(i.value()));
                if (v instanceof SchemeValue.RatVal r) yield k.apply(new SchemeValue.IntVal(r.num()));
                throw new EvalError("numerator: not a rational number");
            }
            case "denominator" -> {
                if (a.size() != 1) throw new EvalError("denominator: needs exactly 1 argument");
                var v = a.getFirst();
                if (v instanceof SchemeValue.IntVal) yield k.apply(new SchemeValue.IntVal(1));
                if (v instanceof SchemeValue.RatVal r) yield k.apply(new SchemeValue.IntVal(r.den()));
                throw new EvalError("denominator: not a rational number");
            }
            case "rational?" -> {
                if (a.size() != 1) throw new EvalError("rational?: needs exactly 1 argument");
                var v = a.getFirst();
                yield k.apply(new SchemeValue.BoolVal(v instanceof SchemeValue.IntVal || v instanceof SchemeValue.RatVal));
            }
            case "integer?" -> {
                if (a.size() != 1) throw new EvalError("integer?: needs exactly 1 argument");
                var v = a.getFirst();
                if (v instanceof SchemeValue.IntVal) yield k.apply(new SchemeValue.BoolVal(true));
                if (v instanceof SchemeValue.DoubleVal dv) yield k.apply(new SchemeValue.BoolVal(dv.value() == Math.floor(dv.value()) && !Double.isInfinite(dv.value())));
                yield k.apply(new SchemeValue.BoolVal(false));
            }
            case "dynamic-wind" -> {
                if (a.size() != 3) throw new EvalError("dynamic-wind: needs exactly 3 arguments");
                SchemeValue inThunk = a.get(0), bodyThunk = a.get(1), outThunk = a.get(2);
                WindFrame frame = new WindFrame(inThunk, outThunk);
                yield applyProc(inThunk, List.of(), _v1 -> {
                    windStack.add(frame);
                    return applyProc(bodyThunk, List.of(), bodyResult -> {
                        windStack.removeLast();
                        return applyProc(outThunk, List.of(), _v2 -> k.apply(bodyResult));
                    });
                });
            }
            default -> throw new EvalError("unknown procedure: " + name);
        };
    }

    // --- Wind transition for continuations ---

    @SuppressWarnings("unchecked")
    private Bounce doWindTransition(List<WindFrame> target, SchemeValue value, Cont capturedK) {
        int common = 0;
        int minLen = Math.min(windStack.size(), target.size());
        while (common < minLen && windStack.get(common) == target.get(common)) common++;

        // Collect thunks: out-thunks (innermost first), then in-thunks (outermost first)
        List<SchemeValue> thunks = new ArrayList<>();
        for (int i = windStack.size() - 1; i >= common; i--) {
            thunks.add(windStack.get(i).outThunk());
        }
        for (int i = common; i < target.size(); i++) {
            thunks.add(target.get(i).inThunk());
        }

        // Update wind stack atomically
        windStack.clear();
        windStack.addAll(target);

        return runThunkChain(thunks, 0, value, capturedK);
    }

    private Bounce runThunkChain(List<SchemeValue> thunks, int idx, SchemeValue value, Cont capturedK) {
        if (idx >= thunks.size()) {
            throw new ContinuationReturn(capturedK.apply(value));
        }
        return new Bounce.More(() -> applyProc(thunks.get(idx), List.of(),
            _v -> runThunkChain(thunks, idx + 1, value, capturedK)));
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

    // --- Map helper ---

    private Bounce mapLoop(SchemeValue proc, List<SchemeValue> lists, Cont k) {
        // Check if any list is nil (done)
        for (var lst : lists) {
            if (lst instanceof SchemeValue.NilVal) return k.apply(new SchemeValue.NilVal());
        }
        // Extract cars and cdrs
        List<SchemeValue> cars = new ArrayList<>();
        List<SchemeValue> cdrs = new ArrayList<>();
        for (var lst : lists) {
            if (!(lst instanceof SchemeValue.PairVal p)) throw new EvalError("map: not a proper list");
            cars.add(p.car());
            cdrs.add(p.cdr());
        }
        return new Bounce.More(() -> applyProc(proc, cars, head ->
            new Bounce.More(() -> mapLoop(proc, cdrs, tail ->
                k.apply(new SchemeValue.PairVal(head, tail))))));
    }

    // --- Helpers ---

    private long asInt(SchemeValue v) {
        if (v instanceof SchemeValue.IntVal i) return i.value();
        throw new EvalError("expected number, got " + v.display());
    }

    private long[] toRat(SchemeValue v) {
        if (v instanceof SchemeValue.IntVal i) return new long[]{i.value(), 1};
        if (v instanceof SchemeValue.RatVal r) return new long[]{r.num(), r.den()};
        throw new EvalError("expected exact number, got " + v.display());
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

    @FunctionalInterface interface DoubleCmp { boolean test(double a, double b); }

    private Bounce numCmpOp(List<SchemeValue> a, DoubleCmp cmp, Cont k) {
        if (a.size() < 2) throw new EvalError("comparison needs at least 2 arguments");
        if (allInts(a)) {
            long prev = ((SchemeValue.IntVal)a.getFirst()).value();
            for (int i = 1; i < a.size(); i++) {
                long cur = ((SchemeValue.IntVal)a.get(i)).value();
                if (!cmp.test(prev, cur)) return k.apply(new SchemeValue.BoolVal(false));
                prev = cur;
            }
            return k.apply(new SchemeValue.BoolVal(true));
        }
        double prev = SchemeValue.toDouble(a.getFirst());
        for (int i = 1; i < a.size(); i++) {
            double cur = SchemeValue.toDouble(a.get(i));
            if (!cmp.test(prev, cur)) return k.apply(new SchemeValue.BoolVal(false));
            prev = cur;
        }
        return k.apply(new SchemeValue.BoolVal(true));
    }

    private static boolean allInts(List<SchemeValue> a) {
        for (var x : a) if (!(x instanceof SchemeValue.IntVal)) return false;
        return true;
    }

    private static boolean hasDouble(List<SchemeValue> a) {
        for (var x : a) if (x instanceof SchemeValue.DoubleVal) return true;
        return false;
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
        if (SchemeValue.isNumber(a) && SchemeValue.isNumber(b)) return SchemeValue.toDouble(a) == SchemeValue.toDouble(b);
        if (a instanceof SchemeValue.IntVal ia && b instanceof SchemeValue.IntVal ib) return ia.value() == ib.value();
        if (a instanceof SchemeValue.BoolVal ba && b instanceof SchemeValue.BoolVal bb) return ba.value() == bb.value();
        if (a instanceof SchemeValue.StringVal sa && b instanceof SchemeValue.StringVal sb) return sa.value().equals(sb.value());
        if (a instanceof SchemeValue.SymbolVal sa && b instanceof SchemeValue.SymbolVal sb) return sa.name().equals(sb.name());
        if (a instanceof SchemeValue.CharVal ca && b instanceof SchemeValue.CharVal cb) return ca.value() == cb.value();
        if (a instanceof SchemeValue.NilVal && b instanceof SchemeValue.NilVal) return true;
        if (a instanceof SchemeValue.PairVal pa && b instanceof SchemeValue.PairVal pb)
            return schemeEqual(pa.car(), pb.car()) && schemeEqual(pa.cdr(), pb.cdr());
        if (a instanceof SchemeValue.VectorVal va && b instanceof SchemeValue.VectorVal vb) {
            if (va.length() != vb.length()) return false;
            for (int i = 0; i < va.length(); i++)
                if (!schemeEqual(va.get(i), vb.get(i))) return false;
            return true;
        }
        if (a instanceof SchemeValue.VoidVal && b instanceof SchemeValue.VoidVal) return true;
        return false;
    }

    private boolean schemeEqv(SchemeValue a, SchemeValue b) {
        if (a instanceof SchemeValue.RatVal ra && b instanceof SchemeValue.RatVal rb) return ra.num() == rb.num() && ra.den() == rb.den();
        if (a instanceof SchemeValue.DoubleVal da && b instanceof SchemeValue.DoubleVal db) return da.value() == db.value();
        if (a instanceof SchemeValue.IntVal ia && b instanceof SchemeValue.IntVal ib) return ia.value() == ib.value();
        if (a instanceof SchemeValue.BoolVal ba && b instanceof SchemeValue.BoolVal bb) return ba.value() == bb.value();
        if (a instanceof SchemeValue.SymbolVal sa && b instanceof SchemeValue.SymbolVal sb) return sa.name().equals(sb.name());
        if (a instanceof SchemeValue.CharVal ca && b instanceof SchemeValue.CharVal cb) return ca.value() == cb.value();
        if (a instanceof SchemeValue.NilVal && b instanceof SchemeValue.NilVal) return true;
        if (a instanceof SchemeValue.VoidVal && b instanceof SchemeValue.VoidVal) return true;
        return a == b; // reference identity for mutable types
    }

    private static EvalError withPos(EvalError e, int line, int col) {
        if (line == 0 && col == 0) return e;
        String msg = e.getMessage();
        if (msg != null && msg.matches(".*\\d+:\\d+.*")) return e;
        return new EvalError(msg + " at " + line + ":" + col);
    }
}
