package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

public class Evaluator {
    private final StringBuilder outputBuffer = new StringBuilder();
    private int gensymCounter = 0;

    private String gensym(String prefix) {
        return prefix + "__" + (gensymCounter++);
    }

    private static final Set<String> SPECIAL_FORMS = Set.of(
        "quote", "set!", "define", "lambda", "if", "begin", "cond", "and", "or",
        "let", "define-syntax", "syntax-rules"
    );

    public String evalStr(String input) throws EvalError {
        var tokens = new Tokenizer(input).tokenize();
        var exprs = new Parser(tokens).parseAll();
        if (exprs.isEmpty()) throw new EvalError("No expressions");
        Environment env = createGlobalEnv();
        SchemeValue result = trampoline(evalProgram(exprs, 0, env));
        if (result instanceof SchemeValue.VoidVal) return "#<void>";
        return result.display();
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        outputBuffer.setLength(0);
        var tokens = new Tokenizer(input).tokenize();
        var exprs = new Parser(tokens).parseAll();
        if (exprs.isEmpty()) throw new EvalError("No expressions");
        Environment env = createGlobalEnv();
        SchemeValue result = trampoline(evalProgram(exprs, 0, env));
        String resultStr = (result instanceof SchemeValue.VoidVal) ? "#<void>" : result.display();
        return new EvalResult(resultStr, outputBuffer.toString());
    }

    private Bounce evalProgram(List<SchemeValue> exprs, int idx, Environment env) {
        if (idx >= exprs.size()) return new Bounce.Done(new SchemeValue.VoidVal());
        if (idx == exprs.size() - 1) {
            return new Bounce.More(() -> eval(exprs.get(idx), env, v -> new Bounce.Done(v)));
        }
        return new Bounce.More(() -> eval(exprs.get(idx), env, ignored ->
            evalProgram(exprs, idx + 1, env)
        ));
    }

    private SchemeValue trampoline(Bounce bounce) throws EvalError {
        while (true) {
            switch (bounce) {
                case Bounce.Done d -> { return d.value(); }
                case Bounce.Err e -> { throw e.error(); }
                case Bounce.More m -> { bounce = m.thunk().get(); }
            }
        }
    }

    private Environment createGlobalEnv() {
        Environment env = new Environment();
        registerBuiltins(env);
        return env;
    }

    private void registerBuiltins(Environment env) {
        // Arithmetic
        env.define("+", new SchemeValue.BuiltinVal("+", args -> {
            long sum = 0;
            for (SchemeValue arg : args) sum += requireInt(arg);
            return new SchemeValue.IntVal(sum);
        }));
        env.define("-", new SchemeValue.BuiltinVal("-", args -> {
            if (args.isEmpty()) throw new EvalError("- requires at least 1 argument");
            if (args.size() == 1) return new SchemeValue.IntVal(-requireInt(args.getFirst()));
            long result = requireInt(args.getFirst());
            for (int i = 1; i < args.size(); i++) result -= requireInt(args.get(i));
            return new SchemeValue.IntVal(result);
        }));
        env.define("*", new SchemeValue.BuiltinVal("*", args -> {
            long product = 1;
            for (SchemeValue arg : args) product *= requireInt(arg);
            return new SchemeValue.IntVal(product);
        }));
        env.define("/", new SchemeValue.BuiltinVal("/", args -> {
            if (args.size() < 2) throw new EvalError("/ requires at least 2 arguments");
            long result = requireInt(args.getFirst());
            for (int i = 1; i < args.size(); i++) {
                long divisor = requireInt(args.get(i));
                if (divisor == 0) throw new EvalError("Division by zero");
                result /= divisor;
            }
            return new SchemeValue.IntVal(result);
        }));

        env.define("modulo", new SchemeValue.BuiltinVal("modulo", args -> {
            if (args.size() != 2) throw new EvalError("modulo requires 2 arguments");
            long a = requireInt(args.get(0));
            long b = requireInt(args.get(1));
            if (b == 0) throw new EvalError("Division by zero");
            return new SchemeValue.IntVal(Math.floorMod(a, b));
        }));
        env.define("remainder", new SchemeValue.BuiltinVal("remainder", args -> {
            if (args.size() != 2) throw new EvalError("remainder requires 2 arguments");
            long a = requireInt(args.get(0));
            long b = requireInt(args.get(1));
            if (b == 0) throw new EvalError("Division by zero");
            return new SchemeValue.IntVal(a % b);
        }));
        env.define("abs", new SchemeValue.BuiltinVal("abs", args -> {
            if (args.size() != 1) throw new EvalError("abs requires 1 argument");
            return new SchemeValue.IntVal(Math.abs(requireInt(args.getFirst())));
        }));

        // Comparisons
        env.define("<", new SchemeValue.BuiltinVal("<", args ->
            new SchemeValue.BoolVal(requireInt(args.get(0)) < requireInt(args.get(1)))));
        env.define(">", new SchemeValue.BuiltinVal(">", args ->
            new SchemeValue.BoolVal(requireInt(args.get(0)) > requireInt(args.get(1)))));
        env.define("=", new SchemeValue.BuiltinVal("=", args ->
            new SchemeValue.BoolVal(requireInt(args.get(0)) == requireInt(args.get(1)))));
        env.define("<=", new SchemeValue.BuiltinVal("<=", args ->
            new SchemeValue.BoolVal(requireInt(args.get(0)) <= requireInt(args.get(1)))));
        env.define(">=", new SchemeValue.BuiltinVal(">=", args ->
            new SchemeValue.BoolVal(requireInt(args.get(0)) >= requireInt(args.get(1)))));
        env.define("not", new SchemeValue.BuiltinVal("not", args ->
            new SchemeValue.BoolVal(!args.getFirst().isTruthy())));

        // Pairs and lists
        env.define("cons", new SchemeValue.BuiltinVal("cons", args -> {
            if (args.size() != 2) throw new EvalError("cons requires exactly 2 arguments");
            return new SchemeValue.PairVal(args.get(0), args.get(1));
        }));
        env.define("car", new SchemeValue.BuiltinVal("car", args -> {
            if (args.size() != 1) throw new EvalError("car requires exactly 1 argument");
            if (args.getFirst() instanceof SchemeValue.PairVal p) return p.car();
            throw new EvalError("car: not a pair: " + args.getFirst().display());
        }));
        env.define("cdr", new SchemeValue.BuiltinVal("cdr", args -> {
            if (args.size() != 1) throw new EvalError("cdr requires exactly 1 argument");
            if (args.getFirst() instanceof SchemeValue.PairVal p) return p.cdr();
            throw new EvalError("cdr: not a pair: " + args.getFirst().display());
        }));
        env.define("null?", new SchemeValue.BuiltinVal("null?", args -> {
            if (args.size() != 1) throw new EvalError("null? requires exactly 1 argument");
            return new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.NilVal);
        }));
        env.define("list", new SchemeValue.BuiltinVal("list", args -> {
            SchemeValue result = SchemeValue.NIL;
            for (int i = args.size() - 1; i >= 0; i--) {
                result = new SchemeValue.PairVal(args.get(i), result);
            }
            return result;
        }));
        env.define("length", new SchemeValue.BuiltinVal("length", args -> {
            if (args.size() != 1) throw new EvalError("length requires exactly 1 argument");
            SchemeValue lst = args.getFirst();
            long count = 0;
            while (lst instanceof SchemeValue.PairVal p) {
                count++;
                lst = p.cdr();
            }
            return new SchemeValue.IntVal(count);
        }));
        env.define("append", new SchemeValue.BuiltinVal("append", args -> {
            if (args.isEmpty()) return SchemeValue.NIL;
            if (args.size() == 1) return args.getFirst();
            SchemeValue result = args.getLast();
            for (int i = args.size() - 2; i >= 0; i--) {
                result = appendTwo(args.get(i), result);
            }
            return result;
        }));

        // Type predicates
        env.define("string?", new SchemeValue.BuiltinVal("string?", args ->
            new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.StringVal)));
        env.define("number?", new SchemeValue.BuiltinVal("number?", args ->
            new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.IntVal)));
        env.define("boolean?", new SchemeValue.BuiltinVal("boolean?", args ->
            new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.BoolVal)));
        env.define("pair?", new SchemeValue.BuiltinVal("pair?", args ->
            new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.PairVal)));
        env.define("symbol?", new SchemeValue.BuiltinVal("symbol?", args ->
            new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.SymbolVal)));
        env.define("char?", new SchemeValue.BuiltinVal("char?", args ->
            new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.CharVal)));
        env.define("procedure?", new SchemeValue.BuiltinVal("procedure?", args ->
            new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.LambdaVal
                || args.getFirst() instanceof SchemeValue.BuiltinVal
                || args.getFirst() instanceof SchemeValue.CpsBuiltinVal
                || args.getFirst() instanceof SchemeValue.ContinuationVal)));

        // I/O
        env.define("display", new SchemeValue.BuiltinVal("display", args -> {
            if (args.size() != 1) throw new EvalError("display requires exactly 1 argument");
            SchemeValue val = args.getFirst();
            if (val instanceof SchemeValue.StringVal s) {
                outputBuffer.append(s.value());
            } else if (val instanceof SchemeValue.CharVal c) {
                outputBuffer.append(c.value());
            } else {
                outputBuffer.append(val.display());
            }
            return new SchemeValue.VoidVal();
        }));
        env.define("write", new SchemeValue.BuiltinVal("write", args -> {
            if (args.size() != 1) throw new EvalError("write requires exactly 1 argument");
            outputBuffer.append(args.getFirst().display());
            return new SchemeValue.VoidVal();
        }));
        env.define("newline", new SchemeValue.BuiltinVal("newline", args -> {
            outputBuffer.append('\n');
            return new SchemeValue.VoidVal();
        }));

        // String operations
        env.define("string-append", new SchemeValue.BuiltinVal("string-append", args -> {
            StringBuilder sb = new StringBuilder();
            for (SchemeValue arg : args) {
                if (!(arg instanceof SchemeValue.StringVal s)) throw new EvalError("string-append: not a string");
                sb.append(s.value());
            }
            return new SchemeValue.StringVal(sb.toString());
        }));
        env.define("string-length", new SchemeValue.BuiltinVal("string-length", args -> {
            if (!(args.getFirst() instanceof SchemeValue.StringVal s)) throw new EvalError("string-length: not a string");
            return new SchemeValue.IntVal(s.value().length());
        }));
        env.define("substring", new SchemeValue.BuiltinVal("substring", args -> {
            if (!(args.get(0) instanceof SchemeValue.StringVal s)) throw new EvalError("substring: not a string");
            int start = (int) requireInt(args.get(1));
            int end = (int) requireInt(args.get(2));
            return new SchemeValue.StringVal(s.value().substring(start, end));
        }));
        env.define("string->number", new SchemeValue.BuiltinVal("string->number", args -> {
            if (!(args.getFirst() instanceof SchemeValue.StringVal s)) throw new EvalError("string->number: not a string");
            try {
                return new SchemeValue.IntVal(Long.parseLong(s.value()));
            } catch (NumberFormatException e) {
                return new SchemeValue.BoolVal(false);
            }
        }));
        env.define("number->string", new SchemeValue.BuiltinVal("number->string", args ->
            new SchemeValue.StringVal(String.valueOf(requireInt(args.getFirst())))));
        env.define("symbol->string", new SchemeValue.BuiltinVal("symbol->string", args -> {
            if (!(args.getFirst() instanceof SchemeValue.SymbolVal s)) throw new EvalError("symbol->string: not a symbol");
            return new SchemeValue.StringVal(s.name());
        }));
        env.define("string->symbol", new SchemeValue.BuiltinVal("string->symbol", args -> {
            if (!(args.getFirst() instanceof SchemeValue.StringVal s)) throw new EvalError("string->symbol: not a string");
            return new SchemeValue.SymbolVal(s.value());
        }));
        env.define("string-ref", new SchemeValue.BuiltinVal("string-ref", args -> {
            if (!(args.get(0) instanceof SchemeValue.StringVal s)) throw new EvalError("string-ref: not a string");
            int idx = (int) requireInt(args.get(1));
            return new SchemeValue.CharVal(s.charAt(idx));
        }));
        env.define("string-copy", new SchemeValue.BuiltinVal("string-copy", args -> {
            if (!(args.getFirst() instanceof SchemeValue.StringVal s)) throw new EvalError("string-copy: not a string");
            return new SchemeValue.StringVal(s.value());
        }));

        // string-set!
        env.define("string-set!", new SchemeValue.BuiltinVal("string-set!", args -> {
            if (args.size() != 3) throw new EvalError("string-set! requires 3 arguments");
            if (!(args.get(0) instanceof SchemeValue.StringVal s)) throw new EvalError("string-set!: not a string");
            int idx = (int) requireInt(args.get(1));
            if (!(args.get(2) instanceof SchemeValue.CharVal c)) throw new EvalError("string-set!: not a character");
            s.setCharAt(idx, c.value());
            return new SchemeValue.VoidVal();
        }));

        // apply (CPS-aware: needs to forward continuation)
        env.define("apply", new SchemeValue.CpsBuiltinVal("apply", (args, k) -> {
            if (args.size() < 2) return new Bounce.Err(new EvalError("apply requires at least 2 arguments"));
            SchemeValue proc = args.getFirst();
            List<SchemeValue> callArgs = new ArrayList<>();
            for (int i = 1; i < args.size() - 1; i++) {
                callArgs.add(args.get(i));
            }
            SchemeValue last = args.getLast();
            while (last instanceof SchemeValue.PairVal p) {
                callArgs.add(p.car());
                last = p.cdr();
            }
            if (!(last instanceof SchemeValue.NilVal)) {
                return new Bounce.Err(new EvalError("apply: last argument must be a proper list"));
            }
            return applyProc(proc, callArgs, "apply", k);
        }));

        // call/cc and call-with-current-continuation
        SchemeValue.CpsBuiltinFunc callccFunc = (args, k) -> {
            if (args.size() != 1) return new Bounce.Err(new EvalError("call/cc requires 1 argument"));
            SchemeValue f = args.getFirst();
            SchemeValue cont = new SchemeValue.ContinuationVal(k);
            return applyProc(f, List.of(cont), "call/cc", k);
        };
        env.define("call/cc", new SchemeValue.CpsBuiltinVal("call/cc", callccFunc));
        env.define("call-with-current-continuation", new SchemeValue.CpsBuiltinVal("call-with-current-continuation", callccFunc));
    }

    // ── CPS eval ──────────────────────────────────────────────────────

    private Bounce eval(SchemeValue expr, Environment env, SchemeValue.Cont k) {
        return switch (expr) {
            case SchemeValue.IntVal v -> k.apply(v);
            case SchemeValue.BoolVal v -> k.apply(v);
            case SchemeValue.StringVal v -> k.apply(v);
            case SchemeValue.CharVal v -> k.apply(v);
            case SchemeValue.VoidVal v -> k.apply(v);
            case SchemeValue.NilVal v -> k.apply(v);
            case SchemeValue.PairVal v -> k.apply(v);
            case SchemeValue.LambdaVal v -> k.apply(v);
            case SchemeValue.BuiltinVal v -> k.apply(v);
            case SchemeValue.CpsBuiltinVal v -> k.apply(v);
            case SchemeValue.ContinuationVal v -> k.apply(v);
            case SchemeValue.SyntaxRulesVal v -> k.apply(v);
            case SchemeValue.SymbolVal v -> {
                try {
                    yield k.apply(env.get(v.name()));
                } catch (EvalError e) {
                    yield new Bounce.Err(new EvalError("Unbound variable: " + v.name() + " at " + v.line() + ":" + v.col()));
                }
            }
            case SchemeValue.ListVal listVal -> evalList(listVal, env, k);
        };
    }

    private Bounce evalList(SchemeValue.ListVal listVal, Environment env, SchemeValue.Cont k) {
        List<SchemeValue> elems = listVal.elements();
        String pos = listVal.line() + ":" + listVal.col();
        if (elems.isEmpty()) return new Bounce.Err(new EvalError("Empty application at " + pos));
        SchemeValue head = elems.getFirst();

        // Check special forms
        if (head instanceof SchemeValue.SymbolVal sym) {
            Bounce special = evalSpecialForm(sym.name(), elems, env, pos, k);
            if (special != null) return special;

            // Check for macro invocation
            try {
                SchemeValue val = env.get(sym.name());
                if (val instanceof SchemeValue.SyntaxRulesVal macro) {
                    SchemeValue expanded = expandMacro(macro, elems, env);
                    return new Bounce.More(() -> eval(expanded, env, k));
                }
            } catch (EvalError ignored) {}
        }

        // Procedure call: eval head, eval args (right-to-left for Guile compat), apply
        return new Bounce.More(() -> eval(head, env, proc ->
            evalArgsRTL(elems, elems.size() - 1, 1, List.of(), env, args ->
                applyProc(proc, args, pos, k)
            )
        ));
    }

    private Bounce evalSpecialForm(String name, List<SchemeValue> elems, Environment env,
                                    String pos, SchemeValue.Cont k) {
        return switch (name) {
            case "quote" -> {
                if (elems.size() != 2)
                    yield new Bounce.Err(new EvalError("quote requires exactly 1 argument at " + pos));
                yield k.apply(quoteDatum(elems.get(1)));
            }
            case "set!" -> {
                if (elems.size() != 3)
                    yield new Bounce.Err(new EvalError("set! requires exactly 2 arguments at " + pos));
                if (!(elems.get(1) instanceof SchemeValue.SymbolVal sym2))
                    yield new Bounce.Err(new EvalError("set!: expected symbol"));
                yield new Bounce.More(() -> eval(elems.get(2), env, val -> {
                    try {
                        env.set(sym2.name(), val);
                        return k.apply(new SchemeValue.VoidVal());
                    } catch (EvalError e) {
                        return new Bounce.Err(e);
                    }
                }));
            }
            case "define" -> evalDefine(elems, env, pos, k);
            case "lambda" -> {
                try {
                    yield k.apply(buildLambda(elems, env, pos));
                } catch (EvalError e) {
                    yield new Bounce.Err(e);
                }
            }
            case "if" -> {
                if (elems.size() < 3 || elems.size() > 4)
                    yield new Bounce.Err(new EvalError("if requires 2 or 3 arguments at " + pos));
                yield new Bounce.More(() -> eval(elems.get(1), env, condVal -> {
                    if (condVal.isTruthy()) {
                        return new Bounce.More(() -> eval(elems.get(2), env, k));
                    } else if (elems.size() == 4) {
                        return new Bounce.More(() -> eval(elems.get(3), env, k));
                    }
                    return k.apply(new SchemeValue.VoidVal());
                }));
            }
            case "begin" -> evalSequence(elems, 1, env, k);
            case "cond" -> evalCond(elems, 1, env, k);
            case "and" -> {
                if (elems.size() == 1) yield k.apply(new SchemeValue.BoolVal(true));
                yield evalAnd(elems, 1, env, k);
            }
            case "or" -> {
                if (elems.size() == 1) yield k.apply(new SchemeValue.BoolVal(false));
                yield evalOr(elems, 1, env, k);
            }
            case "let" -> evalLet(elems, env, pos, k);
            case "define-syntax" -> evalDefineSyntax(elems, env, pos, k);
            default -> null; // not a special form
        };
    }

    // ── Sequence / body evaluation ────────────────────────────────────

    private Bounce evalSequence(List<SchemeValue> exprs, int start, Environment env, SchemeValue.Cont k) {
        if (start >= exprs.size()) return k.apply(new SchemeValue.VoidVal());
        return evalSeqFrom(exprs, start, env, k);
    }

    private Bounce evalSeqFrom(List<SchemeValue> exprs, int idx, Environment env, SchemeValue.Cont k) {
        if (idx == exprs.size() - 1) {
            return new Bounce.More(() -> eval(exprs.get(idx), env, k));
        }
        return new Bounce.More(() -> eval(exprs.get(idx), env, ignored ->
            evalSeqFrom(exprs, idx + 1, env, k)
        ));
    }

    private Bounce evalBody(List<SchemeValue> body, Environment env, SchemeValue.Cont k) {
        return evalSeqFrom(body, 0, env, k);
    }

    // ── Argument evaluation ───────────────────────────────────────────

    /** Evaluate args right-to-left (Guile-compatible), returning them in left-to-right order. */
    private Bounce evalArgsRTL(List<SchemeValue> elems, int idx, int start,
                                List<SchemeValue> acc, Environment env,
                                Function<List<SchemeValue>, Bounce> then) {
        if (idx < start) return then.apply(acc);
        return new Bounce.More(() -> eval(elems.get(idx), env, val -> {
            // Prepend val to acc — builds left-to-right order as we iterate right-to-left
            List<SchemeValue> newAcc = new ArrayList<>(acc.size() + 1);
            newAcc.add(val);
            newAcc.addAll(acc);
            return evalArgsRTL(elems, idx - 1, start, newAcc, env, then);
        }));
    }

    private Bounce evalInitExprs(List<SchemeValue> exprs, int idx, List<SchemeValue> acc,
                                  Environment env, Function<List<SchemeValue>, Bounce> then) {
        if (idx >= exprs.size()) return then.apply(acc);
        return new Bounce.More(() -> eval(exprs.get(idx), env, val -> {
            List<SchemeValue> newAcc = new ArrayList<>(acc);
            newAcc.add(val);
            return evalInitExprs(exprs, idx + 1, newAcc, env, then);
        }));
    }

    // ── Procedure application ─────────────────────────────────────────

    private Bounce applyProc(SchemeValue proc, List<SchemeValue> args, String pos, SchemeValue.Cont k) {
        if (proc instanceof SchemeValue.BuiltinVal builtin) {
            try {
                SchemeValue result = builtin.func().apply(args);
                return k.apply(result);
            } catch (EvalError e) {
                String msg = e.getMessage();
                if (!msg.matches(".*\\d+:\\d+.*")) {
                    return new Bounce.Err(new EvalError(msg + " at " + pos));
                }
                return new Bounce.Err(e);
            }
        }
        if (proc instanceof SchemeValue.CpsBuiltinVal cps) {
            return cps.func().apply(args, k);
        }
        if (proc instanceof SchemeValue.LambdaVal lambda) {
            try {
                Environment callEnv = applyLambdaEnv(lambda, args, pos);
                return evalBody(lambda.body(), callEnv, k);
            } catch (EvalError e) {
                return new Bounce.Err(e);
            }
        }
        if (proc instanceof SchemeValue.ContinuationVal contVal) {
            if (args.size() != 1) {
                return new Bounce.Err(new EvalError("continuation requires exactly 1 argument at " + pos));
            }
            // Invoke the captured continuation — caller's k is discarded
            return contVal.k().apply(args.getFirst());
        }
        return new Bounce.Err(new EvalError("Not a procedure: " + proc.display() + " at " + pos));
    }

    // ── Special form helpers ──────────────────────────────────────────

    private Bounce evalDefine(List<SchemeValue> elems, Environment env, String pos, SchemeValue.Cont k) {
        if (elems.size() < 3)
            return new Bounce.Err(new EvalError("define requires at least 2 arguments at " + pos));
        SchemeValue target = elems.get(1);
        if (target instanceof SchemeValue.SymbolVal sym) {
            return new Bounce.More(() -> eval(elems.get(2), env, val -> {
                env.define(sym.name(), val);
                return k.apply(new SchemeValue.VoidVal());
            }));
        } else if (target instanceof SchemeValue.ListVal nameAndParams) {
            try {
                List<SchemeValue> parts = nameAndParams.elements();
                if (parts.isEmpty()) throw new EvalError("define: empty name list");
                if (!(parts.getFirst() instanceof SchemeValue.SymbolVal fnName))
                    throw new EvalError("define: expected symbol as function name");
                List<String> params = new ArrayList<>();
                String restParam = null;
                for (int i = 1; i < parts.size(); i++) {
                    if (parts.get(i) instanceof SchemeValue.SymbolVal p && p.name().equals(".")) {
                        if (i + 1 >= parts.size()) throw new EvalError("define: expected symbol after dot");
                        if (!(parts.get(i + 1) instanceof SchemeValue.SymbolVal rest))
                            throw new EvalError("define: expected symbol after dot");
                        restParam = rest.name();
                        break;
                    }
                    if (!(parts.get(i) instanceof SchemeValue.SymbolVal p))
                        throw new EvalError("define: expected symbol as parameter");
                    params.add(p.name());
                }
                List<SchemeValue> body = elems.subList(2, elems.size());
                SchemeValue.LambdaVal lambda = new SchemeValue.LambdaVal(params, restParam, body, env);
                env.define(fnName.name(), lambda);
                return k.apply(new SchemeValue.VoidVal());
            } catch (EvalError e) {
                return new Bounce.Err(e);
            }
        }
        return new Bounce.Err(new EvalError("define: invalid syntax"));
    }

    private Bounce evalCond(List<SchemeValue> elems, int idx, Environment env, SchemeValue.Cont k) {
        if (idx >= elems.size()) return k.apply(new SchemeValue.VoidVal());
        if (!(elems.get(idx) instanceof SchemeValue.ListVal clause))
            return new Bounce.Err(new EvalError("cond: expected clause"));
        List<SchemeValue> parts = clause.elements();
        if (parts.isEmpty()) return new Bounce.Err(new EvalError("cond: empty clause"));
        boolean isElse = parts.getFirst() instanceof SchemeValue.SymbolVal s && s.name().equals("else");
        if (isElse) {
            if (parts.size() > 1) return evalSeqFrom(parts, 1, env, k);
            return k.apply(new SchemeValue.VoidVal());
        }
        return new Bounce.More(() -> eval(parts.getFirst(), env, testVal -> {
            if (testVal.isTruthy()) {
                if (parts.size() > 1) return evalSeqFrom(parts, 1, env, k);
                return k.apply(testVal);
            }
            return new Bounce.More(() -> evalCond(elems, idx + 1, env, k));
        }));
    }

    private Bounce evalAnd(List<SchemeValue> elems, int idx, Environment env, SchemeValue.Cont k) {
        if (idx == elems.size() - 1) {
            return new Bounce.More(() -> eval(elems.get(idx), env, k));
        }
        return new Bounce.More(() -> eval(elems.get(idx), env, val -> {
            if (!val.isTruthy()) return k.apply(val);
            return evalAnd(elems, idx + 1, env, k);
        }));
    }

    private Bounce evalOr(List<SchemeValue> elems, int idx, Environment env, SchemeValue.Cont k) {
        if (idx == elems.size() - 1) {
            return new Bounce.More(() -> eval(elems.get(idx), env, k));
        }
        return new Bounce.More(() -> eval(elems.get(idx), env, val -> {
            if (val.isTruthy()) return k.apply(val);
            return evalOr(elems, idx + 1, env, k);
        }));
    }

    private Bounce evalLet(List<SchemeValue> elems, Environment env, String pos, SchemeValue.Cont k) {
        if (elems.size() < 3)
            return new Bounce.Err(new EvalError("let requires bindings and body at " + pos));

        // Named let: (let name ((var init) ...) body...)
        if (elems.get(1) instanceof SchemeValue.SymbolVal nameSym) {
            if (elems.size() < 4)
                return new Bounce.Err(new EvalError("named let requires bindings and body"));
            String loopName = nameSym.name();
            if (!(elems.get(2) instanceof SchemeValue.ListVal bindingsList))
                return new Bounce.Err(new EvalError("let: expected bindings list"));
            List<String> params = new ArrayList<>();
            List<SchemeValue> initExprs = new ArrayList<>();
            for (SchemeValue b : bindingsList.elements()) {
                if (!(b instanceof SchemeValue.ListVal binding) || binding.elements().size() != 2)
                    return new Bounce.Err(new EvalError("let: invalid binding"));
                if (!(binding.elements().get(0) instanceof SchemeValue.SymbolVal s))
                    return new Bounce.Err(new EvalError("let: expected symbol in binding"));
                params.add(s.name());
                initExprs.add(binding.elements().get(1));
            }
            List<SchemeValue> body = elems.subList(3, elems.size());
            return evalInitExprs(initExprs, 0, new ArrayList<>(), env, inits -> {
                Environment letEnv = new Environment(env);
                SchemeValue.LambdaVal lambda = new SchemeValue.LambdaVal(params, body, letEnv);
                letEnv.define(loopName, lambda);
                Environment callEnv = new Environment(lambda.env());
                for (int i = 0; i < params.size(); i++) {
                    callEnv.define(params.get(i), inits.get(i));
                }
                return evalBody(body, callEnv, k);
            });
        }

        // Regular let
        if (!(elems.get(1) instanceof SchemeValue.ListVal bindingsList))
            return new Bounce.Err(new EvalError("let: expected bindings list"));
        Environment letEnv = new Environment(env);
        List<SchemeValue> body = elems.subList(2, elems.size());
        return evalLetBindings(bindingsList.elements(), 0, env, letEnv, body, k);
    }

    private Bounce evalLetBindings(List<SchemeValue> bindings, int idx, Environment outerEnv,
                                    Environment letEnv, List<SchemeValue> body, SchemeValue.Cont k) {
        if (idx >= bindings.size()) return evalBody(body, letEnv, k);
        SchemeValue b = bindings.get(idx);
        if (!(b instanceof SchemeValue.ListVal binding) || binding.elements().size() != 2)
            return new Bounce.Err(new EvalError("let: invalid binding"));
        if (!(binding.elements().get(0) instanceof SchemeValue.SymbolVal s))
            return new Bounce.Err(new EvalError("let: expected symbol in binding"));
        return new Bounce.More(() -> eval(binding.elements().get(1), outerEnv, val -> {
            letEnv.define(s.name(), val);
            return evalLetBindings(bindings, idx + 1, outerEnv, letEnv, body, k);
        }));
    }

    // ── Lambda construction (no CPS needed — doesn't eval anything) ──

    private SchemeValue buildLambda(List<SchemeValue> elems, Environment env, String pos) throws EvalError {
        if (elems.size() < 3) throw new EvalError("lambda requires parameters and body at " + pos);
        SchemeValue paramSpec = elems.get(1);
        if (!(paramSpec instanceof SchemeValue.ListVal paramList))
            throw new EvalError("lambda: expected parameter list");
        List<String> params = new ArrayList<>();
        String restParam = null;
        List<SchemeValue> pElems = paramList.elements();
        for (int i = 0; i < pElems.size(); i++) {
            if (pElems.get(i) instanceof SchemeValue.SymbolVal sym && sym.name().equals(".")) {
                if (i + 1 >= pElems.size()) throw new EvalError("lambda: expected symbol after dot");
                if (!(pElems.get(i + 1) instanceof SchemeValue.SymbolVal rest))
                    throw new EvalError("lambda: expected symbol after dot");
                restParam = rest.name();
                break;
            }
            if (!(pElems.get(i) instanceof SchemeValue.SymbolVal sym))
                throw new EvalError("lambda: expected symbol as parameter");
            params.add(sym.name());
        }
        List<SchemeValue> body = elems.subList(2, elems.size());
        return new SchemeValue.LambdaVal(params, restParam, body, env);
    }

    // ── Helpers ───────────────────────────────────────────────────────

    private SchemeValue appendTwo(SchemeValue a, SchemeValue b) throws EvalError {
        if (a instanceof SchemeValue.NilVal) return b;
        if (a instanceof SchemeValue.PairVal p) {
            return new SchemeValue.PairVal(p.car(), appendTwo(p.cdr(), b));
        }
        throw new EvalError("append: not a proper list");
    }

    private SchemeValue quoteDatum(SchemeValue v) {
        if (v instanceof SchemeValue.ListVal list) {
            if (list.elements().isEmpty()) return SchemeValue.NIL;
            SchemeValue result = SchemeValue.NIL;
            for (int i = list.elements().size() - 1; i >= 0; i--) {
                result = new SchemeValue.PairVal(quoteDatum(list.elements().get(i)), result);
            }
            return result;
        }
        return v;
    }

    private Environment applyLambdaEnv(SchemeValue.LambdaVal lambda, List<SchemeValue> args, String pos) throws EvalError {
        int required = lambda.params().size();
        if (lambda.restParam() != null) {
            if (args.size() < required)
                throw new EvalError("Expected at least " + required + " arguments, got " + args.size() + " at " + pos);
        } else {
            if (args.size() != required)
                throw new EvalError("Expected " + required + " arguments, got " + args.size() + " at " + pos);
        }
        Environment callEnv = new Environment(lambda.env());
        for (int i = 0; i < required; i++) {
            callEnv.define(lambda.params().get(i), args.get(i));
        }
        if (lambda.restParam() != null) {
            SchemeValue rest = SchemeValue.NIL;
            for (int i = args.size() - 1; i >= required; i--) {
                rest = new SchemeValue.PairVal(args.get(i), rest);
            }
            callEnv.define(lambda.restParam(), rest);
        }
        return callEnv;
    }

    // ── Macro system (define-syntax / syntax-rules) ────────────────

    private Bounce evalDefineSyntax(List<SchemeValue> elems, Environment env, String pos, SchemeValue.Cont k) {
        if (elems.size() != 3)
            return new Bounce.Err(new EvalError("define-syntax requires 2 arguments at " + pos));
        if (!(elems.get(1) instanceof SchemeValue.SymbolVal nameSym))
            return new Bounce.Err(new EvalError("define-syntax: expected symbol"));
        SchemeValue transformer = elems.get(2);
        if (!(transformer instanceof SchemeValue.ListVal tList) || tList.elements().isEmpty())
            return new Bounce.Err(new EvalError("define-syntax: expected syntax-rules"));
        List<SchemeValue> tElems = tList.elements();
        if (!(tElems.getFirst() instanceof SchemeValue.SymbolVal sr) || !sr.name().equals("syntax-rules"))
            return new Bounce.Err(new EvalError("define-syntax: expected syntax-rules"));
        if (tElems.size() < 2)
            return new Bounce.Err(new EvalError("syntax-rules requires literals list"));
        // Parse literals list
        if (!(tElems.get(1) instanceof SchemeValue.ListVal litList))
            return new Bounce.Err(new EvalError("syntax-rules: expected literals list"));
        List<String> literals = new ArrayList<>();
        for (SchemeValue lit : litList.elements()) {
            if (lit instanceof SchemeValue.SymbolVal s) literals.add(s.name());
        }
        // Parse pattern-template pairs
        List<SchemeValue> patterns = new ArrayList<>();
        List<SchemeValue> templates = new ArrayList<>();
        for (int i = 2; i < tElems.size(); i++) {
            if (!(tElems.get(i) instanceof SchemeValue.ListVal clause) || clause.elements().size() != 2)
                return new Bounce.Err(new EvalError("syntax-rules: invalid clause"));
            patterns.add(clause.elements().get(0));
            templates.add(clause.elements().get(1));
        }
        env.define(nameSym.name(), new SchemeValue.SyntaxRulesVal(literals, patterns, templates, env));
        return k.apply(new SchemeValue.VoidVal());
    }

    private SchemeValue expandMacro(SchemeValue.SyntaxRulesVal macro, List<SchemeValue> inputElems,
                                     Environment env) throws EvalError {
        for (int i = 0; i < macro.patterns().size(); i++) {
            SchemeValue pattern = macro.patterns().get(i);
            SchemeValue template = macro.templates().get(i);
            if (!(pattern instanceof SchemeValue.ListVal patList)) continue;

            Map<String, SchemeValue> singles = new HashMap<>();
            Map<String, List<SchemeValue>> lists = new HashMap<>();
            if (matchPattern(patList.elements(), inputElems, 1, 1, macro.literals(), singles, lists)) {
                // Collect pattern variable names
                Set<String> patVars = new HashSet<>(singles.keySet());
                patVars.addAll(lists.keySet());

                // Collect free symbols in template for hygiene
                Set<String> freeSyms = new HashSet<>();
                collectFreeSymbols(template, patVars, freeSyms);

                // Generate renames for hygiene
                Map<String, String> renames = new HashMap<>();
                for (String sym : freeSyms) {
                    renames.put(sym, gensym(sym));
                }

                // Expand template
                SchemeValue expanded = expandTemplate(template, singles, lists, renames);

                // Pre-bind renamed symbols from definition env
                for (var entry : renames.entrySet()) {
                    try {
                        SchemeValue val = macro.defEnv().get(entry.getKey());
                        env.define(entry.getValue(), val);
                    } catch (EvalError ignored) {
                        // Not in definition env — introduced binding, skip
                    }
                }
                return expanded;
            }
        }
        throw new EvalError("No matching pattern for macro");
    }

    private boolean matchPattern(List<SchemeValue> patElems, List<SchemeValue> inputElems,
                                  int patStart, int inputStart, List<String> literals,
                                  Map<String, SchemeValue> singles, Map<String, List<SchemeValue>> lists) {
        int inputIdx = inputStart;
        for (int i = patStart; i < patElems.size(); i++) {
            SchemeValue pat = patElems.get(i);
            // Check if next element is ellipsis
            if (i + 1 < patElems.size() && isEllipsis(patElems.get(i + 1))) {
                if (!(pat instanceof SchemeValue.SymbolVal sym)) return false;
                List<SchemeValue> collected = new ArrayList<>();
                while (inputIdx < inputElems.size()) {
                    collected.add(inputElems.get(inputIdx++));
                }
                lists.put(sym.name(), collected);
                i++; // skip ellipsis
                continue;
            }
            if (inputIdx >= inputElems.size()) return false;
            if (pat instanceof SchemeValue.SymbolVal sym) {
                if (literals.contains(sym.name())) {
                    // Literal: must match exactly
                    if (!(inputElems.get(inputIdx) instanceof SchemeValue.SymbolVal inSym)
                        || !inSym.name().equals(sym.name())) return false;
                    inputIdx++;
                } else {
                    // Pattern variable
                    singles.put(sym.name(), inputElems.get(inputIdx));
                    inputIdx++;
                }
            } else if (pat instanceof SchemeValue.ListVal patNested) {
                if (!(inputElems.get(inputIdx) instanceof SchemeValue.ListVal inNested)) return false;
                if (!matchPattern(patNested.elements(), inNested.elements(), 0, 0, literals, singles, lists))
                    return false;
                inputIdx++;
            } else {
                // Literal value match
                inputIdx++;
            }
        }
        return inputIdx == inputElems.size();
    }

    private boolean isEllipsis(SchemeValue v) {
        return v instanceof SchemeValue.SymbolVal s && s.name().equals("...");
    }

    private SchemeValue expandTemplate(SchemeValue template, Map<String, SchemeValue> singles,
                                        Map<String, List<SchemeValue>> lists, Map<String, String> renames) {
        if (template instanceof SchemeValue.SymbolVal sym) {
            String name = sym.name();
            if (singles.containsKey(name)) return singles.get(name);
            if (renames.containsKey(name)) return new SchemeValue.SymbolVal(renames.get(name));
            return template;
        }
        if (template instanceof SchemeValue.ListVal list) {
            List<SchemeValue> expanded = new ArrayList<>();
            List<SchemeValue> elems = list.elements();
            for (int i = 0; i < elems.size(); i++) {
                if (i + 1 < elems.size() && isEllipsis(elems.get(i + 1))) {
                    SchemeValue elem = elems.get(i);
                    if (elem instanceof SchemeValue.SymbolVal sym && lists.containsKey(sym.name())) {
                        expanded.addAll(lists.get(sym.name()));
                    }
                    i++; // skip ellipsis
                    continue;
                }
                expanded.add(expandTemplate(elems.get(i), singles, lists, renames));
            }
            return new SchemeValue.ListVal(expanded);
        }
        return template;
    }

    private void collectFreeSymbols(SchemeValue template, Set<String> patternVars, Set<String> result) {
        if (template instanceof SchemeValue.SymbolVal sym) {
            String name = sym.name();
            if (!patternVars.contains(name) && !SPECIAL_FORMS.contains(name) && !name.equals("...")) {
                result.add(name);
            }
        } else if (template instanceof SchemeValue.ListVal list) {
            for (SchemeValue elem : list.elements()) {
                collectFreeSymbols(elem, patternVars, result);
            }
        }
    }

    private long requireInt(SchemeValue val) throws EvalError {
        if (val instanceof SchemeValue.IntVal iv) return iv.value();
        throw new EvalError("Expected integer, got: " + val.display());
    }
}
