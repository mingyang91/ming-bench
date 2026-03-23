package ming;

import java.util.ArrayList;
import java.util.List;

public class Evaluator {
    private final Environment globalEnv = new Environment();
    private StringBuilder outputBuffer;

    public String evalStr(String input) throws EvalError {
        var parser = new Parser(input);
        List<SchemeValue> exprs = parser.parseAll();
        if (exprs.isEmpty()) throw new EvalError("empty input");

        SchemeValue result = null;
        for (var expr : exprs) {
            result = eval(expr, globalEnv);
        }
        return result.display();
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        outputBuffer = new StringBuilder();
        var parser = new Parser(input);
        List<SchemeValue> exprs = parser.parseAll();
        if (exprs.isEmpty()) throw new EvalError("empty input");

        SchemeValue result = null;
        for (var expr : exprs) {
            result = eval(expr, globalEnv);
        }
        String output = outputBuffer.toString();
        outputBuffer = null;
        return new EvalResult(result.display(), output);
    }

    private SchemeValue eval(SchemeValue expr, Environment env) throws EvalError {
        while (true) {
            switch (expr) {
                case SchemeValue.IntVal v -> { return v; }
                case SchemeValue.BoolVal v -> { return v; }
                case SchemeValue.StringVal v -> { return v; }
                case SchemeValue.VoidVal v -> { return v; }
                case SchemeValue.NilVal v -> { return v; }
                case SchemeValue.PairVal v -> { return v; }
                case SchemeValue.CharVal v -> { return v; }
                case SchemeValue.LambdaVal v -> { return v; }
                case SchemeValue.SymbolVal v -> {
                    try {
                        return env.get(v.name());
                    } catch (EvalError e) {
                        throw withPos(e, v.line(), v.col());
                    }
                }
                case SchemeValue.ListVal list -> {
                    if (list.elements().isEmpty()) throw withPos(new EvalError("empty application"), list.line(), list.col());

                    var first = list.elements().getFirst();
                    var args = list.elements().subList(1, list.elements().size());

                    // Handle special forms
                    if (first instanceof SchemeValue.SymbolVal sym) {
                        try {
                            switch (sym.name()) {
                                case "if": {
                                    if (args.size() < 2 || args.size() > 3) throw new EvalError("if: needs 2 or 3 arguments");
                                    SchemeValue cond = eval(args.get(0), env);
                                    if (cond.isTruthy()) {
                                        expr = args.get(1); // TCO
                                        continue;
                                    } else if (args.size() == 3) {
                                        expr = args.get(2); // TCO
                                        continue;
                                    }
                                    return new SchemeValue.VoidVal();
                                }
                                case "define": return evalDefine(args, env);
                                case "quote": {
                                    if (args.size() != 1) throw new EvalError("quote: needs exactly 1 argument");
                                    return quoteDatum(args.getFirst());
                                }
                                case "lambda": return evalLambda(args, env);
                                case "let": {
                                    // Returns [newExpr, newEnv] for TCO
                                    Object[] r = evalLetTCO(args, env);
                                    expr = (SchemeValue) r[0];
                                    env = (Environment) r[1];
                                    continue;
                                }
                                case "begin": {
                                    if (args.isEmpty()) return new SchemeValue.VoidVal();
                                    for (int i = 0; i < args.size() - 1; i++) {
                                        eval(args.get(i), env);
                                    }
                                    expr = args.getLast(); // TCO
                                    continue;
                                }
                                case "cond": {
                                    Object[] r = evalCondTCO(args, env);
                                    if (r == null) return new SchemeValue.VoidVal();
                                    expr = (SchemeValue) r[0];
                                    env = (Environment) r[1];
                                    continue;
                                }
                                case "and": {
                                    if (args.isEmpty()) return new SchemeValue.BoolVal(true);
                                    for (int i = 0; i < args.size() - 1; i++) {
                                        SchemeValue v = eval(args.get(i), env);
                                        if (!v.isTruthy()) return v;
                                    }
                                    expr = args.getLast(); // TCO
                                    continue;
                                }
                                case "or": {
                                    if (args.isEmpty()) return new SchemeValue.BoolVal(false);
                                    for (int i = 0; i < args.size() - 1; i++) {
                                        SchemeValue v = eval(args.get(i), env);
                                        if (v.isTruthy()) return v;
                                    }
                                    expr = args.getLast(); // TCO
                                    continue;
                                }
                                default: break;
                            }
                        } catch (EvalError e) {
                            throw withPos(e, list.line(), list.col());
                        }
                    }

                    // Procedure call
                    SchemeValue proc;
                    try {
                        if (first instanceof SchemeValue.SymbolVal sym) {
                            try {
                                proc = env.get(sym.name());
                            } catch (EvalError e) {
                                if (isBuiltin(sym.name())) {
                                    return applyBuiltin(sym.name(), args, env);
                                }
                                throw e;
                            }
                        } else {
                            proc = eval(first, env);
                        }
                    } catch (EvalError e) {
                        throw withPos(e, list.line(), list.col());
                    }

                    // Apply proc with TCO
                    if (proc instanceof SchemeValue.SymbolVal symProc) {
                        try {
                            return applyBuiltin(symProc.name(), args, env);
                        } catch (EvalError e) {
                            throw withPos(e, list.line(), list.col());
                        }
                    }
                    if (proc instanceof SchemeValue.LambdaVal lambda) {
                        if (args.size() != lambda.params().size())
                            throw withPos(new EvalError("wrong number of arguments: expected " + lambda.params().size() + ", got " + args.size()), list.line(), list.col());
                        Environment callEnv = new Environment(lambda.env());
                        try {
                            for (int i = 0; i < lambda.params().size(); i++) {
                                callEnv.define(lambda.params().get(i), eval(args.get(i), env));
                            }
                        } catch (EvalError e) {
                            throw withPos(e, list.line(), list.col());
                        }
                        // Eval all body exprs except last, then TCO the last
                        for (int i = 0; i < lambda.body().size() - 1; i++) {
                            eval(lambda.body().get(i), callEnv);
                        }
                        expr = lambda.body().getLast(); // TCO
                        env = callEnv;
                        continue;
                    }
                    throw withPos(new EvalError("not a procedure: " + proc.display()), list.line(), list.col());
                }
            }
        }
    }

    private SchemeValue evalDefine(List<SchemeValue> args, Environment env) throws EvalError {
        if (args.size() < 2) throw new EvalError("define: needs at least 2 arguments");
        var target = args.getFirst();
        if (target instanceof SchemeValue.SymbolVal sym) {
            env.define(sym.name(), eval(args.get(1), env));
            return new SchemeValue.VoidVal();
        }
        if (target instanceof SchemeValue.ListVal nameAndParams) {
            if (nameAndParams.elements().isEmpty()) throw new EvalError("define: empty name list");
            var nameVal = nameAndParams.elements().getFirst();
            if (!(nameVal instanceof SchemeValue.SymbolVal nameSym))
                throw new EvalError("define: name must be a symbol");
            List<String> params = new ArrayList<>();
            for (int i = 1; i < nameAndParams.elements().size(); i++) {
                if (!(nameAndParams.elements().get(i) instanceof SchemeValue.SymbolVal p))
                    throw new EvalError("define: parameter must be a symbol");
                params.add(p.name());
            }
            List<SchemeValue> body = args.subList(1, args.size());
            var lambda = new SchemeValue.LambdaVal(params, body, env);
            env.define(nameSym.name(), lambda);
            return new SchemeValue.VoidVal();
        }
        throw new EvalError("define: invalid syntax");
    }

    private SchemeValue evalLambda(List<SchemeValue> args, Environment env) throws EvalError {
        if (args.size() < 2) throw new EvalError("lambda: needs params and body");
        var paramList = args.getFirst();
        if (!(paramList instanceof SchemeValue.ListVal plist))
            throw new EvalError("lambda: params must be a list");
        List<String> params = new ArrayList<>();
        for (var p : plist.elements()) {
            if (!(p instanceof SchemeValue.SymbolVal sym))
                throw new EvalError("lambda: parameter must be a symbol");
            params.add(sym.name());
        }
        List<SchemeValue> body = args.subList(1, args.size());
        return new SchemeValue.LambdaVal(params, body, env);
    }

    // Returns [expr, env] for the trampoline to continue with
    private Object[] evalLetTCO(List<SchemeValue> args, Environment env) throws EvalError {
        if (args.size() < 2) throw new EvalError("let: needs bindings and body");

        // Named let: (let name ((var init) ...) body ...)
        if (args.getFirst() instanceof SchemeValue.SymbolVal nameSym) {
            if (args.size() < 3) throw new EvalError("let: named let needs bindings and body");
            var bindingsList = args.get(1);
            if (!(bindingsList instanceof SchemeValue.ListVal bl))
                throw new EvalError("let: bindings must be a list");

            List<String> params = new ArrayList<>();
            List<SchemeValue> inits = new ArrayList<>();
            for (var binding : bl.elements()) {
                if (!(binding instanceof SchemeValue.ListVal pair) || pair.elements().size() != 2)
                    throw new EvalError("let: invalid binding");
                if (!(pair.elements().getFirst() instanceof SchemeValue.SymbolVal sym))
                    throw new EvalError("let: binding name must be a symbol");
                params.add(sym.name());
                inits.add(pair.elements().get(1));
            }
            List<SchemeValue> body = args.subList(2, args.size());

            Environment letEnv = new Environment(env);
            var lambda = new SchemeValue.LambdaVal(params, body, letEnv);
            letEnv.define(nameSym.name(), lambda);

            Environment callEnv = new Environment(letEnv);
            for (int i = 0; i < params.size(); i++) {
                callEnv.define(params.get(i), eval(inits.get(i), env));
            }
            // Eval all body except last
            for (int i = 0; i < body.size() - 1; i++) {
                eval(body.get(i), callEnv);
            }
            return new Object[]{ body.getLast(), callEnv };
        }

        // Regular let
        var bindingsList = args.getFirst();
        if (!(bindingsList instanceof SchemeValue.ListVal bl))
            throw new EvalError("let: bindings must be a list");

        Environment letEnv = new Environment(env);
        for (var binding : bl.elements()) {
            if (!(binding instanceof SchemeValue.ListVal pair) || pair.elements().size() != 2)
                throw new EvalError("let: invalid binding");
            if (!(pair.elements().getFirst() instanceof SchemeValue.SymbolVal sym))
                throw new EvalError("let: binding name must be a symbol");
            letEnv.define(sym.name(), eval(pair.elements().get(1), env));
        }

        List<SchemeValue> body = args.subList(1, args.size());
        for (int i = 0; i < body.size() - 1; i++) {
            eval(body.get(i), letEnv);
        }
        return new Object[]{ body.getLast(), letEnv };
    }

    // Returns [expr, env] or null if no clause matched
    private Object[] evalCondTCO(List<SchemeValue> args, Environment env) throws EvalError {
        for (var clause : args) {
            if (!(clause instanceof SchemeValue.ListVal cl) || cl.elements().isEmpty())
                throw new EvalError("cond: invalid clause");
            var test = cl.elements().getFirst();
            if (test instanceof SchemeValue.SymbolVal sym && sym.name().equals("else")) {
                if (cl.elements().size() == 1) return new Object[]{ new SchemeValue.VoidVal(), env };
                for (int i = 1; i < cl.elements().size() - 1; i++) {
                    eval(cl.elements().get(i), env);
                }
                return new Object[]{ cl.elements().getLast(), env };
            }
            SchemeValue testVal = eval(test, env);
            if (testVal.isTruthy()) {
                if (cl.elements().size() == 1) {
                    // Return testVal directly - wrap as a quote-like thing
                    // We can't TCO here, just return the value
                    // Use a trick: return a quoted value
                    return new Object[]{ wrapValue(testVal), env };
                }
                for (int i = 1; i < cl.elements().size() - 1; i++) {
                    eval(cl.elements().get(i), env);
                }
                return new Object[]{ cl.elements().getLast(), env };
            }
        }
        return null;
    }

    // Wrap an already-evaluated value so it passes through eval unchanged
    private SchemeValue wrapValue(SchemeValue v) {
        // Self-evaluating values pass through eval unchanged
        if (v instanceof SchemeValue.IntVal || v instanceof SchemeValue.BoolVal ||
            v instanceof SchemeValue.StringVal || v instanceof SchemeValue.VoidVal ||
            v instanceof SchemeValue.NilVal || v instanceof SchemeValue.PairVal ||
            v instanceof SchemeValue.CharVal || v instanceof SchemeValue.LambdaVal) {
            return v;
        }
        // For symbols, we'd need to quote them, but cond test values that are symbols
        // would have already been evaluated
        return v;
    }

    // --- builtins ---

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
        "string-set!", "string-copy"
    );

    private boolean isBuiltin(String name) {
        return BUILTINS.contains(name);
    }

    private SchemeValue applyBuiltin(String name, List<SchemeValue> args, Environment env) throws EvalError {
        return switch (name) {
            case "+" -> arithOp(args, 0, Long::sum, env);
            case "-" -> minusOp(args, env);
            case "*" -> arithOp(args, 1, (a, b) -> a * b, env);
            case "/" -> divOp(args, env);
            case "<" -> cmpOp(args, (a, b) -> a < b, env);
            case ">" -> cmpOp(args, (a, b) -> a > b, env);
            case "=" -> cmpOp(args, (a, b) -> a == b, env);
            case "<=" -> cmpOp(args, (a, b) -> a <= b, env);
            case ">=" -> cmpOp(args, (a, b) -> a >= b, env);
            case "not" -> notOp(args, env);
            case "cons" -> {
                if (args.size() != 2) throw new EvalError("cons: needs exactly 2 arguments");
                yield new SchemeValue.PairVal(eval(args.get(0), env), eval(args.get(1), env));
            }
            case "car" -> {
                if (args.size() != 1) throw new EvalError("car: needs exactly 1 argument");
                var v = eval(args.getFirst(), env);
                if (!(v instanceof SchemeValue.PairVal p)) throw new EvalError("car: not a pair");
                yield p.car();
            }
            case "cdr" -> {
                if (args.size() != 1) throw new EvalError("cdr: needs exactly 1 argument");
                var v = eval(args.getFirst(), env);
                if (!(v instanceof SchemeValue.PairVal p)) throw new EvalError("cdr: not a pair");
                yield p.cdr();
            }
            case "null?" -> {
                if (args.size() != 1) throw new EvalError("null?: needs exactly 1 argument");
                yield new SchemeValue.BoolVal(eval(args.getFirst(), env) instanceof SchemeValue.NilVal);
            }
            case "list" -> {
                SchemeValue result = new SchemeValue.NilVal();
                for (int i = args.size() - 1; i >= 0; i--) {
                    result = new SchemeValue.PairVal(eval(args.get(i), env), result);
                }
                yield result;
            }
            case "length" -> {
                if (args.size() != 1) throw new EvalError("length: needs exactly 1 argument");
                var v = eval(args.getFirst(), env);
                long len = 0;
                while (v instanceof SchemeValue.PairVal p) {
                    len++;
                    v = p.cdr();
                }
                yield new SchemeValue.IntVal(len);
            }
            case "append" -> evalAppend(args, env);
            case "string?" -> typePred(args, env, SchemeValue.StringVal.class);
            case "number?" -> typePred(args, env, SchemeValue.IntVal.class);
            case "boolean?" -> typePred(args, env, SchemeValue.BoolVal.class);
            case "pair?" -> typePred(args, env, SchemeValue.PairVal.class);
            case "symbol?" -> typePred(args, env, SchemeValue.SymbolVal.class);
            case "zero?" -> {
                if (args.size() != 1) throw new EvalError("zero?: needs exactly 1 argument");
                yield new SchemeValue.BoolVal(asInt(eval(args.getFirst(), env)) == 0);
            }
            case "positive?" -> {
                if (args.size() != 1) throw new EvalError("positive?: needs exactly 1 argument");
                yield new SchemeValue.BoolVal(asInt(eval(args.getFirst(), env)) > 0);
            }
            case "negative?" -> {
                if (args.size() != 1) throw new EvalError("negative?: needs exactly 1 argument");
                yield new SchemeValue.BoolVal(asInt(eval(args.getFirst(), env)) < 0);
            }
            case "abs" -> {
                if (args.size() != 1) throw new EvalError("abs: needs exactly 1 argument");
                yield new SchemeValue.IntVal(Math.abs(asInt(eval(args.getFirst(), env))));
            }
            case "min" -> {
                if (args.size() < 1) throw new EvalError("min: needs at least 1 argument");
                long m = asInt(eval(args.getFirst(), env));
                for (int i = 1; i < args.size(); i++) m = Math.min(m, asInt(eval(args.get(i), env)));
                yield new SchemeValue.IntVal(m);
            }
            case "max" -> {
                if (args.size() < 1) throw new EvalError("max: needs at least 1 argument");
                long m = asInt(eval(args.getFirst(), env));
                for (int i = 1; i < args.size(); i++) m = Math.max(m, asInt(eval(args.get(i), env)));
                yield new SchemeValue.IntVal(m);
            }
            case "equal?", "eq?" -> {
                if (args.size() != 2) throw new EvalError(name + ": needs exactly 2 arguments");
                yield new SchemeValue.BoolVal(schemeEqual(eval(args.get(0), env), eval(args.get(1), env)));
            }
            case "modulo" -> {
                if (args.size() != 2) throw new EvalError("modulo: needs exactly 2 arguments");
                long a = asInt(eval(args.get(0), env));
                long b = asInt(eval(args.get(1), env));
                if (b == 0) throw new EvalError("division by zero");
                yield new SchemeValue.IntVal(Math.floorMod(a, b));
            }
            case "remainder" -> {
                if (args.size() != 2) throw new EvalError("remainder: needs exactly 2 arguments");
                long a = asInt(eval(args.get(0), env));
                long b = asInt(eval(args.get(1), env));
                if (b == 0) throw new EvalError("division by zero");
                yield new SchemeValue.IntVal(a % b);
            }
            case "even?" -> {
                if (args.size() != 1) throw new EvalError("even?: needs exactly 1 argument");
                yield new SchemeValue.BoolVal(asInt(eval(args.getFirst(), env)) % 2 == 0);
            }
            case "odd?" -> {
                if (args.size() != 1) throw new EvalError("odd?: needs exactly 1 argument");
                yield new SchemeValue.BoolVal(asInt(eval(args.getFirst(), env)) % 2 != 0);
            }
            case "display" -> {
                if (args.size() != 1) throw new EvalError("display: needs exactly 1 argument");
                var v = eval(args.getFirst(), env);
                if (outputBuffer != null) outputBuffer.append(v.displayOutput());
                yield new SchemeValue.VoidVal();
            }
            case "write" -> {
                if (args.size() != 1) throw new EvalError("write: needs exactly 1 argument");
                var v = eval(args.getFirst(), env);
                if (outputBuffer != null) outputBuffer.append(v.display());
                yield new SchemeValue.VoidVal();
            }
            case "newline" -> {
                if (outputBuffer != null) outputBuffer.append('\n');
                yield new SchemeValue.VoidVal();
            }
            case "string-append" -> {
                var sb = new StringBuilder();
                for (var arg : args) {
                    var v = eval(arg, env);
                    if (!(v instanceof SchemeValue.StringVal s)) throw new EvalError("string-append: not a string");
                    sb.append(s.value());
                }
                yield new SchemeValue.StringVal(sb.toString());
            }
            case "string-length" -> {
                if (args.size() != 1) throw new EvalError("string-length: needs exactly 1 argument");
                var v = eval(args.getFirst(), env);
                if (!(v instanceof SchemeValue.StringVal s)) throw new EvalError("string-length: not a string");
                yield new SchemeValue.IntVal(s.length());
            }
            case "substring" -> {
                if (args.size() != 3) throw new EvalError("substring: needs exactly 3 arguments");
                var v = eval(args.get(0), env);
                if (!(v instanceof SchemeValue.StringVal s)) throw new EvalError("substring: not a string");
                int start = (int) asInt(eval(args.get(1), env));
                int end = (int) asInt(eval(args.get(2), env));
                yield new SchemeValue.StringVal(s.value().substring(start, end));
            }
            case "string->number" -> {
                if (args.size() != 1) throw new EvalError("string->number: needs exactly 1 argument");
                var v = eval(args.getFirst(), env);
                if (!(v instanceof SchemeValue.StringVal s)) throw new EvalError("string->number: not a string");
                try {
                    yield new SchemeValue.IntVal(Long.parseLong(s.value()));
                } catch (NumberFormatException e) {
                    yield new SchemeValue.BoolVal(false);
                }
            }
            case "number->string" -> {
                if (args.size() != 1) throw new EvalError("number->string: needs exactly 1 argument");
                yield new SchemeValue.StringVal(Long.toString(asInt(eval(args.getFirst(), env))));
            }
            case "symbol->string" -> {
                if (args.size() != 1) throw new EvalError("symbol->string: needs exactly 1 argument");
                var v = eval(args.getFirst(), env);
                if (!(v instanceof SchemeValue.SymbolVal sym)) throw new EvalError("symbol->string: not a symbol");
                yield new SchemeValue.StringVal(sym.name());
            }
            case "string->symbol" -> {
                if (args.size() != 1) throw new EvalError("string->symbol: needs exactly 1 argument");
                var v = eval(args.getFirst(), env);
                if (!(v instanceof SchemeValue.StringVal s)) throw new EvalError("string->symbol: not a string");
                yield new SchemeValue.SymbolVal(s.value());
            }
            case "string-ref" -> {
                if (args.size() != 2) throw new EvalError("string-ref: needs exactly 2 arguments");
                var v = eval(args.get(0), env);
                if (!(v instanceof SchemeValue.StringVal s)) throw new EvalError("string-ref: not a string");
                int idx = (int) asInt(eval(args.get(1), env));
                yield new SchemeValue.CharVal(s.charAt(idx));
            }
            case "char?" -> typePred(args, env, SchemeValue.CharVal.class);
            case "string-set!" -> {
                if (args.size() != 3) throw new EvalError("string-set!: needs exactly 3 arguments");
                var v = eval(args.get(0), env);
                if (!(v instanceof SchemeValue.StringVal s)) throw new EvalError("string-set!: not a string");
                int idx = (int) asInt(eval(args.get(1), env));
                var ch = eval(args.get(2), env);
                if (!(ch instanceof SchemeValue.CharVal c)) throw new EvalError("string-set!: not a character");
                s.setChar(idx, c.value());
                yield new SchemeValue.VoidVal();
            }
            case "string-copy" -> {
                if (args.size() != 1) throw new EvalError("string-copy: needs exactly 1 argument");
                var v = eval(args.getFirst(), env);
                if (!(v instanceof SchemeValue.StringVal s)) throw new EvalError("string-copy: not a string");
                yield s.copy();
            }
            default -> throw new EvalError("unknown procedure: " + name);
        };
    }

    private long asInt(SchemeValue v) throws EvalError {
        if (v instanceof SchemeValue.IntVal i) return i.value();
        throw new EvalError("expected number, got " + v.display());
    }

    @FunctionalInterface
    interface LongBinOp { long apply(long a, long b); }

    @FunctionalInterface
    interface LongCmp { boolean test(long a, long b); }

    private SchemeValue arithOp(List<SchemeValue> args, long identity, LongBinOp op, Environment env) throws EvalError {
        long result = identity;
        for (var arg : args) {
            result = op.apply(result, asInt(eval(arg, env)));
        }
        return new SchemeValue.IntVal(result);
    }

    private SchemeValue minusOp(List<SchemeValue> args, Environment env) throws EvalError {
        if (args.isEmpty()) throw new EvalError("-: needs at least 1 argument");
        if (args.size() == 1) return new SchemeValue.IntVal(-asInt(eval(args.getFirst(), env)));
        long result = asInt(eval(args.getFirst(), env));
        for (int i = 1; i < args.size(); i++) {
            result -= asInt(eval(args.get(i), env));
        }
        return new SchemeValue.IntVal(result);
    }

    private SchemeValue divOp(List<SchemeValue> args, Environment env) throws EvalError {
        if (args.isEmpty()) throw new EvalError("/: needs at least 1 argument");
        long result = asInt(eval(args.getFirst(), env));
        for (int i = 1; i < args.size(); i++) {
            long divisor = asInt(eval(args.get(i), env));
            if (divisor == 0) throw new EvalError("division by zero");
            result /= divisor;
        }
        return new SchemeValue.IntVal(result);
    }

    private SchemeValue cmpOp(List<SchemeValue> args, LongCmp cmp, Environment env) throws EvalError {
        if (args.size() < 2) throw new EvalError("comparison needs at least 2 arguments");
        long prev = asInt(eval(args.getFirst(), env));
        for (int i = 1; i < args.size(); i++) {
            long cur = asInt(eval(args.get(i), env));
            if (!cmp.test(prev, cur)) return new SchemeValue.BoolVal(false);
            prev = cur;
        }
        return new SchemeValue.BoolVal(true);
    }

    private SchemeValue notOp(List<SchemeValue> args, Environment env) throws EvalError {
        if (args.size() != 1) throw new EvalError("not: needs exactly 1 argument");
        return new SchemeValue.BoolVal(!eval(args.getFirst(), env).isTruthy());
    }

    private SchemeValue quoteDatum(SchemeValue v) {
        if (v instanceof SchemeValue.ListVal list) {
            SchemeValue result = new SchemeValue.NilVal();
            for (int i = list.elements().size() - 1; i >= 0; i--) {
                result = new SchemeValue.PairVal(quoteDatum(list.elements().get(i)), result);
            }
            return result;
        }
        return v;
    }

    private SchemeValue evalAppend(List<SchemeValue> args, Environment env) throws EvalError {
        if (args.isEmpty()) return new SchemeValue.NilVal();
        if (args.size() == 1) return eval(args.getFirst(), env);

        List<SchemeValue> evaluated = new ArrayList<>();
        for (var arg : args) {
            evaluated.add(eval(arg, env));
        }

        SchemeValue result = evaluated.getLast();
        for (int i = evaluated.size() - 2; i >= 0; i--) {
            result = appendTwo(evaluated.get(i), result);
        }
        return result;
    }

    private SchemeValue appendTwo(SchemeValue a, SchemeValue b) throws EvalError {
        if (a instanceof SchemeValue.NilVal) return b;
        if (a instanceof SchemeValue.PairVal p) {
            return new SchemeValue.PairVal(p.car(), appendTwo(p.cdr(), b));
        }
        throw new EvalError("append: not a proper list");
    }

    private SchemeValue typePred(List<SchemeValue> args, Environment env, Class<? extends SchemeValue> type) throws EvalError {
        if (args.size() != 1) throw new EvalError("type predicate: needs exactly 1 argument");
        return new SchemeValue.BoolVal(type.isInstance(eval(args.getFirst(), env)));
    }

    private static EvalError withPos(EvalError e, int line, int col) {
        if (line == 0 && col == 0) return e;
        String msg = e.getMessage();
        if (msg != null && msg.matches(".*\\d+:\\d+.*")) return e;
        return new EvalError(msg + " at " + line + ":" + col);
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
}
