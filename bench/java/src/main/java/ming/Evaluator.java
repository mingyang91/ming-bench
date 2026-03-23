package ming;

import java.util.ArrayList;
import java.util.List;

public class Evaluator {
    private final Environment globalEnv = new Environment();

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
        throw new EvalError("not implemented");
    }

    private SchemeValue eval(SchemeValue expr, Environment env) throws EvalError {
        return switch (expr) {
            case SchemeValue.IntVal v -> v;
            case SchemeValue.BoolVal v -> v;
            case SchemeValue.StringVal v -> v;
            case SchemeValue.VoidVal v -> v;
            case SchemeValue.LambdaVal v -> v;
            case SchemeValue.SymbolVal v -> env.get(v.name());
            case SchemeValue.ListVal list -> evalList(list, env);
        };
    }

    private SchemeValue evalList(SchemeValue.ListVal list, Environment env) throws EvalError {
        if (list.elements().isEmpty()) throw new EvalError("empty application");

        var first = list.elements().getFirst();
        var args = list.elements().subList(1, list.elements().size());

        // Handle special forms
        if (first instanceof SchemeValue.SymbolVal sym) {
            switch (sym.name()) {
                case "if": return evalIf(args, env);
                case "define": return evalDefine(args, env);
                case "quote": {
                    if (args.size() != 1) throw new EvalError("quote: needs exactly 1 argument");
                    return args.getFirst();
                }
                case "lambda": return evalLambda(args, env);
                default: break;
            }
        }

        // Procedure call — try builtin first for symbols
        if (first instanceof SchemeValue.SymbolVal sym && isBuiltin(sym.name())) {
            return applyBuiltin(sym.name(), args, env);
        }
        SchemeValue proc = eval(first, env);
        return applyProc(proc, args, env);
    }

    private SchemeValue applyProc(SchemeValue proc, List<SchemeValue> argExprs, Environment env) throws EvalError {
        if (proc instanceof SchemeValue.SymbolVal sym) {
            // Built-in operators
            return applyBuiltin(sym.name(), argExprs, env);
        }
        if (proc instanceof SchemeValue.LambdaVal lambda) {
            if (argExprs.size() != lambda.params().size())
                throw new EvalError("wrong number of arguments: expected " + lambda.params().size() + ", got " + argExprs.size());
            Environment callEnv = new Environment(lambda.env());
            for (int i = 0; i < lambda.params().size(); i++) {
                callEnv.define(lambda.params().get(i), eval(argExprs.get(i), env));
            }
            SchemeValue result = null;
            for (var bodyExpr : lambda.body()) {
                result = eval(bodyExpr, callEnv);
            }
            return result;
        }
        throw new EvalError("not a procedure: " + proc.display());
    }

    private static final java.util.Set<String> BUILTINS = java.util.Set.of(
        "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not", "and", "or"
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
            case "and" -> andOp(args, env);
            case "or" -> orOp(args, env);
            default -> throw new EvalError("unknown procedure: " + name);
        };
    }

    private SchemeValue evalIf(List<SchemeValue> args, Environment env) throws EvalError {
        if (args.size() < 2 || args.size() > 3) throw new EvalError("if: needs 2 or 3 arguments");
        SchemeValue cond = eval(args.get(0), env);
        if (cond.isTruthy()) {
            return eval(args.get(1), env);
        } else if (args.size() == 3) {
            return eval(args.get(2), env);
        }
        return new SchemeValue.VoidVal();
    }

    private SchemeValue evalDefine(List<SchemeValue> args, Environment env) throws EvalError {
        if (args.size() < 2) throw new EvalError("define: needs at least 2 arguments");
        var target = args.getFirst();
        if (target instanceof SchemeValue.SymbolVal sym) {
            // (define x expr)
            env.define(sym.name(), eval(args.get(1), env));
            return new SchemeValue.VoidVal();
        }
        if (target instanceof SchemeValue.ListVal nameAndParams) {
            // (define (f x y) body...)
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

    // --- builtins ---

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

    private SchemeValue andOp(List<SchemeValue> args, Environment env) throws EvalError {
        SchemeValue result = new SchemeValue.BoolVal(true);
        for (var arg : args) {
            result = eval(arg, env);
            if (!result.isTruthy()) return result;
        }
        return result;
    }

    private SchemeValue orOp(List<SchemeValue> args, Environment env) throws EvalError {
        SchemeValue result = new SchemeValue.BoolVal(false);
        for (var arg : args) {
            result = eval(arg, env);
            if (result.isTruthy()) return result;
        }
        return result;
    }
}
