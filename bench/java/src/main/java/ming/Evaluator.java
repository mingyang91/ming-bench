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
            case SchemeValue.NilVal v -> v;
            case SchemeValue.PairVal v -> v;
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
                    return quoteDatum(args.getFirst());
                }
                case "lambda": return evalLambda(args, env);
                case "let": return evalLet(args, env);
                case "begin": return evalBegin(args, env);
                case "cond": return evalCond(args, env);
                default: break;
            }
        }

        // Procedure call — check env first, then builtins
        if (first instanceof SchemeValue.SymbolVal sym) {
            try {
                SchemeValue proc = env.get(sym.name());
                return applyProc(proc, args, env);
            } catch (EvalError e) {
                if (isBuiltin(sym.name())) {
                    return applyBuiltin(sym.name(), args, env);
                }
                throw e;
            }
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
        "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not", "and", "or",
        "cons", "car", "cdr", "null?", "list", "length", "append",
        "string?", "number?", "boolean?", "pair?", "symbol?",
        "zero?", "positive?", "negative?", "abs", "min", "max",
        "equal?", "eq?", "modulo", "remainder", "even?", "odd?"
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

    // Convert parsed AST (ListVal) to runtime pairs for quote
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

    private SchemeValue evalLet(List<SchemeValue> args, Environment env) throws EvalError {
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

            // Create a lambda and bind it recursively
            Environment letEnv = new Environment(env);
            var lambda = new SchemeValue.LambdaVal(params, body, letEnv);
            letEnv.define(nameSym.name(), lambda);

            // Evaluate initial args and call
            List<SchemeValue> evaledArgs = new ArrayList<>();
            for (var init : inits) {
                evaledArgs.add(eval(init, env));
            }
            Environment callEnv = new Environment(letEnv);
            for (int i = 0; i < params.size(); i++) {
                callEnv.define(params.get(i), evaledArgs.get(i));
            }
            SchemeValue result = null;
            for (var bodyExpr : body) {
                result = eval(bodyExpr, callEnv);
            }
            return result;
        }

        // Regular let: (let ((var init) ...) body ...)
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

        SchemeValue result = null;
        for (int i = 1; i < args.size(); i++) {
            result = eval(args.get(i), letEnv);
        }
        return result;
    }

    private SchemeValue evalBegin(List<SchemeValue> args, Environment env) throws EvalError {
        SchemeValue result = new SchemeValue.VoidVal();
        for (var arg : args) {
            result = eval(arg, env);
        }
        return result;
    }

    private SchemeValue evalCond(List<SchemeValue> args, Environment env) throws EvalError {
        for (var clause : args) {
            if (!(clause instanceof SchemeValue.ListVal cl) || cl.elements().isEmpty())
                throw new EvalError("cond: invalid clause");
            var test = cl.elements().getFirst();
            if (test instanceof SchemeValue.SymbolVal sym && sym.name().equals("else")) {
                SchemeValue result = new SchemeValue.VoidVal();
                for (int i = 1; i < cl.elements().size(); i++) {
                    result = eval(cl.elements().get(i), env);
                }
                return result;
            }
            SchemeValue testVal = eval(test, env);
            if (testVal.isTruthy()) {
                if (cl.elements().size() == 1) return testVal;
                SchemeValue result = null;
                for (int i = 1; i < cl.elements().size(); i++) {
                    result = eval(cl.elements().get(i), env);
                }
                return result;
            }
        }
        return new SchemeValue.VoidVal();
    }

    private SchemeValue evalAppend(List<SchemeValue> args, Environment env) throws EvalError {
        if (args.isEmpty()) return new SchemeValue.NilVal();
        if (args.size() == 1) return eval(args.getFirst(), env);

        // Evaluate all args
        List<SchemeValue> evaluated = new ArrayList<>();
        for (var arg : args) {
            evaluated.add(eval(arg, env));
        }

        // Build result right to left
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
