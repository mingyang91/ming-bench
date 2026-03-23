package ming;

import java.util.ArrayList;
import java.util.List;

public class Evaluator {

    private final Environment globalEnv = new Environment();

    public String evalStr(String input) throws EvalError {
        var parser = new Parser(input);
        List<SchemeValue> exprs = parser.parseAll();
        if (exprs.isEmpty()) throw new EvalError("no expressions");
        SchemeValue result = null;
        for (SchemeValue expr : exprs) {
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
            case SchemeValue.LambdaVal v -> v;
            case SchemeValue.SymbolVal v -> env.lookup(v.name());
            case SchemeValue.ListVal v -> evalList(v.elements(), env);
        };
    }

    private SchemeValue evalList(List<SchemeValue> elements, Environment env) throws EvalError {
        if (elements.isEmpty()) throw new EvalError("empty application");

        SchemeValue head = elements.getFirst();
        if (head instanceof SchemeValue.SymbolVal sym) {
            String op = sym.name();
            List<SchemeValue> args = elements.subList(1, elements.size());

            switch (op) {
                case "define" -> { return evalDefine(args, env); }
                case "if" -> { return evalIf(args, env); }
                case "quote" -> {
                    if (args.size() != 1) throw new EvalError("quote: need exactly one argument");
                    return args.getFirst();
                }
                case "lambda" -> { return evalLambda(args, env); }
                case "let" -> { return evalLet(args, env); }
                case "begin" -> { return evalBegin(args, env); }
                case "cond" -> { return evalCond(args, env); }
                default -> {
                    // Try as builtin before falling through to procedure call
                    SchemeValue builtinResult = tryBuiltin(op, args, env);
                    if (builtinResult != null) return builtinResult;
                }
            }
        }

        // Procedure call
        SchemeValue proc = eval(head, env);
        List<SchemeValue> args = elements.subList(1, elements.size());
        List<SchemeValue> evaledArgs = new ArrayList<>();
        for (SchemeValue arg : args) {
            evaledArgs.add(eval(arg, env));
        }
        return apply(proc, evaledArgs);
    }

    private SchemeValue evalDefine(List<SchemeValue> args, Environment env) throws EvalError {
        if (args.size() < 2) throw new EvalError("define: need at least 2 arguments");
        SchemeValue target = args.getFirst();

        if (target instanceof SchemeValue.SymbolVal sym) {
            // (define x expr)
            SchemeValue val = eval(args.get(1), env);
            env.define(sym.name(), val);
            return val;
        } else if (target instanceof SchemeValue.ListVal nameAndParams) {
            // (define (f x y) body...)
            List<SchemeValue> elems = nameAndParams.elements();
            if (elems.isEmpty()) throw new EvalError("define: empty name list");
            if (!(elems.getFirst() instanceof SchemeValue.SymbolVal nameSym))
                throw new EvalError("define: name must be a symbol");

            List<String> params = new ArrayList<>();
            for (int i = 1; i < elems.size(); i++) {
                if (!(elems.get(i) instanceof SchemeValue.SymbolVal p))
                    throw new EvalError("define: parameter must be a symbol");
                params.add(p.name());
            }
            List<SchemeValue> body = args.subList(1, args.size());
            SchemeValue lambda = new SchemeValue.LambdaVal(params, body, env);
            env.define(nameSym.name(), lambda);
            return lambda;
        }
        throw new EvalError("define: invalid syntax");
    }

    private SchemeValue evalIf(List<SchemeValue> args, Environment env) throws EvalError {
        if (args.size() < 2 || args.size() > 3) throw new EvalError("if: need 2 or 3 arguments");
        SchemeValue cond = eval(args.get(0), env);
        if (cond.isTruthy()) {
            return eval(args.get(1), env);
        } else if (args.size() == 3) {
            return eval(args.get(2), env);
        }
        return new SchemeValue.BoolVal(false); // unspecified
    }

    private SchemeValue evalLambda(List<SchemeValue> args, Environment env) throws EvalError {
        if (args.size() < 2) throw new EvalError("lambda: need params and body");
        SchemeValue paramSpec = args.getFirst();
        if (!(paramSpec instanceof SchemeValue.ListVal paramList))
            throw new EvalError("lambda: params must be a list");

        List<String> params = new ArrayList<>();
        for (SchemeValue p : paramList.elements()) {
            if (!(p instanceof SchemeValue.SymbolVal sym))
                throw new EvalError("lambda: parameter must be a symbol");
            params.add(sym.name());
        }
        List<SchemeValue> body = args.subList(1, args.size());
        return new SchemeValue.LambdaVal(params, body, env);
    }

    private SchemeValue apply(SchemeValue proc, List<SchemeValue> args) throws EvalError {
        if (proc instanceof SchemeValue.LambdaVal lambda) {
            if (args.size() != lambda.params().size())
                throw new EvalError("wrong number of arguments: expected " + lambda.params().size() + ", got " + args.size());
            Environment callEnv = new Environment(lambda.env());
            for (int i = 0; i < lambda.params().size(); i++) {
                callEnv.define(lambda.params().get(i), args.get(i));
            }
            SchemeValue result = null;
            for (SchemeValue expr : lambda.body()) {
                result = eval(expr, callEnv);
            }
            return result;
        }
        throw new EvalError("not a procedure: " + proc.display());
    }

    // --- Builtins for L01 ---

    @FunctionalInterface
    private interface LongBinOp {
        long apply(long a, long b);
    }

    @FunctionalInterface
    private interface LongPred {
        boolean test(long a, long b);
    }

    // Check if this is a builtin and evaluate it; returns null if not a builtin
    private SchemeValue tryBuiltin(String op, List<SchemeValue> args, Environment env) throws EvalError {
        return switch (op) {
            case "+" -> arith(args, 0, Long::sum, env);
            case "-" -> minus(args, env);
            case "*" -> arith(args, 1, (a, b) -> a * b, env);
            case "/" -> divide(args, env);
            case "<" -> compare(args, (a, b) -> a < b, env);
            case ">" -> compare(args, (a, b) -> a > b, env);
            case "=" -> compare(args, (a, b) -> a == b, env);
            case "<=" -> compare(args, (a, b) -> a <= b, env);
            case ">=" -> compare(args, (a, b) -> a >= b, env);
            case "not" -> not(args, env);
            case "and" -> and(args, env);
            case "or" -> or(args, env);
            // L03 builtins
            case "cons" -> builtinCons(args, env);
            case "car" -> builtinCar(args, env);
            case "cdr" -> builtinCdr(args, env);
            case "null?" -> builtinNullQ(args, env);
            case "list" -> builtinList(args, env);
            case "length" -> builtinLength(args, env);
            case "append" -> builtinAppend(args, env);
            case "string?" -> typePred(args, env, SchemeValue.StringVal.class);
            case "number?" -> typePred(args, env, SchemeValue.IntVal.class);
            case "boolean?" -> typePred(args, env, SchemeValue.BoolVal.class);
            case "pair?" -> builtinPairQ(args, env);
            case "symbol?" -> typePred(args, env, SchemeValue.SymbolVal.class);
            default -> null;
        };
    }

    private SchemeValue arith(List<SchemeValue> args, long identity, LongBinOp op, Environment env) throws EvalError {
        long result = identity;
        for (SchemeValue arg : args) {
            result = op.apply(result, requireInt(eval(arg, env)));
        }
        return new SchemeValue.IntVal(result);
    }

    private SchemeValue minus(List<SchemeValue> args, Environment env) throws EvalError {
        if (args.isEmpty()) throw new EvalError("-: need at least one argument");
        if (args.size() == 1) {
            return new SchemeValue.IntVal(-requireInt(eval(args.getFirst(), env)));
        }
        long result = requireInt(eval(args.getFirst(), env));
        for (int i = 1; i < args.size(); i++) {
            result -= requireInt(eval(args.get(i), env));
        }
        return new SchemeValue.IntVal(result);
    }

    private SchemeValue divide(List<SchemeValue> args, Environment env) throws EvalError {
        if (args.isEmpty()) throw new EvalError("/: need at least one argument");
        long result = requireInt(eval(args.getFirst(), env));
        for (int i = 1; i < args.size(); i++) {
            long divisor = requireInt(eval(args.get(i), env));
            if (divisor == 0) throw new EvalError("division by zero");
            result /= divisor;
        }
        return new SchemeValue.IntVal(result);
    }

    private SchemeValue compare(List<SchemeValue> args, LongPred pred, Environment env) throws EvalError {
        if (args.size() < 2) throw new EvalError("comparison needs at least 2 arguments");
        long prev = requireInt(eval(args.getFirst(), env));
        for (int i = 1; i < args.size(); i++) {
            long curr = requireInt(eval(args.get(i), env));
            if (!pred.test(prev, curr)) return new SchemeValue.BoolVal(false);
            prev = curr;
        }
        return new SchemeValue.BoolVal(true);
    }

    private SchemeValue not(List<SchemeValue> args, Environment env) throws EvalError {
        if (args.size() != 1) throw new EvalError("not: need exactly one argument");
        return new SchemeValue.BoolVal(!eval(args.getFirst(), env).isTruthy());
    }

    private SchemeValue and(List<SchemeValue> args, Environment env) throws EvalError {
        SchemeValue result = new SchemeValue.BoolVal(true);
        for (SchemeValue arg : args) {
            result = eval(arg, env);
            if (!result.isTruthy()) return result;
        }
        return result;
    }

    private SchemeValue or(List<SchemeValue> args, Environment env) throws EvalError {
        SchemeValue result = new SchemeValue.BoolVal(false);
        for (SchemeValue arg : args) {
            result = eval(arg, env);
            if (result.isTruthy()) return result;
        }
        return result;
    }

    // --- L03 special forms ---

    private SchemeValue evalLet(List<SchemeValue> args, Environment env) throws EvalError {
        if (args.size() < 2) throw new EvalError("let: need bindings and body");

        // Named let: (let name ((var init) ...) body...)
        if (args.getFirst() instanceof SchemeValue.SymbolVal nameSym) {
            if (args.size() < 3) throw new EvalError("named let: need bindings and body");
            SchemeValue bindingsExpr = args.get(1);
            if (!(bindingsExpr instanceof SchemeValue.ListVal bindingsList))
                throw new EvalError("let: bindings must be a list");

            List<String> params = new ArrayList<>();
            List<SchemeValue> inits = new ArrayList<>();
            for (SchemeValue b : bindingsList.elements()) {
                if (!(b instanceof SchemeValue.ListVal pair) || pair.elements().size() != 2)
                    throw new EvalError("let: invalid binding");
                if (!(pair.elements().getFirst() instanceof SchemeValue.SymbolVal s))
                    throw new EvalError("let: binding name must be a symbol");
                params.add(s.name());
                inits.add(pair.elements().get(1));
            }

            List<SchemeValue> body = args.subList(2, args.size());
            Environment letEnv = new Environment(env);
            SchemeValue lambda = new SchemeValue.LambdaVal(params, body, letEnv);
            letEnv.define(nameSym.name(), lambda);

            List<SchemeValue> evaledInits = new ArrayList<>();
            for (SchemeValue init : inits) {
                evaledInits.add(eval(init, env));
            }
            return apply(lambda, evaledInits);
        }

        // Regular let: (let ((var init) ...) body...)
        SchemeValue bindingsExpr = args.getFirst();
        if (!(bindingsExpr instanceof SchemeValue.ListVal bindingsList))
            throw new EvalError("let: bindings must be a list");

        Environment letEnv = new Environment(env);
        for (SchemeValue b : bindingsList.elements()) {
            if (!(b instanceof SchemeValue.ListVal pair) || pair.elements().size() != 2)
                throw new EvalError("let: invalid binding");
            if (!(pair.elements().getFirst() instanceof SchemeValue.SymbolVal s))
                throw new EvalError("let: binding name must be a symbol");
            SchemeValue val = eval(pair.elements().get(1), env);
            letEnv.define(s.name(), val);
        }

        SchemeValue result = null;
        for (int i = 1; i < args.size(); i++) {
            result = eval(args.get(i), letEnv);
        }
        return result;
    }

    private SchemeValue evalBegin(List<SchemeValue> args, Environment env) throws EvalError {
        if (args.isEmpty()) throw new EvalError("begin: need at least one expression");
        SchemeValue result = null;
        for (SchemeValue arg : args) {
            result = eval(arg, env);
        }
        return result;
    }

    private SchemeValue evalCond(List<SchemeValue> args, Environment env) throws EvalError {
        for (SchemeValue clause : args) {
            if (!(clause instanceof SchemeValue.ListVal clauseList) || clauseList.elements().isEmpty())
                throw new EvalError("cond: invalid clause");
            List<SchemeValue> elems = clauseList.elements();
            SchemeValue test = elems.getFirst();

            if (test instanceof SchemeValue.SymbolVal sym && sym.name().equals("else")) {
                SchemeValue result = null;
                for (int i = 1; i < elems.size(); i++) {
                    result = eval(elems.get(i), env);
                }
                return result;
            }

            SchemeValue testVal = eval(test, env);
            if (testVal.isTruthy()) {
                SchemeValue result = testVal;
                for (int i = 1; i < elems.size(); i++) {
                    result = eval(elems.get(i), env);
                }
                return result;
            }
        }
        return new SchemeValue.BoolVal(false); // unspecified
    }

    // --- L03 builtins ---

    private SchemeValue builtinCons(List<SchemeValue> args, Environment env) throws EvalError {
        if (args.size() != 2) throw new EvalError("cons: need exactly 2 arguments");
        SchemeValue a = eval(args.get(0), env);
        SchemeValue b = eval(args.get(1), env);
        if (b instanceof SchemeValue.ListVal lst) {
            var newElems = new ArrayList<SchemeValue>();
            newElems.add(a);
            newElems.addAll(lst.elements());
            return new SchemeValue.ListVal(newElems);
        }
        throw new EvalError("cons: second argument must be a proper list (for now)");
    }

    private SchemeValue builtinCar(List<SchemeValue> args, Environment env) throws EvalError {
        if (args.size() != 1) throw new EvalError("car: need exactly 1 argument");
        SchemeValue val = eval(args.getFirst(), env);
        if (val instanceof SchemeValue.ListVal lst && !lst.elements().isEmpty()) {
            return lst.elements().getFirst();
        }
        throw new EvalError("car: not a pair");
    }

    private SchemeValue builtinCdr(List<SchemeValue> args, Environment env) throws EvalError {
        if (args.size() != 1) throw new EvalError("cdr: need exactly 1 argument");
        SchemeValue val = eval(args.getFirst(), env);
        if (val instanceof SchemeValue.ListVal lst && !lst.elements().isEmpty()) {
            return new SchemeValue.ListVal(lst.elements().subList(1, lst.elements().size()));
        }
        throw new EvalError("cdr: not a pair");
    }

    private SchemeValue builtinNullQ(List<SchemeValue> args, Environment env) throws EvalError {
        if (args.size() != 1) throw new EvalError("null?: need exactly 1 argument");
        SchemeValue val = eval(args.getFirst(), env);
        return new SchemeValue.BoolVal(val instanceof SchemeValue.ListVal lst && lst.elements().isEmpty());
    }

    private SchemeValue builtinList(List<SchemeValue> args, Environment env) throws EvalError {
        List<SchemeValue> elems = new ArrayList<>();
        for (SchemeValue arg : args) {
            elems.add(eval(arg, env));
        }
        return new SchemeValue.ListVal(elems);
    }

    private SchemeValue builtinLength(List<SchemeValue> args, Environment env) throws EvalError {
        if (args.size() != 1) throw new EvalError("length: need exactly 1 argument");
        SchemeValue val = eval(args.getFirst(), env);
        if (val instanceof SchemeValue.ListVal lst) {
            return new SchemeValue.IntVal(lst.elements().size());
        }
        throw new EvalError("length: not a list");
    }

    private SchemeValue builtinAppend(List<SchemeValue> args, Environment env) throws EvalError {
        if (args.size() == 0) return new SchemeValue.ListVal(List.of());
        List<SchemeValue> result = new ArrayList<>();
        for (int i = 0; i < args.size() - 1; i++) {
            SchemeValue val = eval(args.get(i), env);
            if (!(val instanceof SchemeValue.ListVal lst))
                throw new EvalError("append: not a list");
            result.addAll(lst.elements());
        }
        SchemeValue last = eval(args.getLast(), env);
        if (last instanceof SchemeValue.ListVal lst) {
            result.addAll(lst.elements());
            return new SchemeValue.ListVal(result);
        }
        // For now, only proper lists
        throw new EvalError("append: last argument must be a list");
    }

    private SchemeValue builtinPairQ(List<SchemeValue> args, Environment env) throws EvalError {
        if (args.size() != 1) throw new EvalError("pair?: need exactly 1 argument");
        SchemeValue val = eval(args.getFirst(), env);
        return new SchemeValue.BoolVal(val instanceof SchemeValue.ListVal lst && !lst.elements().isEmpty());
    }

    private SchemeValue typePred(List<SchemeValue> args, Environment env, Class<? extends SchemeValue> type) throws EvalError {
        if (args.size() != 1) throw new EvalError("type predicate: need exactly 1 argument");
        SchemeValue val = eval(args.getFirst(), env);
        return new SchemeValue.BoolVal(type.isInstance(val));
    }

    private long requireInt(SchemeValue v) throws EvalError {
        if (v instanceof SchemeValue.IntVal i) return i.value();
        throw new EvalError("expected integer, got: " + v.display());
    }
}
