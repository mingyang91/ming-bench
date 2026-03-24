package ming;

import java.util.ArrayList;
import java.util.List;

public class Interpreter {
    private final Environment globals = new Environment();

    public Interpreter() {
        registerBuiltins();
    }

    private void registerBuiltins() {
        globals.define("+", new SchemeValue.BuiltinVal("+", args -> {
            long result = 0;
            for (var arg : args) result += asLong(arg);
            return new SchemeValue.IntVal(result);
        }));
        globals.define("*", new SchemeValue.BuiltinVal("*", args -> {
            long result = 1;
            for (var arg : args) result *= asLong(arg);
            return new SchemeValue.IntVal(result);
        }));
        globals.define("-", new SchemeValue.BuiltinVal("-", args -> {
            if (args.length == 0) throw new EvalError("-: need at least one argument");
            if (args.length == 1) return new SchemeValue.IntVal(-asLong(args[0]));
            long result = asLong(args[0]);
            for (int i = 1; i < args.length; i++) result -= asLong(args[i]);
            return new SchemeValue.IntVal(result);
        }));
        globals.define("/", new SchemeValue.BuiltinVal("/", args -> {
            if (args.length == 0) throw new EvalError("/: need at least one argument");
            long result = asLong(args[0]);
            for (int i = 1; i < args.length; i++) {
                long d = asLong(args[i]);
                if (d == 0) throw new EvalError("division by zero");
                result /= d;
            }
            return new SchemeValue.IntVal(result);
        }));
        globals.define("<", new SchemeValue.BuiltinVal("<", args -> compare(args, (a, b) -> a < b)));
        globals.define(">", new SchemeValue.BuiltinVal(">", args -> compare(args, (a, b) -> a > b)));
        globals.define("=", new SchemeValue.BuiltinVal("=", args -> compare(args, (a, b) -> a == b)));
        globals.define("<=", new SchemeValue.BuiltinVal("<=", args -> compare(args, (a, b) -> a <= b)));
        globals.define(">=", new SchemeValue.BuiltinVal(">=", args -> compare(args, (a, b) -> a >= b)));
        globals.define("not", new SchemeValue.BuiltinVal("not", args -> {
            if (args.length != 1) throw new EvalError("not: expected 1 argument");
            return new SchemeValue.BoolVal(!args[0].isTruthy());
        }));
    }

    public SchemeValue eval(SchemeValue expr) throws EvalError {
        return eval(expr, globals);
    }

    public SchemeValue eval(SchemeValue expr, Environment env) throws EvalError {
        return switch (expr) {
            case SchemeValue.IntVal v -> v;
            case SchemeValue.BoolVal v -> v;
            case SchemeValue.StringVal v -> v;
            case SchemeValue.VoidVal v -> v;
            case SchemeValue.LambdaVal v -> v;
            case SchemeValue.BuiltinVal v -> v;
            case SchemeValue.SymbolVal v -> env.get(v.name());
            case SchemeValue.ListVal v -> evalList(v, env);
        };
    }

    private SchemeValue evalList(SchemeValue.ListVal list, Environment env) throws EvalError {
        if (list.elements().isEmpty()) throw new EvalError("empty application");

        var first = list.elements().get(0);
        if (first instanceof SchemeValue.SymbolVal sym) {
            return switch (sym.name()) {
                case "define" -> evalDefine(list.elements(), env);
                case "if" -> evalIf(list.elements(), env);
                case "quote" -> evalQuote(list.elements());
                case "lambda" -> evalLambda(list.elements(), env);
                case "and" -> evalAnd(list.elements(), env);
                case "or" -> evalOr(list.elements(), env);
                default -> evalApplication(list.elements(), env);
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
            var lambda = new SchemeValue.LambdaVal(params, body, env);
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
        return elements.get(1);
    }

    private SchemeValue evalLambda(List<SchemeValue> elements, Environment env) throws EvalError {
        if (elements.size() < 3) throw new EvalError("lambda: bad syntax");
        var paramList = elements.get(1);
        if (!(paramList instanceof SchemeValue.ListVal pl))
            throw new EvalError("lambda: expected parameter list");
        var params = new ArrayList<String>();
        for (var p : pl.elements()) {
            if (!(p instanceof SchemeValue.SymbolVal sym))
                throw new EvalError("lambda: expected parameter name");
            params.add(sym.name());
        }
        var body = elements.subList(2, elements.size());
        return new SchemeValue.LambdaVal(params, body, env);
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

    private SchemeValue evalApplication(List<SchemeValue> elements, Environment env) throws EvalError {
        var proc = eval(elements.get(0), env);
        var args = new SchemeValue[elements.size() - 1];
        for (int i = 1; i < elements.size(); i++) {
            args[i - 1] = eval(elements.get(i), env);
        }

        if (proc instanceof SchemeValue.LambdaVal lambda) {
            if (args.length != lambda.params().size())
                throw new EvalError("wrong number of arguments: expected " + lambda.params().size() + ", got " + args.length);
            var callEnv = new Environment(lambda.env());
            for (int i = 0; i < lambda.params().size(); i++) {
                callEnv.define(lambda.params().get(i), args[i]);
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

    @FunctionalInterface
    interface LongCmp { boolean test(long a, long b); }

    private SchemeValue compare(SchemeValue[] args, LongCmp cmp) throws EvalError {
        if (args.length < 2) throw new EvalError("comparison needs at least 2 arguments");
        for (int i = 0; i < args.length - 1; i++) {
            if (!cmp.test(asLong(args[i]), asLong(args[i + 1]))) {
                return new SchemeValue.BoolVal(false);
            }
        }
        return new SchemeValue.BoolVal(true);
    }

    private static long asLong(SchemeValue v) throws EvalError {
        if (v instanceof SchemeValue.IntVal iv) return iv.value();
        throw new EvalError("expected number, got: " + v.display());
    }
}
