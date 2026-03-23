package ming;

import java.util.ArrayList;
import java.util.List;

/**
 * Scheme interpreter entry point.
 * Agents implement this class.
 */
public class Evaluator {
    /**
     * Evaluate one or more Scheme expressions and return the string
     * representation of the last result.
     */
    public String evalStr(String input) throws EvalError {
        var tokens = new Tokenizer(input).tokenize();
        var exprs = new Parser(tokens).parseAll();
        if (exprs.isEmpty()) throw new EvalError("No expressions");
        Environment env = createGlobalEnv();
        SchemeValue result = null;
        for (SchemeValue expr : exprs) {
            result = eval(expr, env);
        }
        if (result instanceof SchemeValue.VoidVal) return "#<void>";
        return result.display();
    }

    /**
     * Evaluate Scheme expressions and return both the result string
     * and any captured output from display/write/newline.
     */
    public EvalResult evalStrWithOutput(String input) throws EvalError {
        throw new EvalError("not implemented");
    }

    private Environment createGlobalEnv() {
        Environment env = new Environment();
        registerBuiltins(env);
        return env;
    }

    private void registerBuiltins(Environment env) {
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
        env.define("<", new SchemeValue.BuiltinVal("<", args ->
            new SchemeValue.BoolVal(requireInt(args.get(0)) < requireInt(args.get(1)))));
        env.define(">", new SchemeValue.BuiltinVal(">", args ->
            new SchemeValue.BoolVal(requireInt(args.get(0)) > requireInt(args.get(1)))));
        env.define("=", new SchemeValue.BuiltinVal("=", args ->
            new SchemeValue.BoolVal(requireInt(args.get(0)) == requireInt(args.get(1)))));
        env.define("<=", new SchemeValue.BuiltinVal("<=", args ->
            new SchemeValue.BoolVal(requireInt(args.get(0)) <= requireInt(args.get(1)))));
        env.define("not", new SchemeValue.BuiltinVal("not", args ->
            new SchemeValue.BoolVal(!args.getFirst().isTruthy())));
    }

    private SchemeValue eval(SchemeValue expr, Environment env) throws EvalError {
        return switch (expr) {
            case SchemeValue.IntVal v -> v;
            case SchemeValue.BoolVal v -> v;
            case SchemeValue.StringVal v -> v;
            case SchemeValue.VoidVal v -> v;
            case SchemeValue.LambdaVal v -> v;
            case SchemeValue.BuiltinVal v -> v;
            case SchemeValue.SymbolVal v -> env.get(v.name());
            case SchemeValue.ListVal v -> evalList(v.elements(), env);
        };
    }

    private SchemeValue evalList(List<SchemeValue> elems, Environment env) throws EvalError {
        if (elems.isEmpty()) throw new EvalError("Empty application");
        SchemeValue head = elems.getFirst();
        if (head instanceof SchemeValue.SymbolVal sym) {
            String name = sym.name();
            switch (name) {
                case "and": return evalAnd(elems, env);
                case "or": return evalOr(elems, env);
                case "if": return evalIf(elems, env);
                case "define": return evalDefine(elems, env);
                case "quote": return evalQuote(elems);
                case "lambda": return evalLambda(elems, env);
                default: break;
            }
        }
        // Procedure call: evaluate all, then apply
        SchemeValue proc = eval(head, env);
        List<SchemeValue> args = new ArrayList<>();
        for (int i = 1; i < elems.size(); i++) {
            args.add(eval(elems.get(i), env));
        }
        return apply(proc, args);
    }

    private SchemeValue apply(SchemeValue proc, List<SchemeValue> args) throws EvalError {
        if (proc instanceof SchemeValue.BuiltinVal builtin) {
            return builtin.func().apply(args);
        }
        if (proc instanceof SchemeValue.LambdaVal lambda) {
            if (lambda.params().size() != args.size()) {
                throw new EvalError("Expected " + lambda.params().size() + " arguments, got " + args.size());
            }
            Environment callEnv = new Environment(lambda.env());
            for (int i = 0; i < lambda.params().size(); i++) {
                callEnv.define(lambda.params().get(i), args.get(i));
            }
            SchemeValue result = null;
            for (SchemeValue bodyExpr : lambda.body()) {
                result = eval(bodyExpr, callEnv);
            }
            return result;
        }
        throw new EvalError("Not a procedure: " + proc.display());
    }

    private SchemeValue evalIf(List<SchemeValue> elems, Environment env) throws EvalError {
        if (elems.size() < 3 || elems.size() > 4) throw new EvalError("if requires 2 or 3 arguments");
        SchemeValue cond = eval(elems.get(1), env);
        if (cond.isTruthy()) {
            return eval(elems.get(2), env);
        } else if (elems.size() == 4) {
            return eval(elems.get(3), env);
        }
        return new SchemeValue.VoidVal();
    }

    private SchemeValue evalDefine(List<SchemeValue> elems, Environment env) throws EvalError {
        if (elems.size() < 3) throw new EvalError("define requires at least 2 arguments");
        SchemeValue target = elems.get(1);
        if (target instanceof SchemeValue.SymbolVal sym) {
            // (define x expr)
            SchemeValue val = eval(elems.get(2), env);
            env.define(sym.name(), val);
            return new SchemeValue.VoidVal();
        } else if (target instanceof SchemeValue.ListVal nameAndParams) {
            // (define (f x y) body...)
            List<SchemeValue> parts = nameAndParams.elements();
            if (parts.isEmpty()) throw new EvalError("define: empty name list");
            if (!(parts.getFirst() instanceof SchemeValue.SymbolVal fnName)) {
                throw new EvalError("define: expected symbol as function name");
            }
            List<String> params = new ArrayList<>();
            for (int i = 1; i < parts.size(); i++) {
                if (!(parts.get(i) instanceof SchemeValue.SymbolVal p)) {
                    throw new EvalError("define: expected symbol as parameter");
                }
                params.add(p.name());
            }
            List<SchemeValue> body = elems.subList(2, elems.size());
            SchemeValue.LambdaVal lambda = new SchemeValue.LambdaVal(params, body, env);
            env.define(fnName.name(), lambda);
            return new SchemeValue.VoidVal();
        }
        throw new EvalError("define: invalid syntax");
    }

    private SchemeValue evalQuote(List<SchemeValue> elems) throws EvalError {
        if (elems.size() != 2) throw new EvalError("quote requires exactly 1 argument");
        return elems.get(1);
    }

    private SchemeValue evalLambda(List<SchemeValue> elems, Environment env) throws EvalError {
        if (elems.size() < 3) throw new EvalError("lambda requires parameters and body");
        SchemeValue paramSpec = elems.get(1);
        if (!(paramSpec instanceof SchemeValue.ListVal paramList)) {
            throw new EvalError("lambda: expected parameter list");
        }
        List<String> params = new ArrayList<>();
        for (SchemeValue p : paramList.elements()) {
            if (!(p instanceof SchemeValue.SymbolVal sym)) {
                throw new EvalError("lambda: expected symbol as parameter");
            }
            params.add(sym.name());
        }
        List<SchemeValue> body = elems.subList(2, elems.size());
        return new SchemeValue.LambdaVal(params, body, env);
    }

    private SchemeValue evalAnd(List<SchemeValue> elems, Environment env) throws EvalError {
        SchemeValue result = new SchemeValue.BoolVal(true);
        for (int i = 1; i < elems.size(); i++) {
            result = eval(elems.get(i), env);
            if (!result.isTruthy()) return result;
        }
        return result;
    }

    private SchemeValue evalOr(List<SchemeValue> elems, Environment env) throws EvalError {
        SchemeValue result = new SchemeValue.BoolVal(false);
        for (int i = 1; i < elems.size(); i++) {
            result = eval(elems.get(i), env);
            if (result.isTruthy()) return result;
        }
        return result;
    }

    private long requireInt(SchemeValue val) throws EvalError {
        if (val instanceof SchemeValue.IntVal iv) return iv.value();
        throw new EvalError("Expected integer, got: " + val.display());
    }
}
