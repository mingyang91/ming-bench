package ming;

import java.util.ArrayList;
import java.util.List;

/**
 * Scheme interpreter entry point.
 */
public class Evaluator {

    public String evalStr(String input) throws EvalError {
        List<Object> exprs = Parser.parse(input);
        Object result = null;
        Env env = Env.global();
        for (Object expr : exprs) {
            result = eval(expr, env);
        }
        return SchemeValue.toStr(result);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        throw new EvalError("not implemented");
    }

    @SuppressWarnings("unchecked")
    static Object eval(Object expr, Env env) throws EvalError {
        if (expr instanceof Long || expr instanceof Boolean) {
            return expr;
        }
        if (expr instanceof String s) {
            if (s.startsWith("\"")) return s; // string literal
            // symbol lookup
            return env.lookup(s);
        }
        if (expr instanceof List<?> list) {
            if (list.isEmpty()) throw new EvalError("empty application");
            Object first = list.get(0);

            // Special forms
            if (first instanceof String op) {
                switch (op) {
                    case "define" -> {
                        return evalDefine(list, env);
                    }
                    case "if" -> {
                        return evalIf(list, env);
                    }
                    case "quote" -> {
                        if (list.size() != 2) throw new EvalError("quote: expected 1 argument");
                        return list.get(1);
                    }
                    case "lambda" -> {
                        return evalLambda(list, env);
                    }
                    case "and" -> {
                        Object result = Boolean.TRUE;
                        for (int i = 1; i < list.size(); i++) {
                            result = eval(list.get(i), env);
                            if (isFalse(result)) return result;
                        }
                        return result;
                    }
                    case "or" -> {
                        Object result = Boolean.FALSE;
                        for (int i = 1; i < list.size(); i++) {
                            result = eval(list.get(i), env);
                            if (!isFalse(result)) return result;
                        }
                        return result;
                    }
                }
            }

            // Function application
            Object func = eval(first, env);
            List<Object> args = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) {
                args.add(eval(list.get(i), env));
            }
            return applyProc(func, args);
        }
        throw new EvalError("cannot evaluate: " + expr);
    }

    private static Object evalDefine(List<?> list, Env env) throws EvalError {
        if (list.size() < 3) throw new EvalError("define: bad syntax");
        Object target = list.get(1);
        if (target instanceof String name) {
            // (define x expr)
            Object val = eval(list.get(2), env);
            env.define(name, val);
            return null; // void
        }
        if (target instanceof List<?> sig) {
            // (define (f params...) body...)
            if (sig.isEmpty() || !(sig.get(0) instanceof String name))
                throw new EvalError("define: bad syntax");
            List<String> params = new ArrayList<>();
            for (int i = 1; i < sig.size(); i++) {
                if (!(sig.get(i) instanceof String p))
                    throw new EvalError("define: parameter must be symbol");
                params.add(p);
            }
            List<Object> body = new ArrayList<>();
            for (int i = 2; i < list.size(); i++) {
                body.add(list.get(i));
            }
            Lambda lambda = new Lambda(params, body, env);
            env.define(name, lambda);
            return null; // void
        }
        throw new EvalError("define: bad syntax");
    }

    private static Object evalIf(List<?> list, Env env) throws EvalError {
        if (list.size() < 3 || list.size() > 4) throw new EvalError("if: bad syntax");
        Object cond = eval(list.get(1), env);
        if (!isFalse(cond)) {
            return eval(list.get(2), env);
        } else if (list.size() == 4) {
            return eval(list.get(3), env);
        }
        return null; // void
    }

    private static Object evalLambda(List<?> list, Env env) throws EvalError {
        if (list.size() < 3) throw new EvalError("lambda: bad syntax");
        Object paramSpec = list.get(1);
        if (!(paramSpec instanceof List<?> paramList))
            throw new EvalError("lambda: parameters must be a list");
        List<String> params = new ArrayList<>();
        for (Object p : paramList) {
            if (!(p instanceof String s)) throw new EvalError("lambda: parameter must be symbol");
            params.add(s);
        }
        List<Object> body = new ArrayList<>();
        for (int i = 2; i < list.size(); i++) {
            body.add(list.get(i));
        }
        return new Lambda(params, body, env);
    }

    static boolean isFalse(Object val) {
        return val instanceof Boolean b && !b;
    }

    static Object applyProc(Object func, List<Object> args) throws EvalError {
        if (func instanceof Builtin b) {
            return b.apply(args);
        }
        if (func instanceof Lambda lam) {
            if (args.size() != lam.params.size())
                throw new EvalError("lambda: expected " + lam.params.size() + " arguments, got " + args.size());
            Env localEnv = new Env(lam.closure);
            for (int i = 0; i < lam.params.size(); i++) {
                localEnv.define(lam.params.get(i), args.get(i));
            }
            Object result = null;
            for (Object bodyExpr : lam.body) {
                result = eval(bodyExpr, localEnv);
            }
            return result;
        }
        throw new EvalError("not a procedure: " + SchemeValue.toStr(func));
    }
}
