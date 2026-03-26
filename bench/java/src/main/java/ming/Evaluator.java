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

    static boolean isFalse(Object val) {
        return val instanceof Boolean b && !b;
    }

    static Object applyProc(Object func, List<Object> args) throws EvalError {
        if (func instanceof Builtin b) {
            return b.apply(args);
        }
        throw new EvalError("not a procedure: " + SchemeValue.toStr(func));
    }
}
