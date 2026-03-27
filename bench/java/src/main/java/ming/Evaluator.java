package ming;

import static ming.Numbers.*;
import static ming.SchemeFormatter.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class Evaluator {

    static final Object EMPTY_LIST = new Object() {
        @Override public String toString() { return "()"; }
    };

    // --- Environment ---

    static class Env {
        final Map<String, Object> bindings = new HashMap<>();
        final Env parent;

        Env(Env parent) {
            this.parent = parent;
        }

        Object lookup(String name, SchemeParser.Pos pos) throws EvalError {
            if (bindings.containsKey(name)) return bindings.get(name);
            if (parent != null) return parent.lookup(name, pos);
            throw new EvalError("unbound variable: " + name + " at " + pos.fmt());
        }

        void define(String name, Object val) {
            bindings.put(name, val);
        }

        void set(String name, Object val, SchemeParser.Pos pos) throws EvalError {
            if (bindings.containsKey(name)) {
                bindings.put(name, val);
                return;
            }
            if (parent != null) {
                parent.set(name, val, pos);
                return;
            }
            throw new EvalError("set!: unbound variable: " + name + " at " + pos.fmt());
        }
    }

    // --- Pair (cons cell) ---

    static class Pair {
        Object car;
        Object cdr;
        Pair(Object car, Object cdr) {
            this.car = car;
            this.cdr = cdr;
        }
    }

    // --- Lambda (closure) ---

    record Lambda(List<String> params, String restParam, List<Object> body, Env closureEnv) {}

    // --- CaseLambda (multiple-arity closure) ---

    record CaseLambda(List<Lambda> clauses) {}

    // --- Builtin procedure ---

    @FunctionalInterface
    interface Builtin {
        Object apply(List<Object> args) throws EvalError;
    }

    record BuiltinProc(String name, Builtin fn) {}

    // Sentinel for tail-call optimization trampoline
    private static final class TailCall {
        final Object expr;
        final Env env;
        TailCall(Object expr, Env env) { this.expr = expr; this.env = env; }
    }

    private static final Set<String> SPECIAL_FORMS = Set.of(
        "if", "begin", "let", "let*", "set!", "define", "quote", "lambda", "case-lambda",
        "and", "or", "cond", "case", "do", "letrec", "letrec*",
        "define-syntax", "syntax-rules", "define-record-type"
    );

    private int gensymCounter = 0;

    // Output buffer for display/write/newline
    private StringBuilder outputBuffer;

    public String evalStr(String input) throws EvalError {
        outputBuffer = new StringBuilder();
        List<SchemeParser.Token> tokens = SchemeParser.tokenize(input);
        int[] pos = {0};
        Object lastResult = null;
        Env env = createGlobalEnv();
        while (pos[0] < tokens.size()) {
            Object expr = SchemeParser.parse(tokens, pos);
            lastResult = eval(expr, env);
        }
        if (lastResult == null) {
            throw new EvalError("no expression");
        }
        return schemeToString(lastResult);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        outputBuffer = new StringBuilder();
        List<SchemeParser.Token> tokens = SchemeParser.tokenize(input);
        int[] pos = {0};
        Object lastResult = null;
        Env env = createGlobalEnv();
        while (pos[0] < tokens.size()) {
            Object expr = SchemeParser.parse(tokens, pos);
            lastResult = eval(expr, env);
        }
        if (lastResult == null) {
            throw new EvalError("no expression");
        }
        return new EvalResult(schemeToString(lastResult), outputBuffer.toString());
    }

    private Env createGlobalEnv() {
        Env env = new Env(null);
        Builtins.registerAll(env, outputBuffer, this::applyProc);
        return env;
    }

    // --- Evaluator ---

    @SuppressWarnings("unchecked")
    private Object eval(Object expr, Env env) throws EvalError {
        while (true) {
            int eLine = 0, eCol = 0;
            if (expr instanceof SchemeParser.Located loc) {
                eLine = loc.line;
                eCol = loc.col;
                expr = loc.expr;
            }
            String posStr = eLine > 0 ? " at " + eLine + ":" + eCol : "";

            if (expr instanceof Long || expr instanceof Double || expr instanceof Rational
                    || expr instanceof Boolean || expr instanceof SchemeString || expr instanceof SchemeChar) {
                return expr;
            }
            if (expr instanceof String sym) {
                return env.lookup(sym, new SchemeParser.Pos(eLine, eCol));
            }
            if (!(expr instanceof List<?> list)) {
                throw new EvalError("cannot evaluate: " + expr + posStr);
            }
            if (list.isEmpty()) {
                throw new EvalError("empty application" + posStr);
            }
            Object head = list.get(0);
            Object rawHead = head;
            if (rawHead instanceof SchemeParser.Located loc) rawHead = loc.expr;

            if (rawHead instanceof String op) {
                switch (op) {
                    case "quote" -> {
                        if (list.size() != 2) throw new EvalError("quote: expected 1 argument" + posStr);
                        Object datum = list.get(1);
                        if (datum instanceof SchemeParser.Located loc) datum = loc.expr;
                        return quoteValue(datum);
                    }
                    case "if" -> {
                        if (list.size() < 3 || list.size() > 4)
                            throw new EvalError("if: expected 2-3 arguments" + posStr);
                        Object cond = eval(list.get(1), env);
                        if (isTruthy(cond)) {
                            expr = list.get(2); continue; // TCO
                        } else if (list.size() == 4) {
                            expr = list.get(3); continue; // TCO
                        } else {
                            return Boolean.FALSE;
                        }
                    }
                    case "define" -> { return evalDefine(list, env, posStr); }
                    case "set!" -> {
                        if (list.size() != 3) throw new EvalError("set!: expected 2 arguments" + posStr);
                        Object target = list.get(1);
                        if (target instanceof SchemeParser.Located loc) target = loc.expr;
                        if (!(target instanceof String name))
                            throw new EvalError("set!: target must be a symbol" + posStr);
                        Object val = eval(list.get(2), env);
                        env.set(name, val, new SchemeParser.Pos(eLine, eCol));
                        return val;
                    }
                    case "lambda" -> { return evalLambda(list, env, posStr); }
                    case "case-lambda" -> { return evalCaseLambda(list, env, posStr); }
                    case "and" -> {
                        if (list.size() == 1) return Boolean.TRUE;
                        for (int i = 1; i < list.size() - 1; i++) {
                            Object result = eval(list.get(i), env);
                            if (!isTruthy(result)) return result;
                        }
                        expr = list.get(list.size() - 1); continue; // TCO
                    }
                    case "or" -> {
                        if (list.size() == 1) return Boolean.FALSE;
                        for (int i = 1; i < list.size() - 1; i++) {
                            Object result = eval(list.get(i), env);
                            if (isTruthy(result)) return result;
                        }
                        expr = list.get(list.size() - 1); continue; // TCO
                    }
                    case "begin" -> {
                        if (list.size() == 1) { return Boolean.FALSE; }
                        for (int i = 1; i < list.size() - 1; i++) eval(list.get(i), env);
                        expr = list.get(list.size() - 1); continue; // TCO
                    }
                    case "cond" -> {
                        Object r = evalCond(list, env, posStr);
                        if (r instanceof TailCall tc) { expr = tc.expr; env = tc.env; continue; }
                        return r;
                    }
                    case "let" -> {
                        Object r = evalLet(list, env, posStr);
                        if (r instanceof TailCall tc) { expr = tc.expr; env = tc.env; continue; }
                        return r;
                    }
                    case "let*" -> {
                        Object r = evalLetStar(list, env, posStr);
                        if (r instanceof TailCall tc) { expr = tc.expr; env = tc.env; continue; }
                        return r;
                    }
                    case "letrec" -> {
                        Object r = evalLetrec(list, env, posStr);
                        if (r instanceof TailCall tc) { expr = tc.expr; env = tc.env; continue; }
                        return r;
                    }
                    case "letrec*" -> {
                        Object r = evalLetrecStar(list, env, posStr);
                        if (r instanceof TailCall tc) { expr = tc.expr; env = tc.env; continue; }
                        return r;
                    }
                    case "case" -> {
                        Object r = evalCase(list, env, posStr);
                        if (r instanceof TailCall tc) { expr = tc.expr; env = tc.env; continue; }
                        return r;
                    }
                    case "do" -> { return evalDo(list, env, posStr); }
                    case "define-syntax" -> { return evalDefineSyntax(list, env, posStr); }
                    case "define-record-type" -> { return evalDefineRecordType(list, env, posStr); }
                    default -> {
                        // Check for macro invocation
                        Object macroVal = null;
                        try { macroVal = env.lookup(op, new SchemeParser.Pos(eLine, eCol)); } catch (EvalError ignored) {}
                        if (macroVal instanceof SyntaxRulesMacro macro) {
                            Object r = evalMacro(macro, list, env);
                            if (r instanceof TailCall tc) { expr = tc.expr; env = tc.env; continue; }
                            return r;
                        }
                    }
                }
            }

            // Procedure application
            Object proc = eval(head, env);
            List<Object> args = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) {
                args.add(eval(list.get(i), env));
            }

            if (proc instanceof Lambda lambda) {
                env = bindLambdaArgs(lambda, args);
                if (lambda.body.isEmpty()) return Boolean.FALSE;
                for (int i = 0; i < lambda.body.size() - 1; i++) eval(lambda.body.get(i), env);
                expr = lambda.body.get(lambda.body.size() - 1);
                continue; // TCO
            }
            if (proc instanceof CaseLambda cl) {
                Lambda matched = findMatchingClause(cl, args);
                env = bindLambdaArgs(matched, args);
                if (matched.body.isEmpty()) return Boolean.FALSE;
                for (int i = 0; i < matched.body.size() - 1; i++) eval(matched.body.get(i), env);
                expr = matched.body.get(matched.body.size() - 1);
                continue; // TCO
            }
            if (proc instanceof BuiltinProc bp) {
                try {
                    return bp.fn.apply(args);
                } catch (EvalError e) {
                    if (eLine > 0 && !e.getMessage().matches(".*\\d+:\\d+.*")) {
                        throw new EvalError(e.getMessage() + posStr);
                    }
                    throw e;
                }
            }
            throw new EvalError("not a procedure: " + schemeToString(proc) + posStr);
        }
    }

    private Env bindLambdaArgs(Lambda lambda, List<Object> args) throws EvalError {
        if (lambda.restParam != null) {
            if (args.size() < lambda.params.size()) {
                throw new EvalError("wrong number of arguments: expected at least " +
                        lambda.params.size() + ", got " + args.size());
            }
        } else {
            if (args.size() != lambda.params.size()) {
                throw new EvalError("wrong number of arguments: expected " +
                        lambda.params.size() + ", got " + args.size());
            }
        }
        Env callEnv = new Env(lambda.closureEnv);
        for (int i = 0; i < lambda.params.size(); i++) {
            callEnv.define(lambda.params.get(i), args.get(i));
        }
        if (lambda.restParam != null) {
            Object rest = EMPTY_LIST;
            for (int i = args.size() - 1; i >= lambda.params.size(); i--) {
                rest = new Pair(args.get(i), rest);
            }
            callEnv.define(lambda.restParam, rest);
        }
        return callEnv;
    }

    private Lambda findMatchingClause(CaseLambda cl, List<Object> args) throws EvalError {
        for (Lambda clause : cl.clauses) {
            if (clause.restParam != null) {
                if (args.size() >= clause.params.size()) return clause;
            } else {
                if (args.size() == clause.params.size()) return clause;
            }
        }
        throw new EvalError("case-lambda: no matching clause for " + args.size() + " arguments");
    }

    private Object evalDefine(List<?> list, Env env, String posStr) throws EvalError {
        if (list.size() < 3) throw new EvalError("define: too few arguments" + posStr);
        Object target = list.get(1);
        if (target instanceof SchemeParser.Located loc) target = loc.expr;
        if (target instanceof String name) {
            Object val = eval(list.get(2), env);
            env.define(name, val);
            return val;
        } else if (target instanceof List<?> sig) {
            if (sig.isEmpty()) throw new EvalError("define: invalid function signature" + posStr);
            Object first = sig.get(0);
            if (first instanceof SchemeParser.Located loc) first = loc.expr;
            if (!(first instanceof String fname))
                throw new EvalError("define: invalid function signature" + posStr);
            List<String> params = new ArrayList<>();
            String restParam = null;
            for (int i = 1; i < sig.size(); i++) {
                Object p = sig.get(i);
                if (p instanceof SchemeParser.Located loc) p = loc.expr;
                if (p instanceof String pname && pname.equals(".")) {
                    if (i + 1 >= sig.size())
                        throw new EvalError("define: missing rest parameter after dot" + posStr);
                    Object rp = sig.get(i + 1);
                    if (rp instanceof SchemeParser.Located loc) rp = loc.expr;
                    if (!(rp instanceof String rpname))
                        throw new EvalError("define: rest parameter must be symbol" + posStr);
                    restParam = rpname;
                    break;
                }
                if (!(p instanceof String pname))
                    throw new EvalError("define: parameter must be symbol" + posStr);
                params.add(pname);
            }
            List<Object> body = new ArrayList<>(list.subList(2, list.size()));
            Lambda lambda = new Lambda(params, restParam, body, env);
            env.define(fname, lambda);
            return lambda;
        } else {
            throw new EvalError("define: invalid syntax" + posStr);
        }
    }

    private Object evalLambda(List<?> list, Env env, String posStr) throws EvalError {
        if (list.size() < 3) throw new EvalError("lambda: too few arguments" + posStr);
        Object paramSpec = list.get(1);
        if (paramSpec instanceof SchemeParser.Located loc) paramSpec = loc.expr;
        if (!(paramSpec instanceof List<?> paramList))
            throw new EvalError("lambda: params must be a list" + posStr);
        List<String> params = new ArrayList<>();
        String restParam = parseParamList(paramList, params, "lambda", posStr);
        List<Object> body = new ArrayList<>(list.subList(2, list.size()));
        return new Lambda(params, restParam, body, env);
    }

    private Object evalCaseLambda(List<?> list, Env env, String posStr) throws EvalError {
        List<Lambda> clauses = new ArrayList<>();
        for (int ci = 1; ci < list.size(); ci++) {
            Object clauseObj = list.get(ci);
            if (clauseObj instanceof SchemeParser.Located loc) clauseObj = loc.expr;
            if (!(clauseObj instanceof List<?> clause) || clause.size() < 2)
                throw new EvalError("case-lambda: bad clause" + posStr);
            Object paramSpec = clause.get(0);
            if (paramSpec instanceof SchemeParser.Located loc) paramSpec = loc.expr;
            if (!(paramSpec instanceof List<?> paramList))
                throw new EvalError("case-lambda: params must be a list" + posStr);
            List<String> params = new ArrayList<>();
            String restParam = parseParamList(paramList, params, "case-lambda", posStr);
            List<Object> body = new ArrayList<>(clause.subList(1, clause.size()));
            clauses.add(new Lambda(params, restParam, body, env));
        }
        return new CaseLambda(clauses);
    }

    private String parseParamList(List<?> paramList, List<String> params,
                                   String formName, String posStr) throws EvalError {
        String restParam = null;
        for (int pi = 0; pi < paramList.size(); pi++) {
            Object p = paramList.get(pi);
            if (p instanceof SchemeParser.Located loc) p = loc.expr;
            if (p instanceof String pname && pname.equals(".")) {
                if (pi + 1 >= paramList.size())
                    throw new EvalError(formName + ": missing rest parameter after dot" + posStr);
                Object rp = paramList.get(pi + 1);
                if (rp instanceof SchemeParser.Located loc) rp = loc.expr;
                if (!(rp instanceof String rpname))
                    throw new EvalError(formName + ": rest parameter must be symbol" + posStr);
                restParam = rpname;
                break;
            }
            if (!(p instanceof String pname))
                throw new EvalError(formName + ": parameter must be symbol" + posStr);
            params.add(pname);
        }
        return restParam;
    }

    private Object evalCond(List<?> list, Env env, String posStr) throws EvalError {
        for (int i = 1; i < list.size(); i++) {
            Object clauseObj = list.get(i);
            if (clauseObj instanceof SchemeParser.Located loc) clauseObj = loc.expr;
            if (!(clauseObj instanceof List<?> clause) || clause.isEmpty())
                throw new EvalError("cond: invalid clause" + posStr);
            Object test = clause.get(0);
            if (test instanceof SchemeParser.Located loc) test = loc.expr;
            if (test instanceof String s && s.equals("else")) {
                return evalBodyTail(new ArrayList<>(clause.subList(1, clause.size())), env);
            }
            Object testVal = eval(clause.get(0), env);
            if (isTruthy(testVal)) {
                if (clause.size() == 1) return testVal;
                return evalBodyTail(new ArrayList<>(clause.subList(1, clause.size())), env);
            }
        }
        return Boolean.FALSE;
    }

    private Object evalLet(List<?> list, Env env, String posStr) throws EvalError {
        int bindingsIdx = 1;
        String namedLetName = null;
        Object firstArg = list.get(1);
        if (firstArg instanceof SchemeParser.Located loc) firstArg = loc.expr;
        if (firstArg instanceof String name) {
            namedLetName = name;
            bindingsIdx = 2;
        }
        Object bindingsObj = list.get(bindingsIdx);
        if (bindingsObj instanceof SchemeParser.Located loc) bindingsObj = loc.expr;
        if (!(bindingsObj instanceof List<?> bindingList))
            throw new EvalError("let: bindings must be a list" + posStr);
        List<String> varNames = new ArrayList<>();
        List<Object> initExprs = new ArrayList<>();
        parseBindings(bindingList, varNames, initExprs, "let", posStr);
        Env letEnv = new Env(env);
        if (namedLetName != null) {
            List<Object> bodyExprs = new ArrayList<>(list.subList(bindingsIdx + 1, list.size()));
            Lambda loopLambda = new Lambda(varNames, null, bodyExprs, letEnv);
            letEnv.define(namedLetName, loopLambda);
            for (int i = 0; i < varNames.size(); i++) {
                letEnv.define(varNames.get(i), eval(initExprs.get(i), env));
            }
            return evalBodyTail(bodyExprs, letEnv);
        } else {
            for (int i = 0; i < varNames.size(); i++) {
                letEnv.define(varNames.get(i), eval(initExprs.get(i), env));
            }
            return evalBodyTail(new ArrayList<>(list.subList(bindingsIdx + 1, list.size())), letEnv);
        }
    }

    private Object evalLetStar(List<?> list, Env env, String posStr) throws EvalError {
        Object bindingsObj = list.get(1);
        if (bindingsObj instanceof SchemeParser.Located loc) bindingsObj = loc.expr;
        if (!(bindingsObj instanceof List<?> bindingList))
            throw new EvalError("let*: bindings must be a list" + posStr);
        Env letStarEnv = new Env(env);
        for (Object b : bindingList) {
            if (b instanceof SchemeParser.Located loc) b = loc.expr;
            if (!(b instanceof List<?> binding) || binding.size() != 2)
                throw new EvalError("let*: invalid binding" + posStr);
            Object bname = binding.get(0);
            if (bname instanceof SchemeParser.Located loc) bname = loc.expr;
            if (!(bname instanceof String vname))
                throw new EvalError("let*: binding name must be symbol" + posStr);
            letStarEnv.define(vname, eval(binding.get(1), letStarEnv));
        }
        return evalBodyTail(new ArrayList<>(list.subList(2, list.size())), letStarEnv);
    }

    private Object evalLetrec(List<?> list, Env env, String posStr) throws EvalError {
        Object bindingsObj = list.get(1);
        if (bindingsObj instanceof SchemeParser.Located loc) bindingsObj = loc.expr;
        if (!(bindingsObj instanceof List<?> bindingList))
            throw new EvalError("letrec: bindings must be a list" + posStr);
        Env letrecEnv = new Env(env);
        List<String> names = new ArrayList<>();
        List<Object> initExprs = new ArrayList<>();
        parseBindings(bindingList, names, initExprs, "letrec", posStr);
        for (String name : names) {
            letrecEnv.define(name, Boolean.FALSE);
        }
        for (int i = 0; i < names.size(); i++) {
            letrecEnv.bindings.put(names.get(i), eval(initExprs.get(i), letrecEnv));
        }
        return evalBodyTail(new ArrayList<>(list.subList(2, list.size())), letrecEnv);
    }

    private Object evalLetrecStar(List<?> list, Env env, String posStr) throws EvalError {
        Object bindingsObj = list.get(1);
        if (bindingsObj instanceof SchemeParser.Located loc) bindingsObj = loc.expr;
        if (!(bindingsObj instanceof List<?> bindingList))
            throw new EvalError("letrec*: bindings must be a list" + posStr);
        Env letrecStarEnv = new Env(env);
        for (Object b : bindingList) {
            if (b instanceof SchemeParser.Located loc) b = loc.expr;
            if (!(b instanceof List<?> binding) || binding.size() != 2)
                throw new EvalError("letrec*: invalid binding" + posStr);
            Object bname = binding.get(0);
            if (bname instanceof SchemeParser.Located loc) bname = loc.expr;
            if (!(bname instanceof String vname))
                throw new EvalError("letrec*: binding name must be symbol" + posStr);
            letrecStarEnv.define(vname, eval(binding.get(1), letrecStarEnv));
        }
        return evalBodyTail(new ArrayList<>(list.subList(2, list.size())), letrecStarEnv);
    }

    private void parseBindings(List<?> bindingList, List<String> names, List<Object> initExprs,
                                String formName, String posStr) throws EvalError {
        for (Object b : bindingList) {
            if (b instanceof SchemeParser.Located loc) b = loc.expr;
            if (!(b instanceof List<?> binding) || binding.size() != 2)
                throw new EvalError(formName + ": invalid binding" + posStr);
            Object bname = binding.get(0);
            if (bname instanceof SchemeParser.Located loc) bname = loc.expr;
            if (!(bname instanceof String vname))
                throw new EvalError(formName + ": binding name must be symbol" + posStr);
            names.add(vname);
            initExprs.add(binding.get(1));
        }
    }

    private Object evalCase(List<?> list, Env env, String posStr) throws EvalError {
        if (list.size() < 3) throw new EvalError("case: too few arguments" + posStr);
        Object key = eval(list.get(1), env);
        for (int i = 2; i < list.size(); i++) {
            Object clauseObj = list.get(i);
            if (clauseObj instanceof SchemeParser.Located loc) clauseObj = loc.expr;
            if (!(clauseObj instanceof List<?> clause) || clause.isEmpty())
                throw new EvalError("case: invalid clause" + posStr);
            Object datums = clause.get(0);
            if (datums instanceof SchemeParser.Located loc) datums = loc.expr;
            if (datums instanceof String s && s.equals("else")) {
                return evalBodyTail(new ArrayList<>(clause.subList(1, clause.size())), env);
            }
            if (datums instanceof List<?> datumList) {
                for (Object d : datumList) {
                    if (d instanceof SchemeParser.Located loc) d = loc.expr;
                    Object datum = quoteValue(d);
                    if (schemeEqv(key, datum)) {
                        return evalBodyTail(new ArrayList<>(clause.subList(1, clause.size())), env);
                    }
                }
            }
        }
        return Boolean.FALSE;
    }

    private Object evalDo(List<?> list, Env env, String posStr) throws EvalError {
        if (list.size() < 3) throw new EvalError("do: too few arguments" + posStr);
        Object varsObj = list.get(1);
        if (varsObj instanceof SchemeParser.Located loc) varsObj = loc.expr;
        if (!(varsObj instanceof List<?> varSpecs))
            throw new EvalError("do: variable specs must be a list" + posStr);
        Object testObj = list.get(2);
        if (testObj instanceof SchemeParser.Located loc) testObj = loc.expr;
        if (!(testObj instanceof List<?> testClause) || testClause.isEmpty())
            throw new EvalError("do: test clause must be a list" + posStr);
        List<Object> bodyExprs = new ArrayList<>(list.subList(3, list.size()));
        List<String> varNames = new ArrayList<>();
        List<Object> initExprs = new ArrayList<>();
        List<Object> stepExprs = new ArrayList<>();
        for (Object vs : varSpecs) {
            if (vs instanceof SchemeParser.Located loc) vs = loc.expr;
            if (!(vs instanceof List<?> spec) || spec.size() < 2)
                throw new EvalError("do: invalid variable spec" + posStr);
            Object vn = spec.get(0);
            if (vn instanceof SchemeParser.Located loc) vn = loc.expr;
            if (!(vn instanceof String name))
                throw new EvalError("do: variable name must be symbol" + posStr);
            varNames.add(name);
            initExprs.add(spec.get(1));
            stepExprs.add(spec.size() >= 3 ? spec.get(2) : null);
        }
        Env doEnv = new Env(env);
        for (int i = 0; i < varNames.size(); i++) {
            doEnv.define(varNames.get(i), eval(initExprs.get(i), env));
        }
        while (true) {
            Object testVal = eval(testClause.get(0), doEnv);
            if (isTruthy(testVal)) {
                if (testClause.size() == 1) return Boolean.FALSE;
                Object result = Boolean.FALSE;
                for (int j = 1; j < testClause.size(); j++) {
                    result = eval(testClause.get(j), doEnv);
                }
                return result;
            }
            for (Object bodyExpr : bodyExprs) {
                eval(bodyExpr, doEnv);
            }
            Object[] newVals = new Object[varNames.size()];
            for (int i = 0; i < varNames.size(); i++) {
                if (stepExprs.get(i) != null) {
                    newVals[i] = eval(stepExprs.get(i), doEnv);
                } else {
                    newVals[i] = doEnv.bindings.get(varNames.get(i));
                }
            }
            for (int i = 0; i < varNames.size(); i++) {
                doEnv.bindings.put(varNames.get(i), newVals[i]);
            }
        }
    }

    private Object applyProc(Object proc, List<Object> args) throws EvalError {
        if (proc instanceof Lambda lambda) {
            if (lambda.restParam != null) {
                if (args.size() < lambda.params.size()) {
                    throw new EvalError("wrong number of arguments: expected at least " +
                            lambda.params.size() + ", got " + args.size());
                }
            } else {
                if (args.size() != lambda.params.size()) {
                    throw new EvalError("wrong number of arguments: expected " +
                            lambda.params.size() + ", got " + args.size());
                }
            }
            Env callEnv = new Env(lambda.closureEnv);
            for (int i = 0; i < lambda.params.size(); i++) {
                callEnv.define(lambda.params.get(i), args.get(i));
            }
            if (lambda.restParam != null) {
                Object rest = EMPTY_LIST;
                for (int i = args.size() - 1; i >= lambda.params.size(); i--) {
                    rest = new Pair(args.get(i), rest);
                }
                callEnv.define(lambda.restParam, rest);
            }
            return evalBody(lambda.body, callEnv);
        }
        if (proc instanceof CaseLambda cl) {
            for (Lambda clause : cl.clauses) {
                if (clause.restParam != null) {
                    if (args.size() >= clause.params.size()) {
                        return applyProc(clause, args);
                    }
                } else {
                    if (args.size() == clause.params.size()) {
                        return applyProc(clause, args);
                    }
                }
            }
            throw new EvalError("case-lambda: no matching clause for " + args.size() + " arguments");
        }
        if (proc instanceof BuiltinProc bp) {
            return bp.fn.apply(args);
        }
        throw new EvalError("not a procedure: " + schemeToString(proc));
    }

    private Object evalDefineRecordType(List<?> list, Env env, String posStr) throws EvalError {
        if (list.size() < 4) throw new EvalError("define-record-type: too few arguments" + posStr);
        Object typeNameObj = list.get(1);
        if (typeNameObj instanceof SchemeParser.Located loc) typeNameObj = loc.expr;
        String typeName = (String) typeNameObj;

        Object ctorObj = list.get(2);
        if (ctorObj instanceof SchemeParser.Located loc) ctorObj = loc.expr;
        List<?> ctorSpec = (List<?>) ctorObj;
        Object ctorNameObj = ctorSpec.get(0);
        if (ctorNameObj instanceof SchemeParser.Located loc) ctorNameObj = loc.expr;
        String ctorName = (String) ctorNameObj;
        List<String> ctorFields = new ArrayList<>();
        for (int ci = 1; ci < ctorSpec.size(); ci++) {
            Object cf = ctorSpec.get(ci);
            if (cf instanceof SchemeParser.Located loc) cf = loc.expr;
            ctorFields.add((String) cf);
        }

        Object predObj = list.get(3);
        if (predObj instanceof SchemeParser.Located loc) predObj = loc.expr;
        String predName = (String) predObj;

        Map<String, Integer> fieldIndex = new HashMap<>();
        for (int fi = 0; fi < ctorFields.size(); fi++) {
            fieldIndex.put(ctorFields.get(fi), fi);
        }
        Map<String, Integer> accessorMap = new HashMap<>();
        for (int fi = 4; fi < list.size(); fi++) {
            Object fspec = list.get(fi);
            if (fspec instanceof SchemeParser.Located loc) fspec = loc.expr;
            List<?> fieldSpec = (List<?>) fspec;
            Object fnObj = fieldSpec.get(0);
            if (fnObj instanceof SchemeParser.Located loc) fnObj = loc.expr;
            String fieldName = (String) fnObj;
            Object accObj = fieldSpec.get(1);
            if (accObj instanceof SchemeParser.Located loc) accObj = loc.expr;
            String accName = (String) accObj;
            accessorMap.put(accName, fieldIndex.get(fieldName));
        }

        RecordType rt = new RecordType(typeName, ctorFields);

        env.define(ctorName, new BuiltinProc(ctorName, args -> {
            if (args.size() != ctorFields.size())
                throw new EvalError(ctorName + ": expected " + ctorFields.size() + " arguments, got " + args.size());
            return new SchemeRecord(rt, args.toArray());
        }));
        env.define(predName, new BuiltinProc(predName, args -> {
            if (args.size() != 1) throw new EvalError(predName + ": expected 1 argument");
            return args.get(0) instanceof SchemeRecord sr && sr.type == rt;
        }));
        for (var entry : accessorMap.entrySet()) {
            String accName = entry.getKey();
            int idx = entry.getValue();
            env.define(accName, new BuiltinProc(accName, args -> {
                if (args.size() != 1) throw new EvalError(accName + ": expected 1 argument");
                if (!(args.get(0) instanceof SchemeRecord sr) || sr.type != rt)
                    throw new EvalError(accName + ": not a " + typeName);
                return sr.fields[idx];
            }));
        }
        return Boolean.FALSE;
    }

    private Object evalDefineSyntax(List<?> list, Env env, String posStr) throws EvalError {
        if (list.size() != 3) throw new EvalError("define-syntax: expected 2 arguments" + posStr);
        Object nameObj = list.get(1);
        if (nameObj instanceof SchemeParser.Located loc) nameObj = loc.expr;
        if (!(nameObj instanceof String macroName))
            throw new EvalError("define-syntax: name must be symbol" + posStr);
        Object transformerExpr = list.get(2);
        if (transformerExpr instanceof SchemeParser.Located loc) transformerExpr = loc.expr;
        if (!(transformerExpr instanceof List<?> transformer))
            throw new EvalError("define-syntax: expected syntax-rules" + posStr);
        Object srHead = transformer.get(0);
        if (srHead instanceof SchemeParser.Located loc) srHead = loc.expr;
        if (!(srHead instanceof String srStr) || !srStr.equals("syntax-rules"))
            throw new EvalError("define-syntax: expected syntax-rules" + posStr);
        Object litsObj = transformer.get(1);
        if (litsObj instanceof SchemeParser.Located loc) litsObj = loc.expr;
        List<String> macroLiterals = new ArrayList<>();
        if (litsObj instanceof List<?> litsList) {
            for (Object l : litsList) {
                if (l instanceof SchemeParser.Located loc) l = loc.expr;
                if (l instanceof String s) macroLiterals.add(s);
            }
        }
        List<Object[]> macroRules = new ArrayList<>();
        for (int ri = 2; ri < transformer.size(); ri++) {
            Object ruleObj = transformer.get(ri);
            if (ruleObj instanceof SchemeParser.Located loc) ruleObj = loc.expr;
            if (!(ruleObj instanceof List<?> rule) || rule.size() != 2)
                throw new EvalError("define-syntax: invalid rule" + posStr);
            macroRules.add(new Object[]{rule.get(0), rule.get(1)});
        }
        env.define(macroName, new SyntaxRulesMacro(macroName, macroLiterals, macroRules, env));
        return Boolean.FALSE;
    }

    private Object evalBody(List<Object> body, Env env) throws EvalError {
        Object result = Boolean.FALSE;
        for (Object expr : body) {
            result = eval(expr, env);
        }
        return result;
    }

    private Object evalBodyTail(List<Object> body, Env env) throws EvalError {
        if (body.isEmpty()) return Boolean.FALSE;
        for (int i = 0; i < body.size() - 1; i++) eval(body.get(i), env);
        return new TailCall(body.get(body.size() - 1), env);
    }

    // --- Macro expansion ---

    @SuppressWarnings("unchecked")
    private Object evalMacro(SyntaxRulesMacro macro, List<?> form, Env useEnv) throws EvalError {
        for (Object[] rule : macro.rules) {
            Map<String, Object> bindings = matchPattern(rule[0], form, macro.literals);
            if (bindings != null) {
                Map<String, String> renameMap = new HashMap<>();
                Object expanded = expandTemplate(rule[1], bindings, renameMap);
                Env evalEnv = useEnv;
                if (!renameMap.isEmpty()) {
                    Map<String, Object> hygieneBindings = new HashMap<>();
                    for (var entry : renameMap.entrySet()) {
                        try {
                            Object val = macro.defEnv.lookup(entry.getKey(), new SchemeParser.Pos(0, 0));
                            hygieneBindings.put(entry.getValue(), val);
                        } catch (EvalError ignored) {}
                    }
                    if (!hygieneBindings.isEmpty()) {
                        evalEnv = new Env(useEnv);
                        for (var e : hygieneBindings.entrySet()) {
                            evalEnv.define(e.getKey(), e.getValue());
                        }
                    }
                }
                return new TailCall(expanded, evalEnv);
            }
        }
        throw new EvalError("no matching pattern for macro " + macro.name);
    }

    private Map<String, Object> matchPattern(Object pattern, List<?> input, List<String> literals) {
        if (pattern instanceof SchemeParser.Located loc) pattern = loc.expr;
        if (!(pattern instanceof List<?> patList)) return null;
        Map<String, Object> bindings = new HashMap<>();
        int pi = 1, ii = 1;
        while (pi < patList.size()) {
            Object patElem = patList.get(pi);
            if (patElem instanceof SchemeParser.Located loc) patElem = loc.expr;
            boolean hasEllipsis = false;
            if (pi + 1 < patList.size()) {
                Object next = patList.get(pi + 1);
                if (next instanceof SchemeParser.Located loc) next = loc.expr;
                if ("...".equals(next)) hasEllipsis = true;
            }
            if (hasEllipsis) {
                if (!(patElem instanceof String varName)) return null;
                List<Object> collected = new ArrayList<>();
                while (ii < input.size()) {
                    collected.add(input.get(ii));
                    ii++;
                }
                bindings.put(varName, collected);
                pi += 2;
            } else if (patElem instanceof String s && literals.contains(s)) {
                if (ii >= input.size()) return null;
                Object inElem = input.get(ii);
                if (inElem instanceof SchemeParser.Located loc) inElem = loc.expr;
                if (!s.equals(inElem)) return null;
                pi++; ii++;
            } else if (patElem instanceof String varName) {
                if (ii >= input.size()) return null;
                bindings.put(varName, input.get(ii));
                pi++; ii++;
            } else {
                return null;
            }
        }
        return ii == input.size() ? bindings : null;
    }

    @SuppressWarnings("unchecked")
    private Object expandTemplate(Object template, Map<String, Object> bindings,
                                   Map<String, String> renameMap) {
        if (template instanceof SchemeParser.Located loc) template = loc.expr;
        if (template instanceof String id) {
            if (bindings.containsKey(id)) return bindings.get(id);
            if ("...".equals(id) || SPECIAL_FORMS.contains(id)) return id;
            if (!renameMap.containsKey(id)) {
                renameMap.put(id, "__" + id + "_" + (gensymCounter++));
            }
            return renameMap.get(id);
        }
        if (template instanceof List<?> tmplList) {
            List<Object> result = new ArrayList<>();
            for (int i = 0; i < tmplList.size(); i++) {
                Object elem = tmplList.get(i);
                Object rawElem = elem;
                if (rawElem instanceof SchemeParser.Located loc) rawElem = loc.expr;
                boolean nextIsEllipsis = false;
                if (i + 1 < tmplList.size()) {
                    Object next = tmplList.get(i + 1);
                    if (next instanceof SchemeParser.Located loc) next = loc.expr;
                    if ("...".equals(next)) nextIsEllipsis = true;
                }
                if (nextIsEllipsis) {
                    String ellipsisVar = findEllipsisVar(elem, bindings);
                    if (ellipsisVar != null) {
                        List<Object> elements = (List<Object>) bindings.get(ellipsisVar);
                        for (Object e : elements) {
                            Map<String, Object> subBindings = new HashMap<>(bindings);
                            subBindings.put(ellipsisVar, e);
                            result.add(expandTemplate(elem, subBindings, renameMap));
                        }
                    }
                    i++;
                } else if (rawElem instanceof String s && "...".equals(s)) {
                    // skip, handled above
                } else {
                    result.add(expandTemplate(elem, bindings, renameMap));
                }
            }
            return result;
        }
        return template;
    }

    private String findEllipsisVar(Object template, Map<String, Object> bindings) {
        if (template instanceof SchemeParser.Located loc) template = loc.expr;
        if (template instanceof String s && bindings.get(s) instanceof List) return s;
        if (template instanceof List<?> list) {
            for (Object elem : list) {
                String found = findEllipsisVar(elem, bindings);
                if (found != null) return found;
            }
        }
        return null;
    }

    private Object quoteValue(Object datum) {
        if (datum instanceof SchemeParser.Located loc) datum = loc.expr;
        if (datum instanceof List<?> list) {
            Object result = EMPTY_LIST;
            for (int i = list.size() - 1; i >= 0; i--) {
                result = new Pair(quoteValue(list.get(i)), result);
            }
            return result;
        }
        return datum;
    }

    private boolean schemeEqv(Object a, Object b) {
        if (a == b) return true;
        if (a instanceof Long && b instanceof Long) return a.equals(b);
        if (a instanceof Double && b instanceof Double) return a.equals(b);
        if (a instanceof Rational && b instanceof Rational) return a.equals(b);
        if (a instanceof Boolean && b instanceof Boolean) return a.equals(b);
        if (a instanceof SchemeChar && b instanceof SchemeChar) return a.equals(b);
        if (a instanceof String && b instanceof String) return a.equals(b);
        return false;
    }

    static boolean schemeEqual(Object a, Object b) {
        if (a == b) return true;
        if (isNumber(a) && isNumber(b)) {
            try { return toDouble(a) == toDouble(b); } catch (EvalError e) { return false; }
        }
        if (a instanceof Boolean && b instanceof Boolean) return a.equals(b);
        if (a instanceof String && b instanceof String) return a.equals(b);
        if (a instanceof SchemeString sa && b instanceof SchemeString sb) return sa.value().equals(sb.value());
        if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) return ca.value() == cb.value();
        if (a == EMPTY_LIST && b == EMPTY_LIST) return true;
        if (a instanceof Pair pa && b instanceof Pair pb) return schemeEqual(pa.car, pb.car) && schemeEqual(pa.cdr, pb.cdr);
        if (a instanceof SchemeVector va && b instanceof SchemeVector vb) {
            if (va.length() != vb.length()) return false;
            for (int i = 0; i < va.length(); i++) {
                if (!schemeEqual(va.ref(i), vb.ref(i))) return false;
            }
            return true;
        }
        return false;
    }

    static boolean isTruthy(Object val) {
        return !(val instanceof Boolean b && !b);
    }
}
