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
        List<Object> exprs = Parser.parse(input);
        Object result = null;
        Env env = Env.global();
        StringBuilder outputBuf = new StringBuilder();
        OUTPUT.set(outputBuf);
        try {
            for (Object expr : exprs) {
                result = eval(expr, env);
            }
        } finally {
            OUTPUT.remove();
        }
        return new EvalResult(SchemeValue.toStr(result), outputBuf.toString());
    }

    static final ThreadLocal<StringBuilder> OUTPUT = new ThreadLocal<>();

    static void emitOutput(String s) {
        StringBuilder buf = OUTPUT.get();
        if (buf != null) buf.append(s);
    }

    @SuppressWarnings("unchecked")
    static Object eval(Object expr, Env env) throws EvalError {
        while (true) {
            if (expr instanceof Long || expr instanceof Boolean || expr instanceof Character
                    || expr instanceof Double || expr instanceof Rational) {
                return expr;
            }
            if (expr instanceof String s) {
                if (s.startsWith("\"")) return s;
                return env.lookup(s);
            }
            if (expr instanceof MutableString) return expr;
            if (!(expr instanceof List<?> list)) {
                throw new EvalError("cannot evaluate: " + expr);
            }
            if (list.isEmpty()) throw new EvalError("empty application");

            try {

            Object first = list.get(0);

            // Special forms
            if (first instanceof String op) {
                switch (op) {
                    case "define" -> { return evalDefine(list, env); }
                    case "quote" -> {
                        if (list.size() != 2) throw new EvalError("quote: expected 1 argument");
                        return SchemeValue.quotedToScheme(list.get(1));
                    }
                    case "lambda" -> { return evalLambda(list, env); }
                    case "set!" -> {
                        if (list.size() != 3) throw new EvalError("set!: bad syntax");
                        Object target = list.get(1);
                        if (!(target instanceof String name) || name.startsWith("\""))
                            throw new EvalError("set!: expected variable name");
                        Object val = eval(list.get(2), env);
                        env.set(name, val);
                        return null;
                    }
                    case "define-record-type" -> { return evalDefineRecordType(list, env); }
                    case "case-lambda" -> { return evalCaseLambda(list, env); }
                    case "define-syntax" -> {
                        if (list.size() != 3) throw new EvalError("define-syntax: bad syntax");
                        if (!(list.get(1) instanceof String name) || name.startsWith("\""))
                            throw new EvalError("define-syntax: expected name");
                        Object transformer = list.get(2);
                        if (!(transformer instanceof List<?> trList) || trList.isEmpty()
                                || !"syntax-rules".equals(trList.get(0)))
                            throw new EvalError("define-syntax: expected syntax-rules");
                        SyntaxRules sr = SyntaxRules.parse(trList, env);
                        env.define(name, sr);
                        return null;
                    }
                    case "do" -> { return evalDo(list, env); }

                    // --- TCO forms: set expr/env and continue the trampoline ---
                    case "if" -> {
                        if (list.size() < 3 || list.size() > 4) throw new EvalError("if: bad syntax");
                        Object cond = eval(list.get(1), env);
                        if (!isFalse(cond)) {
                            expr = list.get(2); continue;
                        } else if (list.size() == 4) {
                            expr = list.get(3); continue;
                        }
                        return null;
                    }
                    case "and" -> {
                        if (list.size() == 1) return Boolean.TRUE;
                        for (int i = 1; i < list.size() - 1; i++) {
                            Object result = eval(list.get(i), env);
                            if (isFalse(result)) return result;
                        }
                        expr = list.get(list.size() - 1); continue;
                    }
                    case "or" -> {
                        if (list.size() == 1) return Boolean.FALSE;
                        for (int i = 1; i < list.size() - 1; i++) {
                            Object result = eval(list.get(i), env);
                            if (!isFalse(result)) return result;
                        }
                        expr = list.get(list.size() - 1); continue;
                    }
                    case "begin" -> {
                        if (list.size() == 1) return null;
                        for (int i = 1; i < list.size() - 1; i++) {
                            eval(list.get(i), env);
                        }
                        expr = list.get(list.size() - 1); continue;
                    }
                    case "let" -> {
                        if (list.size() < 3) throw new EvalError("let: bad syntax");
                        int offset = 1;
                        String letName = null;
                        if (list.get(1) instanceof String ls && !ls.startsWith("\"")) {
                            letName = ls; offset = 2;
                        }
                        if (offset >= list.size()) throw new EvalError("let: bad syntax");
                        Object bindingsObj = list.get(offset);
                        if (!(bindingsObj instanceof List<?> bindings))
                            throw new EvalError("let: bindings must be a list");
                        List<String> params = new ArrayList<>();
                        List<Object> inits = new ArrayList<>();
                        for (Object b : bindings) {
                            if (!(b instanceof List<?> binding) || binding.size() != 2)
                                throw new EvalError("let: bad binding");
                            if (!(binding.get(0) instanceof String p))
                                throw new EvalError("let: binding name must be symbol");
                            params.add(p);
                            inits.add(eval(binding.get(1), env));
                        }
                        List<Object> body = new ArrayList<>();
                        for (int i = offset + 1; i < list.size(); i++) {
                            body.add(list.get(i));
                        }
                        if (letName != null) {
                            Env letEnv = new Env(env);
                            Lambda lambda = new Lambda(params, null, body, letEnv);
                            letEnv.define(letName, lambda);
                            // TCO: set up lambda call in trampoline
                            Env localEnv = new Env(letEnv);
                            for (int i = 0; i < params.size(); i++) {
                                localEnv.define(params.get(i), inits.get(i));
                            }
                            for (int i = 0; i < body.size() - 1; i++) {
                                eval(body.get(i), localEnv);
                            }
                            expr = body.get(body.size() - 1); env = localEnv; continue;
                        } else {
                            Env letEnv = new Env(env);
                            for (int i = 0; i < params.size(); i++) {
                                letEnv.define(params.get(i), inits.get(i));
                            }
                            for (int i = 0; i < body.size() - 1; i++) {
                                eval(body.get(i), letEnv);
                            }
                            expr = body.get(body.size() - 1); env = letEnv; continue;
                        }
                    }
                    case "cond" -> {
                        Object condResult = null;
                        boolean matched = false;
                        for (int i = 1; i < list.size(); i++) {
                            Object clause = list.get(i);
                            if (!(clause instanceof List<?> c) || c.isEmpty())
                                throw new EvalError("cond: bad clause");
                            Object test = c.get(0);
                            if (test instanceof String cs && cs.equals("else")) {
                                if (c.size() == 1) return null;
                                for (int j = 1; j < c.size() - 1; j++) {
                                    eval(c.get(j), env);
                                }
                                expr = c.get(c.size() - 1); matched = true; break;
                            }
                            Object val = eval(test, env);
                            if (!isFalse(val)) {
                                if (c.size() == 1) return val;
                                for (int j = 1; j < c.size() - 1; j++) {
                                    eval(c.get(j), env);
                                }
                                expr = c.get(c.size() - 1); matched = true; break;
                            }
                        }
                        if (matched) continue;
                        return null;
                    }
                    case "letrec" -> {
                        if (list.size() < 3) throw new EvalError("letrec: bad syntax");
                        if (!(list.get(1) instanceof List<?> bindings))
                            throw new EvalError("letrec: bindings must be a list");
                        Env letEnv = new Env(env);
                        List<String> names = new ArrayList<>();
                        List<Object> initExprs = new ArrayList<>();
                        for (Object b : bindings) {
                            if (!(b instanceof List<?> binding) || binding.size() != 2)
                                throw new EvalError("letrec: bad binding");
                            if (!(binding.get(0) instanceof String name))
                                throw new EvalError("letrec: binding name must be symbol");
                            names.add(name);
                            initExprs.add(binding.get(1));
                            letEnv.define(name, null);
                        }
                        for (int i = 0; i < names.size(); i++) {
                            letEnv.set(names.get(i), eval(initExprs.get(i), letEnv));
                        }
                        for (int i = 2; i < list.size() - 1; i++) {
                            eval(list.get(i), letEnv);
                        }
                        expr = list.get(list.size() - 1); env = letEnv; continue;
                    }
                    case "letrec*" -> {
                        if (list.size() < 3) throw new EvalError("letrec*: bad syntax");
                        if (!(list.get(1) instanceof List<?> bindings))
                            throw new EvalError("letrec*: bindings must be a list");
                        Env letEnv = new Env(env);
                        for (Object b : bindings) {
                            if (!(b instanceof List<?> binding) || binding.size() != 2)
                                throw new EvalError("letrec*: bad binding");
                            if (!(binding.get(0) instanceof String name))
                                throw new EvalError("letrec*: binding name must be symbol");
                            letEnv.define(name, eval(binding.get(1), letEnv));
                        }
                        for (int i = 2; i < list.size() - 1; i++) {
                            eval(list.get(i), letEnv);
                        }
                        expr = list.get(list.size() - 1); env = letEnv; continue;
                    }
                    case "case" -> {
                        if (list.size() < 2) throw new EvalError("case: bad syntax");
                        Object key = eval(list.get(1), env);
                        boolean caseMatched = false;
                        for (int i = 2; i < list.size(); i++) {
                            if (!(list.get(i) instanceof List<?> clause) || clause.isEmpty())
                                throw new EvalError("case: bad clause");
                            Object datums = clause.get(0);
                            if (datums instanceof String cs && cs.equals("else")) {
                                if (clause.size() == 1) return null;
                                for (int j = 1; j < clause.size() - 1; j++) {
                                    eval(clause.get(j), env);
                                }
                                expr = clause.get(clause.size() - 1); caseMatched = true; break;
                            }
                            if (!(datums instanceof List<?> datumList))
                                throw new EvalError("case: bad clause");
                            boolean found = false;
                            for (Object datum : datumList) {
                                Object d = SchemeValue.quotedToScheme(datum);
                                if (Env.schemeEqv(key, d)) { found = true; break; }
                            }
                            if (found) {
                                if (clause.size() == 1) return null;
                                for (int j = 1; j < clause.size() - 1; j++) {
                                    eval(clause.get(j), env);
                                }
                                expr = clause.get(clause.size() - 1); caseMatched = true; break;
                            }
                        }
                        if (caseMatched) continue;
                        return null;
                    }
                }
            }

            // Function application (or macro expansion)
            Object func = eval(first, env);
            if (func instanceof SyntaxRules sr) {
                // Macro expansion: re-evaluate expanded form in trampoline
                Object[] expanded = sr.expandToForm(list, env);
                expr = expanded[0];
                env = (Env) expanded[1];
                continue;
            }
            List<Object> args = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) {
                args.add(eval(list.get(i), env));
            }
            // TCO for lambda application
            if (func instanceof Builtin b) {
                return b.apply(args);
            }
            if (func instanceof CaseLambda cl) {
                Lambda matched = null;
                for (Lambda lam : cl.clauses) {
                    if (lam.restParam != null) {
                        if (args.size() >= lam.params.size()) { matched = lam; break; }
                    } else {
                        if (args.size() == lam.params.size()) { matched = lam; break; }
                    }
                }
                if (matched == null)
                    throw new EvalError("case-lambda: no matching clause for " + args.size() + " arguments");
                func = matched;
            }
            if (func instanceof Lambda lam) {
                if (lam.restParam == null) {
                    if (args.size() != lam.params.size())
                        throw new EvalError("lambda: expected " + lam.params.size() + " arguments, got " + args.size());
                } else {
                    if (args.size() < lam.params.size())
                        throw new EvalError("lambda: expected at least " + lam.params.size() + " arguments, got " + args.size());
                }
                Env localEnv = new Env(lam.closure);
                for (int i = 0; i < lam.params.size(); i++) {
                    localEnv.define(lam.params.get(i), args.get(i));
                }
                if (lam.restParam != null) {
                    Object rest = SchemeValue.NIL;
                    for (int i = args.size() - 1; i >= lam.params.size(); i--) {
                        rest = new Pair(args.get(i), rest);
                    }
                    localEnv.define(lam.restParam, rest);
                }
                for (int i = 0; i < lam.body.size() - 1; i++) {
                    eval(lam.body.get(i), localEnv);
                }
                expr = lam.body.get(lam.body.size() - 1);
                env = localEnv;
                continue;
            }
            throw new EvalError("not a procedure: " + SchemeValue.toStr(func));

            } catch (EvalError e) {
                if (list instanceof SourceList sl && !e.getMessage().matches(".*\\d+:\\d+.*")) {
                    throw new EvalError(e.getMessage() + " [" + sl.line + ":" + sl.col + "]");
                }
                throw e;
            }
        }
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
            String restParam = null;
            for (int i = 1; i < sig.size(); i++) {
                if (!(sig.get(i) instanceof String p))
                    throw new EvalError("define: parameter must be symbol");
                if (p.equals(".")) {
                    if (i + 2 != sig.size())
                        throw new EvalError("define: bad dot syntax");
                    if (!(sig.get(i + 1) instanceof String rp))
                        throw new EvalError("define: parameter must be symbol");
                    restParam = rp;
                    break;
                }
                params.add(p);
            }
            List<Object> body = new ArrayList<>();
            for (int i = 2; i < list.size(); i++) {
                body.add(list.get(i));
            }
            Lambda lambda = new Lambda(params, restParam, body, env);
            env.define(name, lambda);
            return null; // void
        }
        throw new EvalError("define: bad syntax");
    }


    private static Object evalLambda(List<?> list, Env env) throws EvalError {
        if (list.size() < 3) throw new EvalError("lambda: bad syntax");
        Object paramSpec = list.get(1);
        if (!(paramSpec instanceof List<?> paramList))
            throw new EvalError("lambda: parameters must be a list");
        List<String> params = new ArrayList<>();
        String restParam = null;
        for (int j = 0; j < paramList.size(); j++) {
            if (!(paramList.get(j) instanceof String s)) throw new EvalError("lambda: parameter must be symbol");
            if (s.equals(".")) {
                if (j + 2 != paramList.size())
                    throw new EvalError("lambda: bad dot syntax");
                if (!(paramList.get(j + 1) instanceof String rp))
                    throw new EvalError("lambda: parameter must be symbol");
                restParam = rp;
                break;
            }
            params.add(s);
        }
        List<Object> body = new ArrayList<>();
        for (int i = 2; i < list.size(); i++) {
            body.add(list.get(i));
        }
        return new Lambda(params, restParam, body, env);
    }


    @SuppressWarnings("unchecked")
    private static Object evalDefineRecordType(List<?> list, Env env) throws EvalError {
        // (define-record-type <name> (constructor field...) predicate (field accessor)...)
        if (list.size() < 4) throw new EvalError("define-record-type: bad syntax");
        // Type name (e.g. <point>)
        if (!(list.get(1) instanceof String typeName))
            throw new EvalError("define-record-type: expected type name");
        // Constructor spec: (make-point x y)
        if (!(list.get(2) instanceof List<?> ctorSpec) || ctorSpec.isEmpty())
            throw new EvalError("define-record-type: expected constructor spec");
        String ctorName = (String) ctorSpec.get(0);
        List<String> ctorFields = new ArrayList<>();
        for (int i = 1; i < ctorSpec.size(); i++) {
            ctorFields.add((String) ctorSpec.get(i));
        }
        // Predicate name
        if (!(list.get(3) instanceof String predName))
            throw new EvalError("define-record-type: expected predicate name");
        // Field specs: (field accessor)
        List<String> fieldNames = new ArrayList<>();
        List<String> accessorNames = new ArrayList<>();
        for (int i = 4; i < list.size(); i++) {
            if (!(list.get(i) instanceof List<?> fieldSpec) || fieldSpec.size() != 2)
                throw new EvalError("define-record-type: bad field spec");
            fieldNames.add((String) fieldSpec.get(0));
            accessorNames.add((String) fieldSpec.get(1));
        }

        Record.RecordType recordType = new Record.RecordType(typeName, ctorFields);

        // Define constructor
        env.define(ctorName, Builtin.named(ctorName, args -> {
            if (args.size() != ctorFields.size())
                throw new EvalError(ctorName + ": expected " + ctorFields.size() + " arguments, got " + args.size());
            return new Record(recordType, args.toArray());
        }));

        // Define predicate
        env.define(predName, Builtin.named(predName, args -> {
            if (args.size() != 1) throw new EvalError(predName + ": expected 1 argument");
            return args.get(0) instanceof Record r && r.type == recordType;
        }));

        // Define accessors
        for (int i = 0; i < fieldNames.size(); i++) {
            String fieldName = fieldNames.get(i);
            String accessorName = accessorNames.get(i);
            int fieldIndex = ctorFields.indexOf(fieldName);
            if (fieldIndex < 0)
                throw new EvalError("define-record-type: field " + fieldName + " not in constructor");
            env.define(accessorName, Builtin.named(accessorName, args -> {
                if (args.size() != 1) throw new EvalError(accessorName + ": expected 1 argument");
                if (!(args.get(0) instanceof Record r) || r.type != recordType)
                    throw new EvalError(accessorName + ": not a " + typeName);
                return r.fields[fieldIndex];
            }));
        }

        return null; // void
    }

    private static Object evalCaseLambda(List<?> list, Env env) throws EvalError {
        List<Lambda> clauses = new ArrayList<>();
        for (int i = 1; i < list.size(); i++) {
            if (!(list.get(i) instanceof List<?> clause) || clause.size() < 2)
                throw new EvalError("case-lambda: bad clause");
            Object paramSpec = clause.get(0);
            if (!(paramSpec instanceof List<?> paramList))
                throw new EvalError("case-lambda: parameters must be a list");
            List<String> params = new ArrayList<>();
            String restParam = null;
            for (int j = 0; j < paramList.size(); j++) {
                if (!(paramList.get(j) instanceof String s))
                    throw new EvalError("case-lambda: parameter must be symbol");
                if (s.equals(".")) {
                    if (j + 2 != paramList.size())
                        throw new EvalError("case-lambda: bad dot syntax");
                    if (!(paramList.get(j + 1) instanceof String rp))
                        throw new EvalError("case-lambda: parameter must be symbol");
                    restParam = rp;
                    break;
                }
                params.add(s);
            }
            List<Object> body = new ArrayList<>();
            for (int j = 1; j < clause.size(); j++) {
                body.add(clause.get(j));
            }
            clauses.add(new Lambda(params, restParam, body, env));
        }
        return new CaseLambda(clauses);
    }


    @SuppressWarnings("unchecked")
    private static Object evalDo(List<?> list, Env env) throws EvalError {
        // (do ((var init step) ...) (test expr ...) body ...)
        if (list.size() < 3) throw new EvalError("do: bad syntax");
        if (!(list.get(1) instanceof List<?> varSpecs))
            throw new EvalError("do: variable specs must be a list");
        if (!(list.get(2) instanceof List<?> testClause) || testClause.isEmpty())
            throw new EvalError("do: test clause must be a non-empty list");

        // Parse variable specs
        int numVars = varSpecs.size();
        String[] names = new String[numVars];
        Object[] stepExprs = new Object[numVars]; // null if no step
        boolean[] hasStep = new boolean[numVars];

        Env doEnv = new Env(env);
        for (int i = 0; i < numVars; i++) {
            if (!(varSpecs.get(i) instanceof List<?> spec) || spec.size() < 2 || spec.size() > 3)
                throw new EvalError("do: bad variable spec");
            if (!(spec.get(0) instanceof String name))
                throw new EvalError("do: variable name must be symbol");
            names[i] = name;
            Object initVal = eval(spec.get(1), env);
            doEnv.define(name, initVal);
            if (spec.size() == 3) {
                stepExprs[i] = spec.get(2);
                hasStep[i] = true;
            }
        }

        // Iteration
        while (true) {
            // Check test
            Object testResult = eval(testClause.get(0), doEnv);
            if (!isFalse(testResult)) {
                // Test is true - evaluate result expressions
                if (testClause.size() == 1) return null; // void
                Object result = null;
                for (int j = 1; j < testClause.size(); j++) {
                    result = eval(testClause.get(j), doEnv);
                }
                return result;
            }
            // Evaluate body
            for (int i = 3; i < list.size(); i++) {
                eval(list.get(i), doEnv);
            }
            // Parallel step: evaluate all steps with current values, then update
            Object[] newVals = new Object[numVars];
            for (int i = 0; i < numVars; i++) {
                if (hasStep[i]) {
                    newVals[i] = eval(stepExprs[i], doEnv);
                }
            }
            for (int i = 0; i < numVars; i++) {
                if (hasStep[i]) {
                    doEnv.set(names[i], newVals[i]);
                }
            }
        }
    }

    static boolean isFalse(Object val) {
        return val instanceof Boolean b && !b;
    }

    static Object applyProc(Object func, List<Object> args) throws EvalError {
        if (func instanceof Builtin b) {
            return b.apply(args);
        }
        if (func instanceof CaseLambda cl) {
            for (Lambda lam : cl.clauses) {
                if (lam.restParam != null) {
                    if (args.size() >= lam.params.size()) return applyProc(lam, args);
                } else {
                    if (args.size() == lam.params.size()) return applyProc(lam, args);
                }
            }
            throw new EvalError("case-lambda: no matching clause for " + args.size() + " arguments");
        }
        if (func instanceof Lambda lam) {
            if (lam.restParam == null) {
                if (args.size() != lam.params.size())
                    throw new EvalError("lambda: expected " + lam.params.size() + " arguments, got " + args.size());
            } else {
                if (args.size() < lam.params.size())
                    throw new EvalError("lambda: expected at least " + lam.params.size() + " arguments, got " + args.size());
            }
            Env localEnv = new Env(lam.closure);
            for (int i = 0; i < lam.params.size(); i++) {
                localEnv.define(lam.params.get(i), args.get(i));
            }
            if (lam.restParam != null) {
                Object rest = SchemeValue.NIL;
                for (int i = args.size() - 1; i >= lam.params.size(); i--) {
                    rest = new Pair(args.get(i), rest);
                }
                localEnv.define(lam.restParam, rest);
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
