package ming;

import java.util.List;

public class Evaluator {
    public String evalStr(String input) throws EvalError {
        var parser = new Parser(input);
        List<SchemeValue> exprs = parser.parseAll();
        var interpreter = new Interpreter();
        SchemeValue result = evalTopLevel(interpreter, exprs, false);
        if (result == null) return "";
        return result.display();
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        var parser = new Parser(input);
        List<SchemeValue> exprs = parser.parseAll();
        var interpreter = new Interpreter();
        SchemeValue result = evalTopLevel(interpreter, exprs, true);
        String resultStr = (result instanceof SchemeValue.VoidVal || result == null) ? "" : result.display();
        return new EvalResult(resultStr, interpreter.getOutput());
    }

    private SchemeValue evalTopLevel(Interpreter interpreter, List<SchemeValue> exprs, boolean keepVoid) throws EvalError {
        int startIndex = 0;
        SchemeValue result = null;
        while (true) {
            try {
                for (int i = startIndex; i < exprs.size(); i++) {
                    interpreter.topLevelIndex = i;
                    var val = interpreter.eval(exprs.get(i));
                    if (keepVoid) {
                        result = val;
                    } else {
                        if (!(val instanceof SchemeValue.VoidVal)) {
                            result = val;
                        }
                    }
                }
                return result;
            } catch (ContinuationException e) {
                var cont = interpreter.continuationData.get(e.contId);
                if (cont == null) {
                    throw new EvalError("continuation invoked outside its dynamic extent");
                }
                interpreter.pendingReturns.put(cont.callccExpr, e.value);
                if (cont.letBody != null && cont.letBodyEnv != null) {
                    // Direct let body restart — avoids re-evaluating let bindings
                    var bodyResult = interpreter.restartLetBody(cont.letBody, cont.letBodyIndex, cont.letBodyEnv);
                    if (keepVoid) {
                        result = bodyResult;
                    } else if (!(bodyResult instanceof SchemeValue.VoidVal)) {
                        result = bodyResult;
                    }
                    startIndex = cont.topLevelIndex + 1;
                } else {
                    // Re-evaluate from the top-level expression containing call/cc
                    startIndex = cont.topLevelIndex;
                }
            }
        }
    }
}
