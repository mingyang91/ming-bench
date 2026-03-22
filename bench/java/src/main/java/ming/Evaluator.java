package ming;

import java.util.List;

public class Evaluator {
    public String evalStr(String input) throws EvalError {
        var parser = new Parser(input);
        List<SchemeValue> exprs = parser.parseAll();
        var interpreter = new Interpreter();
        SchemeValue result = null;
        for (var expr : exprs) {
            var val = interpreter.eval(expr);
            if (!(val instanceof SchemeValue.VoidVal)) {
                result = val;
            }
        }
        if (result == null) return "";
        return result.display();
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        var parser = new Parser(input);
        List<SchemeValue> exprs = parser.parseAll();
        var interpreter = new Interpreter();
        SchemeValue result = null;
        for (var expr : exprs) {
            result = interpreter.eval(expr);
        }
        String resultStr = (result instanceof SchemeValue.VoidVal) ? "" : result.display();
        return new EvalResult(resultStr, interpreter.getOutput());
    }
}
