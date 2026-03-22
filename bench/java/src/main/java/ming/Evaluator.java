package ming;

import java.util.List;

public class Evaluator {
    public String evalStr(String input) throws EvalError {
        var parser = new Parser(input);
        List<SchemeValue> exprs = parser.parseAll();
        var interpreter = new Interpreter();
        SchemeValue result = null;
        for (var expr : exprs) {
            result = interpreter.eval(expr);
        }
        return result.display();
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        String result = evalStr(input);
        return new EvalResult(result, "");
    }
}
