package ming;

public class Evaluator {
    public String evalStr(String input) throws EvalError {
        var reader = new Reader(input);
        var exprs = reader.readAll();
        if (exprs.isEmpty()) throw new EvalError("no expressions");
        var interp = new Interpreter();
        interp.setPositions(reader.getPositions());
        SchemeValue result = null;
        for (var expr : exprs) {
            result = interp.eval(expr);
        }
        return result.display();
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        var reader = new Reader(input);
        var exprs = reader.readAll();
        if (exprs.isEmpty()) throw new EvalError("no expressions");
        var interp = new Interpreter();
        interp.setPositions(reader.getPositions());
        SchemeValue result = null;
        for (var expr : exprs) {
            result = interp.eval(expr);
        }
        String resultStr = result.display();
        if (result instanceof SchemeValue.VoidVal) resultStr = "";
        return new EvalResult(resultStr, interp.getOutput());
    }
}
