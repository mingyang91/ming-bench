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
        String result = evalStr(input);
        return new EvalResult(result, "");
    }
}
