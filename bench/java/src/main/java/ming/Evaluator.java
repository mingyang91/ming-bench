package ming;

import java.util.ArrayList;

public class Evaluator {
    public String evalStr(String input) throws EvalError {
        var reader = new Reader(input);
        var exprs = reader.readAll();
        if (exprs.isEmpty()) throw new EvalError("no expressions");
        var interp = new Interpreter();
        interp.setPositions(reader.getPositions());
        // Use evalAll to evaluate all expressions in a single CEK loop
        // (required for call/cc to capture continuations across top-level expressions)
        var beginElems = new ArrayList<SchemeValue>();
        beginElems.add(new SchemeValue.SymbolVal("begin"));
        beginElems.addAll(exprs);
        SchemeValue result = interp.eval(new SchemeValue.ListVal(beginElems));
        return result.display();
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        var reader = new Reader(input);
        var exprs = reader.readAll();
        if (exprs.isEmpty()) throw new EvalError("no expressions");
        var interp = new Interpreter();
        interp.setPositions(reader.getPositions());
        var beginElems = new ArrayList<SchemeValue>();
        beginElems.add(new SchemeValue.SymbolVal("begin"));
        beginElems.addAll(exprs);
        SchemeValue result = interp.eval(new SchemeValue.ListVal(beginElems));
        String resultStr = result.displayOutput();
        if (result instanceof SchemeValue.VoidVal) resultStr = "";
        return new EvalResult(resultStr, interp.getOutput());
    }

    public String evalStrWithLimit(String input, int maxSteps) throws EvalError {
        var reader = new Reader(input);
        var exprs = reader.readAll();
        if (exprs.isEmpty()) throw new EvalError("no expressions");
        var interp = new Interpreter();
        interp.setPositions(reader.getPositions());
        interp.setStepLimit(maxSteps);
        var beginElems = new ArrayList<SchemeValue>();
        beginElems.add(new SchemeValue.SymbolVal("begin"));
        beginElems.addAll(exprs);
        SchemeValue result = interp.eval(new SchemeValue.ListVal(beginElems));
        return result.display();
    }
}
