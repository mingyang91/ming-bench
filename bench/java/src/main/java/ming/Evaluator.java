package ming;

/**
 * Scheme interpreter entry point.
 * Agents implement this class.
 */
public class Evaluator {
    /**
     * Evaluate one or more Scheme expressions and return the string
     * representation of the last result.
     */
    public String evalStr(String input) throws EvalError {
        return evalStrWithOutput(input).result();
    }

    /**
     * Evaluate Scheme expressions and return both the result string
     * and any captured output from display/write/newline.
     */
    public EvalResult evalStrWithOutput(String input) throws EvalError {
        Parser parser = new Parser(input);
        Interpreter interpreter = new Interpreter();
        Value result = interpreter.evalProgram(parser.parseProgram());
        return new EvalResult(result.render(), interpreter.capturedOutput());
    }
}
