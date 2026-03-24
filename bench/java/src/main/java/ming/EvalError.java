package ming;

public class EvalError extends Exception {
    public EvalError(String message) {
        super(message);
    }

    public EvalError(SourcePos pos, String message) {
        super(pos + ": " + message);
    }

    public static EvalError syntax(SourcePos pos, String message) {
        return new EvalError(pos, "syntax error: " + message);
    }

    public static EvalError type(SourcePos pos, String message) {
        return new EvalError(pos, "type error: " + message);
    }

    public static EvalError arity(SourcePos pos, String name, String message) {
        return new EvalError(pos, name + ": " + message);
    }
}
