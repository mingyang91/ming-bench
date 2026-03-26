package ming;

public class EvalError extends Exception {
    public EvalError(String message) {
        super(message);
    }

    public EvalError(String message, int line, int column) {
        super(message + " at " + line + ":" + column);
    }
}
