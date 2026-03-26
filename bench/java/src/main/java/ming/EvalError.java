package ming;

public class EvalError extends Exception {
    private final String rawMessage;
    private final Integer line;
    private final Integer column;

    public EvalError(String message) {
        super(message);
        this.rawMessage = message;
        this.line = null;
        this.column = null;
    }

    public EvalError(String message, int line, int column) {
        super(message + " at " + line + ":" + column);
        this.rawMessage = message;
        this.line = line;
        this.column = column;
    }

    public boolean hasPosition() {
        return line != null && column != null;
    }

    public EvalError withPosition(int line, int column) {
        if (hasPosition()) {
            return this;
        }
        return new EvalError(rawMessage, line, column);
    }
}
