package ming;

public class EvalError extends Exception {
    private final Integer line;
    private final Integer column;

    public EvalError(String message) {
        this(message, null, null);
    }

    public EvalError(String message, int line, int column) {
        this(message, Integer.valueOf(line), Integer.valueOf(column));
    }

    private EvalError(String message, Integer line, Integer column) {
        super(message);
        this.line = line;
        this.column = column;
    }

    public EvalError withPosition(int line, int column) {
        if (hasPosition()) {
            return this;
        }
        return new EvalError(super.getMessage(), line, column);
    }

    public boolean hasPosition() {
        return line != null && column != null;
    }

    @Override
    public String getMessage() {
        if (!hasPosition()) {
            return super.getMessage();
        }
        return line + ":" + column + ": " + super.getMessage();
    }
}
