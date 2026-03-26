package ming;

public class EvalError extends Exception {
    private final String baseMessage;
    private final Integer line;
    private final Integer column;

    public EvalError(String message) {
        this(message, null, null);
    }

    public EvalError(String message, int line, int column) {
        this(message, Integer.valueOf(line), Integer.valueOf(column));
    }

    private EvalError(String message, Integer line, Integer column) {
        super(formatMessage(message, line, column));
        this.baseMessage = message;
        this.line = line;
        this.column = column;
    }

    public EvalError withPosition(int line, int column) {
        if (hasPosition()) {
            return this;
        }
        return new EvalError(baseMessage, line, column);
    }

    private boolean hasPosition() {
        return line != null && column != null;
    }

    private static String formatMessage(String message, Integer line, Integer column) {
        if (line == null || column == null) {
            return message;
        }
        return message + " at " + line + ":" + column;
    }
}
