package ming;

public class EvalError extends Exception {
    private final String detailMessage;
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
        this.detailMessage = message;
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
        return new EvalError(detailMessage, line, column);
    }

    private static String formatMessage(String message, Integer line, Integer column) {
        if (line == null || column == null) {
            return message;
        }
        return message + " at " + line + ":" + column;
    }
}
