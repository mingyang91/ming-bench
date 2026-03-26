package ming;

public class EvalError extends Exception {
    private final SourcePosition position;
    private final String detail;

    public EvalError(String detail) {
        this(null, detail);
    }

    public EvalError(SourcePosition position, String detail) {
        super(formatMessage(position, detail));
        this.position = position;
        this.detail = detail;
    }

    boolean hasPosition() {
        return position != null;
    }

    EvalError withPosition(SourcePosition fallbackPosition) {
        if (hasPosition() || fallbackPosition == null) {
            return this;
        }
        return new EvalError(fallbackPosition, detail);
    }

    private static String formatMessage(SourcePosition position, String detail) {
        if (position == null) {
            return detail;
        }
        return position + ": " + detail;
    }
}
