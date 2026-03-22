package ming;

public class ContinuationException extends RuntimeException {
    public final int contId;
    public final SchemeValue value;

    public ContinuationException(int contId, SchemeValue value) {
        super(null, null, true, false); // suppress stack trace for performance
        this.contId = contId;
        this.value = value;
    }
}
