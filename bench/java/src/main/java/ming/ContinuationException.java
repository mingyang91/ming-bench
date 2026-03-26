package ming;

/**
 * Thrown when a continuation is invoked, unwinds the stack to the call/cc point.
 */
public class ContinuationException extends Exception {
    final long continuationId;
    final Object value;

    ContinuationException(long continuationId, Object value) {
        super(null, null, true, false); // no stack trace for performance
        this.continuationId = continuationId;
        this.value = value;
    }
}
