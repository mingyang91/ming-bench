package ming;

/**
 * Thrown when a captured continuation is invoked.
 * Unwinds the stack to the top-level eval loop for replay.
 */
public class ContinuationReturn extends RuntimeException {
    final Continuation cont;
    final SchemeValue value;

    ContinuationReturn(Continuation cont, SchemeValue value) {
        super(null, null, true, false); // no stack trace for performance
        this.cont = cont;
        this.value = value;
    }
}
