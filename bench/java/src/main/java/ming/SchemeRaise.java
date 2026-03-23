package ming;

/**
 * Thrown when (raise value) is called in Scheme.
 * Propagates through the Java stack until caught by guard or with-exception-handler.
 */
public class SchemeRaise extends RuntimeException {
    final SchemeValue value;

    SchemeRaise(SchemeValue value) {
        super(null, null, true, false); // no stack trace for performance
        this.value = value;
    }
}
