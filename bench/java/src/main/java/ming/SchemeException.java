package ming;

/**
 * Runtime exception carrying a Scheme value, used by raise/guard/with-exception-handler.
 */
public class SchemeException extends RuntimeException {
    final SchemeValue value;

    SchemeException(SchemeValue value) {
        super(null, null, true, false); // suppress stack trace for performance
        this.value = value;
    }
}
