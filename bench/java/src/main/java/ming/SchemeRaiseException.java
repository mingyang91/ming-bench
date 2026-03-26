package ming;

/**
 * Thrown when Scheme 'raise' is called, carries the raised value.
 */
public class SchemeRaiseException extends Exception {
    final Object value;

    SchemeRaiseException(Object value) {
        super(null, null, true, false); // no stack trace for performance
        this.value = value;
    }
}
