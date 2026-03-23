package ming;

/**
 * Represents an active dynamic-wind extent.
 */
public class WindRecord {
    final int id;
    final SchemeValue inThunk;
    final SchemeValue outThunk;

    WindRecord(int id, SchemeValue inThunk, SchemeValue outThunk) {
        this.id = id;
        this.inThunk = inThunk;
        this.outThunk = outThunk;
    }
}
