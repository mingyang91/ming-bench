package ming;

import java.util.ArrayList;
import java.util.Deque;
import java.util.List;

/**
 * Represents a captured continuation for call/cc.
 * Stores a snapshot of the continuation stack (remaining computation frames),
 * the call/cc ID for replay matching, and the wind stack for dynamic-wind.
 */
public class Continuation {
    /** Which call/cc invocation this is (for counter-based matching during replay). */
    final int callccId;

    /** Snapshot of the continuation stack at capture time. */
    final List<ContFrame> contStackSnapshot;

    /** Snapshot of the wind stack at capture time (for dynamic-wind). */
    final List<WindRecord> windStackSnapshot;

    Continuation(int callccId, Deque<ContFrame> contStack, List<WindRecord> windStack) {
        this.callccId = callccId;
        this.contStackSnapshot = new ArrayList<>(contStack);
        this.windStackSnapshot = new ArrayList<>(windStack);
    }
}
