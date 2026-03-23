package ming;

import java.util.ArrayList;
import java.util.Deque;
import java.util.List;

/**
 * Represents a captured continuation for call/cc.
 * Stores a snapshot of the continuation stack (remaining computation frames)
 * and the call/cc ID for replay matching.
 */
public class Continuation {
    /** Which call/cc invocation this is (for counter-based matching during replay). */
    final int callccId;

    /** Snapshot of the continuation stack at capture time. */
    final List<ContFrame> contStackSnapshot;

    Continuation(int callccId, Deque<ContFrame> contStack) {
        this.callccId = callccId;
        this.contStackSnapshot = new ArrayList<>(contStack);
    }
}
