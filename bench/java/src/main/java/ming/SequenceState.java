package ming;

import java.util.List;

final class SequenceState {
    private final List<Expr> expressions;
    private final Value[] cachedCheckpointValues;
    private final boolean[] cachedCheckpoints;

    SequenceState(List<Expr> expressions) {
        this.expressions = expressions;
        this.cachedCheckpointValues = new Value[expressions.size()];
        this.cachedCheckpoints = new boolean[expressions.size()];
    }

    List<Expr> expressions() {
        return expressions;
    }

    boolean hasCachedCheckpoint(int index) {
        return cachedCheckpoints[index];
    }

    Value cachedCheckpointValue(int index) {
        return cachedCheckpointValues[index];
    }

    void cacheCheckpoint(int index, Value value) {
        cachedCheckpoints[index] = true;
        cachedCheckpointValues[index] = value;
    }
}
