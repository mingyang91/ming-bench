package ming;

import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

/**
 * Represents a captured continuation for call/cc.
 * Stores enough state to replay from the top-level expression containing
 * the call/cc, preserving let-environments for mutable state.
 */
public class Continuation {
    /** Which call/cc invocation this is (for counter-based matching during replay). */
    final int callccId;

    /** Index of the top-level expression containing this call/cc. */
    final int exprIndex;

    /** All top-level expressions (for evaluating remaining ones after replay). */
    final List<SchemeValue> allExprs;

    /** The global environment. */
    final Environment globalEnv;

    /**
     * Map from let-expression (by identity) to its environment.
     * During replay, these environments are reused instead of re-created.
     */
    final Map<SchemeValue, Environment> letEnvMap;

    Continuation(int callccId, int exprIndex, List<SchemeValue> allExprs,
                 Environment globalEnv, Map<SchemeValue, Environment> letEnvMap) {
        this.callccId = callccId;
        this.exprIndex = exprIndex;
        this.allExprs = allExprs;
        this.globalEnv = globalEnv;
        this.letEnvMap = new IdentityHashMap<>(letEnvMap);
    }
}
