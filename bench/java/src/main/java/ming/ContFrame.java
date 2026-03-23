package ming;

import java.util.List;

/**
 * Continuation stack frame types.
 * Captures the remaining computation context for call/cc.
 */
public sealed interface ContFrame {
    /** A body-evaluation frame: tracks remaining expressions in a body sequence. */
    record BodyFrame(SchemeValue currentExpr, List<SchemeValue> remaining,
                     Environment env, int callccCounterBefore) implements ContFrame {}

    /** Context frame for define: after value is computed, define the variable. */
    record DefineFrame(String name, Environment env) implements ContFrame {}

    /** Context frame for set!: after value is computed, set the variable. */
    record SetFrame(String name, Environment env) implements ContFrame {}

    /** Context frame for dynamic-wind: when reached during replay, call out-thunk and pop wind stack. */
    record WindExitFrame(WindRecord windRecord) implements ContFrame {}
}
