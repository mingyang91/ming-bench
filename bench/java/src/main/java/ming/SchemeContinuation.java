package ming;

import java.util.List;

/**
 * Represents a first-class continuation captured by call/cc.
 */
public class SchemeContinuation {
    final long id;
    // Body context at the point of capture (null if at top level or no body)
    final List<Object> bodyExprs;
    final int bodyIndex;
    final Environment bodyEnv;
    // The top-level expression index at capture time
    final int captureTopLevelIndex;
    // The top-level expression that contained this call/cc
    final Object topLevelExpr;
    // Remaining top-level expressions after the one containing this call/cc
    final List<Object> remainingTopLevel;
    final Environment topLevelEnv;
    final Evaluator evaluator;
    private static long nextId = 0;

    SchemeContinuation(List<Object> bodyExprs, int bodyIndex, Environment bodyEnv,
                       int captureTopLevelIndex, Object topLevelExpr,
                       List<Object> remainingTopLevel, Environment topLevelEnv,
                       Evaluator evaluator) {
        this.id = nextId++;
        this.bodyExprs = bodyExprs;
        this.bodyIndex = bodyIndex;
        this.bodyEnv = bodyEnv;
        this.captureTopLevelIndex = captureTopLevelIndex;
        this.topLevelExpr = topLevelExpr;
        this.remainingTopLevel = remainingTopLevel;
        this.topLevelEnv = topLevelEnv;
        this.evaluator = evaluator;
    }
}
