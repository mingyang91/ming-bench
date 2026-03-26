package ming;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

final class SequenceEvaluator {
    private final Evaluator evaluator;
    private final List<Expr> exprs;
    private final Environment env;
    private final Continuation cont;
    private final Set<Integer> committedCallCcIndexes = new HashSet<>();

    private SequenceEvaluator(Evaluator evaluator, List<Expr> exprs, Environment env,
                              Continuation cont) {
        this.evaluator = evaluator;
        this.exprs = List.copyOf(exprs);
        this.env = env;
        this.cont = cont;
    }

    static Bounce evaluate(Evaluator evaluator, List<Expr> exprs, Environment env,
                           Continuation cont) throws EvalError {
        if (exprs.isEmpty()) {
            return evaluator.deliver(cont, VoidValue.INSTANCE);
        }
        if (exprs.size() == 1) {
            return evaluator.evalExpr(exprs.getFirst(), env, cont);
        }
        return new SequenceEvaluator(evaluator, exprs, env, cont).resumeFrom(0);
    }

    private Bounce resumeFrom(int index) throws EvalError {
        int nextIndex = index;
        while (nextIndex < exprs.size() && committedCallCcIndexes.contains(nextIndex)) {
            nextIndex++;
        }

        if (nextIndex >= exprs.size()) {
            return evaluator.deliver(cont, VoidValue.INSTANCE);
        }

        Expr expr = exprs.get(nextIndex);
        int sequenceIndex = nextIndex;
        if (nextIndex == exprs.size() - 1) {
            return evaluator.evalExpr(expr, env,
                    values -> evaluator.withPosition(expr.position(), () -> {
                        markCommitted(sequenceIndex);
                        return cont.resume(values);
                    }));
        }

        return evaluator.evalExpr(expr, env,
                evaluator.positionedCont(expr.position(), ignored -> {
                    markCommitted(sequenceIndex);
                    return resumeFrom(sequenceIndex + 1);
                }));
    }

    private void markCommitted(int index) {
        if (isCallCcExpr(exprs.get(index))) {
            committedCallCcIndexes.add(index);
        }
    }

    private boolean isCallCcExpr(Expr expr) {
        if (!(expr instanceof ListExpr listExpr)) {
            return false;
        }

        List<Expr> elements = listExpr.elements();
        if (elements.isEmpty()) {
            return false;
        }

        if (!(elements.getFirst() instanceof SymbolExpr symbolExpr)) {
            return false;
        }

        return symbolExpr.name().equals("call/cc")
                || symbolExpr.name().equals("call-with-current-continuation");
    }
}
