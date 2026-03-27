package ming;

import java.util.List;

sealed interface EvaluationTask permits ExpressionTask, SequenceTask {
}

record ExpressionTask(Expr expression, Environment env) implements EvaluationTask {
}

record SequenceTask(List<Expr> expressions, Environment env) implements EvaluationTask {
}

interface TailCallable {
    SequenceTask prepareTailCall(List<Value> arguments, SourceLoc callLoc) throws EvalError;
}

final class TailCall extends RuntimeException {
    private final EvaluationTask task;

    TailCall(EvaluationTask task) {
        this.task = task;
    }

    EvaluationTask task() {
        return task;
    }

    @Override
    public synchronized Throwable fillInStackTrace() {
        return this;
    }
}
