package ming;

final class StepBudget {
    private int remainingSteps;

    StepBudget(int maxSteps) throws EvalError {
        if (maxSteps < 0) {
            throw new EvalError("step limit must be non-negative");
        }
        this.remainingSteps = maxSteps;
    }

    void consumeEvalStep() throws EvalError {
        if (remainingSteps <= 0) {
            throw new EvalError("step limit exceeded");
        }
        remainingSteps--;
    }
}
