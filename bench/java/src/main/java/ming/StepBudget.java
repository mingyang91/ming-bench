package ming;

final class StepBudget {
    private long remaining;

    private StepBudget(long remaining) {
        this.remaining = remaining;
    }

    static StepBudget limited(long maxSteps) throws EvalError {
        if (maxSteps < 0) {
            throw new EvalError("step limit must be non-negative");
        }
        return new StepBudget(maxSteps);
    }

    void consume() throws EvalError {
        if (remaining == 0) {
            throw new EvalError("step limit exceeded");
        }
        remaining--;
    }
}
