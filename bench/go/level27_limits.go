package ming

type stepBudget struct {
	remaining int
	limit     int
}

var currentStepBudget *stepBudget

func pushStepBudget(maxSteps int) func() {
	prev := currentStepBudget
	currentStepBudget = &stepBudget{
		remaining: maxSteps,
		limit:     maxSteps,
	}
	return func() {
		currentStepBudget = prev
	}
}

func consumeEvalStep(pos SourcePos) error {
	budget := currentStepBudget
	if budget == nil {
		return nil
	}
	if budget.remaining <= 0 {
		return newEvalError(pos, "step limit exceeded after %d steps", budget.limit)
	}
	budget.remaining--
	return nil
}
