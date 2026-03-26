package ming

type stepBudget struct {
	remaining int
}

func newStepBudget(maxSteps int) *stepBudget {
	return &stepBudget{remaining: maxSteps}
}

func consumeStepBudget(environment *env) error {
	budget := stepBudgetForEnv(environment)
	if budget == nil {
		return nil
	}
	if budget.remaining <= 0 {
		return &EvalError{Message: "step limit exceeded"}
	}
	budget.remaining--
	return nil
}

func stepBudgetForEnv(environment *env) *stepBudget {
	for current := environment; current != nil; current = current.parent {
		if current.budget != nil {
			return current.budget
		}
	}
	return nil
}
