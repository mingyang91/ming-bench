package ming

type stepBudget struct {
	remaining int
	limit     int
}

func newStepBudget(limit int) *stepBudget {
	return &stepBudget{
		remaining: limit,
		limit:     limit,
	}
}

func consumeEvalStep(ctx *evalContext, expr node) error {
	if ctx == nil || ctx.stepBudget == nil {
		return nil
	}
	if ctx.stepBudget.remaining <= 0 {
		return stepLimitExceededError(nodePos(expr), ctx.stepBudget.limit)
	}
	ctx.stepBudget.remaining--
	return nil
}

func procedureEvalContext(proc value) *evalContext {
	switch proc := proc.(type) {
	case *closureValue:
		return proc.env.evalContext()
	case *caseClosureValue:
		if len(proc.clauses) == 0 {
			return nil
		}
		return proc.clauses[0].env.evalContext()
	case *continuationValue:
		return proc.ctx
	default:
		return nil
	}
}
