package ming

import scala.util.DynamicVariable

private[ming] object StepLimitRuntime:

  final private class StepBudget(
    val maxSteps: Int,
    var remaining: Int
  )

  private val activeBudget = DynamicVariable[Option[StepBudget]](None)

  def withStepLimit[A](maxSteps: Int)(thunk: => A): A =
    if maxSteps < 0 then throw new EvalError(s"step limit must be non-negative, got $maxSteps")
    else activeBudget.withValue(Some(new StepBudget(maxSteps, maxSteps)))(thunk)

  def consumeExpressionStep(): Unit =
    activeBudget.value.foreach { budget =>
      if budget.remaining <= 0 then
        throw new EvalError(s"step limit exceeded after ${budget.maxSteps} step(s)")
      budget.remaining -= 1
    }
