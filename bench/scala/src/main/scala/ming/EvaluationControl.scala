package ming

sealed private[ming] trait EvaluationStep

private[ming] type Continuation = Value => EvaluationStep

sealed private[ming] trait EvaluationControl

final private[ming] case class EvalExprControl(
  expression: Expr,
  env: Environment,
  continuation: Continuation
) extends EvaluationControl

final private[ming] case class EvalSequenceControl(
  expressions: List[Expr],
  env: Environment,
  continuation: Continuation
) extends EvaluationControl

final private[ming] case class ApplyControl(
  function: Value,
  arguments: List[Value],
  position: Position,
  continuation: Continuation
) extends EvaluationControl

final private[ming] case class FinalStep(value: Value) extends EvaluationStep

final private[ming] case class ReturnStep(
  value: Value,
  continuation: Continuation
) extends EvaluationStep

final private[ming] case class ContinueStep(control: EvaluationControl) extends EvaluationStep
