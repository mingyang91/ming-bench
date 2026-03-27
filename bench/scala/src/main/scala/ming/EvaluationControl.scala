package ming

sealed private[ming] trait EvaluationControl

final private[ming] case class EvalExprControl(
  expression: Expr,
  env: Environment
) extends EvaluationControl

final private[ming] case class EvalSequenceControl(
  expressions: List[Expr],
  env: Environment
) extends EvaluationControl

final private[ming] case class ApplyControl(
  function: Value,
  arguments: List[Value],
  position: Position
) extends EvaluationControl

sealed private[ming] trait EvaluationStep

final private[ming] case class ReturnStep(value: Value) extends EvaluationStep

final private[ming] case class ContinueStep(control: EvaluationControl) extends EvaluationStep
