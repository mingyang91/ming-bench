package ming

import RuntimeSupport.expectSingleValue

private[ming] object SingleValueExpressionEvaluator:

  def evalAll(
    expressions: List[Expr],
    env: Environment,
    context: String,
    finish: List[Value] => EvaluationStep,
    reversedValues: List[Value] = Nil
  ): EvaluationStep =
    expressions match
      case Nil =>
        finish(reversedValues.reverse)
      case expression :: rest =>
        InterpreterEvaluator.deferExpr(
          expression,
          env,
          value =>
            evalAll(
              rest,
              env,
              context,
              finish,
              expectSingleValue(value, context, expression.position) :: reversedValues
            )
        )
