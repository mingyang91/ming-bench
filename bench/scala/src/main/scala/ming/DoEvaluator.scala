package ming

import RuntimeSupport.{expectSingleValue, isTruthy}
import SpecialFormBindingSyntax.*

private[ming] object DoEvaluator:

  def eval(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    arguments match
      case bindingsExpression :: testClauseExpression :: body =>
        val bindings   = parseDoBindings(bindingsExpression, position)
        val testClause = parseDoTestClause(testClauseExpression)
        SingleValueExpressionEvaluator.evalAll(
          bindings.map(_.initExpression),
          env,
          "binding",
          initialValues =>
            val loopEnv = Environment.child(env, bindings.map(_.name).zip(initialValues))
            evalLoop(bindings, testClause, body, loopEnv, continuation)
        )
      case _ =>
        SchemeFailure.raise("do expected bindings and a test clause", position)

  private def evalLoop(
    bindings: List[DoBinding],
    testClause: DoTestClause,
    body: List[Expr],
    loopEnv: Environment,
    continuation: Continuation
  ): EvaluationStep =
    InterpreterEvaluator.deferExpr(
      testClause.testExpression,
      loopEnv,
      testValue =>
        if isTruthy(testValue) then evalFinalExpressions(testClause.finalExpressions, loopEnv, continuation)
        else evalBodyAndAdvance(bindings, testClause, body, loopEnv, continuation)
    )

  private def evalFinalExpressions(
    finalExpressions: List[Expr],
    loopEnv: Environment,
    continuation: Continuation
  ): EvaluationStep =
    finalExpressions match
      case Nil =>
        InterpreterEvaluator.done(VoidValue, continuation)
      case _ =>
        InterpreterEvaluator.deferSequence(finalExpressions, loopEnv, continuation)

  private def evalBodyAndAdvance(
    bindings: List[DoBinding],
    testClause: DoTestClause,
    body: List[Expr],
    loopEnv: Environment,
    continuation: Continuation
  ): EvaluationStep =
    val finish = () =>
      advanceBindings(bindings, loopEnv, () => evalLoop(bindings, testClause, body, loopEnv, continuation))

    if body.nonEmpty then InterpreterEvaluator.deferSequence(body, loopEnv, _ => finish())
    else finish()

  private def advanceBindings(
    bindings: List[DoBinding],
    loopEnv: Environment,
    finish: () => EvaluationStep
  ): EvaluationStep =
    evalNextValues(
      bindings,
      loopEnv,
      nextValues =>
        bindings
          .zip(nextValues)
          .foreach: (binding, value) =>
            loopEnv.assign(binding.name, value, binding.position)
        finish()
    )

  private def evalNextValues(
    bindings: List[DoBinding],
    loopEnv: Environment,
    finish: List[Value] => EvaluationStep,
    reversedValues: List[Value] = Nil
  ): EvaluationStep =
    bindings match
      case Nil =>
        finish(reversedValues.reverse)
      case binding :: rest =>
        binding.stepExpression match
          case Some(stepExpression) =>
            InterpreterEvaluator.deferExpr(
              stepExpression,
              loopEnv,
              value =>
                evalNextValues(
                  rest,
                  loopEnv,
                  finish,
                  expectSingleValue(value, "do", stepExpression.position) :: reversedValues
                )
            )
          case None =>
            evalNextValues(
              rest,
              loopEnv,
              finish,
              loopEnv.lookup(binding.name, binding.position) :: reversedValues
            )
