package ming

import RuntimeSupport.isTruthy

private[ming] object SpecialFormGuardEvaluator:

  def evalGuard(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    arguments match
      case ListExpr(SymbolExpr(name, _) :: clauses, _) :: body if body.nonEmpty =>
        val frame = ExceptionHandlerFrame(
          exception =>
            val guardEnv = Environment.child(env, List(name -> exception.value))
            evalGuardClauses(clauses, exception, guardEnv, continuation)
          ,
          DynamicWindRuntime.captureWindFrames
        )

        ExceptionRuntime.pushHandler(frame)
        InterpreterEvaluator.deferSequence(
          body,
          env,
          result =>
            ExceptionRuntime.popHandler(frame)
            InterpreterEvaluator.done(result, continuation)
        )
      case _ =>
        SchemeFailure.raise("guard expected (guard (variable clause ...) body ...)", position)

  private def evalGuardClauses(
    clauses: List[Expr],
    exception: SchemeException,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    clauses match
      case Nil =>
        ExceptionRuntime.raiseException(exception)
      case ListExpr(SymbolExpr("else", _) :: body, clausePosition) :: rest =>
        if rest.nonEmpty then SchemeFailure.raise("guard else clause must be last", clausePosition)

        if body.isEmpty then SchemeFailure.raise("guard else clause must have a body", clausePosition)

        InterpreterEvaluator.deferSequence(body, env, continuation)
      case ListExpr(test :: body, _) :: rest =>
        InterpreterEvaluator.deferExpr(
          test,
          env,
          testValue =>
            if isTruthy(testValue) then evalGuardBody(body, testValue, env, continuation)
            else evalGuardClauses(rest, exception, env, continuation)
        )
      case clause :: _ =>
        SchemeFailure.raise("guard expected non-empty list clauses", clause.position)

  private def evalGuardBody(
    body: List[Expr],
    testValue: Value,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    body match
      case Nil =>
        InterpreterEvaluator.done(testValue, continuation)
      case _ =>
        InterpreterEvaluator.deferSequence(body, env, continuation)
