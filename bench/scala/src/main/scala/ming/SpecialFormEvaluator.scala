package ming

import RuntimeSupport.isTruthy

private[ming] object SpecialFormEvaluator:

  def evalAnd(
    expressions: List[Expr],
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    expressions match
      case Nil =>
        InterpreterEvaluator.done(BoolValue(true), continuation)
      case expression :: Nil =>
        InterpreterEvaluator.deferExpr(expression, env, continuation)
      case expression :: rest =>
        InterpreterEvaluator.deferExpr(
          expression,
          env,
          result =>
            if isTruthy(result) then evalAnd(rest, env, continuation)
            else InterpreterEvaluator.done(result, continuation)
        )

  def evalOr(
    expressions: List[Expr],
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    expressions match
      case Nil =>
        InterpreterEvaluator.done(BoolValue(false), continuation)
      case expression :: Nil =>
        InterpreterEvaluator.deferExpr(expression, env, continuation)
      case expression :: rest =>
        InterpreterEvaluator.deferExpr(
          expression,
          env,
          result =>
            if isTruthy(result) then InterpreterEvaluator.done(result, continuation)
            else evalOr(rest, env, continuation)
        )

  def evalCond(
    clauses: List[Expr],
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    clauses match
      case Nil =>
        InterpreterEvaluator.done(VoidValue, continuation)
      case ListExpr(SymbolExpr("else", _) :: body, clausePosition) :: rest =>
        evalCondElseClause(body, rest, clausePosition, env, continuation)
      case ListExpr(test :: body, _) :: rest =>
        InterpreterEvaluator.deferExpr(
          test,
          env,
          testValue =>
            if isTruthy(testValue) then evalCondBody(body, testValue, env, continuation)
            else evalCond(rest, env, continuation)
        )
      case clause :: _ =>
        SchemeFailure.raise("cond expected non-empty list clauses", clause.position)

  def evalDefine(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    SpecialFormDefinitionEvaluator.evalDefine(arguments, position, env, continuation)

  def evalDefineSyntax(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    InterpreterEvaluator.done(
      SpecialFormDefinitionEvaluator.evalDefineSyntax(arguments, position, env),
      continuation
    )

  def evalIf(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    arguments match
      case condition :: consequent :: alternate :: Nil =>
        InterpreterEvaluator.deferExpr(
          condition,
          env,
          value =>
            if isTruthy(value) then InterpreterEvaluator.deferExpr(consequent, env, continuation)
            else InterpreterEvaluator.deferExpr(alternate, env, continuation)
        )
      case condition :: consequent :: Nil =>
        InterpreterEvaluator.deferExpr(
          condition,
          env,
          value =>
            if isTruthy(value) then InterpreterEvaluator.deferExpr(consequent, env, continuation)
            else InterpreterEvaluator.done(VoidValue, continuation)
        )
      case _ =>
        SchemeFailure.raise("if expected 2 or 3 argument(s)", position)

  def evalLet(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    SpecialFormBindingEvaluator.evalLet(arguments, position, env, continuation)

  def evalLetrec(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    SpecialFormBindingEvaluator.evalLetrec(arguments, position, env, continuation)

  def evalLetrecStar(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    SpecialFormBindingEvaluator.evalLetrecStar(arguments, position, env, continuation)

  def evalLetStar(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    SpecialFormBindingEvaluator.evalLetStar(arguments, position, env, continuation)

  def evalQuote(
    arguments: List[Expr],
    position: Position,
    continuation: Continuation
  ): EvaluationStep =
    InterpreterEvaluator.done(
      SpecialFormQuoteEvaluator.evalQuote(arguments, position),
      continuation
    )

  def evalLambda(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    InterpreterEvaluator.done(
      SpecialFormProcedureEvaluator.evalLambda(arguments, position, env),
      continuation
    )

  def evalCaseLambda(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    InterpreterEvaluator.done(
      SpecialFormProcedureEvaluator.evalCaseLambda(arguments, position, env),
      continuation
    )

  def evalCase(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    arguments match
      case keyExpression :: clauses if clauses.nonEmpty =>
        InterpreterEvaluator.deferExpr(
          keyExpression,
          env,
          key => evalCaseClauses(key, clauses, env, continuation)
        )
      case _ =>
        SchemeFailure.raise("case expected a key and at least one clause", position)

  def evalDo(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    SpecialFormBindingEvaluator.evalDo(arguments, position, env, continuation)

  def evalSet(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    SpecialFormDefinitionEvaluator.evalSet(arguments, position, env, continuation)

  private def evalCondElseClause(
    body: List[Expr],
    rest: List[Expr],
    clausePosition: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    if rest.nonEmpty then SchemeFailure.raise("cond else clause must be last", clausePosition)

    if body.isEmpty then SchemeFailure.raise("cond else clause must have a body", clausePosition)

    InterpreterEvaluator.deferSequence(body, env, continuation)

  private def evalCondBody(
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

  private def evalCaseClauses(
    key: Value,
    clauses: List[Expr],
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    clauses match
      case Nil =>
        InterpreterEvaluator.done(VoidValue, continuation)
      case ListExpr(SymbolExpr("else", _) :: body, clausePosition) :: rest =>
        evalCaseElseClause(body, rest, clausePosition, env, continuation)
      case ListExpr(ListExpr(datumExpressions, _) :: body, clausePosition) :: rest =>
        evalDatumCaseClause(key, datumExpressions, body, clausePosition, rest, env, continuation)
      case clause :: _ =>
        SchemeFailure.raise(
          "case expected clauses of the form ((datum ...) body ...) or (else body ...)",
          clause.position
        )

  private def evalCaseElseClause(
    body: List[Expr],
    rest: List[Expr],
    clausePosition: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    if rest.nonEmpty then SchemeFailure.raise("case else clause must be last", clausePosition)

    if body.isEmpty then SchemeFailure.raise("case else clause must have a body", clausePosition)

    InterpreterEvaluator.deferSequence(body, env, continuation)

  private def evalDatumCaseClause(
    key: Value,
    datumExpressions: List[Expr],
    body: List[Expr],
    clausePosition: Position,
    rest: List[Expr],
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    if body.isEmpty then SchemeFailure.raise("case clause must have a body", clausePosition)

    if datumExpressions.exists(datum => Level1ValueSupport.eqvValues(key, SpecialFormQuoteEvaluator.quote(datum))) then
      InterpreterEvaluator.deferSequence(body, env, continuation)
    else evalCaseClauses(key, rest, env, continuation)
