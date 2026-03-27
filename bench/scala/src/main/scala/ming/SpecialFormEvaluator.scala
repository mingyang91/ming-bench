package ming

import RuntimeSupport.isTruthy

import scala.annotation.tailrec

private[ming] object SpecialFormEvaluator:

  @tailrec
  def evalAnd(expressions: List[Expr], env: Environment): EvaluationStep =
    expressions match
      case Nil =>
        InterpreterEvaluator.done(BoolValue(true))
      case expression :: Nil =>
        InterpreterEvaluator.deferExpr(expression, env)
      case expression :: rest =>
        val result = InterpreterEvaluator.eval(expression, env)
        if isTruthy(result) then evalAnd(rest, env)
        else InterpreterEvaluator.done(result)

  @tailrec
  def evalOr(expressions: List[Expr], env: Environment): EvaluationStep =
    expressions match
      case Nil =>
        InterpreterEvaluator.done(BoolValue(false))
      case expression :: Nil =>
        InterpreterEvaluator.deferExpr(expression, env)
      case expression :: rest =>
        val result = InterpreterEvaluator.eval(expression, env)
        if isTruthy(result) then InterpreterEvaluator.done(result)
        else evalOr(rest, env)

  @tailrec
  def evalCond(clauses: List[Expr], env: Environment): EvaluationStep =
    clauses match
      case Nil =>
        InterpreterEvaluator.done(VoidValue)
      case ListExpr(SymbolExpr("else", _) :: body, clausePosition) :: rest =>
        evalCondElseClause(body, rest, clausePosition, env)
      case ListExpr(test :: body, _) :: rest =>
        val testValue = InterpreterEvaluator.eval(test, env)
        if isTruthy(testValue) then evalCondBody(body, testValue, env)
        else evalCond(rest, env)
      case clause :: _ =>
        SchemeFailure.raise("cond expected non-empty list clauses", clause.position)

  def evalDefine(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): EvaluationStep =
    InterpreterEvaluator.done(
      SpecialFormDefinitionEvaluator.evalDefine(arguments, position, env)
    )

  def evalDefineSyntax(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): EvaluationStep =
    InterpreterEvaluator.done(
      SpecialFormDefinitionEvaluator.evalDefineSyntax(arguments, position, env)
    )

  def evalIf(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): EvaluationStep =
    arguments match
      case condition :: consequent :: alternate :: Nil =>
        if isTruthy(InterpreterEvaluator.eval(condition, env)) then InterpreterEvaluator.deferExpr(consequent, env)
        else InterpreterEvaluator.deferExpr(alternate, env)
      case condition :: consequent :: Nil =>
        if isTruthy(InterpreterEvaluator.eval(condition, env)) then InterpreterEvaluator.deferExpr(consequent, env)
        else InterpreterEvaluator.done(VoidValue)
      case _ =>
        SchemeFailure.raise("if expected 2 or 3 argument(s)", position)

  def evalLet(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): EvaluationStep =
    SpecialFormBindingEvaluator.evalLet(arguments, position, env)

  def evalLetrec(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): EvaluationStep =
    SpecialFormBindingEvaluator.evalLetrec(arguments, position, env)

  def evalLetrecStar(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): EvaluationStep =
    SpecialFormBindingEvaluator.evalLetrecStar(arguments, position, env)

  def evalLetStar(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): EvaluationStep =
    SpecialFormBindingEvaluator.evalLetStar(arguments, position, env)

  def evalQuote(arguments: List[Expr], position: Position): EvaluationStep =
    InterpreterEvaluator.done(
      SpecialFormQuoteEvaluator.evalQuote(arguments, position)
    )

  def evalLambda(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): EvaluationStep =
    InterpreterEvaluator.done(
      SpecialFormProcedureEvaluator.evalLambda(arguments, position, env)
    )

  def evalCaseLambda(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): EvaluationStep =
    InterpreterEvaluator.done(
      SpecialFormProcedureEvaluator.evalCaseLambda(arguments, position, env)
    )

  def evalCase(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): EvaluationStep =
    arguments match
      case keyExpression :: clauses if clauses.nonEmpty =>
        evalCaseClauses(InterpreterEvaluator.eval(keyExpression, env), clauses, env)
      case _ =>
        SchemeFailure.raise("case expected a key and at least one clause", position)

  def evalDo(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): EvaluationStep =
    SpecialFormBindingEvaluator.evalDo(arguments, position, env)

  def evalSet(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): EvaluationStep =
    InterpreterEvaluator.done(
      SpecialFormDefinitionEvaluator.evalSet(arguments, position, env)
    )

  private def evalCondElseClause(
    body: List[Expr],
    rest: List[Expr],
    clausePosition: Position,
    env: Environment
  ): EvaluationStep =
    if rest.nonEmpty then SchemeFailure.raise("cond else clause must be last", clausePosition)

    if body.isEmpty then SchemeFailure.raise("cond else clause must have a body", clausePosition)

    InterpreterEvaluator.deferSequence(body, env)

  private def evalCondBody(body: List[Expr], testValue: Value, env: Environment): EvaluationStep =
    body match
      case Nil =>
        InterpreterEvaluator.done(testValue)
      case _ =>
        InterpreterEvaluator.deferSequence(body, env)

  private def evalCaseClauses(
    key: Value,
    clauses: List[Expr],
    env: Environment
  ): EvaluationStep =
    clauses match
      case Nil =>
        InterpreterEvaluator.done(VoidValue)
      case ListExpr(SymbolExpr("else", _) :: body, clausePosition) :: rest =>
        evalCaseElseClause(body, rest, clausePosition, env)
      case ListExpr(ListExpr(datumExpressions, _) :: body, clausePosition) :: rest =>
        evalDatumCaseClause(key, datumExpressions, body, clausePosition, rest, env)
      case clause :: _ =>
        SchemeFailure.raise(
          "case expected clauses of the form ((datum ...) body ...) or (else body ...)",
          clause.position
        )

  private def evalCaseElseClause(
    body: List[Expr],
    rest: List[Expr],
    clausePosition: Position,
    env: Environment
  ): EvaluationStep =
    if rest.nonEmpty then SchemeFailure.raise("case else clause must be last", clausePosition)

    if body.isEmpty then SchemeFailure.raise("case else clause must have a body", clausePosition)

    InterpreterEvaluator.deferSequence(body, env)

  private def evalDatumCaseClause(
    key: Value,
    datumExpressions: List[Expr],
    body: List[Expr],
    clausePosition: Position,
    rest: List[Expr],
    env: Environment
  ): EvaluationStep =
    if body.isEmpty then SchemeFailure.raise("case clause must have a body", clausePosition)

    if datumExpressions.exists(datum => Level1ValueSupport.eqvValues(key, SpecialFormQuoteEvaluator.quote(datum))) then
      InterpreterEvaluator.deferSequence(body, env)
    else evalCaseClauses(key, rest, env)
