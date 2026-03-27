package ming

import RuntimeSupport.isTruthy

import scala.annotation.tailrec

private[ming] object SpecialFormEvaluator:

  @tailrec
  def evalAnd(expressions: List[Expr], env: Environment): Value =
    expressions match
      case Nil =>
        BoolValue(true)
      case expression :: Nil =>
        InterpreterEvaluator.eval(expression, env)
      case expression :: rest =>
        val result = InterpreterEvaluator.eval(expression, env)
        if isTruthy(result) then evalAnd(rest, env)
        else result

  @tailrec
  def evalOr(expressions: List[Expr], env: Environment): Value =
    expressions match
      case Nil =>
        BoolValue(false)
      case expression :: rest =>
        val result = InterpreterEvaluator.eval(expression, env)
        if isTruthy(result) then result
        else evalOr(rest, env)

  @tailrec
  def evalCond(clauses: List[Expr], env: Environment): Value =
    clauses match
      case Nil =>
        VoidValue
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
  ): Value =
    SpecialFormDefinitionEvaluator.evalDefine(arguments, position, env)

  def evalDefineSyntax(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    SpecialFormDefinitionEvaluator.evalDefineSyntax(arguments, position, env)

  def evalIf(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    arguments match
      case condition :: consequent :: alternate :: Nil =>
        if isTruthy(InterpreterEvaluator.eval(condition, env)) then InterpreterEvaluator.eval(consequent, env)
        else InterpreterEvaluator.eval(alternate, env)
      case condition :: consequent :: Nil =>
        if isTruthy(InterpreterEvaluator.eval(condition, env)) then InterpreterEvaluator.eval(consequent, env)
        else VoidValue
      case _ =>
        SchemeFailure.raise("if expected 2 or 3 argument(s)", position)

  def evalLet(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    SpecialFormBindingEvaluator.evalLet(arguments, position, env)

  def evalLetrec(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    SpecialFormBindingEvaluator.evalLetrec(arguments, position, env)

  def evalLetrecStar(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    SpecialFormBindingEvaluator.evalLetrecStar(arguments, position, env)

  def evalQuote(arguments: List[Expr], position: Position): Value =
    SpecialFormQuoteEvaluator.evalQuote(arguments, position)

  def evalLambda(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    SpecialFormProcedureEvaluator.evalLambda(arguments, position, env)

  def evalCaseLambda(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    SpecialFormProcedureEvaluator.evalCaseLambda(arguments, position, env)

  def evalCase(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    arguments match
      case keyExpression :: clauses if clauses.nonEmpty =>
        evalCaseClauses(InterpreterEvaluator.eval(keyExpression, env), clauses, env)
      case _ =>
        SchemeFailure.raise("case expected a key and at least one clause", position)

  def evalDo(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    SpecialFormBindingEvaluator.evalDo(arguments, position, env)

  def evalSet(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    SpecialFormDefinitionEvaluator.evalSet(arguments, position, env)

  private def evalCondElseClause(
    body: List[Expr],
    rest: List[Expr],
    clausePosition: Position,
    env: Environment
  ): Value =
    if rest.nonEmpty then SchemeFailure.raise("cond else clause must be last", clausePosition)

    if body.isEmpty then SchemeFailure.raise("cond else clause must have a body", clausePosition)

    InterpreterEvaluator.evalSequence(body, env)

  private def evalCondBody(body: List[Expr], testValue: Value, env: Environment): Value =
    body match
      case Nil =>
        testValue
      case _ =>
        InterpreterEvaluator.evalSequence(body, env)

  private def evalCaseClauses(
    key: Value,
    clauses: List[Expr],
    env: Environment
  ): Value =
    clauses match
      case Nil =>
        VoidValue
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
  ): Value =
    if rest.nonEmpty then SchemeFailure.raise("case else clause must be last", clausePosition)

    if body.isEmpty then SchemeFailure.raise("case else clause must have a body", clausePosition)

    InterpreterEvaluator.evalSequence(body, env)

  private def evalDatumCaseClause(
    key: Value,
    datumExpressions: List[Expr],
    body: List[Expr],
    clausePosition: Position,
    rest: List[Expr],
    env: Environment
  ): Value =
    if body.isEmpty then SchemeFailure.raise("case clause must have a body", clausePosition)

    if datumExpressions.exists(datum => Level1ValueSupport.eqvValues(key, SpecialFormQuoteEvaluator.quote(datum))) then
      InterpreterEvaluator.evalSequence(body, env)
    else evalCaseClauses(key, rest, env)
