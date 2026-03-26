package ming

import RuntimeSupport.{buildList, isTruthy}

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
    arguments match
      case SymbolExpr(name, _) :: valueExpression :: Nil =>
        evalValueDefine(name, valueExpression, env)
      case ListExpr(SymbolExpr(name, _) :: parameters, _) :: body if body.nonEmpty =>
        evalProcedureDefine(name, parameters, body, env, position)
      case _ =>
        SchemeFailure.raise(
          "define expected (define name expr) or (define (name args) body ...)",
          position
        )

  def evalIf(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    arguments match
      case condition :: consequent :: alternate :: Nil =>
        if isTruthy(InterpreterEvaluator.eval(condition, env)) then InterpreterEvaluator.eval(consequent, env)
        else InterpreterEvaluator.eval(alternate, env)
      case _ =>
        SchemeFailure.raise("if expected 3 argument(s)", position)

  def evalLet(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    arguments match
      case SymbolExpr(name, _) :: bindingsExpression :: body if body.nonEmpty =>
        evalNamedLet(name, bindingsExpression, body, position, env)
      case bindingsExpression :: body if body.nonEmpty =>
        evalUnnamedLet(bindingsExpression, body, position, env)
      case _ =>
        SchemeFailure.raise("let expected bindings and body", position)

  def evalQuote(arguments: List[Expr], position: Position): Value =
    arguments match
      case expression :: Nil =>
        quote(expression)
      case _ =>
        SchemeFailure.raise("quote expected 1 argument(s)", position)

  def evalLambda(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    arguments match
      case parametersExpression :: body if body.nonEmpty =>
        buildClosure(parametersExpression, body, env, None, position)
      case _ =>
        SchemeFailure.raise("lambda expected a parameter list and body", position)

  def evalSet(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    arguments match
      case SymbolExpr(name, symbolPosition) :: valueExpression :: Nil =>
        val value = InterpreterEvaluator.eval(valueExpression, env)
        env.assign(name, value, symbolPosition)
        VoidValue
      case _ =>
        SchemeFailure.raise("set! expected (set! name expr)", position)

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
      case Nil => testValue
      case _   => InterpreterEvaluator.evalSequence(body, env)

  private def evalValueDefine(name: String, valueExpression: Expr, env: Environment): Value =
    env.reserve(name)
    val value = InterpreterEvaluator.eval(valueExpression, env)
    env.define(name, value)
    VoidValue

  private def evalProcedureDefine(
    name: String,
    parameters: List[Expr],
    body: List[Expr],
    env: Environment,
    position: Position
  ): Value =
    env.reserve(name)
    val value = buildClosure(parameters, body, env, Some(name), position)
    env.define(name, value)
    VoidValue

  private def evalUnnamedLet(
    bindingsExpression: Expr,
    body: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    val bindings    = parseBindings(bindingsExpression, position, "let")
    val boundValues = bindings.map { case (_, expression) => InterpreterEvaluator.eval(expression, env) }
    val childEnv    = Environment.child(env, bindings.map(_._1).zip(boundValues))
    InterpreterEvaluator.evalSequence(body, childEnv)

  private def evalNamedLet(
    name: String,
    bindingsExpression: Expr,
    body: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    val bindings   = parseBindings(bindingsExpression, position, "let")
    val arguments  = bindings.map { case (_, expression) => InterpreterEvaluator.eval(expression, env) }
    val closureEnv = Environment.child(env)
    val closure    = ClosureValue(bindings.map(_._1), None, body, closureEnv, Some(name))
    closureEnv.define(name, closure)
    InterpreterEvaluator.applyFunction(closure, arguments, position)

  private def buildClosure(
    parametersExpression: Expr,
    body: List[Expr],
    env: Environment,
    name: Option[String],
    position: Position
  ): ClosureValue =
    val (parameters, restParameter) = parameterSpec(parametersExpression, position)
    ClosureValue(parameters, restParameter, body, env, name)

  private def buildClosure(
    parameterExpressions: List[Expr],
    body: List[Expr],
    env: Environment,
    name: Option[String],
    position: Position
  ): ClosureValue =
    val (parameters, restParameter) = parameterSpec(parameterExpressions, position)
    ClosureValue(parameters, restParameter, body, env, name)

  private def parameterSpec(
    parametersExpression: Expr,
    position: Position
  ): (List[String], Option[String]) =
    parametersExpression match
      case SymbolExpr(name, _) =>
        (Nil, Some(name))
      case ListExpr(parameterExpressions, _) =>
        parameterSpec(parameterExpressions, position)
      case _ =>
        SchemeFailure.raise("lambda expected a parameter list or symbol", position)

  private def parameterSpec(
    parameterExpressions: List[Expr],
    position: Position
  ): (List[String], Option[String]) =
    val dotIndex = parameterExpressions.indexWhere:
      case SymbolExpr(".", _) => true
      case _                  => false

    if dotIndex < 0 then (parameterExpressions.map(parameterName(_, position)), None)
    else
      parameterExpressions.drop(dotIndex) match
        case List(SymbolExpr(".", _), SymbolExpr(restName, _)) =>
          (
            parameterExpressions.take(dotIndex).map(parameterName(_, position)),
            Some(restName)
          )
        case _ =>
          SchemeFailure.raise("parameter list is malformed", position)

  private def parameterName(expression: Expr, position: Position): String =
    expression match
      case SymbolExpr(name, _) => name
      case _ =>
        SchemeFailure.raise("parameters must be symbols", position)

  private def parseBindings(
    bindingsExpression: Expr,
    position: Position,
    formName: String
  ): List[(String, Expr)] =
    bindingsExpression match
      case ListExpr(bindings, _) =>
        bindings.map(binding => parseBinding(binding, formName))
      case _ =>
        SchemeFailure.raise(s"$formName expected a binding list", position)

  private def parseBinding(binding: Expr, formName: String): (String, Expr) =
    binding match
      case ListExpr(List(SymbolExpr(name, _), valueExpression), _) =>
        name -> valueExpression
      case _ =>
        SchemeFailure.raise(
          s"$formName expected bindings of the form (name expr)",
          binding.position
        )

  private def quote(expression: Expr): Value =
    expression match
      case IntExpr(value, _)    => IntValue(value)
      case BoolExpr(value, _)   => BoolValue(value)
      case StringExpr(value, _) => StringValue(value)
      case CharExpr(value, _)   => CharValue(value)
      case SymbolExpr(name, _)  => SymbolValue(name)
      case ListExpr(items, _)   => buildList(items.map(quote))
