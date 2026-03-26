package ming

import scala.annotation.tailrec

private[ming] object Level1Interpreter:
  import RuntimeSupport.{buildList, isTruthy}

  def evalProgram(input: String): String =
    val expressions = SchemeParser.parseProgram(input)
    if expressions.isEmpty then SchemeFailure.raise("expected expression", Position(1, 1))

    val env = Environment.root(Level1Builtins.values)
    evalSequence(expressions, env).render

  private def evalSequence(expressions: List[Expr], env: Environment): Value =
    expressions.foldLeft[Value](VoidValue): (_, expression) =>
      eval(expression, env)

  private def eval(expression: Expr, env: Environment): Value =
    expression match
      case IntExpr(value, _)    => IntValue(value)
      case BoolExpr(value, _)   => BoolValue(value)
      case StringExpr(value, _) => StringValue(value)
      case SymbolExpr(name, position) =>
        env.lookup(name, position)
      case ListExpr(items, position) =>
        evalList(items, position, env)

  private def evalList(items: List[Expr], position: Position, env: Environment): Value =
    items match
      case Nil =>
        SchemeFailure.raise("cannot evaluate an empty list", position)
      case SymbolExpr("and", _) :: rest =>
        evalAnd(rest, env)
      case SymbolExpr("begin", _) :: rest =>
        evalSequence(rest, env)
      case SymbolExpr("cond", _) :: rest =>
        evalCond(rest, env)
      case SymbolExpr("or", _) :: rest =>
        evalOr(rest, env)
      case SymbolExpr("define", _) :: rest =>
        evalDefine(rest, position, env)
      case SymbolExpr("if", _) :: rest =>
        evalIf(rest, position, env)
      case SymbolExpr("let", _) :: rest =>
        evalLet(rest, position, env)
      case SymbolExpr("quote", _) :: rest =>
        evalQuote(rest, position)
      case SymbolExpr("lambda", _) :: rest =>
        evalLambda(rest, position, env)
      case operator :: arguments =>
        evalApplication(operator, arguments, position, env)

  private def evalApplication(
    operator: Expr,
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    val function           = eval(operator, env)
    val evaluatedArguments = arguments.map(argument => eval(argument, env))
    apply(function, evaluatedArguments, position)

  @tailrec
  private def evalAnd(expressions: List[Expr], env: Environment): Value =
    expressions match
      case Nil =>
        BoolValue(true)
      case expression :: Nil =>
        eval(expression, env)
      case expression :: rest =>
        val result = eval(expression, env)
        if isTruthy(result) then evalAnd(rest, env)
        else result

  @tailrec
  private def evalOr(expressions: List[Expr], env: Environment): Value =
    expressions match
      case Nil =>
        BoolValue(false)
      case expression :: rest =>
        val result = eval(expression, env)
        if isTruthy(result) then result
        else evalOr(rest, env)

  private def evalCond(clauses: List[Expr], env: Environment): Value =
    clauses match
      case Nil =>
        VoidValue
      case ListExpr(SymbolExpr("else", _) :: body, clausePosition) :: rest =>
        evalCondElseClause(body, rest, clausePosition, env)
      case ListExpr(test :: body, _) :: rest =>
        val testValue = eval(test, env)
        if isTruthy(testValue) then evalCondBody(body, testValue, env)
        else evalCond(rest, env)
      case clause :: _ =>
        SchemeFailure.raise("cond expected non-empty list clauses", clause.position)

  private def evalCondElseClause(
    body: List[Expr],
    rest: List[Expr],
    clausePosition: Position,
    env: Environment
  ): Value =
    if rest.nonEmpty then SchemeFailure.raise("cond else clause must be last", clausePosition)

    if body.isEmpty then SchemeFailure.raise("cond else clause must have a body", clausePosition)

    evalSequence(body, env)

  private def evalCondBody(body: List[Expr], testValue: Value, env: Environment): Value =
    body match
      case Nil => testValue
      case _   => evalSequence(body, env)

  private def evalDefine(
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

  private def evalValueDefine(name: String, valueExpression: Expr, env: Environment): Value =
    env.reserve(name)
    val value = eval(valueExpression, env)
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

  private def evalIf(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    arguments match
      case condition :: consequent :: alternate :: Nil =>
        if isTruthy(eval(condition, env)) then eval(consequent, env)
        else eval(alternate, env)
      case _ =>
        SchemeFailure.raise("if expected 3 argument(s)", position)

  private def evalLet(
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

  private def evalUnnamedLet(
    bindingsExpression: Expr,
    body: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    val bindings    = parseBindings(bindingsExpression, position, "let")
    val boundValues = bindings.map { case (_, expression) => eval(expression, env) }
    val childEnv    = Environment.child(env, bindings.map(_._1).zip(boundValues))
    evalSequence(body, childEnv)

  private def evalQuote(arguments: List[Expr], position: Position): Value =
    arguments match
      case expression :: Nil =>
        quote(expression)
      case _ =>
        SchemeFailure.raise("quote expected 1 argument(s)", position)

  private def evalLambda(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    arguments match
      case ListExpr(parameters, _) :: body if body.nonEmpty =>
        buildClosure(parameters, body, env, None, position)
      case _ =>
        SchemeFailure.raise("lambda expected a parameter list and body", position)

  private def evalNamedLet(
    name: String,
    bindingsExpression: Expr,
    body: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    val bindings   = parseBindings(bindingsExpression, position, "let")
    val arguments  = bindings.map { case (_, expression) => eval(expression, env) }
    val closureEnv = Environment.child(env)
    val closure    = ClosureValue(bindings.map(_._1), body, closureEnv, Some(name))
    closureEnv.define(name, closure)
    apply(closure, arguments, position)

  private def buildClosure(
    parameterExpressions: List[Expr],
    body: List[Expr],
    env: Environment,
    name: Option[String],
    position: Position
  ): ClosureValue =
    val parameters = parameterExpressions.map(parameterName(_, position))
    ClosureValue(parameters, body, env, name)

  private def parameterName(expression: Expr, position: Position): String =
    expression match
      case SymbolExpr(name, _) => name
      case _ =>
        SchemeFailure.raise("lambda parameters must be symbols", position)

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
      case SymbolExpr(name, _)  => SymbolValue(name)
      case ListExpr(items, _)   => buildList(items.map(quote))

  private def apply(function: Value, arguments: List[Value], position: Position): Value =
    function match
      case BuiltinValue(_, implementation) =>
        implementation(arguments, position)
      case ClosureValue(parameters, body, closureEnv, _) =>
        applyClosure(parameters, body, closureEnv, arguments, position)
      case other =>
        SchemeFailure.raise(
          s"attempted to call a non-procedure value: ${other.render}",
          position
        )

  private def applyClosure(
    parameters: List[String],
    body: List[Expr],
    closureEnv: Environment,
    arguments: List[Value],
    position: Position
  ): Value =
    if arguments.length != parameters.length then
      SchemeFailure.raise(
        s"procedure expected ${parameters.length} argument(s), got ${arguments.length}",
        position
      )

    val callEnv = Environment.child(closureEnv, parameters.zip(arguments))
    evalSequence(body, callEnv)
