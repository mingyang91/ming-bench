package ming

import scala.annotation.tailrec

private[ming] object SchemeSpecialForms:

  def evaluateAnd(
    expressions: List[Expr],
    environment: Environment
  ): EvalOutcome =
    expressions match
      case Nil => EvalOutcome(Value.BooleanValue(true), environment)
      case expr :: rest =>
        val outcome = SchemeRuntime.evaluate(expr, environment)
        if !SchemeRuntime.isTruthy(outcome.value) || rest.isEmpty then outcome
        else evaluateAnd(rest, outcome.environment)

  def evaluateOr(
    expressions: List[Expr],
    environment: Environment
  ): EvalOutcome =
    expressions match
      case Nil => EvalOutcome(Value.BooleanValue(false), environment)
      case expr :: rest =>
        val outcome = SchemeRuntime.evaluate(expr, environment)
        if SchemeRuntime.isTruthy(outcome.value) || rest.isEmpty then outcome
        else evaluateOr(rest, outcome.environment)

  def evaluateIf(
    expressions: List[Expr],
    environment: Environment
  ): EvalOutcome =
    expressions match
      case condition :: consequent :: alternate :: Nil =>
        val conditionOutcome = SchemeRuntime.evaluate(condition, environment)
        if SchemeRuntime.isTruthy(conditionOutcome.value) then
          SchemeRuntime.evaluate(consequent, conditionOutcome.environment)
        else SchemeRuntime.evaluate(alternate, conditionOutcome.environment)
      case condition :: consequent :: Nil =>
        val conditionOutcome = SchemeRuntime.evaluate(condition, environment)
        if SchemeRuntime.isTruthy(conditionOutcome.value) then
          SchemeRuntime.evaluate(consequent, conditionOutcome.environment)
        else EvalOutcome(Value.VoidValue, conditionOutcome.environment)
      case _ =>
        throw new EvalError("if expects 2 or 3 arguments")

  def evaluateQuote(
    expressions: List[Expr],
    environment: Environment
  ): EvalOutcome =
    expressions match
      case quotedExpr :: Nil =>
        EvalOutcome(ValueCodec.quoteExpr(quotedExpr), environment)
      case _ =>
        throw new EvalError("quote expects exactly 1 argument")

  def evaluateLambda(
    expressions: List[Expr],
    environment: Environment
  ): EvalOutcome =
    expressions match
      case Expr.ListExpr(parameters) :: body if body.nonEmpty =>
        EvalOutcome(
          Value.ClosureValue(parameterNames(parameters), body, environment),
          environment
        )
      case Expr.ListExpr(_) :: Nil =>
        throw new EvalError("lambda expects at least 1 body expression")
      case _ =>
        throw new EvalError("lambda expects a parameter list and body")

  def evaluateDefine(
    expressions: List[Expr],
    environment: Environment
  ): EvalOutcome =
    val (name, valueExpr) = parseDefinition(expressions)
    defineBinding(name, valueExpr, environment)

  def evaluateBegin(expressions: List[Expr], environment: Environment): EvalOutcome =
    if expressions.isEmpty then EvalOutcome(Value.VoidValue, environment)
    else SchemeRuntime.evaluateSequence(expressions, environment)

  def evaluateCond(clauses: List[Expr], environment: Environment): EvalOutcome =
    evaluateCondClauses(clauses, environment)

  @tailrec
  private def evaluateCondClauses(
    clauses: List[Expr],
    environment: Environment
  ): EvalOutcome =
    clauses match
      case Nil =>
        EvalOutcome(Value.VoidValue, environment)
      case Expr.ListExpr(Expr.Symbol("else") :: body) :: Nil =>
        evaluateCondClauseBody(body, environment, Value.VoidValue)
      case Expr.ListExpr(Expr.Symbol("else") :: _) :: _ =>
        throw new EvalError("cond else clause must be last")
      case Expr.ListExpr(testExpr :: body) :: rest =>
        val testOutcome = SchemeRuntime.evaluate(testExpr, environment)
        if SchemeRuntime.isTruthy(testOutcome.value) then
          evaluateCondClauseBody(body, testOutcome.environment, testOutcome.value)
        else evaluateCondClauses(rest, testOutcome.environment)
      case Expr.ListExpr(Nil) :: _ =>
        throw new EvalError("cond clause cannot be empty")
      case _ =>
        throw new EvalError("cond clauses must be lists")

  private def evaluateCondClauseBody(
    body: List[Expr],
    environment: Environment,
    fallbackValue: Value
  ): EvalOutcome =
    body match
      case Nil => EvalOutcome(fallbackValue, environment)
      case _   => SchemeRuntime.evaluateSequence(body, environment)

  def evaluateLet(
    expressions: List[Expr],
    environment: Environment
  ): EvalOutcome =
    expressions match
      case Expr.ListExpr(bindings) :: body if body.nonEmpty =>
        evaluateAnonymousLet(bindings, body, environment)
      case Expr.Symbol(name) :: Expr.ListExpr(bindings) :: body if body.nonEmpty =>
        evaluateNamedLet(name, bindings, body, environment)
      case Expr.ListExpr(_) :: Nil =>
        throw new EvalError("let expects a body")
      case Expr.Symbol(_) :: Expr.ListExpr(_) :: Nil =>
        throw new EvalError("let expects a body")
      case _ =>
        throw new EvalError("let expects bindings and a body")

  private def evaluateAnonymousLet(
    bindingsExprs: List[Expr],
    body: List[Expr],
    environment: Environment
  ): EvalOutcome =
    val bindings         = evaluateLetBindings(bindingsExprs, environment)
    val localEnvironment = environment.extend(bindings)
    EvalOutcome(SchemeRuntime.evaluateSequence(body, localEnvironment).value, environment)

  private def evaluateNamedLet(
    name: String,
    bindingsExprs: List[Expr],
    body: List[Expr],
    environment: Environment
  ): EvalOutcome =
    val bindings = parseLetBindings(bindingsExprs)
    val arguments = bindings.map { case (_, valueExpr) =>
      SchemeRuntime.evaluate(valueExpr, environment).value
    }
    lazy val recursiveEnvironment: Environment =
      environment.define(name, loopBinding)
    lazy val loopBinding: Binding =
      Binding.delay(Value.ClosureValue(bindings.map(_._1), body, recursiveEnvironment))
    val result = SchemeRuntime.applyProcedure(loopBinding.force(), arguments)
    EvalOutcome(result, environment)

  def prepareSequence(
    expressions: List[Expr],
    environment: Environment,
    lastValue: Option[Value]
  ): (List[Expr], Environment, Option[Value]) =
    val (definitions, remainingExpressions) = leadingDefinitions(expressions)
    if definitions.isEmpty then (expressions, environment, lastValue)
    else (remainingExpressions, defineRecursively(definitions, environment), Some(Value.VoidValue))

  private def defineBinding(
    name: String,
    valueExpr: Expr,
    environment: Environment
  ): EvalOutcome =
    lazy val binding: Binding =
      Binding.delay(SchemeRuntime.evaluate(valueExpr, recursiveEnvironment).value)
    lazy val recursiveEnvironment: Environment =
      environment.define(name, binding)
    EvalOutcome(Value.VoidValue, recursiveEnvironment)

  private def parseDefinition(expressions: List[Expr]): (String, Expr) =
    expressions match
      case Expr.Symbol(name) :: valueExpr :: Nil =>
        (name, valueExpr)
      case Expr.ListExpr(Expr.Symbol(name) :: parameters) :: body if body.nonEmpty =>
        val lambdaExpr = Expr.ListExpr(Expr.Symbol("lambda") :: Expr.ListExpr(parameters) :: body)
        (name, lambdaExpr)
      case Expr.ListExpr(_) :: Nil =>
        throw new EvalError("define expects a function body")
      case _ =>
        throw new EvalError("define expects a name and value")

  @tailrec
  private def leadingDefinitions(
    expressions: List[Expr],
    definitionsReversed: List[(String, Expr)] = Nil
  ): (List[(String, Expr)], List[Expr]) =
    expressions match
      case Expr.ListExpr(Expr.Symbol("define") :: definitionExpressions) :: rest =>
        leadingDefinitions(rest, parseDefinition(definitionExpressions) :: definitionsReversed)
      case _ =>
        (definitionsReversed.reverse, expressions)

  private def defineRecursively(
    definitions: List[(String, Expr)],
    environment: Environment
  ): Environment =
    lazy val recursiveEnvironment: Environment = environment.extend(bindings)
    lazy val bindings: List[(String, Binding)] = definitions.map { case (name, valueExpr) =>
      name -> Binding.delay(SchemeRuntime.evaluate(valueExpr, recursiveEnvironment).value)
    }
    recursiveEnvironment

  private def parameterNames(parameters: List[Expr]): List[String] =
    parameters.map {
      case Expr.Symbol(name) => name
      case _                 => throw new EvalError("lambda parameters must be symbols")
    }

  private def parseLetBindings(bindings: List[Expr]): List[(String, Expr)] =
    bindings.map {
      case Expr.ListExpr(List(Expr.Symbol(name), valueExpr)) =>
        (name, valueExpr)
      case Expr.ListExpr(_) =>
        throw new EvalError("let bindings must contain a name and value")
      case _ =>
        throw new EvalError("let bindings must be lists")
    }

  private def evaluateLetBindings(
    bindingsExprs: List[Expr],
    environment: Environment
  ): List[(String, Binding)] =
    parseLetBindings(bindingsExprs).map { case (name, valueExpr) =>
      name -> Binding.eager(SchemeRuntime.evaluate(valueExpr, environment).value)
    }
