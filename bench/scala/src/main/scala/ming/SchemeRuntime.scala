package ming

import scala.annotation.tailrec

private[ming] object SchemeRuntime:

  def evaluateProgramToString(expressions: List[Expr]): String =
    ValueCodec.render(evaluateSequence(expressions, Environment.empty).value)

  @tailrec
  private[ming] def evaluateSequence(
    expressions: List[Expr],
    environment: Environment,
    lastValue: Option[Value] = None
  ): EvalOutcome =
    val (preparedExpressions, preparedEnvironment, preparedLastValue) =
      SchemeSpecialForms.prepareSequence(expressions, environment, lastValue)
    preparedExpressions match
      case Nil =>
        preparedLastValue match
          case Some(value) => EvalOutcome(value, preparedEnvironment)
          case None        => throw new EvalError("empty input")
      case expr :: rest =>
        val outcome = evaluate(expr, preparedEnvironment)
        evaluateSequence(rest, outcome.environment, Some(outcome.value))

  private[ming] def evaluate(expr: Expr, environment: Environment): EvalOutcome =
    expr match
      case Expr.IntegerLiteral(value) => EvalOutcome(Value.IntegerValue(value), environment)
      case Expr.BooleanLiteral(value) => EvalOutcome(Value.BooleanValue(value), environment)
      case Expr.StringLiteral(value)  => EvalOutcome(Value.StringValue(value), environment)
      case Expr.Symbol(name)          => EvalOutcome(lookupValue(name, environment), environment)
      case Expr.ListExpr(elements)    => evaluateList(elements, environment)

  private def evaluateList(
    elements: List[Expr],
    environment: Environment
  ): EvalOutcome =
    elements match
      case Nil =>
        throw new EvalError("cannot evaluate empty list")
      case Expr.Symbol("and") :: rest =>
        SchemeSpecialForms.evaluateAnd(rest, environment)
      case Expr.Symbol("or") :: rest =>
        SchemeSpecialForms.evaluateOr(rest, environment)
      case Expr.Symbol("if") :: rest =>
        SchemeSpecialForms.evaluateIf(rest, environment)
      case Expr.Symbol("quote") :: rest =>
        SchemeSpecialForms.evaluateQuote(rest, environment)
      case Expr.Symbol("lambda") :: rest =>
        SchemeSpecialForms.evaluateLambda(rest, environment)
      case Expr.Symbol("define") :: rest =>
        SchemeSpecialForms.evaluateDefine(rest, environment)
      case Expr.Symbol("begin") :: rest =>
        SchemeSpecialForms.evaluateBegin(rest, environment)
      case Expr.Symbol("cond") :: rest =>
        SchemeSpecialForms.evaluateCond(rest, environment)
      case Expr.Symbol("let") :: rest =>
        SchemeSpecialForms.evaluateLet(rest, environment)
      case operatorExpr :: argumentExprs =>
        val operatorOutcome = evaluate(operatorExpr, environment)
        val argumentOutcome =
          evaluateArguments(argumentExprs, operatorOutcome.environment)
        EvalOutcome(
          applyProcedure(operatorOutcome.value, argumentOutcome.values),
          argumentOutcome.environment
        )

  private[ming] def evaluateArguments(
    arguments: List[Expr],
    environment: Environment
  ): ArgumentOutcome =
    val evaluated = arguments.foldLeft(ArgumentOutcome(Nil, environment)) { (state, argumentExpr) =>
      val outcome = evaluate(argumentExpr, state.environment)
      ArgumentOutcome(outcome.value :: state.values, outcome.environment)
    }
    ArgumentOutcome(evaluated.values.reverse, evaluated.environment)

  private[ming] def lookupValue(name: String, environment: Environment): Value =
    environment
      .lookup(name)
      .orElse(SchemeBuiltins.lookup(name))
      .getOrElse(throw new EvalError(s"unbound symbol: $name"))

  private[ming] def applyProcedure(operator: Value, arguments: List[Value]): Value =
    operator match
      case Value.BuiltinValue(_, applyTo) =>
        applyTo(arguments)
      case Value.ClosureValue(parameters, body, closureEnvironment) =>
        if parameters.length != arguments.length then
          throw new EvalError(
            s"procedure expects ${parameters.length} arguments, got ${arguments.length}"
          )
        val callEnvironment =
          closureEnvironment.extend(parameters.zip(arguments).map { case (name, value) =>
            name -> Binding.eager(value)
          })
        evaluateSequence(body, callEnvironment).value
      case other =>
        throw new EvalError(
          s"attempted to call non-procedure: ${ValueCodec.render(other)}"
        )

  private[ming] def isTruthy(value: Value): Boolean =
    value match
      case Value.BooleanValue(false) => false
      case _                         => true
