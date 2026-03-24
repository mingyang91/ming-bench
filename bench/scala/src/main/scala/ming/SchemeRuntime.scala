package ming

import scala.annotation.tailrec

private[ming] object SchemeRuntime:

  private val builtins: Map[String, List[Value] => Value] = Map(
    "+"   -> builtinAdd,
    "-"   -> builtinSubtract,
    "*"   -> builtinMultiply,
    "/"   -> builtinDivide,
    "<"   -> builtinLessThan,
    ">"   -> builtinGreaterThan,
    "="   -> builtinEqual,
    "<="  -> builtinLessThanOrEqual,
    "not" -> builtinNot
  )

  def evaluateProgramToString(expressions: List[Expr]): String =
    ValueCodec.render(evaluateSequence(expressions, Environment.empty).value)

  @tailrec
  private def evaluateSequence(
    expressions: List[Expr],
    environment: Environment,
    lastValue: Option[Value] = None
  ): EvalOutcome =
    expressions match
      case Nil =>
        lastValue match
          case Some(value) => EvalOutcome(value, environment)
          case None        => throw new EvalError("empty input")
      case expr :: rest =>
        val outcome = evaluate(expr, environment)
        evaluateSequence(rest, outcome.environment, Some(outcome.value))

  private def evaluate(expr: Expr, environment: Environment): EvalOutcome =
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
        evaluateAnd(rest, environment)
      case Expr.Symbol("or") :: rest =>
        evaluateOr(rest, environment)
      case Expr.Symbol("if") :: rest =>
        evaluateIf(rest, environment)
      case Expr.Symbol("quote") :: rest =>
        evaluateQuote(rest, environment)
      case Expr.Symbol("lambda") :: rest =>
        evaluateLambda(rest, environment)
      case Expr.Symbol("define") :: rest =>
        evaluateDefine(rest, environment)
      case operatorExpr :: argumentExprs =>
        val operatorOutcome = evaluate(operatorExpr, environment)
        val argumentOutcome =
          evaluateArguments(argumentExprs, operatorOutcome.environment)
        EvalOutcome(
          applyProcedure(operatorOutcome.value, argumentOutcome.values),
          argumentOutcome.environment
        )

  private def evaluateAnd(
    expressions: List[Expr],
    environment: Environment
  ): EvalOutcome =
    expressions match
      case Nil => EvalOutcome(Value.BooleanValue(true), environment)
      case expr :: rest =>
        val outcome = evaluate(expr, environment)
        if !isTruthy(outcome.value) || rest.isEmpty then outcome
        else evaluateAnd(rest, outcome.environment)

  private def evaluateOr(
    expressions: List[Expr],
    environment: Environment
  ): EvalOutcome =
    expressions match
      case Nil => EvalOutcome(Value.BooleanValue(false), environment)
      case expr :: rest =>
        val outcome = evaluate(expr, environment)
        if isTruthy(outcome.value) || rest.isEmpty then outcome
        else evaluateOr(rest, outcome.environment)

  private def evaluateIf(
    expressions: List[Expr],
    environment: Environment
  ): EvalOutcome =
    expressions match
      case condition :: consequent :: alternate :: Nil =>
        val conditionOutcome = evaluate(condition, environment)
        if isTruthy(conditionOutcome.value) then evaluate(consequent, conditionOutcome.environment)
        else evaluate(alternate, conditionOutcome.environment)
      case condition :: consequent :: Nil =>
        val conditionOutcome = evaluate(condition, environment)
        if isTruthy(conditionOutcome.value) then evaluate(consequent, conditionOutcome.environment)
        else EvalOutcome(Value.VoidValue, conditionOutcome.environment)
      case _ =>
        throw new EvalError("if expects 2 or 3 arguments")

  private def evaluateQuote(
    expressions: List[Expr],
    environment: Environment
  ): EvalOutcome =
    expressions match
      case quotedExpr :: Nil =>
        EvalOutcome(ValueCodec.quoteExpr(quotedExpr), environment)
      case _ =>
        throw new EvalError("quote expects exactly 1 argument")

  private def evaluateLambda(
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

  private def evaluateDefine(
    expressions: List[Expr],
    environment: Environment
  ): EvalOutcome =
    expressions match
      case Expr.Symbol(name) :: valueExpr :: Nil =>
        defineBinding(name, valueExpr, environment)
      case Expr.ListExpr(Expr.Symbol(name) :: parameters) :: body if body.nonEmpty =>
        val lambdaExpr =
          Expr.ListExpr(Expr.Symbol("lambda") :: Expr.ListExpr(parameters) :: body)
        defineBinding(name, lambdaExpr, environment)
      case Expr.ListExpr(_) :: Nil =>
        throw new EvalError("define expects a function body")
      case _ =>
        throw new EvalError("define expects a name and value")

  private def defineBinding(
    name: String,
    valueExpr: Expr,
    environment: Environment
  ): EvalOutcome =
    lazy val binding: Binding =
      Binding.delay(evaluate(valueExpr, recursiveEnvironment).value)
    lazy val recursiveEnvironment: Environment =
      environment.define(name, binding)
    EvalOutcome(Value.VoidValue, recursiveEnvironment)

  private def parameterNames(parameters: List[Expr]): List[String] =
    parameters.map {
      case Expr.Symbol(name) => name
      case _                 => throw new EvalError("lambda parameters must be symbols")
    }

  private def evaluateArguments(
    arguments: List[Expr],
    environment: Environment
  ): ArgumentOutcome =
    val evaluated = arguments.foldLeft(ArgumentOutcome(Nil, environment)) { (state, argumentExpr) =>
      val outcome = evaluate(argumentExpr, state.environment)
      ArgumentOutcome(outcome.value :: state.values, outcome.environment)
    }
    ArgumentOutcome(evaluated.values.reverse, evaluated.environment)

  private def lookupValue(name: String, environment: Environment): Value =
    environment
      .lookup(name)
      .orElse(builtins.get(name).map(fn => Value.BuiltinValue(name, fn)))
      .getOrElse(throw new EvalError(s"unbound symbol: $name"))

  private def applyProcedure(operator: Value, arguments: List[Value]): Value =
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

  private def isTruthy(value: Value): Boolean =
    value match
      case Value.BooleanValue(false) => false
      case _                         => true

  private def expectInteger(value: Value): Int =
    value match
      case Value.IntegerValue(number) => number
      case other =>
        throw new EvalError(s"expected number, got ${ValueCodec.render(other)}")

  private def requireExactlyOne(name: String, arguments: List[Value]): Value =
    arguments match
      case argument :: Nil => argument
      case _ =>
        throw new EvalError(s"$name expects exactly 1 argument")

  private def requireAtLeastOne(name: String, arguments: List[Value]): List[Value] =
    if arguments.nonEmpty then arguments
    else throw new EvalError(s"$name expects at least 1 argument")

  private def requireAtLeastTwo(name: String, arguments: List[Value]): List[Value] =
    if arguments.lengthCompare(2) >= 0 then arguments
    else throw new EvalError(s"$name expects at least 2 arguments")

  private def builtinAdd(arguments: List[Value]): Value =
    Value.IntegerValue(arguments.map(expectInteger).sum)

  private def builtinSubtract(arguments: List[Value]): Value =
    requireAtLeastOne("-", arguments) match
      case argument :: Nil =>
        Value.IntegerValue(-expectInteger(argument))
      case first :: rest =>
        val initial = expectInteger(first)
        Value.IntegerValue(rest.foldLeft(initial) { (acc, argument) =>
          acc - expectInteger(argument)
        })
      case Nil =>
        throw new EvalError("- expects at least 1 argument")

  private def builtinMultiply(arguments: List[Value]): Value =
    Value.IntegerValue(arguments.map(expectInteger).product)

  private def builtinDivide(arguments: List[Value]): Value =
    requireAtLeastTwo("/", arguments) match
      case first :: rest =>
        val initial = expectInteger(first)
        Value.IntegerValue(rest.foldLeft(initial) { (acc, argument) =>
          val divisor = expectInteger(argument)
          if divisor == 0 then throw new EvalError("division by zero")
          acc / divisor
        })
      case Nil =>
        throw new EvalError("/ expects at least 2 arguments")

  private def builtinLessThan(arguments: List[Value]): Value =
    comparison(arguments, "<", _ < _)

  private def builtinGreaterThan(arguments: List[Value]): Value =
    comparison(arguments, ">", _ > _)

  private def builtinEqual(arguments: List[Value]): Value =
    comparison(arguments, "=", _ == _)

  private def builtinLessThanOrEqual(arguments: List[Value]): Value =
    comparison(arguments, "<=", _ <= _)

  private def comparison(
    arguments: List[Value],
    name: String,
    predicate: (Int, Int) => Boolean
  ): Value =
    requireAtLeastTwo(name, arguments)
    val numbers = arguments.map(expectInteger)
    Value.BooleanValue(numbers.zip(numbers.drop(1)).forall { (left, right) =>
      predicate(left, right)
    })

  private def builtinNot(arguments: List[Value]): Value =
    val argument = requireExactlyOne("not", arguments)
    Value.BooleanValue(!isTruthy(argument))
