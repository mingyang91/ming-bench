package ming

import scala.annotation.tailrec

object Interpreter:

  private val builtinNames = Set("+", "-", "*", "/", "<", ">", "=", "<=", "not")

  private enum Definition:
    case Variable(name: String, expression: Expr)
    case Function(name: String, parameters: List[Expr], body: List[Expr])

  def evalProgram(input: String): SchemeValue =
    evalSequence(Parser.parseProgram(input), Environment.empty, None)._2

  def evalProgramWithOutput(input: String): (SchemeValue, String) =
    (evalProgram(input), "")

  @tailrec
  private def evalSequence(
    expressions: List[Expr],
    environment: Environment,
    last: Option[SchemeValue]
  ): (Environment, SchemeValue) =
    expressions match
      case Nil =>
        (environment, last.getOrElse(throw EvalError.syntax("expected at least one expression")))
      case expression :: rest =>
        val (nextEnvironment, value) = evalSequenceStep(expression, environment)
        evalSequence(rest, nextEnvironment, Some(value))

  private def evalSequenceStep(expression: Expr, environment: Environment): (Environment, SchemeValue) =
    parseDefine(expression) match
      case Some(definition) => evalDefine(definition, environment)
      case None             => (environment, eval(expression, environment))

  private def eval(expression: Expr, environment: Environment): SchemeValue = expression match
    case Expr.IntegerLiteral(value) => SchemeValue.IntegerValue(value)
    case Expr.BooleanLiteral(value) => SchemeValue.BooleanValue(value)
    case Expr.StringLiteral(value)  => SchemeValue.StringValue(value)
    case Expr.Symbol(name)          => resolveSymbol(name, environment)
    case Expr.ListExpr(items)       => evalList(items, environment)

  private def resolveSymbol(name: String, environment: Environment): SchemeValue =
    environment
      .lookup(name)
      .orElse(builtinValue(name))
      .getOrElse(throw EvalError.unboundSymbol(name))

  private def builtinValue(name: String): Option[SchemeValue] =
    if builtinNames.contains(name) then Some(SchemeValue.ProcedureValue(SchemeProcedure.Builtin(name)))
    else None

  private def evalList(items: List[Expr], environment: Environment): SchemeValue = items match
    case Nil                        => throw EvalError.syntax("cannot evaluate an empty list")
    case Expr.Symbol("and") :: args => evalAnd(args, environment, SchemeValue.BooleanValue(true))
    case Expr.Symbol("or") :: args  => evalOr(args, environment)
    case Expr.Symbol("if") :: args  => evalIf(args, environment)
    case Expr.Symbol("quote") :: args =>
      evalQuote(args)
    case Expr.Symbol("lambda") :: args =>
      evalLambda(args, environment)
    case head :: args =>
      applyProcedure(eval(head, environment), args.map(argument => eval(argument, environment)))

  @tailrec
  private def evalAnd(
    expressions: List[Expr],
    environment: Environment,
    last: SchemeValue
  ): SchemeValue =
    expressions match
      case Nil => last
      case expression :: rest =>
        val value = eval(expression, environment)
        if SchemeValue.truthy(value) then evalAnd(rest, environment, value)
        else value

  @tailrec
  private def evalOr(expressions: List[Expr], environment: Environment): SchemeValue =
    expressions match
      case Nil => SchemeValue.BooleanValue(false)
      case expression :: rest =>
        val value = eval(expression, environment)
        if SchemeValue.truthy(value) then value
        else evalOr(rest, environment)

  private def evalIf(arguments: List[Expr], environment: Environment): SchemeValue = arguments match
    case condition :: whenTrue :: whenFalse :: Nil =>
      if SchemeValue.truthy(eval(condition, environment)) then eval(whenTrue, environment)
      else eval(whenFalse, environment)
    case _ =>
      throw EvalError.wrongArgCount("if", "exactly 3", arguments.length)

  private def evalQuote(arguments: List[Expr]): SchemeValue = arguments match
    case expression :: Nil => quoteValue(expression)
    case _                 => throw EvalError.wrongArgCount("quote", "exactly 1", arguments.length)

  private def quoteValue(expression: Expr): SchemeValue = expression match
    case Expr.IntegerLiteral(value) => SchemeValue.IntegerValue(value)
    case Expr.BooleanLiteral(value) => SchemeValue.BooleanValue(value)
    case Expr.StringLiteral(value)  => SchemeValue.StringValue(value)
    case Expr.Symbol(name)          => SchemeValue.SymbolValue(name)
    case Expr.ListExpr(items)       => SchemeValue.list(items.map(quoteValue))

  private def evalLambda(arguments: List[Expr], environment: Environment): SchemeValue = arguments match
    case parameterList :: body if body.nonEmpty =>
      SchemeValue.ProcedureValue(
        SchemeProcedure.Lambda(None, parseParameterNames(parameterList), body, environment)
      )
    case _ =>
      throw EvalError.syntax("lambda requires a parameter list and at least one body expression")

  private def parseParameterNames(expression: Expr): List[String] = expression match
    case Expr.ListExpr(parameters) =>
      val names = parameters.map {
        case Expr.Symbol(name) => name
        case _                 => throw EvalError.syntax("lambda parameters must be symbols")
      }
      if names.distinct == names then names
      else throw EvalError.syntax("lambda parameters must be unique")
    case _ =>
      throw EvalError.syntax("lambda parameters must be a list")

  private def parseDefine(expression: Expr): Option[Definition] = expression match
    case Expr.ListExpr(Expr.Symbol("define") :: rest) =>
      rest match
        case Expr.Symbol(name) :: value :: Nil =>
          Some(Definition.Variable(name, value))
        case Expr.ListExpr(Expr.Symbol(name) :: parameters) :: body if body.nonEmpty =>
          Some(Definition.Function(name, parameters, body))
        case _ =>
          throw EvalError.syntax("invalid define form")
    case _ => None

  private def evalDefine(definition: Definition, environment: Environment): (Environment, SchemeValue) =
    definition match
      case Definition.Variable(name, expression) if isLambdaExpression(expression) =>
        evalRecursiveDefinition(name, environment) { recursiveEnvironment =>
          attachName(name, eval(expression, recursiveEnvironment))
        }
      case Definition.Variable(name, expression) =>
        (
          environment.extend(name, eval(expression, environment)),
          SchemeValue.VoidValue
        )
      case Definition.Function(name, parameters, body) =>
        val parameterNames = parseParameterNames(Expr.ListExpr(parameters))
        evalRecursiveDefinition(name, environment) { recursiveEnvironment =>
          SchemeValue.ProcedureValue(
            SchemeProcedure.Lambda(Some(name), parameterNames, body, recursiveEnvironment)
          )
        }

  private def evalRecursiveDefinition(
    name: String,
    environment: Environment
  )(buildValue: Environment => SchemeValue): (Environment, SchemeValue) =
    lazy val recursiveEnvironment: Environment =
      environment.defineRecursive(name, buildValue(recursiveEnvironment))
    (recursiveEnvironment, SchemeValue.VoidValue)

  private def isLambdaExpression(expression: Expr): Boolean = expression match
    case Expr.ListExpr(Expr.Symbol("lambda") :: _) => true
    case _                                         => false

  private def attachName(name: String, value: SchemeValue): SchemeValue = value match
    case SchemeValue.ProcedureValue(procedure) =>
      SchemeValue.ProcedureValue(SchemeProcedure.named(procedure, name))
    case _ =>
      value

  private def applyProcedure(procedure: SchemeValue, arguments: List[SchemeValue]): SchemeValue =
    procedure match
      case SchemeValue.ProcedureValue(callable) =>
        applyCallable(callable, arguments)
      case _ =>
        throw EvalError.notAProcedure()

  private def applyCallable(procedure: SchemeProcedure, arguments: List[SchemeValue]): SchemeValue =
    procedure match
      case SchemeProcedure.Builtin(name) =>
        applyBuiltin(name, arguments)
      case SchemeProcedure.Lambda(_, parameters, body, closureEnvironment) =>
        if parameters.length != arguments.length then
          throw EvalError.wrongArgCount(procedure.displayName, parameters.length.toString, arguments.length)
        val localEnvironment = closureEnvironment.extendMany(parameters.zip(arguments))
        evalSequence(body, localEnvironment, None)._2

  private def applyBuiltin(name: String, arguments: List[SchemeValue]): SchemeValue = name match
    case "+"   => evalAdd(arguments)
    case "-"   => evalSubtract(arguments)
    case "*"   => evalMultiply(arguments)
    case "/"   => evalDivide(arguments)
    case "<"   => evalComparison("<", arguments, _ < _)
    case ">"   => evalComparison(">", arguments, _ > _)
    case "="   => evalComparison("=", arguments, _ == _)
    case "<="  => evalComparison("<=", arguments, _ <= _)
    case "not" => evalNot(arguments)
    case _     => throw EvalError.unboundSymbol(name)

  private def evalAdd(arguments: List[SchemeValue]): SchemeValue =
    SchemeValue.IntegerValue(evalIntegerArguments("+", arguments).foldLeft(BigInt(0))(_ + _))

  private def evalMultiply(arguments: List[SchemeValue]): SchemeValue =
    SchemeValue.IntegerValue(evalIntegerArguments("*", arguments).foldLeft(BigInt(1))(_ * _))

  private def evalSubtract(arguments: List[SchemeValue]): SchemeValue =
    evalIntegerArguments("-", arguments) match
      case Nil =>
        throw EvalError.wrongArgCount("-", "at least 1", 0)
      case value :: Nil =>
        SchemeValue.IntegerValue(-value)
      case first :: rest =>
        SchemeValue.IntegerValue(rest.foldLeft(first)(_ - _))

  private def evalDivide(arguments: List[SchemeValue]): SchemeValue =
    evalIntegerArguments("/", arguments) match
      case Nil | _ :: Nil =>
        throw EvalError.wrongArgCount("/", "at least 2", arguments.length)
      case first :: rest =>
        SchemeValue.IntegerValue(rest.foldLeft(first)(divideStep))

  private def evalComparison(
    name: String,
    arguments: List[SchemeValue],
    predicate: (BigInt, BigInt) => Boolean
  ): SchemeValue =
    evalIntegerArguments(name, arguments) match
      case Nil =>
        throw EvalError.wrongArgCount(name, "at least 2", 0)
      case _ :: Nil =>
        throw EvalError.wrongArgCount(name, "at least 2", 1)
      case first :: rest =>
        SchemeValue.BooleanValue(compareChain(first, rest, predicate))

  @tailrec
  private def compareChain(
    previous: BigInt,
    remaining: List[BigInt],
    predicate: (BigInt, BigInt) => Boolean
  ): Boolean =
    remaining match
      case Nil => true
      case current :: rest =>
        if predicate(previous, current) then compareChain(current, rest, predicate)
        else false

  private def evalIntegerArguments(name: String, arguments: List[SchemeValue]): List[BigInt] =
    arguments.map(argument => expectInteger(argument, name))

  private def expectInteger(value: SchemeValue, name: String): BigInt = value match
    case SchemeValue.IntegerValue(number) => number
    case _                                => throw EvalError.typeMismatch(s"integer for $name", value)

  private def evalNot(arguments: List[SchemeValue]): SchemeValue = arguments match
    case value :: Nil =>
      SchemeValue.BooleanValue(!SchemeValue.truthy(value))
    case _ =>
      throw EvalError.wrongArgCount("not", "exactly 1", arguments.length)

  private def divideStep(left: BigInt, right: BigInt): BigInt =
    if right == 0 then throw EvalError.divisionByZero()
    else left / right
