package ming

import scala.annotation.tailrec

object Interpreter:

  def evalProgram(input: String): SchemeValue =
    evalSequence(Parser.parseProgram(input), None)

  def evalProgramWithOutput(input: String): (SchemeValue, String) =
    (evalProgram(input), "")

  @tailrec
  private def evalSequence(expressions: List[Expr], last: Option[SchemeValue]): SchemeValue =
    expressions match
      case Nil =>
        last.getOrElse(throw EvalError.syntax("expected at least one expression"))
      case expression :: rest =>
        evalSequence(rest, Some(eval(expression)))

  private def eval(expression: Expr): SchemeValue = expression match
    case Expr.IntegerLiteral(value) => SchemeValue.IntegerValue(value)
    case Expr.BooleanLiteral(value) => SchemeValue.BooleanValue(value)
    case Expr.StringLiteral(value)  => SchemeValue.StringValue(value)
    case Expr.Symbol(name)          => throw EvalError.unboundSymbol(name)
    case Expr.ListExpr(items)       => evalList(items)

  private def evalList(items: List[Expr]): SchemeValue = items match
    case Nil                           => throw EvalError.syntax("cannot evaluate an empty list")
    case Expr.Symbol("and") :: args    => evalAnd(args, SchemeValue.BooleanValue(true))
    case Expr.Symbol("or") :: args     => evalOr(args)
    case Expr.Symbol("not") :: args    => evalNot(args)
    case Expr.Symbol(operator) :: args => applyBuiltin(operator, args)
    case _                             => throw EvalError.notAProcedure()

  @tailrec
  private def evalAnd(expressions: List[Expr], last: SchemeValue): SchemeValue =
    expressions match
      case Nil => last
      case expression :: rest =>
        val value = eval(expression)
        if SchemeValue.truthy(value) then evalAnd(rest, value)
        else value

  @tailrec
  private def evalOr(expressions: List[Expr]): SchemeValue =
    expressions match
      case Nil => SchemeValue.BooleanValue(false)
      case expression :: rest =>
        val value = eval(expression)
        if SchemeValue.truthy(value) then value
        else evalOr(rest)

  private def evalNot(expressions: List[Expr]): SchemeValue = expressions match
    case expression :: Nil =>
      SchemeValue.BooleanValue(!SchemeValue.truthy(eval(expression)))
    case _ =>
      throw EvalError.wrongArgCount("not", "exactly 1", expressions.length)

  private def applyBuiltin(name: String, arguments: List[Expr]): SchemeValue = name match
    case "+"  => evalAdd(arguments)
    case "-"  => evalSubtract(arguments)
    case "*"  => evalMultiply(arguments)
    case "/"  => evalDivide(arguments)
    case "<"  => evalComparison("<", arguments, _ < _)
    case ">"  => evalComparison(">", arguments, _ > _)
    case "="  => evalComparison("=", arguments, _ == _)
    case "<=" => evalComparison("<=", arguments, _ <= _)
    case _    => throw EvalError.unboundSymbol(name)

  private def evalAdd(arguments: List[Expr]): SchemeValue =
    SchemeValue.IntegerValue(evalIntegerArguments("+", arguments).foldLeft(BigInt(0))(_ + _))

  private def evalMultiply(arguments: List[Expr]): SchemeValue =
    SchemeValue.IntegerValue(evalIntegerArguments("*", arguments).foldLeft(BigInt(1))(_ * _))

  private def evalSubtract(arguments: List[Expr]): SchemeValue =
    evalIntegerArguments("-", arguments) match
      case Nil =>
        throw EvalError.wrongArgCount("-", "at least 1", 0)
      case value :: Nil =>
        SchemeValue.IntegerValue(-value)
      case first :: rest =>
        SchemeValue.IntegerValue(rest.foldLeft(first)(_ - _))

  private def evalDivide(arguments: List[Expr]): SchemeValue =
    evalIntegerArguments("/", arguments) match
      case Nil | _ :: Nil =>
        throw EvalError.wrongArgCount("/", "at least 2", arguments.length)
      case first :: rest =>
        SchemeValue.IntegerValue(rest.foldLeft(first)(divideStep))

  private def evalComparison(
    name: String,
    arguments: List[Expr],
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

  private def evalIntegerArguments(name: String, arguments: List[Expr]): List[BigInt] =
    arguments.map(argument => expectInteger(eval(argument), name))

  private def expectInteger(value: SchemeValue, name: String): BigInt = value match
    case SchemeValue.IntegerValue(number) => number
    case _                                => throw EvalError.typeMismatch(s"integer for $name", value)

  private def divideStep(left: BigInt, right: BigInt): BigInt =
    if right == 0 then throw EvalError.divisionByZero()
    else left / right
