package ming

import scala.annotation.tailrec

object SchemeInterpreter:

  def evaluateProgram(input: String): Value =
    evaluateAll(SchemeReader.readAll(input))

  private def evaluateAll(expressions: List[Expr]): Value = expressions match
    case Nil          => throw EvalError("expected at least one expression")
    case head :: tail => evaluateRest(evaluate(head), tail)

  @tailrec
  private def evaluateRest(current: Value, remaining: List[Expr]): Value =
    remaining match
      case Nil          => current
      case head :: tail => evaluateRest(evaluate(head), tail)

  private def evaluate(expr: Expr): Value = expr match
    case Expr.Literal(value, _) =>
      value
    case Expr.Symbol(name, position) =>
      throw EvalError.at(position, s"unbound symbol '$name'")
    case Expr.ListExpr(Nil, position) =>
      throw EvalError.at(position, "cannot evaluate empty list")
    case Expr.ListExpr(operator :: arguments, _) =>
      operator match
        case Expr.Symbol(name, position) =>
          evaluateCall(name, arguments, position)
        case _ =>
          throw EvalError.at(operator.sourcePos, "operator must be a symbol")

  private def evaluateCall(name: String, arguments: List[Expr], position: SourcePos): Value =
    name match
      case "+"  => Value.Number(evaluateNumbers(arguments).foldLeft(BigInt(0))(_ + _))
      case "*"  => Value.Number(evaluateNumbers(arguments).foldLeft(BigInt(1))(_ * _))
      case "-"  => Value.Number(evaluateSub(arguments, position))
      case "/"  => Value.Number(evaluateDiv(arguments, position))
      case "<"  => Value.Bool(compare(arguments, position)(_ < _))
      case ">"  => Value.Bool(compare(arguments, position)(_ > _))
      case "="  => Value.Bool(compare(arguments, position)(_ == _))
      case "<=" => Value.Bool(compare(arguments, position)(_ <= _))
      case "not" =>
        evaluateNot(arguments, position)
      case "and" =>
        evaluateAnd(arguments)
      case "or" =>
        evaluateOr(arguments)
      case _ =>
        throw EvalError.at(position, s"unknown operator '$name'")

  private def evaluateNumbers(arguments: List[Expr]): List[BigInt] =
    arguments.map(argument => expectNumber(evaluate(argument), argument.sourcePos))

  private def evaluateSub(arguments: List[Expr], position: SourcePos): BigInt =
    evaluateNumbers(arguments) match
      case Nil =>
        throw EvalError.at(position, "'-' expects at least 1 argument")
      case head :: Nil =>
        -head
      case head :: tail =>
        tail.foldLeft(head)(_ - _)

  private def evaluateDiv(arguments: List[Expr], position: SourcePos): BigInt =
    evaluateNumbers(arguments) match
      case _ :: Nil | Nil =>
        throw EvalError.at(position, "'/' expects at least 2 arguments")
      case head :: tail =>
        divide(head, tail, position)

  @tailrec
  private def divide(current: BigInt, remaining: List[BigInt], position: SourcePos): BigInt =
    remaining match
      case Nil =>
        current
      case head :: _ if head == 0 =>
        throw EvalError.at(position, "division by zero")
      case head :: tail =>
        divide(current / head, tail, position)

  private def compare(arguments: List[Expr], position: SourcePos)(predicate: (BigInt, BigInt) => Boolean): Boolean =
    evaluateNumbers(arguments) match
      case left :: right :: rest =>
        compareChain(right, rest, left, predicate)
      case _ =>
        throw EvalError.at(position, "comparison expects at least 2 arguments")

  @tailrec
  private def compareChain(
    current: BigInt,
    remaining: List[BigInt],
    previous: BigInt,
    predicate: (BigInt, BigInt) => Boolean
  ): Boolean =
    if !predicate(previous, current) then false
    else
      remaining match
        case Nil          => true
        case head :: tail => compareChain(head, tail, current, predicate)

  private def evaluateNot(arguments: List[Expr], position: SourcePos): Value =
    arguments match
      case argument :: Nil =>
        Value.Bool(!evaluate(argument).isTruthy)
      case _ =>
        throw EvalError.at(position, "'not' expects exactly 1 argument")

  private def evaluateAnd(arguments: List[Expr]): Value =
    arguments match
      case Nil =>
        Value.Bool(true)
      case argument :: Nil =>
        evaluate(argument)
      case argument :: rest =>
        val value = evaluate(argument)
        if value.isTruthy then evaluateAnd(rest) else value

  private def evaluateOr(arguments: List[Expr]): Value =
    arguments match
      case Nil =>
        Value.Bool(false)
      case argument :: Nil =>
        evaluate(argument)
      case argument :: rest =>
        val value = evaluate(argument)
        if value.isTruthy then value else evaluateOr(rest)

  private def expectNumber(value: Value, position: SourcePos): BigInt =
    value match
      case Value.Number(number) => number
      case _                    => throw EvalError.at(position, "expected number")
