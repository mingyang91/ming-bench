package ming

import scala.annotation.tailrec

final case class EvaluatedArg(value: Value, position: SourcePos)

object BuiltinProcedure:

  private val builtinNames = Set(
    "+",
    "-",
    "*",
    "/",
    "<",
    ">",
    "=",
    "<=",
    "not",
    "cons",
    "car",
    "cdr",
    "null?",
    "list",
    "length",
    "string?",
    "number?",
    "boolean?",
    "pair?",
    "symbol?"
  )

  def resolve(name: String): Option[Value] =
    Option.when(builtinNames.contains(name))(Value.Builtin(name))

  def apply(name: String, arguments: List[EvaluatedArg], position: SourcePos): Value = name match
    case "+" =>
      Value.Number(expectNumbers(arguments).foldLeft(BigInt(0))(_ + _))
    case "*" =>
      Value.Number(expectNumbers(arguments).foldLeft(BigInt(1))(_ * _))
    case "-" =>
      Value.Number(evaluateSub(arguments, position))
    case "/" =>
      Value.Number(evaluateDiv(arguments, position))
    case "<" =>
      Value.Bool(compare(arguments, position)(_ < _))
    case ">" =>
      Value.Bool(compare(arguments, position)(_ > _))
    case "=" =>
      Value.Bool(compare(arguments, position)(_ == _))
    case "<=" =>
      Value.Bool(compare(arguments, position)(_ <= _))
    case "not" =>
      evaluateNot(arguments, position)
    case "cons" =>
      val pairArgs = expectExactArity(arguments, 2, "cons", position)
      Value.Pair(pairArgs.head.value, pairArgs(1).value)
    case "car" =>
      val argument = expectSingleArg(arguments, "car", position)
      expectPair(argument)._1
    case "cdr" =>
      val argument = expectSingleArg(arguments, "cdr", position)
      expectPair(argument)._2
    case "null?" =>
      Value.Bool(expectSingleArg(arguments, "null?", position).value == Value.EmptyList)
    case "list" =>
      listFrom(arguments.map(_.value))
    case "length" =>
      Value.Number(properListLength(expectSingleArg(arguments, "length", position)))
    case "string?" =>
      evaluatePredicate(arguments, "string?", position):
        case Value.Str(_) => true
        case _            => false
    case "number?" =>
      evaluatePredicate(arguments, "number?", position):
        case Value.Number(_) => true
        case _               => false
    case "boolean?" =>
      evaluatePredicate(arguments, "boolean?", position):
        case Value.Bool(_) => true
        case _             => false
    case "pair?" =>
      evaluatePredicate(arguments, "pair?", position):
        case Value.Pair(_, _) => true
        case _                => false
    case "symbol?" =>
      evaluatePredicate(arguments, "symbol?", position):
        case Value.Symbol(_) => true
        case _               => false
    case _ =>
      throw EvalError.at(position, s"unknown operator '$name'")

  private def evaluatePredicate(arguments: List[EvaluatedArg], name: String, position: SourcePos)(
    predicate: Value => Boolean
  ): Value =
    Value.Bool(predicate(expectSingleArg(arguments, name, position).value))

  private def expectSingleArg(
    arguments: List[EvaluatedArg],
    name: String,
    position: SourcePos
  ): EvaluatedArg =
    expectExactArity(arguments, 1, name, position).head

  private def expectExactArity(
    arguments: List[EvaluatedArg],
    expected: Int,
    name: String,
    position: SourcePos
  ): List[EvaluatedArg] =
    if arguments.length == expected then arguments
    else throw EvalError.at(position, s"'$name' expects exactly $expected arguments")

  private def expectPair(argument: EvaluatedArg): (Value, Value) =
    argument.value match
      case Value.Pair(car, cdr) => (car, cdr)
      case _                    => throw EvalError.at(argument.position, "expected pair")

  private def properListLength(argument: EvaluatedArg): BigInt =
    @tailrec
    def loop(current: Value, acc: BigInt): BigInt = current match
      case Value.EmptyList =>
        acc
      case Value.Pair(_, cdr) =>
        loop(cdr, acc + 1)
      case _ =>
        throw EvalError.at(argument.position, "expected proper list")

    loop(argument.value, BigInt(0))

  private def listFrom(values: List[Value]): Value =
    values.foldRight(Value.EmptyList: Value)(Value.Pair(_, _))

  private def expectNumbers(arguments: List[EvaluatedArg]): List[BigInt] =
    arguments.map(expectNumber)

  private def evaluateSub(arguments: List[EvaluatedArg], position: SourcePos): BigInt =
    expectNumbers(arguments) match
      case Nil =>
        throw EvalError.at(position, "'-' expects at least 1 argument")
      case head :: Nil =>
        -head
      case head :: tail =>
        tail.foldLeft(head)(_ - _)

  private def evaluateDiv(arguments: List[EvaluatedArg], position: SourcePos): BigInt =
    expectNumbers(arguments) match
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

  private def compare(arguments: List[EvaluatedArg], position: SourcePos)(
    predicate: (BigInt, BigInt) => Boolean
  ): Boolean =
    expectNumbers(arguments) match
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

  private def evaluateNot(arguments: List[EvaluatedArg], position: SourcePos): Value =
    arguments match
      case argument :: Nil =>
        Value.Bool(!argument.value.isTruthy)
      case _ =>
        throw EvalError.at(position, "'not' expects exactly 1 argument")

  private def expectNumber(argument: EvaluatedArg): BigInt =
    argument.value match
      case Value.Number(number) => number
      case _                    => throw EvalError.at(argument.position, "expected number")
