package ming

import scala.annotation.tailrec

private[ming] object RuntimeSupport:

  def buildList(values: List[Value]): Value =
    values.foldRight[Value](EmptyListValue)(PairValue(_, _))

  def expectExact(
    arguments: List[Value],
    expected: Int,
    name: String,
    position: Position
  ): List[Value] =
    if arguments.length != expected then
      SchemeFailure.raise(s"$name expected $expected argument(s), got ${arguments.length}", position)

    arguments

  def expectAtLeast(
    arguments: List[Value],
    minimum: Int,
    name: String,
    position: Position
  ): List[Value] =
    if arguments.length < minimum then
      SchemeFailure.raise(s"$name expected at least $minimum argument(s), got ${arguments.length}", position)

    arguments

  def expectSingleArgument(arguments: List[Value], name: String, position: Position): Value =
    expectExact(arguments, 1, name, position) match
      case argument :: Nil => argument
      case _               => throw new IllegalStateException("validated single argument list")

  def expectTwoArguments(
    arguments: List[Value],
    name: String,
    position: Position
  ): (Value, Value) =
    expectExact(arguments, 2, name, position) match
      case first :: second :: Nil => (first, second)
      case _ =>
        throw new IllegalStateException("validated two-argument list")

  def expectNumber(value: Value, name: String, position: Position): BigInt =
    value match
      case IntValue(number) => number
      case other =>
        SchemeFailure.raise(s"$name expected a number, got ${typeName(other)}", position)

  def expectPair(value: Value, name: String, position: Position): PairValue =
    value match
      case pair: PairValue => pair
      case other =>
        SchemeFailure.raise(s"$name expected a pair, got ${typeName(other)}", position)

  @tailrec
  def expectProperList(
    value: Value,
    name: String,
    position: Position,
    reversed: List[Value] = Nil
  ): List[Value] =
    value match
      case EmptyListValue =>
        reversed.reverse
      case PairValue(head, tail) =>
        expectProperList(tail, name, position, head :: reversed)
      case other =>
        SchemeFailure.raise(s"$name expected a proper list, got ${typeName(other)}", position)

  def isTruthy(value: Value): Boolean =
    value match
      case BoolValue(false) => false
      case _                => true

  def typeName(value: Value): String =
    value match
      case IntValue(_)        => "number"
      case BoolValue(_)       => "boolean"
      case StringValue(_)     => "string"
      case SymbolValue(_)     => "symbol"
      case EmptyListValue     => "list"
      case PairValue(_, _)    => "pair"
      case BuiltinValue(_, _) => "procedure"
      case ClosureValue(_, _, _, _) =>
        "procedure"
      case VoidValue =>
        "void"
      case UninitializedValue =>
        "uninitialized"
