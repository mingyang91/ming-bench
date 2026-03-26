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

  def expectString(value: Value, name: String, position: Position): String =
    value match
      case stringValue: StringLikeValue => stringValue.text
      case other =>
        SchemeFailure.raise(s"$name expected a string, got ${typeName(other)}", position)

  def expectMutableString(
    value: Value,
    name: String,
    position: Position
  ): MutableStringValue =
    value match
      case stringValue: MutableStringValue => stringValue
      case _: StringLikeValue =>
        SchemeFailure.raise(s"$name expected a mutable string", position)
      case other =>
        SchemeFailure.raise(s"$name expected a string, got ${typeName(other)}", position)

  def expectChar(value: Value, name: String, position: Position): Char =
    value match
      case CharValue(ch) => ch
      case other =>
        SchemeFailure.raise(s"$name expected a char, got ${typeName(other)}", position)

  def expectSymbol(value: Value, name: String, position: Position): String =
    value match
      case SymbolValue(symbol) => symbol
      case other =>
        SchemeFailure.raise(s"$name expected a symbol, got ${typeName(other)}", position)

  def expectPair(value: Value, name: String, position: Position): PairValue =
    value match
      case pair: PairValue => pair
      case other =>
        SchemeFailure.raise(s"$name expected a pair, got ${typeName(other)}", position)

  def expectIndex(value: Value, name: String, position: Position): Int =
    val number = expectNumber(value, name, position)
    if number < 0 || !number.isValidInt then
      SchemeFailure.raise(s"$name expected a non-negative integer index", position)

    number.toInt

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
      case _: StringLikeValue => "string"
      case SymbolValue(_)     => "symbol"
      case CharValue(_)       => "char"
      case EmptyListValue     => "list"
      case PairValue(_, _)    => "pair"
      case BuiltinValue(_, _) => "procedure"
      case ClosureValue(_, _, _, _) =>
        "procedure"
      case VoidValue =>
        "void"
      case UninitializedValue =>
        "uninitialized"
