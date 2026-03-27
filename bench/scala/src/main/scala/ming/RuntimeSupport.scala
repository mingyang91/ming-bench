package ming

import scala.collection.mutable
import scala.annotation.tailrec

private[ming] object RuntimeSupport:

  def buildList(values: List[Value]): Value =
    values.foldRight[Value](EmptyListValue)(PairValue(_, _))

  def unpackValues(value: Value): List[Value] =
    value match
      case MultipleValuesValue(values) => values
      case singleValue                 => List(singleValue)

  def expectSingleValue(value: Value, context: String, position: Position): Value =
    value match
      case MultipleValuesValue(values) =>
        SchemeFailure.raise(s"$context expected a single value, got ${values.length} value(s)", position)
      case singleValue =>
        singleValue

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

  def expectNumber(value: Value, name: String, position: Position): NumberValue =
    value match
      case number: NumberValue => number
      case other =>
        SchemeFailure.raise(s"$name expected a number, got ${typeName(other)}", position)

  def expectProcedure(value: Value, name: String, position: Position): ProcedureValue =
    value match
      case procedure: ProcedureValue => procedure
      case other =>
        SchemeFailure.raise(s"$name expected a procedure, got ${typeName(other)}", position)

  def expectInteger(value: Value, name: String, position: Position): BigInt =
    expectNumber(value, name, position) match
      case IntValue(number) =>
        number
      case _ =>
        SchemeFailure.raise(s"$name expected an exact integer", position)

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

  def expectVector(value: Value, name: String, position: Position): VectorValue =
    value match
      case vectorValue: VectorValue => vectorValue
      case other =>
        SchemeFailure.raise(s"$name expected a vector, got ${typeName(other)}", position)

  def expectSyntaxObject(
    value: Value,
    name: String,
    position: Position
  ): SyntaxObjectValue =
    value match
      case syntaxObject: SyntaxObjectValue => syntaxObject
      case other =>
        SchemeFailure.raise(s"$name expected a syntax object, got ${typeName(other)}", position)

  def expectIndex(value: Value, name: String, position: Position): Int =
    val number = expectInteger(value, name, position)
    if number < 0 || !number.isValidInt then
      SchemeFailure.raise(s"$name expected a non-negative integer index", position)

    number.toInt

  def expectProperList(
    value: Value,
    name: String,
    position: Position,
    reversed: List[Value] = Nil
  ): List[Value] =
    val elements = List.newBuilder[Value]
    reversed.reverseIterator.foreach(elements += _)
    val visited = mutable.HashSet.empty[PairValue]

    var current: Value = value
    while true do
      current match
        case EmptyListValue =>
          return elements.result()
        case pair: PairValue =>
          if visited.contains(pair) then
            SchemeFailure.raise(s"$name expected a proper list, got circular list", position)

          visited += pair
          elements += pair.car
          current = pair.cdr
        case other =>
          SchemeFailure.raise(s"$name expected a proper list, got ${typeName(other)}", position)

    throw new IllegalStateException("unreachable proper list traversal")

  def isTruthy(value: Value): Boolean =
    value match
      case BoolValue(false) => false
      case _                => true

  def typeName(value: Value): String =
    value match
      case _: NumberValue     => "number"
      case BoolValue(_)       => "boolean"
      case _: StringLikeValue => "string"
      case SymbolValue(_)     => "symbol"
      case CharValue(_)       => "char"
      case EmptyListValue     => "list"
      case PairValue(_, _)    => "pair"
      case _: VectorValue     => "vector"
      case _: SyntaxRuntimeValue =>
        "syntax"
      case _: ProcedureValue =>
        "procedure"
      case MultipleValuesValue(_) =>
        "values"
      case _: RecordValue =>
        "record"
      case VoidValue =>
        "void"
      case UninitializedValue =>
        "uninitialized"
