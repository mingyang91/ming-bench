package ming

private[ming] object BuiltinSupport:
  import SchemeInterpreter.Value

  def compareAdjacent(
    name: String,
    args: List[Value],
    pos: SourcePos
  )(predicate: (BigInt, BigInt) => Boolean): Boolean =
    val numbers = numbersAtLeast(name, args, expected = 2, pos)
    numbers.zip(numbers.tail).forall { case (left, right) =>
      predicate(left, right)
    }

  def numbersAtLeast(
    name: String,
    args: List[Value],
    expected: Int,
    pos: SourcePos
  ): List[BigInt] =
    requireAtLeast(name, args, expected, pos)
    args.map(asNumber(_, name, pos))

  def singleArg(name: String, args: List[Value], pos: SourcePos): Value =
    requireExactly(name, args, expected = 1, pos)
    args match
      case value :: Nil => value
      case _            => unreachable()

  def twoArgs(name: String, args: List[Value], pos: SourcePos): (Value, Value) =
    requireExactly(name, args, expected = 2, pos)
    args match
      case left :: right :: Nil => (left, right)
      case _                    => unreachable()

  def threeArgs(name: String, args: List[Value], pos: SourcePos): (Value, Value, Value) =
    requireExactly(name, args, expected = 3, pos)
    args match
      case first :: second :: third :: Nil => (first, second, third)
      case _                               => unreachable()

  def nonEmptyListArg(
    name: String,
    args: List[Value],
    pos: SourcePos
  ): (Value, List[Value]) =
    asList(singleArg(name, args, pos), name, pos) match
      case head :: tail => (head, tail)
      case Nil          => fail(pos, s"$name expected non-empty list")

  def asNumber(value: Value, context: String, pos: SourcePos): BigInt =
    value match
      case Value.Number(number) => number
      case other =>
        fail(
          pos,
          s"$context expected number, got ${SchemeInterpreter.render(other)}"
        )

  def asString(value: Value, context: String, pos: SourcePos): String =
    value match
      case Value.StringLit(text) => text
      case other =>
        fail(
          pos,
          s"$context expected string, got ${SchemeInterpreter.render(other)}"
        )

  def asSymbol(value: Value, context: String, pos: SourcePos): String =
    value match
      case Value.Symbol(name) => name
      case other =>
        fail(
          pos,
          s"$context expected symbol, got ${SchemeInterpreter.render(other)}"
        )

  def asList(value: Value, context: String, pos: SourcePos): List[Value] =
    value match
      case Value.ListValue(items) => items
      case other =>
        fail(
          pos,
          s"$context expected list, got ${SchemeInterpreter.render(other)}"
        )

  def asIndex(value: Value, context: String, pos: SourcePos): Int =
    val number = asNumber(value, context, pos)
    if number < 0 || !number.isValidInt then fail(pos, s"$context expected non-negative integer index, got $number")
    number.toInt

  def divide(left: BigInt, right: BigInt, context: String, pos: SourcePos): BigInt =
    if right == 0 then fail(pos, s"$context division by zero")
    left / right

  def requireExactly(
    name: String,
    args: List[Value],
    expected: Int,
    pos: SourcePos
  ): Unit =
    if args.length != expected then fail(pos, s"$name expected $expected arguments, got ${args.length}")

  def requireAtLeast(
    name: String,
    args: List[Value],
    expected: Int,
    pos: SourcePos
  ): Unit =
    if args.length < expected then fail(pos, s"$name expected at least $expected arguments, got ${args.length}")

  def isTruthy(value: Value): Boolean =
    value match
      case Value.Bool(false) => false
      case _                 => true

  def fail(pos: SourcePos, message: String): Nothing =
    throw EvalError.at(pos, message)

  def unreachable(): Nothing =
    throw IllegalStateException("unreachable")
