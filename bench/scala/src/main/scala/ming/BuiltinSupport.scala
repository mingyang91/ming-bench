package ming

private[ming] object BuiltinSupport:

  def requireArgCount[T](name: String, args: List[T], expected: Int, pos: SourcePos): List[T] =
    if args.lengthCompare(expected) != 0 then
      throw EvalError.at(pos, s"$name expects $expected argument(s), got ${args.length}")
    args

  def requireMinArgs[T](name: String, args: List[T], min: Int, pos: SourcePos): List[T] =
    if args.lengthCompare(min) < 0 then
      throw EvalError.at(pos, s"$name expects at least $min argument(s), got ${args.length}")
    args

  def requireSingleArg(name: String, args: List[Value], pos: SourcePos): Value =
    requireArgCount(name, args, expected = 1, pos).head

  def requireNumber(name: String, value: Value, pos: SourcePos): SchemeNumber =
    SchemeNumber
      .fromValue(value)
      .getOrElse(throw EvalError.at(pos, s"$name expected a number, got ${ValueSemantics.typeName(value)}"))

  def requireExactInteger(name: String, value: Value, pos: SourcePos): Long =
    requireNumber(name, value, pos) match
      case SchemeNumber.ExactInt(number) => number
      case _                             => throw EvalError.at(pos, s"$name expected an exact integer")

  def requireIndex(name: String, value: Value, pos: SourcePos): Int =
    val index = requireExactInteger(name, value, pos)
    if !index.isValidInt then throw EvalError.at(pos, s"$name index out of range")
    index.toInt

  def requireChar(name: String, value: Value, pos: SourcePos): Char =
    value match
      case Value.CharVal(ch) => ch
      case other =>
        throw EvalError.at(pos, s"$name expected a character, got ${ValueSemantics.typeName(other)}")

  def requireString(name: String, value: Value, pos: SourcePos): MutableString =
    value match
      case Value.StringVal(text) => text
      case other =>
        throw EvalError.at(pos, s"$name expected a string, got ${ValueSemantics.typeName(other)}")

  def requireSymbol(name: String, value: Value, pos: SourcePos): String =
    value match
      case Value.SymbolVal(symbol) => symbol
      case other =>
        throw EvalError.at(pos, s"$name expected a symbol, got ${ValueSemantics.typeName(other)}")

  def requirePair(name: String, value: Value, pos: SourcePos): (Value, Value) =
    value match
      case Value.PairVal(car, cdr) => (car, cdr)
      case other =>
        throw EvalError.at(pos, s"$name expected a pair, got ${ValueSemantics.typeName(other)}")

  def requireVector(name: String, value: Value, pos: SourcePos): VectorInstance =
    value match
      case Value.VectorVal(instance) => instance
      case other =>
        throw EvalError.at(pos, s"$name expected a vector, got ${ValueSemantics.typeName(other)}")

  def requireBinaryNumbers(name: String, args: List[Value], pos: SourcePos): (SchemeNumber, SchemeNumber) =
    val values = requireArgCount(name, args, expected = 2, pos)
    (requireNumber(name, values.head, pos), requireNumber(name, values(1), pos))

  def evalNumbers(name: String, args: List[Value], pos: SourcePos): List[SchemeNumber] =
    args.map(arg => requireNumber(name, arg, pos))

  def unaryPredicate(name: String, args: List[Value], pos: SourcePos)(predicate: Value => Boolean): Value =
    Value.BoolVal(predicate(requireSingleArg(name, args, pos)))

  def compareNumbers(
    name: String,
    args: List[Value],
    pos: SourcePos
  )(predicate: (SchemeNumber, SchemeNumber) => Boolean): Value =
    compareAdjacent(requireMinArgs(name, evalNumbers(name, args, pos), min = 2, pos))(predicate)

  def compareChars(
    name: String,
    args: List[Value],
    pos: SourcePos
  )(predicate: (Char, Char) => Boolean): Value =
    compareAdjacent(requireMinArgs(name, args, min = 2, pos).map(arg => requireChar(name, arg, pos)))(predicate)

  def compareStrings(
    name: String,
    args: List[Value],
    pos: SourcePos
  )(predicate: (String, String) => Boolean): Value =
    compareAdjacent(requireMinArgs(name, args, min = 2, pos).map(arg => requireString(name, arg, pos).text))(predicate)

  def unknownProcedure(name: String, pos: SourcePos): Nothing =
    throw EvalError.at(pos, s"unknown procedure: $name")

  private def compareAdjacent[T](values: List[T])(predicate: (T, T) => Boolean): Value =
    Value.BoolVal(values.zip(values.tail).forall(predicate.tupled))
