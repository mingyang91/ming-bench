package ming

private[ming] object Level1Builtins:
  import RuntimeSupport.*

  val values: Map[String, Value] = Map(
    "+"        -> BuiltinValue("+", add),
    "-"        -> BuiltinValue("-", subtract),
    "*"        -> BuiltinValue("*", multiply),
    "/"        -> BuiltinValue("/", divide),
    "<"        -> BuiltinValue("<", compareNumbers("<")(_ < _)),
    ">"        -> BuiltinValue(">", compareNumbers(">")(_ > _)),
    "="        -> BuiltinValue("=", compareNumbers("=")(_ == _)),
    "<="       -> BuiltinValue("<=", compareNumbers("<=")(_ <= _)),
    "not"      -> BuiltinValue("not", logicalNot),
    "cons"     -> BuiltinValue("cons", cons),
    "car"      -> BuiltinValue("car", car),
    "cdr"      -> BuiltinValue("cdr", cdr),
    "null?"    -> BuiltinValue("null?", unaryPredicate("null?")(_ == EmptyListValue)),
    "list"     -> BuiltinValue("list", list),
    "length"   -> BuiltinValue("length", length),
    "append"   -> BuiltinValue("append", append),
    "string?"  -> BuiltinValue("string?", unaryPredicate("string?")(isString)),
    "number?"  -> BuiltinValue("number?", unaryPredicate("number?")(isNumber)),
    "boolean?" -> BuiltinValue("boolean?", unaryPredicate("boolean?")(isBoolean)),
    "pair?"    -> BuiltinValue("pair?", unaryPredicate("pair?")(isPair)),
    "symbol?"  -> BuiltinValue("symbol?", unaryPredicate("symbol?")(isSymbol))
  )

  private def add(arguments: List[Value], position: Position): Value =
    IntValue(numericArguments(arguments, "+", position).foldLeft(BigInt(0))(_ + _))

  private def subtract(arguments: List[Value], position: Position): Value =
    val numbers = numericArgumentsAtLeast(arguments, 1, "-", position)
    val result =
      numbers match
        case number :: Nil  => -number
        case number :: rest => rest.foldLeft(number)(_ - _)
        case Nil            => throw new IllegalStateException("validated non-empty argument list")
    IntValue(result)

  private def multiply(arguments: List[Value], position: Position): Value =
    IntValue(numericArguments(arguments, "*", position).foldLeft(BigInt(1))(_ * _))

  private def divide(arguments: List[Value], position: Position): Value =
    val numbers = numericArgumentsAtLeast(arguments, 1, "/", position)
    val result =
      numbers match
        case denominator :: Nil =>
          divideExactly(BigInt(1), denominator, position)
        case numerator :: rest =>
          rest.foldLeft(numerator): (accumulator, denominator) =>
            divideExactly(accumulator, denominator, position)
        case Nil =>
          throw new IllegalStateException("validated non-empty argument list")
    IntValue(result)

  private def divideExactly(
    numerator: BigInt,
    denominator: BigInt,
    position: Position
  ): BigInt =
    if denominator == 0 then SchemeFailure.raise("division by zero", position)

    if numerator % denominator != 0 then SchemeFailure.raise("division produced a non-integer result", position)

    numerator / denominator

  private def compareNumbers(
    name: String
  )(predicate: (BigInt, BigInt) => Boolean): (List[Value], Position) => Value =
    (arguments, position) =>
      val numbers = numericArgumentsAtLeast(arguments, 2, name, position)
      BoolValue(numbers.zip(numbers.tail).forall((left, right) => predicate(left, right)))

  private def logicalNot(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "not", position)
    BoolValue(!isTruthy(value))

  private def cons(arguments: List[Value], position: Position): Value =
    val (first, second) = expectTwoArguments(arguments, "cons", position)
    PairValue(first, second)

  private def car(arguments: List[Value], position: Position): Value =
    expectPair(expectSingleArgument(arguments, "car", position), "car", position).car

  private def cdr(arguments: List[Value], position: Position): Value =
    expectPair(expectSingleArgument(arguments, "cdr", position), "cdr", position).cdr

  private def list(arguments: List[Value], position: Position): Value =
    buildList(arguments)

  private def length(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "length", position)
    IntValue(expectProperList(value, "length", position).length)

  private def append(arguments: List[Value], position: Position): Value =
    val elements = arguments.flatMap(argument => expectProperList(argument, "append", position))
    buildList(elements)

  private def numericArguments(
    arguments: List[Value],
    name: String,
    position: Position
  ): List[BigInt] =
    arguments.map(expectNumber(_, name, position))

  private def numericArgumentsAtLeast(
    arguments: List[Value],
    minimum: Int,
    name: String,
    position: Position
  ): List[BigInt] =
    expectAtLeast(arguments, minimum, name, position).map(expectNumber(_, name, position))

  private def unaryPredicate(
    name: String
  )(predicate: Value => Boolean): (List[Value], Position) => Value =
    (arguments, position) => BoolValue(predicate(expectSingleArgument(arguments, name, position)))

  private def isString(value: Value): Boolean =
    value match
      case StringValue(_) => true
      case _              => false

  private def isNumber(value: Value): Boolean =
    value match
      case IntValue(_) => true
      case _           => false

  private def isBoolean(value: Value): Boolean =
    value match
      case BoolValue(_) => true
      case _            => false

  private def isPair(value: Value): Boolean =
    value match
      case PairValue(_, _) => true
      case _               => false

  private def isSymbol(value: Value): Boolean =
    value match
      case SymbolValue(_) => true
      case _              => false
