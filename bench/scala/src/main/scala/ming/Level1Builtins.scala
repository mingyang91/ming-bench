package ming

private[ming] object Level1Builtins:
  import RuntimeSupport.*

  val values: Map[String, Value] = Map(
    "+"              -> BuiltinValue("+", add),
    "-"              -> BuiltinValue("-", subtract),
    "*"              -> BuiltinValue("*", multiply),
    "/"              -> BuiltinValue("/", divide),
    "<"              -> BuiltinValue("<", compareNumbers("<")(_ < _)),
    ">"              -> BuiltinValue(">", compareNumbers(">")(_ > _)),
    "="              -> BuiltinValue("=", compareNumbers("=")(_ == _)),
    "<="             -> BuiltinValue("<=", compareNumbers("<=")(_ <= _)),
    "not"            -> BuiltinValue("not", logicalNot),
    "cons"           -> BuiltinValue("cons", cons),
    "car"            -> BuiltinValue("car", car),
    "cdr"            -> BuiltinValue("cdr", cdr),
    "apply"          -> BuiltinValue("apply", applyProcedure),
    "null?"          -> BuiltinValue("null?", unaryPredicate("null?")(_ == EmptyListValue)),
    "list"           -> BuiltinValue("list", list),
    "length"         -> BuiltinValue("length", length),
    "append"         -> BuiltinValue("append", append),
    "display"        -> BuiltinValue("display", display),
    "write"          -> BuiltinValue("write", write),
    "newline"        -> BuiltinValue("newline", newline),
    "string-copy"    -> BuiltinValue("string-copy", stringCopy),
    "string-set!"    -> BuiltinValue("string-set!", stringSet),
    "string-append"  -> BuiltinValue("string-append", stringAppend),
    "string-length"  -> BuiltinValue("string-length", stringLength),
    "substring"      -> BuiltinValue("substring", substring),
    "string->number" -> BuiltinValue("string->number", stringToNumber),
    "number->string" -> BuiltinValue("number->string", numberToString),
    "symbol->string" -> BuiltinValue("symbol->string", symbolToString),
    "string->symbol" -> BuiltinValue("string->symbol", stringToSymbol),
    "string-ref"     -> BuiltinValue("string-ref", stringRef),
    "string?"        -> BuiltinValue("string?", unaryPredicate("string?")(isString)),
    "number?"        -> BuiltinValue("number?", unaryPredicate("number?")(isNumber)),
    "boolean?"       -> BuiltinValue("boolean?", unaryPredicate("boolean?")(isBoolean)),
    "pair?"          -> BuiltinValue("pair?", unaryPredicate("pair?")(isPair)),
    "symbol?"        -> BuiltinValue("symbol?", unaryPredicate("symbol?")(isSymbol)),
    "char?"          -> BuiltinValue("char?", unaryPredicate("char?")(isChar))
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

  private def applyProcedure(arguments: List[Value], position: Position): Value =
    expectAtLeast(arguments, 2, "apply", position) match
      case function :: rest =>
        val prefixArguments = rest.dropRight(1)
        val listArguments   = expectProperList(rest.last, "apply", position)
        InterpreterEvaluator.applyFunction(function, prefixArguments ++ listArguments, position)
      case _ =>
        throw new IllegalStateException("validated apply argument list")

  private def list(arguments: List[Value], position: Position): Value =
    buildList(arguments)

  private def length(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "length", position)
    IntValue(expectProperList(value, "length", position).length)

  private def append(arguments: List[Value], position: Position): Value =
    val elements = arguments.flatMap(argument => expectProperList(argument, "append", position))
    buildList(elements)

  private def display(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "display", position)
    OutputCapture.append(value.renderDisplay)
    VoidValue

  private def write(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "write", position)
    OutputCapture.append(value.render)
    VoidValue

  private def newline(arguments: List[Value], position: Position): Value =
    expectExact(arguments, 0, "newline", position)
    OutputCapture.append("\n")
    VoidValue

  private def stringCopy(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "string-copy", position)
    MutableStringValue(expectString(value, "string-copy", position))

  private def stringSet(arguments: List[Value], position: Position): Value =
    expectExact(arguments, 3, "string-set!", position) match
      case stringValue :: indexValue :: charValue :: Nil =>
        val mutableString = expectMutableString(stringValue, "string-set!", position)
        val index         = expectIndex(indexValue, "string-set!", position)
        val char          = expectChar(charValue, "string-set!", position)
        if index >= mutableString.text.length then SchemeFailure.raise("string-set! index out of bounds", position)

        mutableString.update(index, char)
        VoidValue
      case _ =>
        throw new IllegalStateException("validated three-argument list")

  private def stringAppend(arguments: List[Value], position: Position): Value =
    StringValue(arguments.map(expectString(_, "string-append", position)).mkString)

  private def stringLength(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "string-length", position)
    IntValue(expectString(value, "string-length", position).length)

  private def substring(arguments: List[Value], position: Position): Value =
    expectExact(arguments, 3, "substring", position) match
      case stringValue :: startValue :: endValue :: Nil =>
        val text  = expectString(stringValue, "substring", position)
        val start = expectIndex(startValue, "substring", position)
        val end   = expectIndex(endValue, "substring", position)
        if start > end || end > text.length then SchemeFailure.raise("substring indices out of bounds", position)

        StringValue(text.substring(start, end))
      case _ =>
        throw new IllegalStateException("validated three-argument list")

  private def stringToNumber(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "string->number", position)
    val text  = expectString(value, "string->number", position)
    if text.matches("[+-]?\\d+") then IntValue(BigInt(text))
    else BoolValue(false)

  private def numberToString(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "number->string", position)
    StringValue(expectNumber(value, "number->string", position).toString)

  private def symbolToString(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "symbol->string", position)
    StringValue(expectSymbol(value, "symbol->string", position))

  private def stringToSymbol(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "string->symbol", position)
    SymbolValue(expectString(value, "string->symbol", position))

  private def stringRef(arguments: List[Value], position: Position): Value =
    expectExact(arguments, 2, "string-ref", position) match
      case stringValue :: indexValue :: Nil =>
        val text  = expectString(stringValue, "string-ref", position)
        val index = expectIndex(indexValue, "string-ref", position)
        if index >= text.length then SchemeFailure.raise("string-ref index out of bounds", position)

        CharValue(text.charAt(index))
      case _ =>
        throw new IllegalStateException("validated two-argument list")

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
      case _: StringLikeValue => true
      case _                  => false

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

  private def isChar(value: Value): Boolean =
    value match
      case CharValue(_) => true
      case _            => false
