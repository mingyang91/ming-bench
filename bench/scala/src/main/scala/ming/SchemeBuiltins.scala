package ming

private[ming] object SchemeBuiltins:
  import SchemeInterpreter.Value

  def all: List[Value.Builtin] =
    List.concat(
      arithmeticBuiltins,
      comparisonBuiltins,
      logicalBuiltins,
      listBuiltins,
      predicateBuiltins
    )

  private def arithmeticBuiltins: List[Value.Builtin] =
    List(
      additionBuiltin,
      subtractionBuiltin,
      multiplicationBuiltin,
      divisionBuiltin
    )

  private val additionBuiltin: Value.Builtin =
    Value.Builtin(
      "+",
      (args, pos) =>
        Value.Number(args.foldLeft(BigInt(0)) { (acc, value) =>
          acc + asNumber(value, "+", pos)
        })
    )

  private val subtractionBuiltin: Value.Builtin =
    Value.Builtin(
      "-",
      (args, pos) =>
        numbersAtLeast("-", args, expected = 1, pos) match
          case value :: Nil  => Value.Number(-value)
          case value :: rest => Value.Number(rest.foldLeft(value)(_ - _))
          case Nil           => unreachable()
    )

  private val multiplicationBuiltin: Value.Builtin =
    Value.Builtin(
      "*",
      (args, pos) =>
        Value.Number(args.foldLeft(BigInt(1)) { (acc, value) =>
          acc * asNumber(value, "*", pos)
        })
    )

  private val divisionBuiltin: Value.Builtin =
    Value.Builtin(
      "/",
      (args, pos) =>
        numbersAtLeast("/", args, expected = 2, pos) match
          case value :: rest => Value.Number(rest.foldLeft(value)(divide(_, _, "/", pos)))
          case Nil           => unreachable()
    )

  private def comparisonBuiltins: List[Value.Builtin] =
    List(
      comparisonBuiltin("<")(_ < _),
      comparisonBuiltin(">")(_ > _),
      comparisonBuiltin("=")(_ == _),
      comparisonBuiltin("<=")(_ <= _)
    )

  private def comparisonBuiltin(
    name: String
  )(predicate: (BigInt, BigInt) => Boolean): Value.Builtin =
    Value.Builtin(name, (args, pos) => Value.Bool(compareAdjacent(name, args, pos)(predicate)))

  private def logicalBuiltins: List[Value.Builtin] =
    List(notBuiltin)

  private val notBuiltin: Value.Builtin =
    Value.Builtin(
      "not",
      (args, pos) => Value.Bool(!isTruthy(singleArg("not", args, pos)))
    )

  private def listBuiltins: List[Value.Builtin] =
    List(
      consBuiltin,
      carBuiltin,
      cdrBuiltin,
      nullBuiltin,
      listBuiltin,
      lengthBuiltin,
      appendBuiltin
    )

  private val consBuiltin: Value.Builtin =
    Value.Builtin(
      "cons",
      (args, pos) =>
        val (head, tail) = twoArgs("cons", args, pos)
        Value.ListValue(head :: asList(tail, "cons", pos))
    )

  private val carBuiltin: Value.Builtin =
    Value.Builtin(
      "car",
      (args, pos) =>
        val (head, _) = nonEmptyListArg("car", args, pos)
        head
    )

  private val cdrBuiltin: Value.Builtin =
    Value.Builtin(
      "cdr",
      (args, pos) =>
        val (_, tail) = nonEmptyListArg("cdr", args, pos)
        Value.ListValue(tail)
    )

  private val nullBuiltin: Value.Builtin =
    Value.Builtin(
      "null?",
      (args, pos) =>
        val value = singleArg("null?", args, pos)
        Value.Bool(value == Value.ListValue(Nil))
    )

  private val listBuiltin: Value.Builtin =
    Value.Builtin("list", (args, _) => Value.ListValue(args))

  private val lengthBuiltin: Value.Builtin =
    Value.Builtin(
      "length",
      (args, pos) =>
        val items = asList(singleArg("length", args, pos), "length", pos)
        Value.Number(BigInt(items.length))
    )

  private val appendBuiltin: Value.Builtin =
    Value.Builtin(
      "append",
      (args, pos) => Value.ListValue(args.flatMap(asList(_, "append", pos)))
    )

  private def predicateBuiltins: List[Value.Builtin] =
    List(
      predicateBuiltin("string?") {
        case Value.StringLit(_) => true
        case _                  => false
      },
      predicateBuiltin("number?") {
        case Value.Number(_) => true
        case _               => false
      },
      predicateBuiltin("boolean?") {
        case Value.Bool(_) => true
        case _             => false
      },
      predicateBuiltin("pair?") {
        case Value.ListValue(_ :: _) => true
        case _                       => false
      },
      predicateBuiltin("symbol?") {
        case Value.Symbol(_) => true
        case _               => false
      }
    )

  private def predicateBuiltin(
    name: String
  )(predicate: Value => Boolean): Value.Builtin =
    Value.Builtin(
      name,
      (args, pos) => Value.Bool(predicate(singleArg(name, args, pos)))
    )

  private def compareAdjacent(
    name: String,
    args: List[Value],
    pos: SourcePos
  )(predicate: (BigInt, BigInt) => Boolean): Boolean =
    val numbers = numbersAtLeast(name, args, expected = 2, pos)
    numbers.zip(numbers.tail).forall { case (left, right) =>
      predicate(left, right)
    }

  private def numbersAtLeast(
    name: String,
    args: List[Value],
    expected: Int,
    pos: SourcePos
  ): List[BigInt] =
    requireAtLeast(name, args, expected, pos)
    args.map(asNumber(_, name, pos))

  private def singleArg(name: String, args: List[Value], pos: SourcePos): Value =
    requireExactly(name, args, expected = 1, pos)
    args match
      case value :: Nil => value
      case _            => unreachable()

  private def twoArgs(name: String, args: List[Value], pos: SourcePos): (Value, Value) =
    requireExactly(name, args, expected = 2, pos)
    args match
      case left :: right :: Nil => (left, right)
      case _                    => unreachable()

  private def nonEmptyListArg(
    name: String,
    args: List[Value],
    pos: SourcePos
  ): (Value, List[Value]) =
    asList(singleArg(name, args, pos), name, pos) match
      case head :: tail => (head, tail)
      case Nil          => fail(pos, s"$name expected non-empty list")

  private def asNumber(value: Value, context: String, pos: SourcePos): BigInt =
    value match
      case Value.Number(number) => number
      case other =>
        fail(
          pos,
          s"$context expected number, got ${SchemeInterpreter.render(other)}"
        )

  private def asList(value: Value, context: String, pos: SourcePos): List[Value] =
    value match
      case Value.ListValue(items) => items
      case other =>
        fail(
          pos,
          s"$context expected list, got ${SchemeInterpreter.render(other)}"
        )

  private def divide(left: BigInt, right: BigInt, context: String, pos: SourcePos): BigInt =
    if right == 0 then fail(pos, s"$context division by zero")
    left / right

  private def requireExactly(
    name: String,
    args: List[Value],
    expected: Int,
    pos: SourcePos
  ): Unit =
    if args.length != expected then fail(pos, s"$name expected $expected arguments, got ${args.length}")

  private def requireAtLeast(
    name: String,
    args: List[Value],
    expected: Int,
    pos: SourcePos
  ): Unit =
    if args.length < expected then fail(pos, s"$name expected at least $expected arguments, got ${args.length}")

  private def isTruthy(value: Value): Boolean =
    value match
      case Value.Bool(false) => false
      case _                 => true

  private def fail(pos: SourcePos, message: String): Nothing =
    throw EvalError.at(pos, message)

  private def unreachable(): Nothing =
    throw IllegalStateException("unreachable")
