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
      args =>
        Value.Number(args.foldLeft(BigInt(0)) { (acc, value) =>
          acc + asNumber(value, "+")
        })
    )

  private val subtractionBuiltin: Value.Builtin =
    Value.Builtin(
      "-",
      args =>
        numbersAtLeast("-", args, expected = 1) match
          case value :: Nil  => Value.Number(-value)
          case value :: rest => Value.Number(rest.foldLeft(value)(_ - _))
          case Nil           => unreachable()
    )

  private val multiplicationBuiltin: Value.Builtin =
    Value.Builtin(
      "*",
      args =>
        Value.Number(args.foldLeft(BigInt(1)) { (acc, value) =>
          acc * asNumber(value, "*")
        })
    )

  private val divisionBuiltin: Value.Builtin =
    Value.Builtin(
      "/",
      args =>
        numbersAtLeast("/", args, expected = 2) match
          case value :: rest => Value.Number(rest.foldLeft(value)(divide(_, _, "/")))
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
    Value.Builtin(name, args => Value.Bool(compareAdjacent(name, args)(predicate)))

  private def logicalBuiltins: List[Value.Builtin] =
    List(notBuiltin)

  private val notBuiltin: Value.Builtin =
    Value.Builtin(
      "not",
      args => Value.Bool(!isTruthy(singleArg("not", args)))
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
      args =>
        val (head, tail) = twoArgs("cons", args)
        Value.ListValue(head :: asList(tail, "cons"))
    )

  private val carBuiltin: Value.Builtin =
    Value.Builtin(
      "car",
      args =>
        val (head, _) = nonEmptyListArg("car", args)
        head
    )

  private val cdrBuiltin: Value.Builtin =
    Value.Builtin(
      "cdr",
      args =>
        val (_, tail) = nonEmptyListArg("cdr", args)
        Value.ListValue(tail)
    )

  private val nullBuiltin: Value.Builtin =
    Value.Builtin(
      "null?",
      args =>
        val value = singleArg("null?", args)
        Value.Bool(value == Value.ListValue(Nil))
    )

  private val listBuiltin: Value.Builtin =
    Value.Builtin("list", args => Value.ListValue(args))

  private val lengthBuiltin: Value.Builtin =
    Value.Builtin(
      "length",
      args =>
        val items = asList(singleArg("length", args), "length")
        Value.Number(BigInt(items.length))
    )

  private val appendBuiltin: Value.Builtin =
    Value.Builtin(
      "append",
      args => Value.ListValue(args.flatMap(asList(_, "append")))
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
      args => Value.Bool(predicate(singleArg(name, args)))
    )

  private def compareAdjacent(
    name: String,
    args: List[Value]
  )(predicate: (BigInt, BigInt) => Boolean): Boolean =
    val numbers = numbersAtLeast(name, args, expected = 2)
    numbers.zip(numbers.tail).forall { case (left, right) =>
      predicate(left, right)
    }

  private def numbersAtLeast(
    name: String,
    args: List[Value],
    expected: Int
  ): List[BigInt] =
    requireAtLeast(name, args, expected)
    args.map(asNumber(_, name))

  private def singleArg(name: String, args: List[Value]): Value =
    requireExactly(name, args, expected = 1)
    args match
      case value :: Nil => value
      case _            => unreachable()

  private def twoArgs(name: String, args: List[Value]): (Value, Value) =
    requireExactly(name, args, expected = 2)
    args match
      case left :: right :: Nil => (left, right)
      case _                    => unreachable()

  private def nonEmptyListArg(name: String, args: List[Value]): (Value, List[Value]) =
    asList(singleArg(name, args), name) match
      case head :: tail => (head, tail)
      case Nil          => throw new EvalError(s"$name expected non-empty list")

  private def asNumber(value: Value, context: String): BigInt =
    value match
      case Value.Number(number) => number
      case other =>
        throw new EvalError(
          s"$context expected number, got ${SchemeInterpreter.render(other)}"
        )

  private def asList(value: Value, context: String): List[Value] =
    value match
      case Value.ListValue(items) => items
      case other =>
        throw new EvalError(
          s"$context expected list, got ${SchemeInterpreter.render(other)}"
        )

  private def divide(left: BigInt, right: BigInt, context: String): BigInt =
    if right == 0 then throw new EvalError(s"$context division by zero")
    left / right

  private def requireExactly(name: String, args: List[Value], expected: Int): Unit =
    if args.length != expected then throw new EvalError(s"$name expected $expected arguments, got ${args.length}")

  private def requireAtLeast(name: String, args: List[Value], expected: Int): Unit =
    if args.length < expected then
      throw new EvalError(s"$name expected at least $expected arguments, got ${args.length}")

  private def isTruthy(value: Value): Boolean =
    value match
      case Value.Bool(false) => false
      case _                 => true

  private def unreachable(): Nothing =
    throw IllegalStateException("unreachable")
