package ming

private[ming] object SchemeBuiltins:

  private val definitions: Map[String, List[Value] => Value] = Map(
    "+"        -> builtinAdd,
    "-"        -> builtinSubtract,
    "*"        -> builtinMultiply,
    "/"        -> builtinDivide,
    "<"        -> builtinLessThan,
    ">"        -> builtinGreaterThan,
    "="        -> builtinEqual,
    "<="       -> builtinLessThanOrEqual,
    "not"      -> builtinNot,
    "cons"     -> builtinCons,
    "car"      -> builtinCar,
    "cdr"      -> builtinCdr,
    "null?"    -> builtinNullPredicate,
    "list"     -> builtinList,
    "length"   -> builtinLength,
    "append"   -> builtinAppend,
    "string?"  -> builtinStringPredicate,
    "number?"  -> builtinNumberPredicate,
    "boolean?" -> builtinBooleanPredicate,
    "pair?"    -> builtinPairPredicate,
    "symbol?"  -> builtinSymbolPredicate
  )

  def lookup(name: String): Option[Value] =
    definitions.get(name).map(fn => Value.BuiltinValue(name, fn))

  private def expectInteger(value: Value): Int =
    value match
      case Value.IntegerValue(number) => number
      case other =>
        throw new EvalError(s"expected number, got ${ValueCodec.render(other)}")

  private def expectPair(name: String, value: Value): (Value, Value) =
    value match
      case Value.PairValue(head, tail) =>
        (head, tail)
      case other =>
        throw new EvalError(s"$name expects a pair, got ${ValueCodec.render(other)}")

  private def expectProperList(name: String, value: Value): List[Value] =
    value match
      case Value.EmptyListValue =>
        Nil
      case Value.PairValue(head, tail) =>
        head :: expectProperList(name, tail)
      case other =>
        throw new EvalError(s"$name expects a list, got ${ValueCodec.render(other)}")

  private def properList(values: List[Value]): Value =
    values.foldRight[Value](Value.EmptyListValue) { (value, tail) =>
      Value.PairValue(value, tail)
    }

  private def unaryPredicate(
    name: String,
    arguments: List[Value],
    predicate: Value => Boolean
  ): Value =
    val argument = requireExactlyOne(name, arguments)
    Value.BooleanValue(predicate(argument))

  private def requireExactlyOne(name: String, arguments: List[Value]): Value =
    arguments match
      case argument :: Nil => argument
      case _ =>
        throw new EvalError(s"$name expects exactly 1 argument")

  private def requireExactlyTwo(name: String, arguments: List[Value]): (Value, Value) =
    arguments match
      case first :: second :: Nil => (first, second)
      case _ =>
        throw new EvalError(s"$name expects exactly 2 arguments")

  private def requireAtLeastOne(name: String, arguments: List[Value]): List[Value] =
    if arguments.nonEmpty then arguments
    else throw new EvalError(s"$name expects at least 1 argument")

  private def requireAtLeastTwo(name: String, arguments: List[Value]): List[Value] =
    if arguments.lengthCompare(2) >= 0 then arguments
    else throw new EvalError(s"$name expects at least 2 arguments")

  private def builtinAdd(arguments: List[Value]): Value =
    Value.IntegerValue(arguments.map(expectInteger).sum)

  private def builtinSubtract(arguments: List[Value]): Value =
    requireAtLeastOne("-", arguments) match
      case argument :: Nil =>
        Value.IntegerValue(-expectInteger(argument))
      case first :: rest =>
        val initial = expectInteger(first)
        Value.IntegerValue(rest.foldLeft(initial) { (acc, argument) =>
          acc - expectInteger(argument)
        })
      case Nil =>
        throw new EvalError("- expects at least 1 argument")

  private def builtinMultiply(arguments: List[Value]): Value =
    Value.IntegerValue(arguments.map(expectInteger).product)

  private def builtinDivide(arguments: List[Value]): Value =
    requireAtLeastTwo("/", arguments) match
      case first :: rest =>
        val initial = expectInteger(first)
        Value.IntegerValue(rest.foldLeft(initial) { (acc, argument) =>
          val divisor = expectInteger(argument)
          if divisor == 0 then throw new EvalError("division by zero")
          acc / divisor
        })
      case Nil =>
        throw new EvalError("/ expects at least 2 arguments")

  private def builtinLessThan(arguments: List[Value]): Value =
    comparison(arguments, "<", _ < _)

  private def builtinGreaterThan(arguments: List[Value]): Value =
    comparison(arguments, ">", _ > _)

  private def builtinEqual(arguments: List[Value]): Value =
    comparison(arguments, "=", _ == _)

  private def builtinLessThanOrEqual(arguments: List[Value]): Value =
    comparison(arguments, "<=", _ <= _)

  private def comparison(
    arguments: List[Value],
    name: String,
    predicate: (Int, Int) => Boolean
  ): Value =
    requireAtLeastTwo(name, arguments)
    val numbers = arguments.map(expectInteger)
    Value.BooleanValue(numbers.zip(numbers.drop(1)).forall { (left, right) =>
      predicate(left, right)
    })

  private def builtinNot(arguments: List[Value]): Value =
    val argument = requireExactlyOne("not", arguments)
    Value.BooleanValue(!SchemeRuntime.isTruthy(argument))

  private def builtinCons(arguments: List[Value]): Value =
    requireExactlyTwo("cons", arguments) match
      case (head, tail) => Value.PairValue(head, tail)

  private def builtinCar(arguments: List[Value]): Value =
    expectPair("car", requireExactlyOne("car", arguments))._1

  private def builtinCdr(arguments: List[Value]): Value =
    expectPair("cdr", requireExactlyOne("cdr", arguments))._2

  private def builtinNullPredicate(arguments: List[Value]): Value =
    unaryPredicate("null?", arguments, _ == Value.EmptyListValue)

  private def builtinList(arguments: List[Value]): Value = properList(arguments)

  private def builtinLength(arguments: List[Value]): Value =
    Value.IntegerValue(expectProperList("length", requireExactlyOne("length", arguments)).length)

  private def builtinAppend(arguments: List[Value]): Value =
    val elements = arguments.flatMap(argument => expectProperList("append", argument))
    properList(elements)

  private def builtinStringPredicate(arguments: List[Value]): Value =
    unaryPredicate("string?", arguments, { case Value.StringValue(_) => true; case _ => false })

  private def builtinNumberPredicate(arguments: List[Value]): Value =
    unaryPredicate("number?", arguments, { case Value.IntegerValue(_) => true; case _ => false })

  private def builtinBooleanPredicate(arguments: List[Value]): Value =
    unaryPredicate("boolean?", arguments, { case Value.BooleanValue(_) => true; case _ => false })

  private def builtinPairPredicate(arguments: List[Value]): Value =
    unaryPredicate("pair?", arguments, { case Value.PairValue(_, _) => true; case _ => false })

  private def builtinSymbolPredicate(arguments: List[Value]): Value =
    unaryPredicate("symbol?", arguments, { case Value.SymbolValue(_) => true; case _ => false })
