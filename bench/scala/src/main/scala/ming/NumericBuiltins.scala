package ming

private[ming] object NumericBuiltins:

  import BuiltinSupport.*
  import SchemeInterpreter.Value

  def all: List[Value.Builtin] =
    List(
      additionBuiltin,
      subtractionBuiltin,
      multiplicationBuiltin,
      divisionBuiltin,
      comparisonBuiltin("<")(_ < _),
      comparisonBuiltin(">")(_ > _),
      comparisonBuiltin("=")(_ == _),
      comparisonBuiltin("<=")(_ <= _)
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
          case value :: rest =>
            Value.Number(rest.foldLeft(value)(divide(_, _, "/", pos)))
          case Nil =>
            unreachable()
    )

  private def comparisonBuiltin(
    name: String
  )(predicate: (BigInt, BigInt) => Boolean): Value.Builtin =
    Value.Builtin(
      name,
      (args, pos) => Value.Bool(compareAdjacent(name, args, pos)(predicate))
    )
