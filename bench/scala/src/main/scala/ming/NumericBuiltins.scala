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
      absBuiltin,
      moduloBuiltin,
      remainderBuiltin,
      quotientBuiltin,
      minBuiltin,
      maxBuiltin,
      exptBuiltin,
      zeroBuiltin,
      positiveBuiltin,
      negativeBuiltin,
      oddBuiltin,
      evenBuiltin,
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

  private val absBuiltin: Value.Builtin =
    Value.Builtin(
      "abs",
      (args, pos) => Value.Number(asNumber(singleArg("abs", args, pos), "abs", pos).abs)
    )

  private val moduloBuiltin: Value.Builtin =
    Value.Builtin(
      "modulo",
      (args, pos) =>
        val (leftValue, rightValue) = twoArgs("modulo", args, pos)
        val left                    = asNumber(leftValue, "modulo", pos)
        val right                   = asNumber(rightValue, "modulo", pos)
        if right == 0 then fail(pos, "modulo division by zero")
        val remainder = left % right
        val adjusted =
          if remainder != 0 && remainder.signum != right.signum then remainder + right
          else remainder
        Value.Number(adjusted)
    )

  private val remainderBuiltin: Value.Builtin =
    Value.Builtin(
      "remainder",
      (args, pos) =>
        val (leftValue, rightValue) = twoArgs("remainder", args, pos)
        val left                    = asNumber(leftValue, "remainder", pos)
        val right                   = asNumber(rightValue, "remainder", pos)
        if right == 0 then fail(pos, "remainder division by zero")
        Value.Number(left % right)
    )

  private val quotientBuiltin: Value.Builtin =
    Value.Builtin(
      "quotient",
      (args, pos) =>
        val (leftValue, rightValue) = twoArgs("quotient", args, pos)
        val left                    = asNumber(leftValue, "quotient", pos)
        val right                   = asNumber(rightValue, "quotient", pos)
        if right == 0 then fail(pos, "quotient division by zero")
        Value.Number(left / right)
    )

  private val minBuiltin: Value.Builtin =
    Value.Builtin(
      "min",
      (args, pos) => Value.Number(numbersAtLeast("min", args, expected = 1, pos).min)
    )

  private val maxBuiltin: Value.Builtin =
    Value.Builtin(
      "max",
      (args, pos) => Value.Number(numbersAtLeast("max", args, expected = 1, pos).max)
    )

  private val exptBuiltin: Value.Builtin =
    Value.Builtin(
      "expt",
      (args, pos) =>
        val (baseValue, exponentValue) = twoArgs("expt", args, pos)
        val base                       = asNumber(baseValue, "expt", pos)
        val exponent                   = asNumber(exponentValue, "expt", pos)
        if exponent < 0 then fail(pos, s"expt expected non-negative exponent, got $exponent")
        Value.Number(pow(base, exponent))
    )

  private val zeroBuiltin: Value.Builtin =
    numericPredicateBuiltin("zero?")(_ == 0)

  private val positiveBuiltin: Value.Builtin =
    numericPredicateBuiltin("positive?")(_ > 0)

  private val negativeBuiltin: Value.Builtin =
    numericPredicateBuiltin("negative?")(_ < 0)

  private val oddBuiltin: Value.Builtin =
    numericPredicateBuiltin("odd?")(_ % 2 != 0)

  private val evenBuiltin: Value.Builtin =
    numericPredicateBuiltin("even?")(_ % 2 == 0)

  private def numericPredicateBuiltin(
    name: String
  )(predicate: BigInt => Boolean): Value.Builtin =
    Value.Builtin(
      name,
      (args, pos) => Value.Bool(predicate(asNumber(singleArg(name, args, pos), name, pos)))
    )

  private def pow(base: BigInt, exponent: BigInt): BigInt =
    var result = BigInt(1)
    var factor = base
    var power  = exponent

    while power > 0 do
      if (power & 1) == 1 then result *= factor
      factor *= factor
      power /= 2

    result
