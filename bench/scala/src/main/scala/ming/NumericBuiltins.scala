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
      gcdBuiltin,
      lcmBuiltin,
      truncateBuiltin,
      roundBuiltin,
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
      exactToInexactBuiltin,
      inexactToExactBuiltin,
      numeratorBuiltin,
      denominatorBuiltin,
      comparisonBuiltin("<")(SchemeNumber.compare(_, _) < 0),
      comparisonBuiltin(">")(SchemeNumber.compare(_, _) > 0),
      comparisonBuiltin("=")(SchemeNumber.areEqual),
      comparisonBuiltin("<=")(SchemeNumber.compare(_, _) <= 0),
      comparisonBuiltin(">=")(SchemeNumber.compare(_, _) >= 0)
    )

  private val additionBuiltin: Value.Builtin =
    Value.Builtin(
      "+",
      (args, pos) =>
        Value.Number(args.foldLeft(SchemeNumber.Zero) { (acc, value) =>
          SchemeNumber.add(acc, asNumber(value, "+", pos))
        })
    )

  private val subtractionBuiltin: Value.Builtin =
    Value.Builtin(
      "-",
      (args, pos) =>
        numbersAtLeast("-", args, expected = 1, pos) match
          case value :: Nil  => Value.Number(SchemeNumber.negate(value))
          case value :: rest => Value.Number(rest.foldLeft(value)(SchemeNumber.subtract))
          case Nil           => unreachable()
    )

  private val multiplicationBuiltin: Value.Builtin =
    Value.Builtin(
      "*",
      (args, pos) =>
        Value.Number(args.foldLeft(SchemeNumber.One) { (acc, value) =>
          SchemeNumber.multiply(acc, asNumber(value, "*", pos))
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
  )(predicate: (SchemeNumber, SchemeNumber) => Boolean): Value.Builtin =
    Value.Builtin(
      name,
      (args, pos) => Value.Bool(compareAdjacent(name, args, pos)(predicate))
    )

  private val absBuiltin: Value.Builtin =
    Value.Builtin(
      "abs",
      (args, pos) => Value.Number(SchemeNumber.abs(asNumber(singleArg("abs", args, pos), "abs", pos)))
    )

  private val gcdBuiltin: Value.Builtin =
    Value.Builtin(
      "gcd",
      (args, pos) =>
        val result = args.map(value => asExactInteger(value, "gcd", pos).abs).foldLeft(BigInt(0))(_.gcd(_))
        Value.Number(SchemeNumber.exact(result))
    )

  private val lcmBuiltin: Value.Builtin =
    Value.Builtin(
      "lcm",
      (args, pos) =>
        val numbers = args.map(value => asExactInteger(value, "lcm", pos).abs)
        val result =
          numbers.foldLeft(BigInt(1)) { (acc, value) =>
            if acc == 0 || value == 0 then BigInt(0)
            else (acc / acc.gcd(value)) * value
          }
        Value.Number(SchemeNumber.exact(result))
    )

  private val truncateBuiltin: Value.Builtin =
    Value.Builtin(
      "truncate",
      (args, pos) =>
        val number = asNumber(singleArg("truncate", args, pos), "truncate", pos)
        Value.Number(truncateNumber(number))
    )

  private val roundBuiltin: Value.Builtin =
    Value.Builtin(
      "round",
      (args, pos) =>
        val number = asNumber(singleArg("round", args, pos), "round", pos)
        Value.Number(roundNumber(number))
    )

  private val moduloBuiltin: Value.Builtin =
    Value.Builtin(
      "modulo",
      (args, pos) =>
        val (leftValue, rightValue) = twoArgs("modulo", args, pos)
        val left                    = asExactInteger(leftValue, "modulo", pos)
        val right                   = asExactInteger(rightValue, "modulo", pos)
        if right == 0 then fail(pos, "modulo division by zero")
        val remainder = left % right
        val adjusted =
          if remainder != 0 && remainder.signum != right.signum then remainder + right
          else remainder
        Value.Number(SchemeNumber.exact(adjusted))
    )

  private val remainderBuiltin: Value.Builtin =
    Value.Builtin(
      "remainder",
      (args, pos) =>
        val (leftValue, rightValue) = twoArgs("remainder", args, pos)
        val left                    = asExactInteger(leftValue, "remainder", pos)
        val right                   = asExactInteger(rightValue, "remainder", pos)
        if right == 0 then fail(pos, "remainder division by zero")
        Value.Number(SchemeNumber.exact(left % right))
    )

  private val quotientBuiltin: Value.Builtin =
    Value.Builtin(
      "quotient",
      (args, pos) =>
        val (leftValue, rightValue) = twoArgs("quotient", args, pos)
        val left                    = asExactInteger(leftValue, "quotient", pos)
        val right                   = asExactInteger(rightValue, "quotient", pos)
        if right == 0 then fail(pos, "quotient division by zero")
        Value.Number(SchemeNumber.exact(left / right))
    )

  private val minBuiltin: Value.Builtin =
    Value.Builtin(
      "min",
      (args, pos) => Value.Number(numbersAtLeast("min", args, expected = 1, pos).reduceLeft(SchemeNumber.min))
    )

  private val maxBuiltin: Value.Builtin =
    Value.Builtin(
      "max",
      (args, pos) => Value.Number(numbersAtLeast("max", args, expected = 1, pos).reduceLeft(SchemeNumber.max))
    )

  private val exptBuiltin: Value.Builtin =
    Value.Builtin(
      "expt",
      (args, pos) =>
        val (baseValue, exponentValue) = twoArgs("expt", args, pos)
        val base                       = asNumber(baseValue, "expt", pos)
        val exponent                   = asExactInteger(exponentValue, "expt", pos)
        if exponent < 0 then fail(pos, s"expt expected non-negative exponent, got $exponent")
        Value.Number(SchemeNumber.pow(base, exponent))
    )

  private val zeroBuiltin: Value.Builtin =
    numericPredicateBuiltin("zero?")(SchemeNumber.isZero)

  private val positiveBuiltin: Value.Builtin =
    numericPredicateBuiltin("positive?")(SchemeNumber.isPositive)

  private val negativeBuiltin: Value.Builtin =
    numericPredicateBuiltin("negative?")(SchemeNumber.isNegative)

  private val oddBuiltin: Value.Builtin =
    integerPredicateBuiltin("odd?")(_ % 2 != 0)

  private val evenBuiltin: Value.Builtin =
    integerPredicateBuiltin("even?")(_ % 2 == 0)

  private val exactToInexactBuiltin: Value.Builtin =
    Value.Builtin(
      "exact->inexact",
      (args, pos) =>
        val value = asNumber(singleArg("exact->inexact", args, pos), "exact->inexact", pos)
        Value.Number(SchemeNumber.toInexact(value))
    )

  private val inexactToExactBuiltin: Value.Builtin =
    Value.Builtin(
      "inexact->exact",
      (args, pos) =>
        val value = asNumber(singleArg("inexact->exact", args, pos), "inexact->exact", pos)
        Value.Number(SchemeNumber.toExact(value))
    )

  private val numeratorBuiltin: Value.Builtin =
    Value.Builtin(
      "numerator",
      (args, pos) =>
        asNumber(singleArg("numerator", args, pos), "numerator", pos) match
          case SchemeNumber.Exact(numerator, _) =>
            Value.Number(SchemeNumber.exact(numerator))
          case other =>
            fail(
              pos,
              s"numerator expected exact number, got ${SchemeInterpreter.render(Value.Number(other))}"
            )
    )

  private val denominatorBuiltin: Value.Builtin =
    Value.Builtin(
      "denominator",
      (args, pos) =>
        asNumber(singleArg("denominator", args, pos), "denominator", pos) match
          case SchemeNumber.Exact(_, denominator) =>
            Value.Number(SchemeNumber.exact(denominator))
          case other =>
            fail(
              pos,
              s"denominator expected exact number, got ${SchemeInterpreter.render(Value.Number(other))}"
            )
    )

  private def numericPredicateBuiltin(
    name: String
  )(predicate: SchemeNumber => Boolean): Value.Builtin =
    Value.Builtin(
      name,
      (args, pos) => Value.Bool(predicate(asNumber(singleArg(name, args, pos), name, pos)))
    )

  private def integerPredicateBuiltin(
    name: String
  )(predicate: BigInt => Boolean): Value.Builtin =
    Value.Builtin(
      name,
      (args, pos) => Value.Bool(predicate(asExactInteger(singleArg(name, args, pos), name, pos)))
    )

  private def truncateNumber(number: SchemeNumber): SchemeNumber =
    number match
      case SchemeNumber.Exact(numerator, denominator) =>
        SchemeNumber.exact(numerator / denominator)
      case SchemeNumber.Inexact(value) =>
        val truncated =
          if value >= 0 then math.floor(value)
          else math.ceil(value)
        SchemeNumber.inexact(truncated)

  private def roundNumber(number: SchemeNumber): SchemeNumber =
    number match
      case SchemeNumber.Exact(numerator, denominator) =>
        val quotient  = numerator / denominator
        val remainder = numerator % denominator
        val twiceAbs  = remainder.abs * 2
        val step      = BigInt(remainder.signum)

        val rounded =
          if twiceAbs < denominator then quotient
          else if twiceAbs > denominator then quotient + step
          else if (quotient.abs % 2) == 0 then quotient
          else quotient + step

        SchemeNumber.exact(rounded)
      case SchemeNumber.Inexact(value) =>
        SchemeNumber.inexact(math.rint(value))
