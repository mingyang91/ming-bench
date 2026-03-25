package ming

private[ming] object NumericBuiltins extends BuiltinSupport:

  val entries: Map[String, Value] = Map(
    "+"              -> Value.Builtin("+", add),
    "-"              -> Value.Builtin("-", subtract),
    "*"              -> Value.Builtin("*", multiply),
    "/"              -> Value.Builtin("/", divide),
    "abs"            -> Value.Builtin("abs", abs),
    "modulo"         -> Value.Builtin("modulo", modulo),
    "remainder"      -> Value.Builtin("remainder", remainder),
    "quotient"       -> Value.Builtin("quotient", quotient),
    "min"            -> Value.Builtin("min", min),
    "max"            -> Value.Builtin("max", max),
    "expt"           -> Value.Builtin("expt", expt),
    "exact->inexact" -> Value.Builtin("exact->inexact", exactToInexact),
    "inexact->exact" -> Value.Builtin("inexact->exact", inexactToExact),
    "numerator"      -> Value.Builtin("numerator", numerator),
    "denominator"    -> Value.Builtin("denominator", denominator),
    "<"              -> comparison("<", (left, right) => SchemeNumber.compare(left, right) < 0),
    ">"              -> comparison(">", (left, right) => SchemeNumber.compare(left, right) > 0),
    "="              -> comparison("=", SchemeNumber.equal),
    "<="             -> comparison("<=", (left, right) => SchemeNumber.compare(left, right) <= 0),
    "zero?"          -> numberPredicate("zero?", SchemeNumber.isZero),
    "positive?" -> numberPredicate("positive?", number => SchemeNumber.compare(number, SchemeNumber.integer(0)) > 0),
    "negative?" -> numberPredicate("negative?", number => SchemeNumber.compare(number, SchemeNumber.integer(0)) < 0),
    "odd?"      -> integerPredicate("odd?", _ % 2 != 0),
    "even?"     -> integerPredicate("even?", _ % 2 == 0)
  )

  private def add(args: List[Value], pos: SourcePos): Value =
    SchemeNumber.toValue(asNumericNumbers(args, pos, "+").foldLeft(SchemeNumber.integer(0))(SchemeNumber.add))

  private def subtract(args: List[Value], pos: SourcePos): Value =
    val numbers = asNumericNumbers(args, pos, "-")
    val result =
      numbers match
        case Nil =>
          throw EvalError.at(pos, "- expects at least 1 argument")

        case value :: Nil =>
          SchemeNumber.negate(value)

        case value :: rest =>
          rest.foldLeft(value)(SchemeNumber.subtract)

    SchemeNumber.toValue(result)

  private def multiply(args: List[Value], pos: SourcePos): Value =
    SchemeNumber.toValue(asNumericNumbers(args, pos, "*").foldLeft(SchemeNumber.integer(1))(SchemeNumber.multiply))

  private def divide(args: List[Value], pos: SourcePos): Value =
    val numbers = asNumericNumbers(args, pos, "/")
    val result =
      numbers match
        case first :: second :: rest =>
          try (second :: rest).foldLeft(first)(SchemeNumber.divide)
          catch
            case _: ArithmeticException =>
              throw EvalError.at(pos, "division by zero")

        case _ =>
          throw EvalError.at(pos, "/ expects at least 2 arguments")

    SchemeNumber.toValue(result)

  private def abs(args: List[Value], pos: SourcePos): Value =
    SchemeNumber.toValue(SchemeNumber.abs(expectNumeric(expectSingleArg(args, pos, "abs"), pos, "abs")))

  private def quotient(args: List[Value], pos: SourcePos): Value =
    val (dividend, divisor) = expectTwoNumbers(args, pos, "quotient")
    Value.IntVal(truncatedQuotient(dividend, divisor, pos, "quotient"))

  private def remainder(args: List[Value], pos: SourcePos): Value =
    val (dividend, divisor) = expectTwoNumbers(args, pos, "remainder")
    Value.IntVal(truncatedRemainder(dividend, divisor, pos, "remainder"))

  private def modulo(args: List[Value], pos: SourcePos): Value =
    val (dividend, divisor) = expectTwoNumbers(args, pos, "modulo")
    val rawRemainder        = truncatedRemainder(dividend, divisor, pos, "modulo")
    val adjustedRemainder =
      if rawRemainder != 0 && rawRemainder.signum != divisor.signum then rawRemainder + divisor
      else rawRemainder

    Value.IntVal(adjustedRemainder)

  private def min(args: List[Value], pos: SourcePos): Value =
    val numbers = asNumericNumbers(args, pos, "min")
    numbers match
      case Nil =>
        throw EvalError.at(pos, "min expects at least 1 argument")

      case first :: rest =>
        val result = rest.foldLeft(first) { (currentMin, candidate) =>
          if SchemeNumber.compare(candidate, currentMin) < 0 then candidate else currentMin
        }

        SchemeNumber.toValue(normalizeExtrema(result, numbers))

  private def max(args: List[Value], pos: SourcePos): Value =
    val numbers = asNumericNumbers(args, pos, "max")
    numbers match
      case Nil =>
        throw EvalError.at(pos, "max expects at least 1 argument")

      case first :: rest =>
        val result = rest.foldLeft(first) { (currentMax, candidate) =>
          if SchemeNumber.compare(candidate, currentMax) > 0 then candidate else currentMax
        }

        SchemeNumber.toValue(normalizeExtrema(result, numbers))

  private def expt(args: List[Value], pos: SourcePos): Value =
    val (base, exponent) = expectTwoNumbers(args, pos, "expt")
    if exponent < 0 then throw EvalError.at(pos, "expt expects a non-negative exponent")

    Value.IntVal(integerPower(base, exponent))

  private def exactToInexact(args: List[Value], pos: SourcePos): Value =
    SchemeNumber.toValue(
      SchemeNumber.exactToInexact(expectNumeric(expectSingleArg(args, pos, "exact->inexact"), pos, "exact->inexact"))
    )

  private def inexactToExact(args: List[Value], pos: SourcePos): Value =
    try
      SchemeNumber.toValue(
        SchemeNumber.inexactToExact(expectNumeric(expectSingleArg(args, pos, "inexact->exact"), pos, "inexact->exact"))
      )
    catch
      case _: ArithmeticException =>
        throw EvalError.at(pos, "inexact->exact cannot convert a non-finite number")

  private def numerator(args: List[Value], pos: SourcePos): Value =
    expectNumeric(expectSingleArg(args, pos, "numerator"), pos, "numerator") match
      case SchemeNumber.Exact(numerator, _) =>
        Value.IntVal(numerator)

      case SchemeNumber.Inexact(_) =>
        throw EvalError.at(pos, "numerator expects an exact rational")

  private def denominator(args: List[Value], pos: SourcePos): Value =
    expectNumeric(expectSingleArg(args, pos, "denominator"), pos, "denominator") match
      case SchemeNumber.Exact(_, denominator) =>
        Value.IntVal(denominator)

      case SchemeNumber.Inexact(_) =>
        throw EvalError.at(pos, "denominator expects an exact rational")

  private def comparison(name: String, relation: (SchemeNumber, SchemeNumber) => Boolean): Value =
    Value.Builtin(
      name,
      (args, pos) =>
        val numbers = asNumericNumbers(args, pos, name)
        if numbers.lengthCompare(2) < 0 then throw EvalError.at(pos, s"$name expects at least 2 arguments")

        Value.BoolVal(numbers.zip(numbers.tail).forall(relation.tupled))
    )

  private def numberPredicate(name: String, test: SchemeNumber => Boolean): Value =
    Value.Builtin(
      name,
      (args, pos) => Value.BoolVal(test(expectNumeric(expectSingleArg(args, pos, name), pos, name)))
    )

  private def integerPredicate(name: String, test: BigInt => Boolean): Value =
    Value.Builtin(
      name,
      (args, pos) => Value.BoolVal(test(expectNumber(expectSingleArg(args, pos, name), pos, name)))
    )

  private def truncatedQuotient(dividend: BigInt, divisor: BigInt, pos: SourcePos, name: String): BigInt =
    if divisor == 0 then throw EvalError.at(pos, s"$name division by zero")

    val quotient = dividend.abs / divisor.abs
    if dividend.signum * divisor.signum < 0 then -quotient else quotient

  private def truncatedRemainder(dividend: BigInt, divisor: BigInt, pos: SourcePos, name: String): BigInt =
    dividend - (truncatedQuotient(dividend, divisor, pos, name) * divisor)

  private def integerPower(base: BigInt, exponent: BigInt): BigInt =
    def loop(currentBase: BigInt, currentExponent: BigInt, acc: BigInt): BigInt =
      if currentExponent == 0 then acc
      else if currentExponent % 2 != 0 then loop(currentBase * currentBase, currentExponent / 2, acc * currentBase)
      else loop(currentBase * currentBase, currentExponent / 2, acc)

    loop(base, exponent, 1)

  private def normalizeExtrema(result: SchemeNumber, inputs: List[SchemeNumber]): SchemeNumber =
    if inputs.exists(_.isInexact) then SchemeNumber.inexact(result.toDouble)
    else result
