package ming

private[ming] object NumericBuiltins extends BuiltinSupport:

  val entries: Map[String, Value] = Map(
    "+"         -> Value.Builtin("+", add),
    "-"         -> Value.Builtin("-", subtract),
    "*"         -> Value.Builtin("*", multiply),
    "/"         -> Value.Builtin("/", divide),
    "abs"       -> Value.Builtin("abs", abs),
    "modulo"    -> Value.Builtin("modulo", modulo),
    "remainder" -> Value.Builtin("remainder", remainder),
    "quotient"  -> Value.Builtin("quotient", quotient),
    "min"       -> Value.Builtin("min", min),
    "max"       -> Value.Builtin("max", max),
    "expt"      -> Value.Builtin("expt", expt),
    "<"         -> comparison("<", _ < _),
    ">"         -> comparison(">", _ > _),
    "="         -> comparison("=", _ == _),
    "<="        -> comparison("<=", _ <= _),
    "zero?"     -> numberPredicate("zero?", _ == 0),
    "positive?" -> numberPredicate("positive?", _ > 0),
    "negative?" -> numberPredicate("negative?", _ < 0),
    "odd?"      -> numberPredicate("odd?", _ % 2 != 0),
    "even?"     -> numberPredicate("even?", _ % 2 == 0)
  )

  private def add(args: List[Value], pos: SourcePos): Value =
    Value.IntVal(asNumbers(args, pos, "+").foldLeft(BigInt(0))(_ + _))

  private def subtract(args: List[Value], pos: SourcePos): Value =
    val numbers = asNumbers(args, pos, "-")
    val result =
      numbers match
        case Nil =>
          throw EvalError.at(pos, "- expects at least 1 argument")

        case value :: Nil =>
          -value

        case value :: rest =>
          rest.foldLeft(value)(_ - _)

    Value.IntVal(result)

  private def multiply(args: List[Value], pos: SourcePos): Value =
    Value.IntVal(asNumbers(args, pos, "*").foldLeft(BigInt(1))(_ * _))

  private def divide(args: List[Value], pos: SourcePos): Value =
    val numbers = asNumbers(args, pos, "/")
    val result =
      numbers match
        case first :: second :: rest =>
          (second :: rest).foldLeft(first) { (left, right) =>
            if right == 0 then throw EvalError.at(pos, "division by zero")

            val (quotient, remainder) = left /% right
            if remainder != 0 then throw EvalError.at(pos, "/ expects an integer result")

            quotient
          }

        case _ =>
          throw EvalError.at(pos, "/ expects at least 2 arguments")

    Value.IntVal(result)

  private def abs(args: List[Value], pos: SourcePos): Value =
    Value.IntVal(expectNumber(expectSingleArg(args, pos, "abs"), pos, "abs").abs)

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
    val numbers = asNumbers(args, pos, "min")
    numbers match
      case Nil =>
        throw EvalError.at(pos, "min expects at least 1 argument")

      case _ =>
        Value.IntVal(numbers.min)

  private def max(args: List[Value], pos: SourcePos): Value =
    val numbers = asNumbers(args, pos, "max")
    numbers match
      case Nil =>
        throw EvalError.at(pos, "max expects at least 1 argument")

      case _ =>
        Value.IntVal(numbers.max)

  private def expt(args: List[Value], pos: SourcePos): Value =
    val (base, exponent) = expectTwoNumbers(args, pos, "expt")
    if exponent < 0 then throw EvalError.at(pos, "expt expects a non-negative exponent")

    Value.IntVal(integerPower(base, exponent))

  private def comparison(name: String, relation: (BigInt, BigInt) => Boolean): Value =
    Value.Builtin(
      name,
      (args, pos) =>
        val numbers = asNumbers(args, pos, name)
        if numbers.lengthCompare(2) < 0 then throw EvalError.at(pos, s"$name expects at least 2 arguments")

        Value.BoolVal(numbers.zip(numbers.tail).forall(relation.tupled))
    )

  private def numberPredicate(name: String, test: BigInt => Boolean): Value =
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
