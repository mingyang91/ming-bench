package ming

import scala.annotation.tailrec

private[ming] object Level1NumericBuiltins:

  import RuntimeSupport.*
  import NumericSupport.ExactFraction

  val values: Map[String, Value] = Map(
    "+"              -> BuiltinValue("+", add),
    "-"              -> BuiltinValue("-", subtract),
    "*"              -> BuiltinValue("*", multiply),
    "/"              -> BuiltinValue("/", divide),
    "<"              -> BuiltinValue("<", compareNumbers("<")(_ < 0)),
    ">"              -> BuiltinValue(">", compareNumbers(">")(_ > 0)),
    "="              -> BuiltinValue("=", compareNumbers("=")(_ == 0)),
    "<="             -> BuiltinValue("<=", compareNumbers("<=")(_ <= 0)),
    ">="             -> BuiltinValue(">=", compareNumbers(">=")(_ >= 0)),
    "abs"            -> BuiltinValue("abs", abs),
    "modulo"         -> BuiltinValue("modulo", modulo),
    "remainder"      -> BuiltinValue("remainder", remainder),
    "quotient"       -> BuiltinValue("quotient", quotient),
    "gcd"            -> BuiltinValue("gcd", gcd),
    "lcm"            -> BuiltinValue("lcm", lcm),
    "min"            -> BuiltinValue("min", minValue),
    "max"            -> BuiltinValue("max", maxValue),
    "expt"           -> BuiltinValue("expt", expt),
    "truncate"       -> BuiltinValue("truncate", truncateNumber),
    "round"          -> BuiltinValue("round", roundNumber),
    "exact->inexact" -> BuiltinValue("exact->inexact", exactToInexact),
    "inexact->exact" -> BuiltinValue("inexact->exact", inexactToExact),
    "numerator"      -> BuiltinValue("numerator", numerator),
    "denominator"    -> BuiltinValue("denominator", denominator),
    "zero?"          -> BuiltinValue("zero?", unaryNumberPredicate("zero?")(NumericSupport.isZero)),
    "positive?" -> BuiltinValue(
      "positive?",
      unaryNumberPredicate("positive?")(number => NumericSupport.compare(number, IntValue(0)) > 0)
    ),
    "negative?" -> BuiltinValue(
      "negative?",
      unaryNumberPredicate("negative?")(number => NumericSupport.compare(number, IntValue(0)) < 0)
    ),
    "odd?"  -> BuiltinValue("odd?", unaryIntegerPredicate("odd?")(_ % 2 != 0)),
    "even?" -> BuiltinValue("even?", unaryIntegerPredicate("even?")(_ % 2 == 0))
  )

  private def add(arguments: List[Value], position: Position): Value =
    val numbers = numericArguments(arguments, "+", position)
    if numbers.exists(NumericSupport.isInexact) then
      InexactValue(numbers.foldLeft(0.0)((sum, number) => sum + NumericSupport.toDouble(number)))
    else
      NumericSupport.fromFraction(
        numbers.foldLeft(ExactFraction(0, 1))((sum, number) => sum + NumericSupport.exactFraction(number))
      )

  private def subtract(arguments: List[Value], position: Position): Value =
    val numbers = numericArgumentsAtLeast(arguments, 1, "-", position)
    if numbers.exists(NumericSupport.isInexact) then
      val result =
        numbers match
          case number :: Nil =>
            -NumericSupport.toDouble(number)
          case number :: rest =>
            rest.foldLeft(NumericSupport.toDouble(number))((current, next) => current - NumericSupport.toDouble(next))
          case Nil =>
            throw new IllegalStateException("validated non-empty argument list")
      InexactValue(result)
    else
      val result =
        numbers match
          case number :: Nil =>
            -NumericSupport.exactFraction(number)
          case number :: rest =>
            rest.foldLeft(NumericSupport.exactFraction(number))((current, next) =>
              current - NumericSupport.exactFraction(next)
            )
          case Nil =>
            throw new IllegalStateException("validated non-empty argument list")
      NumericSupport.fromFraction(result)

  private def multiply(arguments: List[Value], position: Position): Value =
    val numbers = numericArguments(arguments, "*", position)
    if numbers.exists(NumericSupport.isInexact) then
      InexactValue(numbers.foldLeft(1.0)((product, number) => product * NumericSupport.toDouble(number)))
    else
      NumericSupport.fromFraction(
        numbers.foldLeft(ExactFraction(1, 1))((product, number) => product * NumericSupport.exactFraction(number))
      )

  private def divide(arguments: List[Value], position: Position): Value =
    val numbers = numericArgumentsAtLeast(arguments, 1, "/", position)
    if numbers.exists(NumericSupport.isInexact) then
      val result =
        numbers match
          case denominator :: Nil =>
            if NumericSupport.isZero(denominator) then SchemeFailure.raise("division by zero", position)
            1.0 / NumericSupport.toDouble(denominator)
          case numerator :: rest =>
            rest.foldLeft(NumericSupport.toDouble(numerator))((current, denominator) =>
              if NumericSupport.isZero(denominator) then SchemeFailure.raise("division by zero", position)
              current / NumericSupport.toDouble(denominator)
            )
          case Nil =>
            throw new IllegalStateException("validated non-empty argument list")
      InexactValue(result)
    else
      val result =
        numbers match
          case denominator :: Nil =>
            val divisor = NumericSupport.exactFraction(denominator)
            if divisor.numerator == 0 then SchemeFailure.raise("division by zero", position)
            ExactFraction(1, 1) / divisor
          case numerator :: rest =>
            rest.foldLeft(NumericSupport.exactFraction(numerator))((current, denominator) =>
              val divisor = NumericSupport.exactFraction(denominator)
              if divisor.numerator == 0 then SchemeFailure.raise("division by zero", position)
              current / divisor
            )
          case Nil =>
            throw new IllegalStateException("validated non-empty argument list")
      NumericSupport.fromFraction(result)

  private def abs(arguments: List[Value], position: Position): Value =
    val number = expectNumber(expectSingleArgument(arguments, "abs", position), "abs", position)
    if NumericSupport.isInexact(number) then InexactValue(math.abs(NumericSupport.toDouble(number)))
    else NumericSupport.fromFraction(NumericSupport.exactFraction(number).abs)

  private def modulo(arguments: List[Value], position: Position): Value =
    val (dividend, divisor) = expectTwoNumbers(arguments, "modulo", position)
    IntValue(moduloValue(dividend, divisor, position))

  private def remainder(arguments: List[Value], position: Position): Value =
    val (dividend, divisor) = expectTwoNumbers(arguments, "remainder", position)
    IntValue(remainderValue(dividend, divisor, position))

  private def quotient(arguments: List[Value], position: Position): Value =
    val (dividend, divisor) = expectTwoNumbers(arguments, "quotient", position)
    IntValue(quotientValue(dividend, divisor, position))

  private def gcd(arguments: List[Value], position: Position): Value =
    val integers = arguments.map(expectInteger(_, "gcd", position).abs)
    IntValue(integers.foldLeft(BigInt(0))(_.gcd(_)))

  private def lcm(arguments: List[Value], position: Position): Value =
    val integers = arguments.map(expectInteger(_, "lcm", position).abs)
    IntValue(
      integers.foldLeft(BigInt(1)) { (current, next) =>
        if current == 0 || next == 0 then BigInt(0)
        else (current / current.gcd(next)) * next
      }
    )

  private def minValue(arguments: List[Value], position: Position): Value =
    val numbers = numericArgumentsAtLeast(arguments, 1, "min", position)
    numbers.reduceLeft((current: NumberValue, next: NumberValue) =>
      if NumericSupport.compare(current, next) <= 0 then current else next
    )

  private def maxValue(arguments: List[Value], position: Position): Value =
    val numbers = numericArgumentsAtLeast(arguments, 1, "max", position)
    numbers.reduceLeft((current: NumberValue, next: NumberValue) =>
      if NumericSupport.compare(current, next) >= 0 then current else next
    )

  private def expt(arguments: List[Value], position: Position): Value =
    val (base, exponent) = expectTwoNumbers(arguments, "expt", position)
    if exponent < 0 then SchemeFailure.raise("expt expected a non-negative exponent", position)

    @tailrec
    def loop(currentBase: BigInt, currentExponent: BigInt, acc: BigInt): BigInt =
      if currentExponent == 0 then acc
      else if currentExponent % 2 == 1 then loop(currentBase * currentBase, currentExponent / 2, acc * currentBase)
      else loop(currentBase * currentBase, currentExponent / 2, acc)

    IntValue(loop(base, exponent, 1))

  private def truncateNumber(arguments: List[Value], position: Position): Value =
    val number = expectNumber(expectSingleArgument(arguments, "truncate", position), "truncate", position)
    number match
      case inexact: InexactValue =>
        val truncated =
          if inexact.value >= 0 then math.floor(inexact.value)
          else math.ceil(inexact.value)
        InexactValue(truncated)
      case _ =>
        val fraction = NumericSupport.exactFractionOnly(number, "truncate", position)
        IntValue(fraction.numerator / fraction.denominator)

  private def roundNumber(arguments: List[Value], position: Position): Value =
    val number = expectNumber(expectSingleArgument(arguments, "round", position), "round", position)
    number match
      case InexactValue(value) =>
        InexactValue(math.rint(value))
      case _ =>
        IntValue(roundExact(NumericSupport.exactFractionOnly(number, "round", position)))

  private def exactToInexact(arguments: List[Value], position: Position): Value =
    val number = expectNumber(expectSingleArgument(arguments, "exact->inexact", position), "exact->inexact", position)
    NumericSupport.toInexact(number)

  private def inexactToExact(arguments: List[Value], position: Position): Value =
    val number = expectNumber(expectSingleArgument(arguments, "inexact->exact", position), "inexact->exact", position)
    NumericSupport.toExact(number)

  private def numerator(arguments: List[Value], position: Position): Value =
    val number = expectNumber(expectSingleArgument(arguments, "numerator", position), "numerator", position)
    IntValue(NumericSupport.exactFractionOnly(number, "numerator", position).numerator)

  private def denominator(arguments: List[Value], position: Position): Value =
    val number = expectNumber(expectSingleArgument(arguments, "denominator", position), "denominator", position)
    IntValue(NumericSupport.exactFractionOnly(number, "denominator", position).denominator)

  private def compareNumbers(
    name: String
  )(predicate: Int => Boolean): (List[Value], Position) => Value =
    (arguments, position) =>
      val numbers = numericArgumentsAtLeast(arguments, 2, name, position)
      BoolValue(numbers.zip(numbers.tail).forall((left, right) => predicate(NumericSupport.compare(left, right))))

  private def unaryNumberPredicate(
    name: String
  )(predicate: NumberValue => Boolean): (List[Value], Position) => Value =
    (arguments, position) =>
      BoolValue(predicate(expectNumber(expectSingleArgument(arguments, name, position), name, position)))

  private def unaryIntegerPredicate(
    name: String
  )(predicate: BigInt => Boolean): (List[Value], Position) => Value =
    (arguments, position) =>
      BoolValue(predicate(expectInteger(expectSingleArgument(arguments, name, position), name, position)))

  private def numericArguments(
    arguments: List[Value],
    name: String,
    position: Position
  ): List[NumberValue] =
    arguments.map(expectNumber(_, name, position))

  private def numericArgumentsAtLeast(
    arguments: List[Value],
    minimum: Int,
    name: String,
    position: Position
  ): List[NumberValue] =
    expectAtLeast(arguments, minimum, name, position).map(expectNumber(_, name, position))

  private def expectTwoNumbers(
    arguments: List[Value],
    name: String,
    position: Position
  ): (BigInt, BigInt) =
    val (left, right) = expectTwoArguments(arguments, name, position)
    (expectInteger(left, name, position), expectInteger(right, name, position))

  private def quotientValue(
    dividend: BigInt,
    divisor: BigInt,
    position: Position
  ): BigInt =
    if divisor == 0 then SchemeFailure.raise("division by zero", position)

    val quotient = dividend.abs / divisor.abs
    if dividend.sign * divisor.sign < 0 then -quotient else quotient

  private def remainderValue(
    dividend: BigInt,
    divisor: BigInt,
    position: Position
  ): BigInt =
    dividend - divisor * quotientValue(dividend, divisor, position)

  private def moduloValue(
    dividend: BigInt,
    divisor: BigInt,
    position: Position
  ): BigInt =
    val remainder = remainderValue(dividend, divisor, position)
    if remainder == 0 || remainder.sign == divisor.sign then remainder
    else remainder + divisor

  private def roundExact(fraction: ExactFraction): BigInt =
    val quotient   = fraction.numerator / fraction.denominator
    val remainder  = (fraction.numerator % fraction.denominator).abs
    val comparison = (remainder * 2).compare(fraction.denominator)

    if comparison < 0 then quotient
    else if comparison > 0 then quotient + fraction.numerator.sign
    else if quotient % 2 == 0 then quotient
    else quotient + fraction.numerator.sign
