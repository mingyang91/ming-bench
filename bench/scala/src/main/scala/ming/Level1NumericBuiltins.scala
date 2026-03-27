package ming

import scala.annotation.tailrec

private[ming] object Level1NumericBuiltins:
  import RuntimeSupport.*

  val values: Map[String, Value] = Map(
    "+"         -> BuiltinValue("+", add),
    "-"         -> BuiltinValue("-", subtract),
    "*"         -> BuiltinValue("*", multiply),
    "/"         -> BuiltinValue("/", divide),
    "<"         -> BuiltinValue("<", compareNumbers("<")(_ < _)),
    ">"         -> BuiltinValue(">", compareNumbers(">")(_ > _)),
    "="         -> BuiltinValue("=", compareNumbers("=")(_ == _)),
    "<="        -> BuiltinValue("<=", compareNumbers("<=")(_ <= _)),
    "abs"       -> BuiltinValue("abs", abs),
    "modulo"    -> BuiltinValue("modulo", modulo),
    "remainder" -> BuiltinValue("remainder", remainder),
    "quotient"  -> BuiltinValue("quotient", quotient),
    "min"       -> BuiltinValue("min", minValue),
    "max"       -> BuiltinValue("max", maxValue),
    "expt"      -> BuiltinValue("expt", expt),
    "zero?"     -> BuiltinValue("zero?", unaryNumericPredicate("zero?")(_ == 0)),
    "positive?" -> BuiltinValue("positive?", unaryNumericPredicate("positive?")(_ > 0)),
    "negative?" -> BuiltinValue("negative?", unaryNumericPredicate("negative?")(_ < 0)),
    "odd?"      -> BuiltinValue("odd?", unaryNumericPredicate("odd?")(_ % 2 != 0)),
    "even?"     -> BuiltinValue("even?", unaryNumericPredicate("even?")(_ % 2 == 0))
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

  private def abs(arguments: List[Value], position: Position): Value =
    IntValue(expectNumber(expectSingleArgument(arguments, "abs", position), "abs", position).abs)

  private def modulo(arguments: List[Value], position: Position): Value =
    val (dividend, divisor) = expectTwoNumbers(arguments, "modulo", position)
    IntValue(moduloValue(dividend, divisor, position))

  private def remainder(arguments: List[Value], position: Position): Value =
    val (dividend, divisor) = expectTwoNumbers(arguments, "remainder", position)
    IntValue(remainderValue(dividend, divisor, position))

  private def quotient(arguments: List[Value], position: Position): Value =
    val (dividend, divisor) = expectTwoNumbers(arguments, "quotient", position)
    IntValue(quotientValue(dividend, divisor, position))

  private def minValue(arguments: List[Value], position: Position): Value =
    val numbers = numericArgumentsAtLeast(arguments, 1, "min", position)
    IntValue(numbers.min)

  private def maxValue(arguments: List[Value], position: Position): Value =
    val numbers = numericArgumentsAtLeast(arguments, 1, "max", position)
    IntValue(numbers.max)

  private def expt(arguments: List[Value], position: Position): Value =
    val (base, exponent) = expectTwoNumbers(arguments, "expt", position)
    if exponent < 0 then SchemeFailure.raise("expt expected a non-negative exponent", position)

    @tailrec
    def loop(currentBase: BigInt, currentExponent: BigInt, acc: BigInt): BigInt =
      if currentExponent == 0 then acc
      else if currentExponent % 2 == 1 then loop(currentBase * currentBase, currentExponent / 2, acc * currentBase)
      else loop(currentBase * currentBase, currentExponent / 2, acc)

    IntValue(loop(base, exponent, 1))

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

  private def unaryNumericPredicate(
    name: String
  )(predicate: BigInt => Boolean): (List[Value], Position) => Value =
    (arguments, position) =>
      BoolValue(predicate(expectNumber(expectSingleArgument(arguments, name, position), name, position)))

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

  private def expectTwoNumbers(
    arguments: List[Value],
    name: String,
    position: Position
  ): (BigInt, BigInt) =
    val (left, right) = expectTwoArguments(arguments, name, position)
    (expectNumber(left, name, position), expectNumber(right, name, position))

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
