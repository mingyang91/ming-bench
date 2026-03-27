package ming

import BuiltinSupport.*

private[ming] object NumericBuiltins:

  private val utilityNames = Set(
    "abs",
    "gcd",
    "lcm",
    "modulo",
    "remainder",
    "quotient",
    "truncate",
    "round",
    "numerator",
    "denominator",
    "min",
    "max",
    "expt",
    "exact->inexact",
    "inexact->exact"
  )

  private val predicateNames = Set(
    "zero?",
    "positive?",
    "negative?",
    "odd?",
    "even?"
  )

  val names: Set[String] = utilityNames ++ predicateNames

  def handlesUtility(name: String): Boolean =
    utilityNames.contains(name)

  def handlesPredicate(name: String): Boolean =
    predicateNames.contains(name)

  def invokeUtility(name: String, args: List[Value], pos: SourcePos): Value =
    name match
      case "abs" =>
        SchemeNumber.abs(requireNumber(name, requireSingleArg(name, args, pos), pos)).toValue
      case "gcd" =>
        Value.IntVal(gcdAll(args.map(arg => requireExactInteger(name, arg, pos))))
      case "lcm" =>
        Value.IntVal(lcmAll(args.map(arg => requireExactInteger(name, arg, pos))))
      case "modulo" =>
        val (dividend, divisor) = requireBinaryExactIntegers(name, args, pos)
        Value.IntVal(modulo(dividend, divisor, pos))
      case "remainder" =>
        val (dividend, divisor) = requireBinaryExactIntegers(name, args, pos)
        if divisor == 0 then throw EvalError.at(pos, "division by zero")
        Value.IntVal(dividend % divisor)
      case "quotient" =>
        val (dividend, divisor) = requireBinaryExactIntegers(name, args, pos)
        if divisor == 0 then throw EvalError.at(pos, "division by zero")
        Value.IntVal(dividend / divisor)
      case "truncate" =>
        truncateNumber(requireNumber(name, requireSingleArg(name, args, pos), pos))
      case "round" =>
        roundNumber(requireNumber(name, requireSingleArg(name, args, pos), pos))
      case "numerator" =>
        Value.IntVal(SchemeNumber.numerator(requireExactNumber(name, args, pos)))
      case "denominator" =>
        Value.IntVal(SchemeNumber.denominator(requireExactNumber(name, args, pos)))
      case "min" =>
        SchemeNumber.min(requireMinArgs(name, evalNumbers(name, args, pos), min = 1, pos)).toValue
      case "max" =>
        SchemeNumber.max(requireMinArgs(name, evalNumbers(name, args, pos), min = 1, pos)).toValue
      case "expt" =>
        val (base, exponent) = requireBinaryExactIntegers(name, args, pos)
        if exponent < 0 then throw EvalError.at(pos, s"$name expected a non-negative exponent")
        Value.IntVal(expt(base, exponent))
      case "exact->inexact" =>
        SchemeNumber.exactToInexact(requireNumber(name, requireSingleArg(name, args, pos), pos)).toValue
      case "inexact->exact" =>
        SchemeNumber.inexactToExact(requireNumber(name, requireSingleArg(name, args, pos), pos)).toValue
      case _ =>
        unknownProcedure(name, pos)

  def invokePredicate(name: String, args: List[Value], pos: SourcePos): Value =
    val value = requireSingleArg(name, args, pos)
    name match
      case "zero?" =>
        Value.BoolVal(SchemeNumber.isZero(requireNumber(name, value, pos)))
      case "positive?" =>
        Value.BoolVal(SchemeNumber.compare(requireNumber(name, value, pos), SchemeNumber.ExactInt(0L)) > 0)
      case "negative?" =>
        Value.BoolVal(SchemeNumber.compare(requireNumber(name, value, pos), SchemeNumber.ExactInt(0L)) < 0)
      case "odd?" =>
        Value.BoolVal(requireExactInteger(name, value, pos) % 2L != 0L)
      case "even?" =>
        Value.BoolVal(requireExactInteger(name, value, pos) % 2L == 0L)
      case _ =>
        unknownProcedure(name, pos)

  private def requireExactNumber(name: String, args: List[Value], pos: SourcePos): SchemeNumber =
    requireNumber(name, requireSingleArg(name, args, pos), pos) match
      case exact if exact.isExact => exact
      case _                      => throw EvalError.at(pos, s"$name expected an exact number")

  private def requireBinaryExactIntegers(name: String, args: List[Value], pos: SourcePos): (Long, Long) =
    val values = requireArgCount(name, args, expected = 2, pos)
    (
      requireExactInteger(name, values.head, pos),
      requireExactInteger(name, values(1), pos)
    )

  private def modulo(dividend: Long, divisor: Long, pos: SourcePos): Long =
    if divisor == 0 then throw EvalError.at(pos, "division by zero")
    val remainder = dividend % divisor
    if remainder == 0 || java.lang.Long.signum(remainder) == java.lang.Long.signum(divisor) then remainder
    else remainder + divisor

  private def expt(base: Long, exponent: Long): Long =
    @annotation.tailrec
    def loop(factor: Long, power: Long, acc: Long): Long =
      if power == 0 then acc
      else if (power & 1) == 1 then loop(factor * factor, power >>> 1, acc * factor)
      else loop(factor * factor, power >>> 1, acc)

    loop(base, exponent, acc = 1L)

  private def gcdAll(values: List[Long]): Long =
    values.foldLeft(0L)(gcd)

  private def lcmAll(values: List[Long]): Long =
    values.foldLeft(1L)(lcm)

  @annotation.tailrec
  private def gcd(left: Long, right: Long): Long =
    if right == 0L then math.abs(left)
    else gcd(right, left % right)

  private def lcm(left: Long, right: Long): Long =
    if left == 0L || right == 0L then 0L
    else math.abs(left / gcd(left, right) * right)

  private def truncateNumber(number: SchemeNumber): Value =
    number match
      case SchemeNumber.ExactInt(value) =>
        Value.IntVal(value)
      case SchemeNumber.ExactRational(numerator, denominator) =>
        Value.IntVal(numerator / denominator)
      case SchemeNumber.Inexact(value) =>
        Value.InexactVal(if value >= 0 then math.floor(value) else math.ceil(value))

  private def roundNumber(number: SchemeNumber): Value =
    number match
      case SchemeNumber.ExactInt(value) =>
        Value.IntVal(value)
      case SchemeNumber.ExactRational(numerator, denominator) =>
        Value.IntVal(math.rint(numerator.toDouble / denominator.toDouble).toLong)
      case SchemeNumber.Inexact(value) =>
        Value.InexactVal(math.rint(value))
