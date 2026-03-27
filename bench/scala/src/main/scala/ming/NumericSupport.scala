package ming

import scala.math.BigDecimal

private[ming] object NumericSupport:

  sealed trait ParsedNumber

  final case class ParsedExactInteger(value: BigInt) extends ParsedNumber

  final case class ParsedExactRational(numerator: BigInt, denominator: BigInt) extends ParsedNumber

  final case class ParsedInexact(value: Double) extends ParsedNumber

  final case class ExactFraction private (numerator: BigInt, denominator: BigInt):

    def +(other: ExactFraction): ExactFraction =
      ExactFraction(
        numerator * other.denominator + other.numerator * denominator,
        denominator * other.denominator
      )

    def -(other: ExactFraction): ExactFraction =
      ExactFraction(
        numerator * other.denominator - other.numerator * denominator,
        denominator * other.denominator
      )

    def *(other: ExactFraction): ExactFraction =
      ExactFraction(numerator * other.numerator, denominator * other.denominator)

    def /(other: ExactFraction): ExactFraction =
      if other.numerator == 0 then throw new ArithmeticException("division by zero")
      ExactFraction(numerator * other.denominator, denominator * other.numerator)

    def unary_- : ExactFraction =
      ExactFraction(-numerator, denominator)

    def abs: ExactFraction =
      ExactFraction(numerator.abs, denominator)

    def compare(other: ExactFraction): Int =
      (numerator * other.denominator).compare(other.numerator * denominator)

    def isInteger: Boolean =
      denominator == 1

    def toDouble: Double =
      numerator.toDouble / denominator.toDouble

  object ExactFraction:

    def apply(numerator: BigInt, denominator: BigInt): ExactFraction =
      if denominator == 0 then throw new ArithmeticException("division by zero")

      val normalizedDenominator = denominator.abs
      val normalizedNumerator   = if denominator.sign < 0 then -numerator else numerator
      val divisor               = normalizedNumerator.gcd(normalizedDenominator)
      new ExactFraction(normalizedNumerator / divisor, normalizedDenominator / divisor)

  private val IntegerPattern  = raw"[+-]?\d+".r
  private val RationalPattern = raw"([+-]?\d+)/(\d+)".r
  private val DecimalPattern  = raw"[+-]?\d+\.\d+".r

  def parseNumberToken(token: String, position: Position): Option[ParsedNumber] =
    token match
      case RationalPattern(numeratorToken, denominatorToken) =>
        val denominator = BigInt(denominatorToken)
        if denominator == 0 then SchemeFailure.raise("invalid rational literal", position)
        Some(ParsedExactRational(BigInt(numeratorToken), denominator))
      case _ if IntegerPattern.matches(token) =>
        Some(ParsedExactInteger(BigInt(token)))
      case _ if DecimalPattern.matches(token) =>
        Some(ParsedInexact(token.toDouble))
      case _ =>
        None

  def valueForLiteral(literal: ParsedNumber): NumberValue =
    literal match
      case ParsedExactInteger(value) =>
        IntValue(value)
      case ParsedExactRational(numerator, denominator) =>
        exactValue(numerator, denominator)
      case ParsedInexact(value) =>
        InexactValue(value)

  def exactValue(numerator: BigInt, denominator: BigInt): NumberValue =
    val fraction = ExactFraction(numerator, denominator)
    if fraction.denominator == 1 then IntValue(fraction.numerator)
    else RationalValue(fraction.numerator, fraction.denominator)

  def exactFraction(value: NumberValue): ExactFraction =
    value match
      case IntValue(number) =>
        ExactFraction(number, 1)
      case RationalValue(numerator, denominator) =>
        ExactFraction(numerator, denominator)
      case InexactValue(value) =>
        decimalStringToFraction(java.lang.Double.toString(value))

  def exactFractionOnly(value: NumberValue, name: String, position: Position): ExactFraction =
    value match
      case IntValue(number) =>
        ExactFraction(number, 1)
      case RationalValue(numerator, denominator) =>
        ExactFraction(numerator, denominator)
      case InexactValue(_) =>
        SchemeFailure.raise(s"$name expected an exact number", position)

  def toDouble(value: NumberValue): Double =
    value match
      case IntValue(number) =>
        number.toDouble
      case RationalValue(numerator, denominator) =>
        numerator.toDouble / denominator.toDouble
      case InexactValue(number) =>
        number

  def toInexact(value: NumberValue): InexactValue =
    value match
      case inexact: InexactValue => inexact
      case _                     => InexactValue(toDouble(value))

  def toExact(value: NumberValue): NumberValue =
    value match
      case exact @ IntValue(_)         => exact
      case exact @ RationalValue(_, _) => exact
      case inexact: InexactValue       => fromFraction(exactFraction(inexact))

  def fromFraction(fraction: ExactFraction): NumberValue =
    exactValue(fraction.numerator, fraction.denominator)

  def compare(left: NumberValue, right: NumberValue): Int =
    exactFraction(left).compare(exactFraction(right))

  def equal(left: NumberValue, right: NumberValue): Boolean =
    compare(left, right) == 0

  def isExact(value: NumberValue): Boolean =
    value match
      case InexactValue(_) => false
      case _               => true

  def isInexact(value: NumberValue): Boolean =
    !isExact(value)

  def isInteger(value: NumberValue): Boolean =
    exactFraction(value).isInteger

  def isRational(value: NumberValue): Boolean =
    true

  def isZero(value: NumberValue): Boolean =
    exactFraction(value).numerator == 0

  private def decimalStringToFraction(text: String): ExactFraction =
    val decimal  = BigDecimal(text).bigDecimal
    val unscaled = BigInt(decimal.unscaledValue)
    val scale    = decimal.scale
    val denominator =
      if scale <= 0 then BigInt(1)
      else BigInt(10).pow(scale)
    val numerator =
      if scale < 0 then unscaled * BigInt(10).pow(-scale)
      else unscaled
    ExactFraction(numerator, denominator)
