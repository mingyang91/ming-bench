package ming

import scala.util.Try

sealed private[ming] trait SchemeNumber:
  def isExact: Boolean
  def isInteger: Boolean
  def toDoubleValue: Double
  def render: String

  final def isInexact: Boolean =
    !isExact

private[ming] object SchemeNumber:

  final case class Exact(numerator: BigInt, denominator: BigInt) extends SchemeNumber:

    def isExact: Boolean =
      true

    def isInteger: Boolean =
      denominator == 1

    def toDoubleValue: Double =
      numerator.toDouble / denominator.toDouble

    def render: String =
      if denominator == 1 then numerator.toString
      else s"$numerator/$denominator"

  final case class Inexact(value: Double) extends SchemeNumber:

    def isExact: Boolean =
      false

    def isInteger: Boolean =
      java.lang.Double.isFinite(value) && value == math.rint(value)

    def toDoubleValue: Double =
      value

    def render: String =
      val text = java.lang.Double.toString(value)
      if text == "-0.0" then "0.0" else text

  private val IntegerPattern  = raw"[+-]?\d+".r
  private val RationalPattern = raw"([+-]?\d+)/(\d+)".r
  private val DecimalPattern  = raw"[+-]?(?:\d+\.\d*|\.\d+)".r

  val Zero: SchemeNumber =
    exact(BigInt(0))

  val One: SchemeNumber =
    exact(BigInt(1))

  def exact(value: BigInt): SchemeNumber =
    Exact(value, 1)

  def exact(numerator: BigInt, denominator: BigInt): SchemeNumber =
    if denominator == 0 then throw IllegalArgumentException("exact number denominator cannot be zero")
    else if numerator == 0 then Exact(0, 1)
    else
      val normalizedDenominator =
        if denominator.signum < 0 then -denominator else denominator
      val normalizedNumerator =
        if denominator.signum < 0 then -numerator else numerator
      val divisor = normalizedNumerator.gcd(normalizedDenominator)
      Exact(normalizedNumerator / divisor, normalizedDenominator / divisor)

  def inexact(value: Double): SchemeNumber =
    Inexact(value)

  def parseToken(token: String): Option[SchemeNumber] =
    token match
      case RationalPattern(numeratorText, denominatorText) =>
        val denominator = BigInt(denominatorText)
        Option.when(denominator != 0)(exact(BigInt(numeratorText), denominator))
      case _ if IntegerPattern.pattern.matcher(token).matches() =>
        Some(exact(BigInt(token)))
      case _ if DecimalPattern.pattern.matcher(token).matches() =>
        Try(inexact(token.toDouble)).toOption
      case _ =>
        None

  def add(left: SchemeNumber, right: SchemeNumber): SchemeNumber =
    combine(left, right)((leftNum, leftDen, rightNum, rightDen) =>
      exact(leftNum * rightDen + rightNum * leftDen, leftDen * rightDen)
    )(_ + _)

  def subtract(left: SchemeNumber, right: SchemeNumber): SchemeNumber =
    combine(left, right)((leftNum, leftDen, rightNum, rightDen) =>
      exact(leftNum * rightDen - rightNum * leftDen, leftDen * rightDen)
    )(_ - _)

  def multiply(left: SchemeNumber, right: SchemeNumber): SchemeNumber =
    combine(left, right)((leftNum, leftDen, rightNum, rightDen) => exact(leftNum * rightNum, leftDen * rightDen))(_ * _)

  def divide(left: SchemeNumber, right: SchemeNumber): SchemeNumber =
    combine(left, right)((leftNum, leftDen, rightNum, rightDen) => exact(leftNum * rightDen, leftDen * rightNum))(_ / _)

  def negate(number: SchemeNumber): SchemeNumber =
    number match
      case Exact(numerator, denominator) =>
        exact(-numerator, denominator)
      case Inexact(value) =>
        inexact(-value)

  def abs(number: SchemeNumber): SchemeNumber =
    number match
      case Exact(numerator, denominator) =>
        exact(numerator.abs, denominator)
      case Inexact(value) =>
        inexact(math.abs(value))

  def compare(left: SchemeNumber, right: SchemeNumber): Int =
    (left, right) match
      case (Exact(leftNum, leftDen), Exact(rightNum, rightDen)) =>
        (leftNum * rightDen).compare(rightNum * leftDen)
      case _ =>
        java.lang.Double.compare(left.toDoubleValue, right.toDoubleValue)

  def areEqual(left: SchemeNumber, right: SchemeNumber): Boolean =
    compare(left, right) == 0

  def isZero(number: SchemeNumber): Boolean =
    number match
      case Exact(numerator, _) => numerator == 0
      case Inexact(value)      => value == 0.0

  def isPositive(number: SchemeNumber): Boolean =
    compare(number, Zero) > 0

  def isNegative(number: SchemeNumber): Boolean =
    compare(number, Zero) < 0

  def min(left: SchemeNumber, right: SchemeNumber): SchemeNumber =
    if compare(left, right) <= 0 then left else right

  def max(left: SchemeNumber, right: SchemeNumber): SchemeNumber =
    if compare(left, right) >= 0 then left else right

  def pow(base: SchemeNumber, exponent: BigInt): SchemeNumber =
    require(exponent >= 0, "exponent must be non-negative")
    base match
      case Exact(numerator, denominator) =>
        exact(powBigInt(numerator, exponent), powBigInt(denominator, exponent))
      case Inexact(value) =>
        inexact(math.pow(value, exponent.toDouble))

  def toInexact(number: SchemeNumber): SchemeNumber =
    number match
      case value: Inexact => value
      case _              => inexact(number.toDoubleValue)

  def toExact(number: SchemeNumber): SchemeNumber =
    number match
      case value: Exact =>
        value
      case Inexact(value) =>
        val decimal = java.math.BigDecimal.valueOf(value).stripTrailingZeros()
        val scale   = math.max(decimal.scale(), 0)
        val factor  = BigInt(10).pow(scale)
        exact(BigInt(decimal.unscaledValue()), factor)

  private def combine(left: SchemeNumber, right: SchemeNumber)(
    exactOp: (BigInt, BigInt, BigInt, BigInt) => SchemeNumber
  )(inexactOp: (Double, Double) => Double): SchemeNumber =
    (left, right) match
      case (Exact(leftNum, leftDen), Exact(rightNum, rightDen)) =>
        exactOp(leftNum, leftDen, rightNum, rightDen)
      case _ =>
        inexact(inexactOp(left.toDoubleValue, right.toDoubleValue))

  private def powBigInt(base: BigInt, exponent: BigInt): BigInt =
    var result = BigInt(1)
    var factor = base
    var power  = exponent

    while power > 0 do
      if (power & 1) == 1 then result *= factor
      factor *= factor
      power /= 2

    result
