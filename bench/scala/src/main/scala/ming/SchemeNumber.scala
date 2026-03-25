package ming

private[ming] enum SchemeNumber:

  case Exact(numerator: BigInt, denominator: BigInt)
  case Inexact(value: Double)

  def isExact: Boolean =
    this match
      case SchemeNumber.Exact(_, _) =>
        true

      case SchemeNumber.Inexact(_) =>
        false

  def isInexact: Boolean =
    !isExact

  def isInteger: Boolean =
    this match
      case SchemeNumber.Exact(_, denominator) =>
        denominator == 1

      case SchemeNumber.Inexact(value) =>
        value.isFinite && value.isWhole

  def toDouble: Double =
    this match
      case SchemeNumber.Exact(numerator, denominator) =>
        numerator.toDouble / denominator.toDouble

      case SchemeNumber.Inexact(value) =>
        value

  def render: String =
    this match
      case SchemeNumber.Exact(numerator, denominator) =>
        if denominator == 1 then numerator.toString
        else s"$numerator/$denominator"

      case SchemeNumber.Inexact(value) =>
        java.lang.Double.toString(value)

private[ming] object SchemeNumber:

  def integer(value: BigInt): SchemeNumber =
    SchemeNumber.Exact(value, 1)

  def rational(numerator: BigInt, denominator: BigInt): SchemeNumber =
    if denominator == 0 then throw new ArithmeticException("division by zero")
    else if numerator == 0 then SchemeNumber.Exact(0, 1)
    else
      val normalizedNumerator =
        if denominator.signum < 0 then -numerator
        else numerator
      val normalizedDenominator = denominator.abs
      val divisor               = normalizedNumerator.abs.gcd(normalizedDenominator)

      SchemeNumber.Exact(normalizedNumerator / divisor, normalizedDenominator / divisor)

  def inexact(value: Double): SchemeNumber =
    SchemeNumber.Inexact(value)

  def isNumberValue(value: Value): Boolean =
    value match
      case Value.IntVal(_) | Value.RationalVal(_, _) | Value.InexactVal(_) =>
        true

      case _ =>
        false

  def fromValue(value: Value, pos: SourcePos, name: String): SchemeNumber =
    fromValueOption(value).getOrElse(throw EvalError.at(pos, s"$name expected number arguments, got ${value.typeName}"))

  def fromValueOption(value: Value): Option[SchemeNumber] =
    value match
      case Value.IntVal(number) =>
        Some(integer(number))

      case Value.RationalVal(numerator, denominator) =>
        Some(rational(numerator, denominator))

      case Value.InexactVal(number) =>
        Some(inexact(number))

      case _ =>
        None

  def toValue(number: SchemeNumber): Value =
    number match
      case SchemeNumber.Exact(numerator, denominator) if denominator == 1 =>
        Value.IntVal(numerator)

      case SchemeNumber.Exact(numerator, denominator) =>
        Value.RationalVal(numerator, denominator)

      case SchemeNumber.Inexact(value) =>
        Value.InexactVal(value)

  def parseLiteral(token: String): Either[String, SchemeNumber] =
    if token.contains('/') then parseRationalLiteral(token)
    else if token.exists(ch => ch == '.' || ch == 'e' || ch == 'E') then parseInexactLiteral(token)
    else parseIntegerLiteral(token)

  def add(left: SchemeNumber, right: SchemeNumber): SchemeNumber =
    binary(left, right)(
      (leftNumerator, leftDenominator, rightNumerator, rightDenominator) =>
        rational(
          (leftNumerator * rightDenominator) + (rightNumerator * leftDenominator),
          leftDenominator * rightDenominator
        ),
      _ + _
    )

  def subtract(left: SchemeNumber, right: SchemeNumber): SchemeNumber =
    binary(left, right)(
      (leftNumerator, leftDenominator, rightNumerator, rightDenominator) =>
        rational(
          (leftNumerator * rightDenominator) - (rightNumerator * leftDenominator),
          leftDenominator * rightDenominator
        ),
      _ - _
    )

  def multiply(left: SchemeNumber, right: SchemeNumber): SchemeNumber =
    binary(left, right)(
      (leftNumerator, leftDenominator, rightNumerator, rightDenominator) =>
        rational(leftNumerator * rightNumerator, leftDenominator * rightDenominator),
      _ * _
    )

  def divide(left: SchemeNumber, right: SchemeNumber): SchemeNumber =
    if isZero(right) then throw new ArithmeticException("division by zero")
    else
      binary(left, right)(
        (leftNumerator, leftDenominator, rightNumerator, rightDenominator) =>
          rational(leftNumerator * rightDenominator, leftDenominator * rightNumerator),
        _ / _
      )

  def negate(number: SchemeNumber): SchemeNumber =
    number match
      case SchemeNumber.Exact(numerator, denominator) =>
        SchemeNumber.Exact(-numerator, denominator)

      case SchemeNumber.Inexact(value) =>
        SchemeNumber.Inexact(-value)

  def abs(number: SchemeNumber): SchemeNumber =
    number match
      case SchemeNumber.Exact(numerator, denominator) =>
        SchemeNumber.Exact(numerator.abs, denominator)

      case SchemeNumber.Inexact(value) =>
        SchemeNumber.Inexact(math.abs(value))

  def compare(left: SchemeNumber, right: SchemeNumber): Int =
    (asComparableRational(left), asComparableRational(right)) match
      case (Some((leftNumerator, leftDenominator)), Some((rightNumerator, rightDenominator))) =>
        (leftNumerator * rightDenominator).compare(rightNumerator * leftDenominator)

      case _ =>
        val leftValue  = left.toDouble
        val rightValue = right.toDouble
        if leftValue < rightValue then -1
        else if leftValue > rightValue then 1
        else 0

  def equal(left: SchemeNumber, right: SchemeNumber): Boolean =
    (asComparableRational(left), asComparableRational(right)) match
      case (Some((leftNumerator, leftDenominator)), Some((rightNumerator, rightDenominator))) =>
        leftNumerator == rightNumerator && leftDenominator == rightDenominator

      case _ =>
        left.toDouble == right.toDouble

  def isZero(number: SchemeNumber): Boolean =
    compare(number, integer(0)) == 0

  def exactToInexact(number: SchemeNumber): SchemeNumber =
    SchemeNumber.Inexact(number.toDouble)

  def inexactToExact(number: SchemeNumber): SchemeNumber =
    number match
      case exact @ SchemeNumber.Exact(_, _) =>
        exact

      case SchemeNumber.Inexact(value) =>
        if !value.isFinite then throw new ArithmeticException("cannot convert a non-finite inexact number")
        inexactValueToExact(value)

  private def binary(
    left: SchemeNumber,
    right: SchemeNumber
  )(
    exactOp: (BigInt, BigInt, BigInt, BigInt) => SchemeNumber,
    inexactOp: (Double, Double) => Double
  ): SchemeNumber =
    (left, right) match
      case (SchemeNumber.Exact(leftNumerator, leftDenominator), SchemeNumber.Exact(rightNumerator, rightDenominator)) =>
        exactOp(leftNumerator, leftDenominator, rightNumerator, rightDenominator)

      case _ =>
        SchemeNumber.Inexact(inexactOp(left.toDouble, right.toDouble))

  private def parseIntegerLiteral(token: String): Either[String, SchemeNumber] =
    try Right(integer(BigInt(token)))
    catch
      case _: NumberFormatException =>
        Left(s"invalid integer literal: $token")

  private def parseInexactLiteral(token: String): Either[String, SchemeNumber] =
    try
      val value = java.lang.Double.parseDouble(token)
      if !value.isFinite then Left(s"invalid inexact literal: $token")
      else Right(SchemeNumber.Inexact(value))
    catch
      case _: NumberFormatException =>
        Left(s"invalid inexact literal: $token")

  private def parseRationalLiteral(token: String): Either[String, SchemeNumber] =
    token.split("/", -1).toList match
      case numeratorText :: denominatorText :: Nil =>
        for
          numerator   <- parseBigIntToken(numeratorText, s"invalid rational literal: $token")
          denominator <- parseBigIntToken(denominatorText, s"invalid rational literal: $token")
          number <-
            if denominator == 0 then Left(s"invalid rational literal: $token")
            else Right(rational(numerator, denominator))
        yield number

      case _ =>
        Left(s"invalid rational literal: $token")

  private def parseBigIntToken(token: String, error: String): Either[String, BigInt] =
    try Right(BigInt(token))
    catch
      case _: NumberFormatException =>
        Left(error)

  private def asComparableRational(number: SchemeNumber): Option[(BigInt, BigInt)] =
    number match
      case SchemeNumber.Exact(numerator, denominator) =>
        Some((numerator, denominator))

      case SchemeNumber.Inexact(value) if value.isFinite =>
        inexactValueToExact(value) match
          case SchemeNumber.Exact(numerator, denominator) =>
            Some((numerator, denominator))

          case SchemeNumber.Inexact(_) =>
            None

      case SchemeNumber.Inexact(_) =>
        None

  private def inexactValueToExact(value: Double): SchemeNumber =
    val decimal  = java.math.BigDecimal.valueOf(value)
    val scale    = decimal.scale
    val unscaled = BigInt(decimal.unscaledValue)

    if scale >= 0 then rational(unscaled, BigInt(10).pow(scale))
    else integer(unscaled * BigInt(10).pow(-scale))
