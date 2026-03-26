package ming

import java.math.{BigDecimal as JBigDecimal, MathContext}

import SchemeModel.*

private[ming] object SchemeNumbers:

  private val InexactMathContext = MathContext.DECIMAL64

  final private case class Fraction private (numerator: BigInt, denominator: BigInt):

    def +(other: Fraction): Fraction =
      Fraction.normalized(
        numerator * other.denominator + other.numerator * denominator,
        denominator * other.denominator
      )

    def -(other: Fraction): Fraction =
      Fraction.normalized(
        numerator * other.denominator - other.numerator * denominator,
        denominator * other.denominator
      )

    def *(other: Fraction): Fraction =
      Fraction.normalized(numerator * other.numerator, denominator * other.denominator)

    def /(other: Fraction): Fraction =
      Fraction.normalized(numerator * other.denominator, denominator * other.numerator)

    def unary_- : Fraction =
      Fraction.normalized(-numerator, denominator)

    def abs: Fraction =
      if numerator.signum < 0 then -this else this

    def compare(other: Fraction): Int =
      (numerator * other.denominator).compare(other.numerator * denominator)

    def toValue: Value =
      if denominator == 1 then Value.IntegerValue(numerator)
      else Value.RationalValue(numerator, denominator)

  private object Fraction:
    val Zero: Fraction = normalized(0, 1)
    val One: Fraction  = normalized(1, 1)

    def normalized(numerator: BigInt, denominator: BigInt): Fraction =
      if denominator == 0 then throw new EvalError("division by zero")
      else if numerator == 0 then new Fraction(0, 1)
      else
        val positiveDenominator = denominator.abs
        val adjustedNumerator =
          if denominator.signum < 0 then -numerator else numerator
        val divisor = adjustedNumerator.abs.gcd(positiveDenominator)
        new Fraction(adjustedNumerator / divisor, positiveDenominator / divisor)

    def fromExactValue(value: Value): Fraction =
      value match
        case Value.IntegerValue(number)                  => normalized(number, 1)
        case Value.RationalValue(numerator, denominator) => normalized(numerator, denominator)
        case other =>
          throw new EvalError(s"expected an exact number, got ${SchemeRuntime.render(other)}")

    def fromValue(value: Value): Fraction =
      value match
        case Value.IntegerValue(number) => normalized(number, 1)
        case Value.RationalValue(numerator, denominator) =>
          normalized(numerator, denominator)
        case Value.InexactValue(number) =>
          fromDecimal(number)
        case other =>
          throw new EvalError(s"expected a number, got ${SchemeRuntime.render(other)}")

    def fromDecimal(value: BigDecimal): Fraction =
      val normalizedDecimal = value.bigDecimal.stripTrailingZeros()
      val scale             = normalizedDecimal.scale()
      val unscaled          = BigInt(normalizedDecimal.unscaledValue())
      if scale <= 0 then normalized(unscaled * tenTo(-scale), 1)
      else normalized(unscaled, tenTo(scale))

    private def tenTo(power: Int): BigInt =
      BigInt(10).pow(power)

  private val RationalPattern = raw"([+-]?\d+)/([+-]?\d+)".r
  private val DecimalPattern  = raw"[+-]?\d+\.\d+".r
  private val IntegerPattern  = raw"[+-]?\d+".r

  def parseNumberExpr(token: String, pos: SourcePos): Option[Expr] =
    token match
      case IntegerPattern() =>
        Some(Expr.IntegerLiteral(BigInt(token), pos))
      case RationalPattern(numerator, denominator) =>
        Some(Expr.RationalLiteral(BigInt(numerator), BigInt(denominator), pos))
      case DecimalPattern() =>
        Some(Expr.InexactLiteral(BigDecimal(token), pos))
      case _ =>
        None

  def parseStringNumber(text: String): Option[Value] =
    text match
      case IntegerPattern() =>
        Some(Value.IntegerValue(BigInt(text)))
      case RationalPattern(numerator, denominator) =>
        try Some(exactRational(BigInt(numerator), BigInt(denominator)))
        catch case _: EvalError => None
      case DecimalPattern() =>
        Some(Value.InexactValue(BigDecimal(text)))
      case _ =>
        None

  def isNumber(value: Value): Boolean =
    value match
      case Value.IntegerValue(_)     => true
      case Value.RationalValue(_, _) => true
      case Value.InexactValue(_)     => true
      case _                         => false

  def isExact(value: Value): Boolean =
    value match
      case Value.IntegerValue(_)     => true
      case Value.RationalValue(_, _) => true
      case _                         => false

  def isInexact(value: Value): Boolean =
    value match
      case Value.InexactValue(_) => true
      case _                     => false

  def isRational(value: Value): Boolean =
    value match
      case Value.IntegerValue(_)     => true
      case Value.RationalValue(_, _) => true
      case _                         => false

  def isInteger(value: Value): Boolean =
    integerValue(value).nonEmpty

  def integerValue(value: Value): Option[BigInt] =
    value match
      case Value.IntegerValue(number) =>
        Some(number)
      case Value.RationalValue(numerator, denominator) =>
        Option.when(denominator != 0 && numerator % denominator == 0)(numerator / denominator)
      case Value.InexactValue(number) =>
        val fraction = Fraction.fromDecimal(number)
        Option.when(fraction.denominator == 1)(fraction.numerator)
      case _ =>
        None

  def exactRational(numerator: BigInt, denominator: BigInt): Value =
    Fraction.normalized(numerator, denominator).toValue

  def add(values: List[Value]): Value =
    arithmeticResult(
      values,
      values.foldLeft(Fraction.Zero) { (acc, value) =>
        acc + Fraction.fromValue(value)
      }
    )

  def subtract(values: List[Value]): Value =
    val numbers = values.map(Fraction.fromValue)
    val result =
      if numbers.length == 1 then -numbers.head
      else numbers.tail.foldLeft(numbers.head)(_ - _)
    arithmeticResult(values, result)

  def multiply(values: List[Value]): Value =
    arithmeticResult(
      values,
      values.foldLeft(Fraction.One) { (acc, value) =>
        acc * Fraction.fromValue(value)
      }
    )

  def divide(values: List[Value]): Value =
    val numbers = values.map(Fraction.fromValue)
    val result  = numbers.tail.foldLeft(numbers.head)(_ / _)
    arithmeticResult(values, result)

  def abs(value: Value): Value =
    arithmeticResult(List(value), Fraction.fromValue(value).abs)

  def min(values: List[Value]): Value =
    values.reduceLeft { (left, right) =>
      if compare(left, right) <= 0 then left else right
    }

  def max(values: List[Value]): Value =
    values.reduceLeft { (left, right) =>
      if compare(left, right) >= 0 then left else right
    }

  def compare(left: Value, right: Value): Int =
    Fraction.fromValue(left).compare(Fraction.fromValue(right))

  def equal(left: Value, right: Value): Boolean =
    compare(left, right) == 0

  def exactToInexact(value: Value): Value =
    value match
      case Value.InexactValue(_) =>
        value
      case other =>
        val fraction = Fraction.fromExactValue(other)
        val decimal =
          if fraction.denominator == 1 then BigDecimal(new JBigDecimal(fraction.numerator.bigInteger).setScale(1))
          else
            BigDecimal(
              new JBigDecimal(fraction.numerator.bigInteger)
                .divide(new JBigDecimal(fraction.denominator.bigInteger), InexactMathContext)
            )
        Value.InexactValue(decimal)

  def inexactToExact(value: Value): Value =
    value match
      case Value.InexactValue(number) =>
        Fraction.fromDecimal(number).toValue
      case exact if isExact(exact) =>
        exact
      case other =>
        throw new EvalError(s"inexact->exact expected a number, got ${SchemeRuntime.render(other)}")

  def numerator(value: Value): BigInt =
    Fraction.fromExactValue(value).numerator

  def denominator(value: Value): BigInt =
    Fraction.fromExactValue(value).denominator

  def render(value: Value): String =
    value match
      case Value.IntegerValue(number) =>
        number.toString
      case Value.RationalValue(numerator, denominator) =>
        s"$numerator/$denominator"
      case Value.InexactValue(number) =>
        renderInexact(number)
      case other =>
        throw new EvalError(s"expected a number, got ${SchemeRuntime.render(other)}")

  private def arithmeticResult(inputs: List[Value], result: Fraction): Value =
    val exactResult = result.toValue
    if inputs.exists(isInexact) then exactToInexact(exactResult) else exactResult

  private def renderInexact(value: BigDecimal): String =
    val plain = value.bigDecimal.stripTrailingZeros().toPlainString
    if plain.contains(".") then plain else s"$plain.0"
