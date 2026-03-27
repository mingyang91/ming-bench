package ming

import scala.math.BigDecimal
import scala.math.BigInt

private[ming] enum SchemeNumber:
  case ExactInt(value: Long)
  case ExactRational(numerator: Long, denominator: Long)
  case Inexact(value: Double)

  def isExact: Boolean =
    this match
      case SchemeNumber.Inexact(_) => false
      case _                       => true

  def isInteger: Boolean =
    this match
      case SchemeNumber.ExactInt(_)                     => true
      case SchemeNumber.ExactRational(_, _)             => false
      case SchemeNumber.Inexact(value) if value.isWhole => true
      case SchemeNumber.Inexact(_)                      => false

  def isRational: Boolean =
    true

  def toDouble: Double =
    this match
      case SchemeNumber.ExactInt(value) =>
        value.toDouble
      case SchemeNumber.ExactRational(numerator, denominator) =>
        numerator.toDouble / denominator.toDouble
      case SchemeNumber.Inexact(value) =>
        value

  def toValue: Value =
    this match
      case SchemeNumber.ExactInt(value) =>
        Value.IntVal(value)
      case SchemeNumber.ExactRational(numerator, denominator) =>
        Value.RationalVal(numerator, denominator)
      case SchemeNumber.Inexact(value) =>
        Value.InexactVal(value)

  def toExpr(pos: SourcePos): Expr =
    this match
      case SchemeNumber.ExactInt(value) =>
        Expr.IntLit(value, pos)
      case SchemeNumber.ExactRational(numerator, denominator) =>
        Expr.RationalLit(numerator, denominator, pos)
      case SchemeNumber.Inexact(value) =>
        Expr.InexactLit(value, pos)

private[ming] object SchemeNumber:

  private val IntegerPattern  = """[+-]?\d+""".r
  private val RationalPattern = """[+-]?\d+/\d+""".r
  private val InexactPattern  = """[+-]?(?:\d+\.\d+|\d+\.|\.\d+)""".r

  def isNumericToken(token: String): Boolean =
    IntegerPattern.matches(token) || RationalPattern.matches(token) || InexactPattern.matches(token)

  def parseToken(token: String): Option[SchemeNumber] =
    if IntegerPattern.matches(token) then token.toLongOption.map(ExactInt.apply)
    else if RationalPattern.matches(token) then parseRationalToken(token)
    else if InexactPattern.matches(token) then
      token.toDoubleOption
        .filter(value => !value.isNaN && !value.isInfinity)
        .map(Inexact.apply)
    else None

  def fromValue(value: Value): Option[SchemeNumber] =
    value match
      case Value.IntVal(number)                      => Some(ExactInt(number))
      case Value.RationalVal(numerator, denominator) => Some(ExactRational(numerator, denominator))
      case Value.InexactVal(number)                  => Some(Inexact(number))
      case _                                         => None

  def render(number: SchemeNumber): String =
    number match
      case ExactInt(value) =>
        value.toString
      case ExactRational(numerator, denominator) =>
        s"$numerator/$denominator"
      case Inexact(value) =>
        java.lang.Double.toString(value)

  def areEqual(left: SchemeNumber, right: SchemeNumber): Boolean =
    compare(left, right) == 0

  def compare(left: SchemeNumber, right: SchemeNumber): Int =
    if left.isExact && right.isExact then compareExact(left, right)
    else java.lang.Double.compare(left.toDouble, right.toDouble)

  def add(values: List[SchemeNumber]): SchemeNumber =
    if values.exists(!_.isExact) then Inexact(values.map(_.toDouble).sum)
    else
      values.foldLeft[SchemeNumber](ExactInt(0L)) { (acc, value) =>
        val (leftNum, leftDen)   = toExactFraction(acc)
        val (rightNum, rightDen) = toExactFraction(value)
        normalize(leftNum * rightDen + rightNum * leftDen, leftDen * rightDen)
      }

  def subtract(values: List[SchemeNumber]): SchemeNumber =
    values match
      case value :: Nil =>
        if value.isExact then
          val (numerator, denominator) = toExactFraction(value)
          normalize(-numerator, denominator)
        else Inexact(-value.toDouble)
      case value :: rest if values.exists(!_.isExact) =>
        Inexact(rest.foldLeft(value.toDouble)(_ - _.toDouble))
      case value :: rest =>
        rest.foldLeft(value) { (acc, next) =>
          val (leftNum, leftDen)   = toExactFraction(acc)
          val (rightNum, rightDen) = toExactFraction(next)
          normalize(leftNum * rightDen - rightNum * leftDen, leftDen * rightDen)
        }
      case Nil =>
        ExactInt(0L)

  def multiply(values: List[SchemeNumber]): SchemeNumber =
    if values.exists(!_.isExact) then Inexact(values.map(_.toDouble).product)
    else
      values.foldLeft[SchemeNumber](ExactInt(1L)) { (acc, value) =>
        val (leftNum, leftDen)   = toExactFraction(acc)
        val (rightNum, rightDen) = toExactFraction(value)
        normalize(leftNum * rightNum, leftDen * rightDen)
      }

  def divide(values: List[SchemeNumber], pos: SourcePos): SchemeNumber =
    values match
      case value :: rest if rest.exists(isZero) =>
        throw EvalError.at(pos, "division by zero")
      case value :: rest if values.exists(!_.isExact) =>
        Inexact(rest.foldLeft(value.toDouble)(_ / _.toDouble))
      case value :: rest =>
        rest.foldLeft(value) { (acc, next) =>
          val (leftNum, leftDen)   = toExactFraction(acc)
          val (rightNum, rightDen) = toExactFraction(next)
          normalize(leftNum * rightDen, leftDen * rightNum)
        }
      case Nil =>
        ExactInt(1L)

  def abs(number: SchemeNumber): SchemeNumber =
    number match
      case ExactInt(value) =>
        ExactInt(math.abs(value))
      case ExactRational(numerator, denominator) =>
        ExactRational(math.abs(numerator), denominator)
      case Inexact(value) =>
        Inexact(math.abs(value))

  def min(values: List[SchemeNumber]): SchemeNumber =
    values.reduceLeft { (left, right) =>
      if compare(left, right) <= 0 then left else right
    }

  def max(values: List[SchemeNumber]): SchemeNumber =
    values.reduceLeft { (left, right) =>
      if compare(left, right) >= 0 then left else right
    }

  def isZero(number: SchemeNumber): Boolean =
    number match
      case ExactInt(value) =>
        value == 0L
      case ExactRational(numerator, _) =>
        numerator == 0L
      case Inexact(value) =>
        value == 0.0

  def exactToInexact(number: SchemeNumber): SchemeNumber =
    number match
      case exact if exact.isExact => Inexact(exact.toDouble)
      case inexact                => inexact

  def inexactToExact(number: SchemeNumber): SchemeNumber =
    number match
      case Inexact(value) =>
        val decimal  = BigDecimal.decimal(value).bigDecimal.stripTrailingZeros()
        val scale    = decimal.scale()
        val unscaled = BigInt(decimal.unscaledValue())
        if scale <= 0 then normalize(unscaled * BigInt(10).pow(-scale), BigInt(1))
        else normalize(unscaled, BigInt(10).pow(scale))
      case exact =>
        exact

  def numerator(number: SchemeNumber): Long =
    number match
      case ExactInt(value) =>
        value
      case ExactRational(value, _) =>
        value
      case Inexact(_) =>
        throw new IllegalArgumentException("numerator requires an exact number")

  def denominator(number: SchemeNumber): Long =
    number match
      case ExactInt(_) =>
        1L
      case ExactRational(_, value) =>
        value
      case Inexact(_) =>
        throw new IllegalArgumentException("denominator requires an exact number")

  private def parseRationalToken(token: String): Option[SchemeNumber] =
    token.split("/", 2).toList match
      case numeratorText :: denominatorText :: Nil =>
        for
          numerator   <- numeratorText.toLongOption
          denominator <- denominatorText.toLongOption
          if denominator != 0L
        yield normalize(BigInt(numerator), BigInt(denominator))
      case _ =>
        None

  private def compareExact(left: SchemeNumber, right: SchemeNumber): Int =
    val (leftNum, leftDen)   = toExactFraction(left)
    val (rightNum, rightDen) = toExactFraction(right)
    (leftNum * rightDen).compare(rightNum * leftDen)

  private def toExactFraction(number: SchemeNumber): (BigInt, BigInt) =
    number match
      case ExactInt(value) =>
        (BigInt(value), BigInt(1))
      case ExactRational(numerator, denominator) =>
        (BigInt(numerator), BigInt(denominator))
      case Inexact(_) =>
        throw new IllegalArgumentException("expected an exact number")

  private def normalize(numerator: BigInt, denominator: BigInt): SchemeNumber =
    val signAdjusted =
      if denominator.signum < 0 then (-numerator, -denominator)
      else (numerator, denominator)

    val divisor     = signAdjusted._1.gcd(signAdjusted._2)
    val normalizedN = signAdjusted._1 / divisor
    val normalizedD = signAdjusted._2 / divisor

    if normalizedD == 1 then ExactInt(toLong(normalizedN))
    else ExactRational(toLong(normalizedN), toLong(normalizedD))

  private def toLong(value: BigInt): Long =
    if value.isValidLong then value.longValue
    else throw new ArithmeticException(s"numeric overflow: $value")
