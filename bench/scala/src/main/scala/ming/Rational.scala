package ming

import SchemeValue.*

/** Helpers for exact rational arithmetic and numeric tower operations. */
object Rational:

  def gcd(a: Long, b: Long): Long =
    var x = math.abs(a)
    var y = math.abs(b)
    while y != 0 do
      val t = y
      y = x % y
      x = t
    x

  /** Create a normalized SchemeValue from numerator/denominator. Returns IntVal when denominator divides evenly,
    * RationalVal otherwise.
    */
  def make(num: Long, den: Long): SchemeValue =
    if den == 0 then throw new EvalError("division by zero")
    val sign = if den < 0 then -1L else 1L
    val n    = num * sign
    val d    = den * sign
    val g    = gcd(math.abs(n), d)
    val rn   = n / g
    val rd   = d / g
    if rd == 1 then IntVal(rn)
    else RationalVal(rn, rd)

  def make(num: Long, den: Long, pos: Option[SourcePos]): SchemeValue =
    make(num, den) match
      case IntVal(n, _)         => IntVal(n, pos)
      case RationalVal(n, d, _) => RationalVal(n, d, pos)
      case other                => other

  def isNumeric(v: SchemeValue): Boolean = v match
    case _: IntVal | _: RationalVal | _: DoubleVal => true
    case _                                         => false

  def isExact(v: SchemeValue): Boolean = v match
    case _: IntVal | _: RationalVal => true
    case _                          => false

  def isInexact(v: SchemeValue): Boolean = v match
    case _: DoubleVal => true
    case _            => false

  def hasInexact(args: List[SchemeValue]): Boolean =
    args.exists(_.isInstanceOf[DoubleVal])

  def toDouble(v: SchemeValue): Double = v match
    case IntVal(n, _)         => n.toDouble
    case RationalVal(n, d, _) => n.toDouble / d.toDouble
    case DoubleVal(d, _)      => d
    case _                    => throw new EvalError("expected number")

  def toRational(v: SchemeValue): (Long, Long) = v match
    case IntVal(n, _)         => (n, 1L)
    case RationalVal(n, d, _) => (n, d)
    case _                    => throw new EvalError("expected exact number")

  /** Convert inexact double to exact rational. */
  def doubleToExact(d: Double): SchemeValue =
    if d == math.floor(d) && !d.isInfinite then
      val r = make(d.toLong, 1L)
      r
    else
      val bd    = java.math.BigDecimal.valueOf(d)
      val scale = bd.scale()
      if scale <= 0 then IntVal(bd.longValueExact())
      else
        val unscaled    = bd.unscaledValue().longValueExact()
        val denominator = pow10(scale)
        make(unscaled, denominator)

  private def pow10(n: Int): Long =
    var result = 1L
    var i      = 0
    while i < n do
      result *= 10
      i += 1
    result

  /** Format a Double the way Scheme expects (always with decimal point). */
  def formatDouble(d: Double): String =
    val s = d.toString
    if s.contains('.') || s.contains('E') || s.contains('e') then s
    else s + ".0"
