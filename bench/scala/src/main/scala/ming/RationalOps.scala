package ming

/** Rational number arithmetic helpers for Level 19 exact arithmetic. */
object RationalOps:

  @scala.annotation.tailrec
  def gcd(a: Long, b: Long): Long =
    if b == 0 then a else gcd(b, a % b)

  /** Create a simplified rational or integer if denominator is 1. */
  def makeRational(num: Long, den: Long): Value =
    if den == 0 then throw new EvalError("division by zero")
    val sign = if den < 0 then -1L else 1L
    val g    = gcd(math.abs(num), math.abs(den))
    val sNum = sign * num / g
    val sDen = sign * den / g
    if sDen == 1 then Value.Integer(sNum)
    else Value.Rational(sNum, sDen)

  /** Convert a Value to a (numerator, denominator) pair. */
  def toRational(v: Value): (Long, Long) = v match
    case Value.Integer(n)         => (n, 1L)
    case Value.Rational(num, den) => (num, den)
    case _                        => throw new EvalError(s"expected number, got: ${v.display}")

  /** Convert a Value to Double. */
  def toDouble(v: Value): Double = v match
    case Value.Integer(n)         => n.toDouble
    case Value.Rational(num, den) => num.toDouble / den.toDouble
    case Value.Float(d)           => d
    case _                        => throw new EvalError(s"expected number, got: ${v.display}")

  def isExact(v: Value): Boolean = v match
    case Value.Integer(_) | Value.Rational(_, _) => true
    case Value.Float(_)                          => false
    case _                                       => throw new EvalError(s"expected number, got: ${v.display}")

  def isNumber(v: Value): Boolean = v match
    case Value.Integer(_) | Value.Rational(_, _) | Value.Float(_) => true
    case _                                                        => false

  def isInteger(v: Value): Boolean = v match
    case Value.Integer(_) => true
    case Value.Rational(num, den) =>
      val g = gcd(math.abs(num), math.abs(den))
      math.abs(den) / g == 1
    case Value.Float(d) => d == d.floor && !d.isInfinite
    case _              => false

  /** Add two exact values. */
  def addExact(a: (Long, Long), b: (Long, Long)): Value =
    makeRational(a._1 * b._2 + b._1 * a._2, a._2 * b._2)

  /** Subtract two exact values. */
  def subExact(a: (Long, Long), b: (Long, Long)): Value =
    makeRational(a._1 * b._2 - b._1 * a._2, a._2 * b._2)

  /** Multiply two exact values. */
  def mulExact(a: (Long, Long), b: (Long, Long)): Value =
    makeRational(a._1 * b._1, a._2 * b._2)

  /** Divide two exact values. */
  def divExact(a: (Long, Long), b: (Long, Long)): Value =
    if b._1 == 0 then throw new EvalError("division by zero")
    makeRational(a._1 * b._2, a._2 * b._1)

  /** Check if value has any inexact component. */
  def hasInexact(args: List[Value]): Boolean =
    args.exists { case Value.Float(_) => true; case _ => false }

  /** Compare two numeric values as doubles. */
  def compareNumeric(a: Value, b: Value): Int =
    val da = toDouble(a)
    val db = toDouble(b)
    java.lang.Double.compare(da, db)
