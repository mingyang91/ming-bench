package ming

import SchemeValue.*

/** Numeric built-in procedures. */
object NumericBuiltins:

  @scala.annotation.tailrec
  def gcd(a: Long, b: Long): Long =
    if b == 0 then a else gcd(b, a % b)

  private def lcm(a: Long, b: Long): Long =
    if a == 0 || b == 0 then 0 else math.abs(a / gcd(a, b) * b)

  /** Create a rational, normalizing sign and reducing. Returns SchemeInt if den==1. */
  def makeRational(num: Long, den: Long): SchemeValue =
    if den == 0 then throw new EvalError("/: division by zero")
    val sign = if den < 0 then -1L else 1L
    val n    = num * sign
    val d    = den * sign
    val g    = math.abs(gcd(n, d))
    val rn   = n / g
    val rd   = d / g
    if rd == 1 then SchemeInt(rn) else SchemeRational(rn, rd)

  /** Convert any numeric value to a Double. */
  def toDouble(v: SchemeValue): Double = v match
    case SchemeInt(n)         => n.toDouble
    case SchemeRational(n, d) => n.toDouble / d.toDouble
    case SchemeFloat(f)       => f
    case other                => throw new EvalError(s"expected number, got: ${other.display}")

  /** Check if a value is any numeric type. */
  def isNumber(v: SchemeValue): Boolean = v match
    case _: SchemeInt | _: SchemeRational | _: SchemeFloat => true
    case _                                                 => false

  /** Check if a value is exact. */
  def isExact(v: SchemeValue): Boolean = v match
    case _: SchemeInt | _: SchemeRational => true
    case _                                => false

  // --- Binary operations on the numeric tower ---

  private def addTwo(a: SchemeValue, b: SchemeValue): SchemeValue = (a, b) match
    case (SchemeInt(x), SchemeInt(y))         => SchemeInt(x + y)
    case (SchemeInt(x), SchemeRational(n, d)) => makeRational(x * d + n, d)
    case (SchemeRational(n, d), SchemeInt(y)) => makeRational(n + y * d, d)
    case (SchemeRational(n1, d1), SchemeRational(n2, d2)) =>
      makeRational(n1 * d2 + n2 * d1, d1 * d2)
    case _ => SchemeFloat(toDouble(a) + toDouble(b))

  private def subTwo(a: SchemeValue, b: SchemeValue): SchemeValue = (a, b) match
    case (SchemeInt(x), SchemeInt(y))         => SchemeInt(x - y)
    case (SchemeInt(x), SchemeRational(n, d)) => makeRational(x * d - n, d)
    case (SchemeRational(n, d), SchemeInt(y)) => makeRational(n - y * d, d)
    case (SchemeRational(n1, d1), SchemeRational(n2, d2)) =>
      makeRational(n1 * d2 - n2 * d1, d1 * d2)
    case _ => SchemeFloat(toDouble(a) - toDouble(b))

  private def mulTwo(a: SchemeValue, b: SchemeValue): SchemeValue = (a, b) match
    case (SchemeInt(x), SchemeInt(y))         => SchemeInt(x * y)
    case (SchemeInt(x), SchemeRational(n, d)) => makeRational(x * n, d)
    case (SchemeRational(n, d), SchemeInt(y)) => makeRational(n * y, d)
    case (SchemeRational(n1, d1), SchemeRational(n2, d2)) =>
      makeRational(n1 * n2, d1 * d2)
    case _ => SchemeFloat(toDouble(a) * toDouble(b))

  private def divTwo(a: SchemeValue, b: SchemeValue): SchemeValue = (a, b) match
    case (SchemeInt(x), SchemeInt(y)) =>
      if y == 0 then throw new EvalError("/: division by zero")
      makeRational(x, y)
    case (SchemeInt(x), SchemeRational(n, d)) =>
      if n == 0 then throw new EvalError("/: division by zero")
      makeRational(x * d, n)
    case (SchemeRational(n, d), SchemeInt(y)) =>
      if y == 0 then throw new EvalError("/: division by zero")
      makeRational(n, d * y)
    case (SchemeRational(n1, d1), SchemeRational(n2, d2)) =>
      if n2 == 0 then throw new EvalError("/: division by zero")
      makeRational(n1 * d2, d1 * n2)
    case _ =>
      val dv = toDouble(b)
      if dv == 0.0 then throw new EvalError("/: division by zero")
      SchemeFloat(toDouble(a) / dv)

  private def negateNum(v: SchemeValue): SchemeValue = v match
    case SchemeInt(n)         => SchemeInt(-n)
    case SchemeRational(n, d) => SchemeRational(-n, d)
    case SchemeFloat(f)       => SchemeFloat(-f)
    case other                => throw new EvalError(s"expected number, got: ${other.display}")

  // --- Public arithmetic entry points ---

  def evalPlus(args: List[SchemeValue]): SchemeValue =
    args.foldLeft(SchemeInt(0L): SchemeValue)(addTwo)

  def evalMul(args: List[SchemeValue]): SchemeValue =
    args.foldLeft(SchemeInt(1L): SchemeValue)(mulTwo)

  def evalMinus(args: List[SchemeValue]): SchemeValue =
    if args.isEmpty then throw new EvalError("-: expected at least 1 argument")
    else if args.length == 1 then negateNum(args.head)
    else args.reduceLeft(subTwo)

  def evalDivide(args: List[SchemeValue]): SchemeValue =
    if args.length < 2 then throw new EvalError("/: expected at least 2 arguments")
    else args.reduceLeft(divTwo)

  def compareOp(
    args: List[SchemeValue],
    cmp: (Double, Double) => Boolean
  ): SchemeValue =
    if args.length != 2 then throw new EvalError("comparison: expected 2 arguments")
    SchemeBool(cmp(toDouble(args.head), toDouble(args(1))))

  def evalAbs(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("abs: expected 1 argument")
    args.head match
      case SchemeInt(n)         => SchemeInt(math.abs(n))
      case SchemeRational(n, d) => SchemeRational(math.abs(n), d)
      case SchemeFloat(f)       => SchemeFloat(math.abs(f))
      case other                => throw new EvalError(s"abs: not a number: ${other.display}")

  def evalModulo(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("modulo: expected 2 arguments")
    val (a, b) = (Builtins.asInt(args.head), Builtins.asInt(args(1)))
    if b == 0 then throw new EvalError("modulo: division by zero")
    SchemeInt(math.floorMod(a, b))

  def evalRemainder(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("remainder: expected 2 arguments")
    val (a, b) = (Builtins.asInt(args.head), Builtins.asInt(args(1)))
    if b == 0 then throw new EvalError("remainder: division by zero")
    SchemeInt(a % b)

  def evalQuotient(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("quotient: expected 2 arguments")
    val (a, b) = (Builtins.asInt(args.head), Builtins.asInt(args(1)))
    if b == 0 then throw new EvalError("quotient: division by zero")
    SchemeInt(a / b)

  def evalMinMax(args: List[SchemeValue], min: Boolean): SchemeValue =
    if args.isEmpty then throw new EvalError(s"${if min then "min" else "max"}: expected at least 1 argument")
    val doubles = args.map(toDouble)
    val result  = if min then doubles.min else doubles.max
    // Preserve exactness if all args are exact
    if args.forall(isExact) then args.find(v => toDouble(v) == result).getOrElse(SchemeFloat(result))
    else SchemeFloat(result)

  def evalExpt(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("expt: expected 2 arguments")
    val (base, exp) = (Builtins.asInt(args.head), Builtins.asInt(args(1)))
    SchemeInt(longPow(base, exp))

  @scala.annotation.tailrec
  private def longPow(base: Long, exp: Long, acc: Long = 1L): Long =
    if exp <= 0 then acc
    else longPow(base, exp - 1, acc * base)

  def numPred(args: List[SchemeValue], pred: Long => Boolean): SchemeValue =
    if args.length != 1 then throw new EvalError("numeric predicate: expected 1 argument")
    SchemeBool(pred(Builtins.asInt(args.head)))

  def evalGcd(args: List[SchemeValue]): SchemeValue =
    if args.isEmpty then SchemeInt(0)
    else SchemeInt(args.map(Builtins.asInt).map(math.abs).reduceLeft(gcd))

  def evalLcm(args: List[SchemeValue]): SchemeValue =
    if args.isEmpty then SchemeInt(1)
    else SchemeInt(args.map(Builtins.asInt).map(math.abs).reduceLeft(lcm))

  // --- L19 builtins ---

  def evalExactQ(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("exact?: expected 1 argument")
    SchemeBool(isExact(args.head))

  def evalInexactQ(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("inexact?: expected 1 argument")
    SchemeBool(args.head.isInstanceOf[SchemeFloat])

  def evalExactToInexact(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("exact->inexact: expected 1 argument")
    SchemeFloat(toDouble(args.head))

  def evalInexactToExact(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("inexact->exact: expected 1 argument")
    args.head match
      case v if isExact(v) => v
      case SchemeFloat(f)  =>
        // Convert double to rational via its exact representation
        doubleToExact(f)
      case other => throw new EvalError(s"inexact->exact: not a number: ${other.display}")

  private def doubleToExact(f: Double): SchemeValue =
    // Use the approach: multiply by power of 2 to get integer numerator
    val bits = java.lang.Double.doubleToLongBits(f)
    val sign = if (bits >> 63) != 0 then -1L else 1L
    val exp  = ((bits >> 52) & 0x7ffL).toInt
    val mant =
      if exp == 0 then (bits & 0xfffffffffffffL) << 1
      else (bits & 0xfffffffffffffL) | 0x10000000000000L
    val e = exp - 1075
    if e >= 0 then SchemeInt(sign * mant * (1L << e))
    else makeRational(sign * mant, 1L << (-e))

  def evalNumerator(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("numerator: expected 1 argument")
    args.head match
      case SchemeInt(n)         => SchemeInt(n)
      case SchemeRational(n, _) => SchemeInt(n)
      case other                => throw new EvalError(s"numerator: not an exact number: ${other.display}")

  def evalDenominator(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("denominator: expected 1 argument")
    args.head match
      case SchemeInt(_)         => SchemeInt(1)
      case SchemeRational(_, d) => SchemeInt(d)
      case other                => throw new EvalError(s"denominator: not an exact number: ${other.display}")

  def evalIntegerQ(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("integer?: expected 1 argument")
    SchemeBool(args.head match
      case _: SchemeInt   => true
      case SchemeFloat(f) => f == math.floor(f) && !f.isInfinite
      case _              => false)

  def evalRationalQ(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("rational?: expected 1 argument")
    SchemeBool(args.head match
      case _: SchemeInt | _: SchemeRational => true
      case _                                => false)
