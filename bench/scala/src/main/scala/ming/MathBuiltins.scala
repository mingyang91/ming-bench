package ming

/** Arithmetic and numeric built-in procedures. */
object MathBuiltins:

  /** Internal numeric representation for arithmetic. */
  private enum Num:
    case Exact(num: Long, den: Long)
    case Inexact(d: Double)

  private def toNum(v: Value, pos: Option[(Int, Int)]): Num =
    v match
      case Value.IntVal(n)         => Num.Exact(n, 1L)
      case Value.RationalVal(n, d) => Num.Exact(n, d)
      case Value.DoubleVal(d)      => Num.Inexact(d)
      case other =>
        throw EvalError.withPos(
          s"expected number, got: ${other.display}",
          pos
        )

  private def toValue(n: Num): Value = n match
    case Num.Exact(num, den) => Value.makeRational(num, den)
    case Num.Inexact(d)      => Value.DoubleVal(d)

  private def toDouble(n: Num): Double = n match
    case Num.Exact(num, den) => num.toDouble / den.toDouble
    case Num.Inexact(d)      => d

  private def addNum(a: Num, b: Num): Num = (a, b) match
    case (Num.Exact(an, ad), Num.Exact(bn, bd)) =>
      Num.Exact(an * bd + bn * ad, ad * bd)
    case _ => Num.Inexact(toDouble(a) + toDouble(b))

  private def subNum(a: Num, b: Num): Num = (a, b) match
    case (Num.Exact(an, ad), Num.Exact(bn, bd)) =>
      Num.Exact(an * bd - bn * ad, ad * bd)
    case _ => Num.Inexact(toDouble(a) - toDouble(b))

  private def mulNum(a: Num, b: Num): Num = (a, b) match
    case (Num.Exact(an, ad), Num.Exact(bn, bd)) =>
      Num.Exact(an * bn, ad * bd)
    case _ => Num.Inexact(toDouble(a) * toDouble(b))

  private def divNum(
    a: Num,
    b: Num,
    pos: Option[(Int, Int)]
  ): Num =
    (a, b) match
      case (Num.Exact(an, ad), Num.Exact(bn, bd)) =>
        if bn == 0L then throw EvalError.withPos("division by zero", pos)
        Num.Exact(an * bd, ad * bn)
      case _ =>
        val dv = toDouble(b)
        if dv == 0.0 then throw EvalError.withPos("division by zero", pos)
        Num.Inexact(toDouble(a) / dv)

  private[ming] def asInt(
    v: Value,
    pos: Option[(Int, Int)]
  ): Long = v match
    case Value.IntVal(n) => n
    case other =>
      throw EvalError.withPos(
        s"expected integer, got: ${other.display}",
        pos
      )

  def evalAdd(
    args: List[Value],
    pos: Option[(Int, Int)]
  ): Value =
    toValue(
      args.foldLeft(Num.Exact(0L, 1L): Num)((acc, v) => addNum(acc, toNum(v, pos)))
    )

  def evalSub(
    args: List[Value],
    pos: Option[(Int, Int)]
  ): Value = args match
    case Nil =>
      throw new EvalError("- requires at least 1 argument")
    case head :: Nil =>
      toValue(subNum(Num.Exact(0L, 1L), toNum(head, pos)))
    case head :: tail =>
      toValue(
        tail.foldLeft(toNum(head, pos))((a, v) => subNum(a, toNum(v, pos)))
      )

  def evalMul(
    args: List[Value],
    pos: Option[(Int, Int)]
  ): Value =
    toValue(
      args.foldLeft(Num.Exact(1L, 1L): Num)((acc, v) => mulNum(acc, toNum(v, pos)))
    )

  def evalDiv(
    args: List[Value],
    pos: Option[(Int, Int)]
  ): Value = args match
    case Nil =>
      throw new EvalError("/ requires at least 1 argument")
    case head :: Nil =>
      toValue(divNum(Num.Exact(1L, 1L), toNum(head, pos), pos))
    case head :: tail =>
      toValue(
        tail.foldLeft(toNum(head, pos))((a, v) => divNum(a, toNum(v, pos), pos))
      )

  def evalCmp(
    args: List[Value],
    op: (Double, Double) => Boolean,
    pos: Option[(Int, Int)]
  ): Value =
    args match
      case a :: b :: Nil =>
        Value.BoolVal(
          op(toDouble(toNum(a, pos)), toDouble(toNum(b, pos)))
        )
      case _ =>
        throw new EvalError("comparison requires exactly 2 arguments")

  def evalAbs(args: List[Value], pos: Option[(Int, Int)]): Value =
    args match
      case v :: Nil =>
        toNum(v, pos) match
          case Num.Exact(n, d) =>
            Value.makeRational(math.abs(n), d)
          case Num.Inexact(d) => Value.DoubleVal(math.abs(d))
      case _ =>
        throw new EvalError("abs requires exactly 1 argument")

  def evalModulo(
    args: List[Value],
    pos: Option[(Int, Int)]
  ): Value =
    args match
      case a :: b :: Nil =>
        val x = asInt(a, pos); val y = asInt(b, pos)
        if y == 0L then throw EvalError.withPos("division by zero", pos)
        Value.IntVal(math.floorMod(x, y))
      case _ =>
        throw new EvalError("modulo requires exactly 2 arguments")

  def evalRemainder(
    args: List[Value],
    pos: Option[(Int, Int)]
  ): Value =
    args match
      case a :: b :: Nil =>
        val x = asInt(a, pos); val y = asInt(b, pos)
        if y == 0L then throw EvalError.withPos("division by zero", pos)
        Value.IntVal(x % y)
      case _ =>
        throw new EvalError(
          "remainder requires exactly 2 arguments"
        )

  def evalQuotient(
    args: List[Value],
    pos: Option[(Int, Int)]
  ): Value =
    args match
      case a :: b :: Nil =>
        val x = asInt(a, pos); val y = asInt(b, pos)
        if y == 0L then throw EvalError.withPos("division by zero", pos)
        Value.IntVal(x / y)
      case _ =>
        throw new EvalError(
          "quotient requires exactly 2 arguments"
        )

  def evalMinMax(
    args: List[Value],
    pos: Option[(Int, Int)],
    op: (Double, Double) => Double
  ): Value =
    args match
      case Nil =>
        throw new EvalError("min/max requires at least 1 argument")
      case head :: tail =>
        val start = toNum(head, pos)
        val result = tail.foldLeft(start) { (a, v) =>
          val b  = toNum(v, pos)
          val da = toDouble(a); val db = toDouble(b)
          if op(da, db) == da then a else b
        }
        toValue(result)

  def evalExpt(
    args: List[Value],
    pos: Option[(Int, Int)]
  ): Value =
    args match
      case base :: exp :: Nil =>
        val b = asInt(base, pos); val e = asInt(exp, pos)
        Value.IntVal(longPow(b, e))
      case _ =>
        throw new EvalError("expt requires exactly 2 arguments")

  @scala.annotation.tailrec
  private def longPow(
    base: Long,
    exp: Long,
    acc: Long = 1L
  ): Long =
    if exp <= 0L then acc
    else longPow(base, exp - 1L, acc * base)

  def numPred(
    args: List[Value],
    pos: Option[(Int, Int)],
    pred: Long => Boolean
  ): Value =
    args match
      case v :: Nil => Value.BoolVal(pred(asInt(v, pos)))
      case _ =>
        throw new EvalError(
          "numeric predicate requires exactly 1 argument"
        )

  def isNumeric(v: Value): Boolean = v match
    case _: Value.IntVal      => true
    case _: Value.RationalVal => true
    case _: Value.DoubleVal   => true
    case _                    => false

  def isExact(v: Value): Boolean = v match
    case _: Value.IntVal      => true
    case _: Value.RationalVal => true
    case _                    => false

  def isInteger(v: Value): Boolean = v match
    case _: Value.IntVal => true
    case _               => false

  def exactToInexact(
    v: Value,
    pos: Option[(Int, Int)]
  ): Value =
    Value.DoubleVal(toDouble(toNum(v, pos)))

  def inexactToExact(
    v: Value,
    pos: Option[(Int, Int)]
  ): Value =
    v match
      case Value.DoubleVal(d) =>
        doubleToExact(d)
      case Value.IntVal(_) | Value.RationalVal(_, _) => v
      case other =>
        throw EvalError.withPos(
          s"expected number, got: ${other.display}",
          pos
        )

  private def doubleToExact(d: Double): Value =
    val bits   = java.lang.Double.doubleToLongBits(d)
    val sign   = if (bits >> 63) != 0 then -1L else 1L
    val rawExp = ((bits >> 52) & 0x7ffL).toInt
    val mant =
      if rawExp == 0 then bits & 0xfffffffffffffL
      else (bits & 0xfffffffffffffL) | (1L << 52)
    val exp = rawExp - 1023 - 52
    if exp >= 0 then Value.IntVal(sign * mant * (1L << exp))
    else
      val den = 1L << (-exp)
      Value.makeRational(sign * mant, den)
