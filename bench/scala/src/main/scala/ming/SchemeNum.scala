package ming

object SchemeNum:

  def gcd(a: Long, b: Long): Long =
    val aa = math.abs(a)
    val bb = math.abs(b)
    if bb == 0 then aa else gcd(bb, aa % bb)

  def makeRational(n: Long, d: Long): SchemeVal =
    if d == 0 then throw new EvalError("division by zero")
    val sign = if d < 0 then -1L else 1L
    val nn   = n * sign
    val dd   = d * sign
    val g    = gcd(math.abs(nn), dd)
    val sn   = nn / g
    val sd   = dd / g
    if sd == 1 then SchemeVal.IntVal(sn)
    else SchemeVal.RatVal(sn, sd)

  def toDouble(v: SchemeVal): Double = v match
    case SchemeVal.IntVal(n)    => n.toDouble
    case SchemeVal.FloatVal(d)  => d
    case SchemeVal.RatVal(n, d) => n.toDouble / d.toDouble
    case other                  => throw new EvalError(s"expected number, got ${other.display}")

  def isExact(v: SchemeVal): Boolean = v match
    case SchemeVal.IntVal(_)    => true
    case SchemeVal.RatVal(_, _) => true
    case SchemeVal.FloatVal(_)  => false
    case _                      => throw new EvalError(s"exact?: expected number, got ${v.display}")

  def add(a: SchemeVal, b: SchemeVal): SchemeVal = (a, b) match
    case (SchemeVal.IntVal(x), SchemeVal.IntVal(y)) => SchemeVal.IntVal(x + y)
    case (SchemeVal.FloatVal(_), _) | (_, SchemeVal.FloatVal(_)) =>
      SchemeVal.FloatVal(toDouble(a) + toDouble(b))
    case _ =>
      val (an, ad) = toRatParts(a)
      val (bn, bd) = toRatParts(b)
      makeRational(an * bd + bn * ad, ad * bd)

  def sub(a: SchemeVal, b: SchemeVal): SchemeVal = (a, b) match
    case (SchemeVal.IntVal(x), SchemeVal.IntVal(y)) => SchemeVal.IntVal(x - y)
    case (SchemeVal.FloatVal(_), _) | (_, SchemeVal.FloatVal(_)) =>
      SchemeVal.FloatVal(toDouble(a) - toDouble(b))
    case _ =>
      val (an, ad) = toRatParts(a)
      val (bn, bd) = toRatParts(b)
      makeRational(an * bd - bn * ad, ad * bd)

  def mul(a: SchemeVal, b: SchemeVal): SchemeVal = (a, b) match
    case (SchemeVal.IntVal(x), SchemeVal.IntVal(y)) => SchemeVal.IntVal(x * y)
    case (SchemeVal.FloatVal(_), _) | (_, SchemeVal.FloatVal(_)) =>
      SchemeVal.FloatVal(toDouble(a) * toDouble(b))
    case _ =>
      val (an, ad) = toRatParts(a)
      val (bn, bd) = toRatParts(b)
      makeRational(an * bn, ad * bd)

  def div(a: SchemeVal, b: SchemeVal): SchemeVal = (a, b) match
    case (SchemeVal.FloatVal(_), _) | (_, SchemeVal.FloatVal(_)) =>
      SchemeVal.FloatVal(toDouble(a) / toDouble(b))
    case _ =>
      val (an, ad) = toRatParts(a)
      val (bn, bd) = toRatParts(b)
      makeRational(an * bd, ad * bn)

  def negate(a: SchemeVal): SchemeVal = a match
    case SchemeVal.IntVal(n)    => SchemeVal.IntVal(-n)
    case SchemeVal.FloatVal(d)  => SchemeVal.FloatVal(-d)
    case SchemeVal.RatVal(n, d) => SchemeVal.RatVal(-n, d)
    case other                  => throw new EvalError(s"expected number, got ${other.display}")

  def numEq(a: SchemeVal, b: SchemeVal): Boolean =
    toDouble(a) == toDouble(b)

  def numLt(a: SchemeVal, b: SchemeVal): Boolean =
    toDouble(a) < toDouble(b)

  def numGt(a: SchemeVal, b: SchemeVal): Boolean =
    toDouble(a) > toDouble(b)

  def numLe(a: SchemeVal, b: SchemeVal): Boolean =
    toDouble(a) <= toDouble(b)

  def numGe(a: SchemeVal, b: SchemeVal): Boolean =
    toDouble(a) >= toDouble(b)

  private def toRatParts(v: SchemeVal): (Long, Long) = v match
    case SchemeVal.IntVal(n)    => (n, 1L)
    case SchemeVal.RatVal(n, d) => (n, d)
    case other                  => throw new EvalError(s"expected exact number, got ${other.display}")

  def requireNum(name: String, v: SchemeVal): SchemeVal = v match
    case _: SchemeVal.IntVal | _: SchemeVal.FloatVal | _: SchemeVal.RatVal => v
    case other => throw new EvalError(s"$name: expected number, got ${other.display}")
