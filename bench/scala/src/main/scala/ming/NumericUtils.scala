package ming

private[ming] object NumericUtils:

  def gcd(a: Long, b: Long): Long =
    val x = math.abs(a)
    val y = math.abs(b)
    if y == 0 then x else gcd(y, x % y)

  def makeRational(n: Long, d: Long): Expr =
    if d == 0 then throw EvalError("division by zero")
    val sign = if d < 0 then -1L else 1L
    val nn   = n * sign
    val dd   = d * sign
    val g    = gcd(math.abs(nn), dd)
    val rn   = nn / g
    val rd   = dd / g
    if rd == 1 then Expr.Num(rn)
    else Expr.Rational(rn, rd)

  // Convert any numeric Expr to Double
  def toDouble(e: Expr): Double = e match
    case Expr.Num(n)         => n.toDouble
    case Expr.Rational(n, d) => n.toDouble / d.toDouble
    case Expr.Real(v)        => v
    case _                   => throw EvalError(s"expected number, got ${Builtins.display(e)}")

  def isExact(e: Expr): Boolean = e match
    case Expr.Num(_)         => true
    case Expr.Rational(_, _) => true
    case Expr.Real(_)        => false
    case _                   => false

  def isNumber(e: Expr): Boolean = e match
    case Expr.Num(_) | Expr.Rational(_, _) | Expr.Real(_) => true
    case _                                                => false

  def isInteger(e: Expr): Boolean = e match
    case Expr.Num(_)         => true
    case Expr.Rational(_, d) => d == 1 // already simplified, so this shouldn't happen
    case Expr.Real(v)        => v == v.floor && !v.isInfinite
    case _                   => false

  // Exact arithmetic helpers
  def addExact(a: Expr, b: Expr): Expr = (a, b) match
    case (Expr.Num(x), Expr.Num(y))                     => Expr.Num(x + y)
    case (Expr.Num(x), Expr.Rational(n, d))             => makeRational(x * d + n, d)
    case (Expr.Rational(n, d), Expr.Num(y))             => makeRational(n + y * d, d)
    case (Expr.Rational(n1, d1), Expr.Rational(n2, d2)) => makeRational(n1 * d2 + n2 * d1, d1 * d2)
    case _                                              => throw EvalError("addExact: not exact numbers")

  def subExact(a: Expr, b: Expr): Expr = (a, b) match
    case (Expr.Num(x), Expr.Num(y))                     => Expr.Num(x - y)
    case (Expr.Num(x), Expr.Rational(n, d))             => makeRational(x * d - n, d)
    case (Expr.Rational(n, d), Expr.Num(y))             => makeRational(n - y * d, d)
    case (Expr.Rational(n1, d1), Expr.Rational(n2, d2)) => makeRational(n1 * d2 - n2 * d1, d1 * d2)
    case _                                              => throw EvalError("subExact: not exact numbers")

  def mulExact(a: Expr, b: Expr): Expr = (a, b) match
    case (Expr.Num(x), Expr.Num(y))                     => Expr.Num(x * y)
    case (Expr.Num(x), Expr.Rational(n, d))             => makeRational(x * n, d)
    case (Expr.Rational(n, d), Expr.Num(y))             => makeRational(n * y, d)
    case (Expr.Rational(n1, d1), Expr.Rational(n2, d2)) => makeRational(n1 * n2, d1 * d2)
    case _                                              => throw EvalError("mulExact: not exact numbers")

  def divExact(a: Expr, b: Expr): Expr = (a, b) match
    case (Expr.Num(x), Expr.Num(y))                     => makeRational(x, y)
    case (Expr.Num(x), Expr.Rational(n, d))             => makeRational(x * d, n)
    case (Expr.Rational(n, d), Expr.Num(y))             => makeRational(n, d * y)
    case (Expr.Rational(n1, d1), Expr.Rational(n2, d2)) => makeRational(n1 * d2, d1 * n2)
    case _                                              => throw EvalError("divExact: not exact numbers")

  def negateExact(a: Expr): Expr = a match
    case Expr.Num(x)         => Expr.Num(-x)
    case Expr.Rational(n, d) => Expr.Rational(-n, d)
    case _                   => throw EvalError("negateExact: not exact number")
