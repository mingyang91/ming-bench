package ming

private[ming] object EqualityOps:

  def eqv(a: Expr, b: Expr): Boolean = (a, b) match
    case (Expr.Num(x), Expr.Num(y))                     => x == y
    case (Expr.Rational(n1, d1), Expr.Rational(n2, d2)) => n1 == n2 && d1 == d2
    case (Expr.Real(x), Expr.Real(y))                   => x == y
    case (Expr.Bool(x), Expr.Bool(y))                   => x == y
    case (Expr.Sym(x), Expr.Sym(y))                     => x == y
    case (Expr.Chr(x), Expr.Chr(y))                     => x == y
    case (Expr.Lst(Nil), Expr.Lst(Nil))                 => true
    case _                                              => a eq b

  def schemeEqual(a: Expr, b: Expr): Boolean = (a, b) match
    case (Expr.Num(x), Expr.Num(y))                     => x == y
    case (Expr.Rational(n1, d1), Expr.Rational(n2, d2)) => n1 == n2 && d1 == d2
    case (Expr.Real(x), Expr.Real(y))                   => x == y
    case (Expr.Bool(x), Expr.Bool(y))                   => x == y
    case (Expr.Sym(x), Expr.Sym(y))                     => x == y
    case (Expr.Chr(x), Expr.Chr(y))                     => x == y
    case (Expr.Str(x), Expr.Str(y))                     => java.util.Arrays.equals(x, y)
    case (Expr.Lst(xs), Expr.Lst(ys)) =>
      xs.length == ys.length && xs.zip(ys).forall((a, b) => schemeEqual(a, b))
    case (Expr.Pair(a1, d1), Expr.Pair(a2, d2)) =>
      schemeEqual(a1, a2) && schemeEqual(d1, d2)
    case _ => false
