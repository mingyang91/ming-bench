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
    case (Expr.Pair(c1), Expr.Pair(c2))                 => c1 eq c2
    case _                                              => a eq b

  def schemeEqual(a: Expr, b: Expr): Boolean =
    val visited = java.util.Collections.newSetFromMap(
      new java.util.IdentityHashMap[(AnyRef, AnyRef), java.lang.Boolean]()
    )
    equalRec(a, b, visited)

  private def equalRec(
    a: Expr,
    b: Expr,
    visited: java.util.Set[(AnyRef, AnyRef)]
  ): Boolean = (a, b) match
    case (Expr.Num(x), Expr.Num(y))                     => x == y
    case (Expr.Rational(n1, d1), Expr.Rational(n2, d2)) => n1 == n2 && d1 == d2
    case (Expr.Real(x), Expr.Real(y))                   => x == y
    case (Expr.Bool(x), Expr.Bool(y))                   => x == y
    case (Expr.Sym(x), Expr.Sym(y))                     => x == y
    case (Expr.Chr(x), Expr.Chr(y))                     => x == y
    case (Expr.Str(x, _), Expr.Str(y, _))               => java.util.Arrays.equals(x, y)
    case (Expr.Pair(c1), Expr.Pair(c2)) =>
      if c1 eq c2 then true
      else
        val key = (c1.asInstanceOf[AnyRef], c2.asInstanceOf[AnyRef])
        if !visited.add(key) then true // already comparing, assume equal
        else equalRec(c1.car, c2.car, visited) && equalRec(c1.cdr, c2.cdr, visited)
    case (Expr.Lst(xs), Expr.Lst(ys)) =>
      xs.length == ys.length && xs.zip(ys).forall((a, b) => equalRec(a, b, visited))
    case (Expr.Pair(_), Expr.Lst(_)) | (Expr.Lst(_), Expr.Pair(_)) =>
      // Mixed pair/lst comparison: convert both to walk
      equalMixed(a, b, visited)
    case (Expr.Vec(xs), Expr.Vec(ys)) =>
      xs.length == ys.length && xs.zip(ys).forall((a, b) => equalRec(a, b, visited))
    case _ => false

  private def equalMixed(
    a: Expr,
    b: Expr,
    visited: java.util.Set[(AnyRef, AnyRef)]
  ): Boolean =
    (a, b) match
      case (Expr.Lst(Nil), Expr.Lst(Nil)) => true
      case (Expr.Pair(c1), Expr.Pair(c2)) =>
        equalRec(c1.car, c2.car, visited) && equalMixed(c1.cdr, c2.cdr, visited)
      case (Expr.Pair(c1), Expr.Lst(h :: t)) =>
        equalRec(c1.car, h, visited) && equalMixed(c1.cdr, Expr.Lst(t), visited)
      case (Expr.Lst(h :: t), Expr.Pair(c2)) =>
        equalRec(h, c2.car, visited) && equalMixed(Expr.Lst(t), c2.cdr, visited)
      case (Expr.Lst(xs), Expr.Lst(ys)) =>
        xs.length == ys.length && xs.zip(ys).forall((a, b) => equalRec(a, b, visited))
      case _ => false
