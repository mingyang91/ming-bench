package ming

private[ming] object Display:

  def display(e: Expr): String = e match
    case Expr.Num(n)                => n.toString
    case Expr.Rational(n, d)        => s"$n/$d"
    case Expr.Real(v)               => formatReal(v)
    case Expr.Bool(true)            => "#t"
    case Expr.Bool(false)           => "#f"
    case Expr.Str(s, _)             => "\"" + new String(s) + "\""
    case Expr.Chr(c)                => s"#\\$c"
    case Expr.Sym(name)             => name
    case Expr.Lst(elems)            => "(" + elems.map(display).mkString(" ") + ")"
    case Expr.Pair(cell)            => displayPair(cell)
    case Expr.Lambda(_, _, _, _)    => "#<procedure>"
    case Expr.CaseLambda(_, _)      => "#<procedure>"
    case Expr.Macro(_, _, _)        => "#<macro>"
    case Expr.Vec(elems)            => "#(" + elems.map(display).mkString(" ") + ")"
    case Expr.Record(name, _, _, _) => s"#<record:$name>"
    case Expr.Cont(_)               => "#<continuation>"
    case Expr.Values(elems)         => elems.map(display).mkString(" ")

  private def displayPair(cell: MutablePair): String =
    val visited = java.util.Collections.newSetFromMap(
      new java.util.IdentityHashMap[MutablePair, java.lang.Boolean]()
    )
    val sb = new StringBuilder("(")
    displayPairContents(cell, visited, sb)
    sb.append(")")
    sb.toString

  private def displayPairContents(
    cell: MutablePair,
    visited: java.util.Set[MutablePair],
    sb: StringBuilder
  ): Unit =
    if !visited.add(cell) then
      sb.append("...")
      return
    sb.append(display(cell.car))
    cell.cdr match
      case Expr.Lst(Nil) => ()
      case Expr.Pair(next) =>
        sb.append(" ")
        displayPairContents(next, visited, sb)
      case Expr.Lst(elems) =>
        for e <- elems do
          sb.append(" ")
          sb.append(display(e))
      case other =>
        sb.append(" . ")
        sb.append(display(other))

  def displayOutput(e: Expr): String = e match
    case Expr.Str(s, _) => new String(s)
    case other          => display(other)

  private def formatReal(v: Double): String =
    if v == v.floor && !v.isInfinite && !v.isNaN then
      val l = v.toLong
      if l.toDouble == v then s"$l.0"
      else v.toString
    else v.toString
