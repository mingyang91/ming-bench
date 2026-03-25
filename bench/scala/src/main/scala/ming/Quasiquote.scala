package ming

/** Expands quasiquote templates into explicit cons/list/append/quote expressions. */
private[ming] object Quasiquote:

  /** Expand a quasiquote template into an expression that, when evaluated, produces the desired structure.
    */
  def expand(tmpl: Expr): Expr = expandQQ(tmpl, 0)

  private def expandQQ(tmpl: Expr, depth: Int): Expr = tmpl match
    // (unquote x) at depth 0 => x (evaluate it)
    case Expr.Lst(List(Expr.Sym("unquote"), x)) =>
      if depth == 0 then x
      else
        Expr.Lst(List(Expr.Sym("list"), Expr.Lst(List(Expr.Sym("quote"), Expr.Sym("unquote"))), expandQQ(x, depth - 1)))

    // (quasiquote x) => increment depth
    case Expr.Lst(List(Expr.Sym("quasiquote"), x)) =>
      Expr.Lst(
        List(Expr.Sym("list"), Expr.Lst(List(Expr.Sym("quote"), Expr.Sym("quasiquote"))), expandQQ(x, depth + 1))
      )

    // List - process elements, handling unquote-splicing
    case Expr.Lst(elems) =>
      expandList(elems, depth)

    // Pair (dotted pair) - process car and cdr
    case Expr.Pair(mp) =>
      val expandedCar = mp.car match
        case Expr.Lst(List(Expr.Sym("unquote-splicing"), x)) if depth == 0 =>
          // splicing in car of a pair doesn't make sense as a standalone,
          // but we handle it by appending
          return Expr.Lst(List(Expr.Sym("append"), x, expandQQ(mp.cdr, depth)))
        case other => expandQQ(other, depth)
      Expr.Lst(List(Expr.Sym("cons"), expandedCar, expandQQ(mp.cdr, depth)))

    // Vector
    case Expr.Vec(elems) =>
      Expr.Lst(List(Expr.Sym("list->vector"), expandList(elems.toList, depth)))

    // Atom (symbol, number, string, bool, etc.) => quote it
    case _ =>
      Expr.Lst(List(Expr.Sym("quote"), tmpl))

  private def expandList(elems: List[Expr], depth: Int): Expr =
    if elems.isEmpty then Expr.Lst(List(Expr.Sym("quote"), Expr.Lst(Nil)))
    else
      // Check if any element is (unquote-splicing ...)
      val hasSplice = elems.exists {
        case Expr.Lst(List(Expr.Sym("unquote-splicing"), _)) if depth == 0 => true
        case _                                                             => false
      }
      if hasSplice then
        // Use append to combine segments
        val segments = elems.map {
          case Expr.Lst(List(Expr.Sym("unquote-splicing"), x)) if depth == 0 => x
          case elem => Expr.Lst(List(Expr.Sym("list"), expandQQ(elem, depth)))
        }
        segments match
          case single :: Nil => single
          case _             => Expr.Lst(Expr.Sym("append") :: segments)
      else Expr.Lst(Expr.Sym("list") :: elems.map(expandQQ(_, depth)))
