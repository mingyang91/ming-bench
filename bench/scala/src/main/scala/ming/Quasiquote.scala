package ming

private[ming] object Quasiquote:

  def evalQuasiquote(args: List[Expr], env: Env, k: Kont): MState =
    if args.size != 1 then throw new EvalError("quasiquote: expected 1 argument")
    val expanded = expandQuasiquote(args.head, env, 0)
    SEval(expanded, env, k)

  private def expandQuasiquote(expr: Expr, env: Env, depth: Int): Expr =
    expr match
      case SList(Symbol("unquote", _) :: inner :: Nil, pos) =>
        if depth == 0 then inner // splice directly
        else
          SList(
            List(
              Symbol("list", pos),
              SList(List(Symbol("quote", pos), Symbol("unquote", pos)), pos),
              expandQuasiquote(inner, env, depth - 1)
            ),
            pos
          )
      case SList(Symbol("quasiquote", _) :: inner :: Nil, pos) =>
        SList(
          List(
            Symbol("list", pos),
            SList(List(Symbol("quote", pos), Symbol("quasiquote", pos)), pos),
            expandQuasiquote(inner, env, depth + 1)
          ),
          pos
        )
      case SList(elems, pos) =>
        val dotIdx = elems.lastIndexWhere { case Symbol(".", _) => true; case _ => false }
        if dotIdx > 0 && dotIdx == elems.size - 2 then
          val headExprs = elems.take(dotIdx).map(e => expandQQElement(e, env, depth, pos))
          val tailExpr  = expandQuasiquote(elems.last, env, depth)
          headExprs.foldRight(tailExpr) { (elem, acc) =>
            SList(List(Symbol("cons", pos), elem, acc), pos)
          }
        else
          val expanded = elems.map(e => expandQQElement(e, env, depth, pos))
          if expanded.exists {
              case SList(Symbol("__qq_splice", _) :: _, _) => true; case _ => false
            }
          then
            val parts = expanded.map {
              case SList(Symbol("__qq_splice", _) :: inner :: Nil, p) => inner
              case other                                              => SList(List(Symbol("list", pos), other), pos)
            }
            parts.reduceLeft((a, b) => SList(List(Symbol("append", pos), a, b), pos))
          else SList(Symbol("list", pos) :: expanded, pos)
      case _ =>
        SList(List(Symbol("quote", expr.pos), expr), expr.pos)

  private def expandQQElement(expr: Expr, env: Env, depth: Int, parentPos: Pos): Expr =
    expr match
      case SList(Symbol("unquote-splicing", _) :: inner :: Nil, pos) =>
        if depth == 0 then SList(List(Symbol("__qq_splice", pos), inner), pos)
        else expandQuasiquote(expr, env, depth)
      case _ => expandQuasiquote(expr, env, depth)
