package ming

private[ming] object BuiltinSyntaxes:

  val quasiquote: SyntaxTransformer = QuasiquoteTransformer

private object QuasiquoteTransformer extends SyntaxTransformer:

  override def expand(invocation: Expr.ListExpr): Expr =
    invocation match
      case Expr.ListExpr(Expr.Symbol("quasiquote", pos) :: expr :: Nil, _) =>
        QuasiquoteExpander.expand(expr, pos)
      case Expr.ListExpr(_ :: args, pos) =>
        throw EvalError.at(pos, s"quasiquote expects exactly 1 argument, got ${args.length}")
      case _ =>
        throw EvalError.at(invocation.pos, "invalid quasiquote")

private object QuasiquoteExpander:

  private enum Segment:
    case Item(expr: Expr)
    case Splice(expr: Expr)

  def expand(expr: Expr, pos: SourcePos): Expr =
    expandExpr(expr, depth = 0, pos)

  private def expandExpr(expr: Expr, depth: Int, pos: SourcePos): Expr =
    unquoteArg(expr) match
      case Some(argument) if depth == 0 =>
        argument
      case Some(argument) =>
        buildList(
          List(quoteSymbol("unquote", pos), expandExpr(argument, depth - 1, pos)),
          tail = None,
          pos
        )
      case None =>
        unquoteSplicingArg(expr) match
          case Some(_) if depth == 0 =>
            throw EvalError.at(expr.pos, "unquote-splicing is only valid within a list or vector quasiquote")
          case Some(argument) =>
            buildList(
              List(quoteSymbol("unquote-splicing", pos), expandExpr(argument, depth - 1, pos)),
              tail = None,
              pos
            )
          case None =>
            quasiquoteArg(expr) match
              case Some(argument) =>
                buildList(
                  List(quoteSymbol("quasiquote", pos), expandExpr(argument, depth + 1, pos)),
                  tail = None,
                  pos
                )
              case None =>
                expr match
                  case Expr.ListExpr(items, listPos) =>
                    expandList(ExprListSupport.parseItems(items, "quasiquote"), depth, listPos)
                  case Expr.VectorExpr(items, vectorPos) =>
                    makeCall(
                      "list->vector",
                      List(expandList(ExprListSupport.ExprListParts(items, None), depth, vectorPos)),
                      vectorPos
                    )
                  case _ =>
                    quoteExpr(expr, pos)

  private def expandList(
    parts: ExprListSupport.ExprListParts,
    depth: Int,
    pos: SourcePos
  ): Expr =
    val initial =
      parts.tail match
        case Some(tailExpr) =>
          expandTail(tailExpr, depth, pos)
        case None =>
          quoteExpr(Expr.ListExpr(Nil, pos), pos)

    parts.items.reverse.foldLeft(initial) { (acc, item) =>
      expandSegment(item, depth, pos) match
        case Segment.Item(valueExpr) =>
          makeCall("cons", List(valueExpr, acc), pos)
        case Segment.Splice(listExpr) =>
          makeCall("append", List(listExpr, acc), pos)
    }

  private def expandTail(expr: Expr, depth: Int, pos: SourcePos): Expr =
    unquoteArg(expr) match
      case Some(argument) if depth == 0 =>
        argument
      case _ =>
        unquoteSplicingArg(expr) match
          case Some(_) if depth == 0 =>
            throw EvalError.at(expr.pos, "unquote-splicing is not valid in dotted quasiquote tails")
          case _ =>
            expandExpr(expr, depth, pos)

  private def expandSegment(expr: Expr, depth: Int, pos: SourcePos): Segment =
    unquoteSplicingArg(expr) match
      case Some(argument) if depth == 0 =>
        Segment.Splice(argument)
      case _ =>
        Segment.Item(expandExpr(expr, depth, pos))

  private def quoteExpr(expr: Expr, pos: SourcePos): Expr =
    Expr.ListExpr(List(Expr.Symbol("quote", pos), expr), pos)

  private def quoteSymbol(name: String, pos: SourcePos): Expr =
    quoteExpr(Expr.Symbol(name, pos), pos)

  private def buildList(items: List[Expr], tail: Option[Expr], pos: SourcePos): Expr =
    items.reverse.foldLeft(tail.getOrElse(quoteExpr(Expr.ListExpr(Nil, pos), pos))) { (acc, item) =>
      makeCall("cons", List(item, acc), pos)
    }

  private def makeCall(name: String, args: List[Expr], pos: SourcePos): Expr =
    Expr.ListExpr(Expr.Symbol(name, pos) :: args, pos)

  private def quasiquoteArg(expr: Expr): Option[Expr] =
    unaryForm(expr, "quasiquote")

  private def unquoteArg(expr: Expr): Option[Expr] =
    unaryForm(expr, "unquote")

  private def unquoteSplicingArg(expr: Expr): Option[Expr] =
    unaryForm(expr, "unquote-splicing")

  private def unaryForm(expr: Expr, name: String): Option[Expr] =
    expr match
      case Expr.ListExpr(items, _) =>
        ExprListSupport.parseItems(items, s"$name form") match
          case ExprListSupport.ExprListParts(Expr.Symbol(`name`, _) :: argument :: Nil, None) =>
            Some(argument)
          case _ =>
            None
      case _ =>
        None
