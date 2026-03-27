package ming

private[ming] object ListExprSupport:

  final private[ming] case class DecodedList(
    items: List[Expr],
    tail: Option[Expr]
  )

  def decode(
    items: List[Expr],
    position: Position,
    context: String
  ): DecodedList =
    val (prefix, remainder) = items.span(item => !isDotMarker(item))

    remainder match
      case Nil =>
        DecodedList(prefix, None)
      case SymbolExpr(".", _) :: tail :: Nil if prefix.nonEmpty && !isDotMarker(tail) =>
        DecodedList(prefix, Some(tail))
      case _ =>
        SchemeFailure.raise(s"$context contains a malformed dotted list", position)

  def build(
    items: List[Expr],
    tail: Option[Expr],
    position: Position,
    context: String
  ): Expr =
    tail match
      case None =>
        ListExpr(items, position)
      case Some(tailExpression) =>
        appendTail(items, tailExpression, position, context)

  def appendTail(
    items: List[Expr],
    tailExpression: Expr,
    position: Position,
    context: String
  ): Expr =
    tailExpression match
      case ListExpr(tailItems, tailPosition) =>
        val decoded = decode(tailItems, tailPosition, context)
        build(items ++ decoded.items, decoded.tail, position, context)
      case other =>
        ListExpr(items ++ List(SymbolExpr(".", position), other), position)

  def requireProperList(expression: Expr, context: String): List[Expr] =
    expression match
      case ListExpr(items, position) =>
        val decoded = decode(items, position, context)
        decoded.tail match
          case None =>
            decoded.items
          case Some(_) =>
            SchemeFailure.raise(s"$context expected a proper list", position)
      case _ =>
        SchemeFailure.raise(s"$context expected a list", expression.position)

  def isDotMarker(expression: Expr): Boolean =
    expression match
      case SymbolExpr(".", _) => true
      case _                  => false
