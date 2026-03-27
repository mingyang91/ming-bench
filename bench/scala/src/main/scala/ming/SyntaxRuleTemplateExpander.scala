package ming

import MacroSyntax.*

private[ming] object SyntaxRuleTemplateExpander:

  def expand(
    template: Expr,
    bindings: PatternBindings,
    definitionEnv: Env
  ): Expr =
    expandTemplate(template, bindings, MacroHygiene.empty(definitionEnv), None)._1

  private def expandTemplate(
    template: Expr,
    bindings: PatternBindings,
    hygiene: MacroHygiene,
    repetitionIndex: Option[Int]
  ): (Expr, MacroHygiene) =
    template match
      case Expr.Symbol(symbol, pos) =>
        expandSymbol(symbol, pos, bindings, hygiene, repetitionIndex)
      case Expr.ListExpr(Expr.Symbol("quote", quotePos) :: datum :: Nil, pos) =>
        (
          Expr.ListExpr(List(Expr.Symbol("quote", quotePos), datum), pos),
          hygiene
        )
      case Expr.VectorExpr(items, pos) =>
        val (expandedItems, nextHygiene) =
          expandTemplateItems(items, bindings, hygiene, repetitionIndex)
        (Expr.VectorExpr(expandedItems, pos), nextHygiene)
      case Expr.ListExpr(items, pos) =>
        val (expandedItems, nextHygiene) =
          expandTemplateItems(items, bindings, hygiene, repetitionIndex)
        (Expr.ListExpr(expandedItems, pos), nextHygiene)
      case other =>
        (other, hygiene)

  private def expandSymbol(
    symbol: String,
    pos: SourcePos,
    bindings: PatternBindings,
    hygiene: MacroHygiene,
    repetitionIndex: Option[Int]
  ): (Expr, MacroHygiene) =
    bindings.single.get(symbol) match
      case Some(expr) =>
        (expr, hygiene)
      case None =>
        expandRepeatedSymbol(symbol, pos, bindings, hygiene, repetitionIndex)

  private def expandRepeatedSymbol(
    symbol: String,
    pos: SourcePos,
    bindings: PatternBindings,
    hygiene: MacroHygiene,
    repetitionIndex: Option[Int]
  ): (Expr, MacroHygiene) =
    bindings.repeated.get(symbol) match
      case Some(values) =>
        (repeatedBindingValue(symbol, pos, values, repetitionIndex), hygiene)
      case None =>
        hygiene.rewriteSymbol(symbol, pos)

  private def repeatedBindingValue(
    symbol: String,
    pos: SourcePos,
    values: Vector[Expr],
    repetitionIndex: Option[Int]
  ): Expr =
    repetitionIndex match
      case Some(index) if index < values.length =>
        values(index)
      case Some(_) =>
        throw EvalError.at(pos, s"macro expansion index out of bounds for $symbol")
      case None if values.lengthCompare(1) == 0 =>
        values.head
      case None =>
        throw EvalError.at(pos, s"repeated pattern variable used without ellipsis: $symbol")

  private def expandTemplateItems(
    items: List[Expr],
    bindings: PatternBindings,
    hygiene: MacroHygiene,
    repetitionIndex: Option[Int]
  ): (List[Expr], MacroHygiene) =
    @annotation.tailrec
    def loop(
      remaining: List[Expr],
      currentHygiene: MacroHygiene,
      accReversed: List[Expr]
    ): (List[Expr], MacroHygiene) =
      remaining match
        case Nil =>
          (accReversed.reverse, currentHygiene)
        case template :: ellipsis :: tail if isEllipsis(ellipsis) =>
          val count = repeatedTemplateCount(template, bindings)
          val (expandedItems, nextHygiene) =
            expandRepeatedTemplate(template, count, bindings, currentHygiene)
          loop(tail, nextHygiene, expandedItems.reverse ::: accReversed)
        case template :: tail =>
          val (expanded, nextHygiene) =
            expandTemplate(template, bindings, currentHygiene, repetitionIndex)
          loop(tail, nextHygiene, expanded :: accReversed)

    loop(items, hygiene, Nil)

  private def expandRepeatedTemplate(
    template: Expr,
    count: Int,
    bindings: PatternBindings,
    hygiene: MacroHygiene
  ): (List[Expr], MacroHygiene) =
    @annotation.tailrec
    def loop(
      index: Int,
      currentHygiene: MacroHygiene,
      accReversed: List[Expr]
    ): (List[Expr], MacroHygiene) =
      if index >= count then (accReversed.reverse, currentHygiene)
      else
        val (expanded, nextHygiene) =
          expandTemplate(template, bindings, currentHygiene, Some(index))
        loop(index + 1, nextHygiene, expanded :: accReversed)

    loop(index = 0, hygiene, Nil)

  private def repeatedTemplateCount(template: Expr, bindings: PatternBindings): Int =
    collectRepeatedCounts(template, bindings).distinct match
      case Nil =>
        throw EvalError.at(template.pos, "ellipsis template must reference a repeated pattern variable")
      case count :: Nil =>
        count
      case _ =>
        throw EvalError.at(template.pos, "ellipsis template has mismatched repetition counts")

  private def collectRepeatedCounts(template: Expr, bindings: PatternBindings): List[Int] =
    template match
      case Expr.Symbol(symbol, _) =>
        bindings.repeated.get(symbol).map(_.length).toList
      case Expr.ListExpr(Expr.Symbol("quote", _) :: _ :: Nil, _) =>
        Nil
      case Expr.VectorExpr(items, _) =>
        items.zipWithIndex.flatMap {
          case (item, index) if index > 0 && isEllipsis(items(index - 1)) =>
            Nil
          case (item, _) =>
            collectRepeatedCounts(item, bindings)
        }
      case Expr.ListExpr(items, _) =>
        items.zipWithIndex.flatMap {
          case (item, index) if index > 0 && isEllipsis(items(index - 1)) =>
            Nil
          case (item, _) =>
            collectRepeatedCounts(item, bindings)
        }
      case _ =>
        Nil
