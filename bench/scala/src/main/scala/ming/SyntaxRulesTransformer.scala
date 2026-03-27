package ming

import MacroSyntax.*

final private[ming] case class SyntaxRulesTransformer(
  name: String,
  literalNames: Set[String],
  rules: List[SyntaxRule],
  definitionEnv: Env
) extends SyntaxTransformer:

  override def expand(invocation: Expr.ListExpr): Expr =
    rules.iterator
      .map(rule => matchRule(rule.pattern, invocation.items).map(expandRule(rule, _)))
      .collectFirst { case Some(expanded) => expanded }
      .getOrElse(throw EvalError.at(invocation.pos, s"no matching syntax-rules clause for $name"))

  private def expandRule(rule: SyntaxRule, bindings: PatternBindings): Expr =
    expandTemplate(rule.template, bindings, MacroHygiene.empty(definitionEnv), None)._1

  private def matchRule(pattern: Expr, invocationItems: List[Expr]): Option[PatternBindings] =
    pattern match
      case Expr.ListExpr(patternItems, _) =>
        matchSequence(patternItems, invocationItems)
      case _ =>
        None

  private def matchSequence(patterns: List[Expr], inputs: List[Expr]): Option[PatternBindings] =
    patterns match
      case Nil =>
        Option.when(inputs.isEmpty)(PatternBindings.empty)
      case pattern :: ellipsis :: rest if isEllipsis(ellipsis) =>
        val minRemaining = minimumInputCount(rest)
        if inputs.lengthCompare(minRemaining) < 0 then None
        else
          val repeatCount     = inputs.length - minRemaining
          val repeatedInputs  = inputs.take(repeatCount)
          val remainingInputs = inputs.drop(repeatCount)
          for
            repeatedBindings <- matchRepeated(pattern, repeatedInputs)
            restBindings     <- matchSequence(rest, remainingInputs)
            merged           <- repeatedBindings.merge(restBindings)
          yield merged
      case pattern :: rest =>
        inputs match
          case head :: tail =>
            for
              current <- matchPattern(pattern, head)
              next    <- matchSequence(rest, tail)
              merged  <- current.merge(next)
            yield merged
          case Nil =>
            None

  private def minimumInputCount(patterns: List[Expr]): Int =
    @annotation.tailrec
    def loop(remaining: List[Expr], count: Int): Int =
      remaining match
        case Nil => count
        case _ :: ellipsis :: tail if isEllipsis(ellipsis) =>
          loop(tail, count)
        case _ :: tail =>
          loop(tail, count + 1)

    loop(patterns, 0)

  private def matchRepeated(pattern: Expr, inputs: List[Expr]): Option[PatternBindings] =
    val initial = PatternBindings.withEmptyRepeated(repeatedPatternVariables(pattern))
    inputs.foldLeft[Option[PatternBindings]](Some(initial)) { (acc, input) =>
      for
        current <- acc
        matched <- matchPattern(pattern, input)
      yield current.appendRepeated(matched)
    }

  private def matchPattern(pattern: Expr, input: Expr): Option[PatternBindings] =
    pattern match
      case Expr.Symbol(symbol, _) if isLiteralSymbol(symbol) =>
        matchLiteral(symbol, input)
      case Expr.Symbol(symbol, _) if symbol == Ellipsis =>
        None
      case Expr.Symbol(symbol, _) =>
        PatternBindings.empty.bindSingle(symbol, input)
      case Expr.ListExpr(patternItems, _) =>
        matchListPattern(patternItems, input)
      case Expr.IntLit(value, _) =>
        matchIntLiteral(value, input)
      case Expr.BoolLit(value, _) =>
        matchBoolLiteral(value, input)
      case Expr.StringLit(value, _) =>
        matchStringLiteral(value, input)
      case Expr.CharLit(value, _) =>
        matchCharLiteral(value, input)

  private def isLiteralSymbol(symbol: String): Boolean =
    symbol == name || literalNames.contains(symbol)

  private def matchLiteral(symbol: String, input: Expr): Option[PatternBindings] =
    input match
      case Expr.Symbol(inputName, _) if inputName == symbol =>
        Some(PatternBindings.empty)
      case _ =>
        None

  private def matchListPattern(patternItems: List[Expr], input: Expr): Option[PatternBindings] =
    input match
      case Expr.ListExpr(inputItems, _) =>
        matchSequence(patternItems, inputItems)
      case _ =>
        None

  private def matchIntLiteral(value: Int, input: Expr): Option[PatternBindings] =
    input match
      case Expr.IntLit(other, _) if other == value =>
        Some(PatternBindings.empty)
      case _ =>
        None

  private def matchBoolLiteral(value: Boolean, input: Expr): Option[PatternBindings] =
    input match
      case Expr.BoolLit(other, _) if other == value =>
        Some(PatternBindings.empty)
      case _ =>
        None

  private def matchStringLiteral(value: String, input: Expr): Option[PatternBindings] =
    input match
      case Expr.StringLit(other, _) if other == value =>
        Some(PatternBindings.empty)
      case _ =>
        None

  private def matchCharLiteral(value: Char, input: Expr): Option[PatternBindings] =
    input match
      case Expr.CharLit(other, _) if other == value =>
        Some(PatternBindings.empty)
      case _ =>
        None

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
      case Expr.ListExpr(items, _) =>
        items.zipWithIndex.flatMap {
          case (item, index) if index > 0 && isEllipsis(items(index - 1)) =>
            Nil
          case (item, _) =>
            collectRepeatedCounts(item, bindings)
        }
      case _ =>
        Nil

  private def repeatedPatternVariables(pattern: Expr): Set[String] =
    pattern match
      case Expr.Symbol(symbol, _) if symbol == name || literalNames.contains(symbol) || symbol == Ellipsis =>
        Set.empty
      case Expr.Symbol(symbol, _) =>
        Set(symbol)
      case Expr.ListExpr(items, _) =>
        items.foldLeft(Set.empty[String]) { (acc, item) =>
          acc ++ repeatedPatternVariables(item)
        }
      case _ =>
        Set.empty
