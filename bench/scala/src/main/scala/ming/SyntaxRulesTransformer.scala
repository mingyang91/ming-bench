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
    SyntaxRuleTemplateExpander.expand(rule.template, bindings, definitionEnv)

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
      case Expr.RationalLit(numerator, denominator, _) =>
        matchRationalLiteral(numerator, denominator, input)
      case Expr.InexactLit(value, _) =>
        matchInexactLiteral(value, input)
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

  private def matchIntLiteral(value: Long, input: Expr): Option[PatternBindings] =
    input match
      case Expr.IntLit(other, _) if other == value =>
        Some(PatternBindings.empty)
      case _ =>
        None

  private def matchRationalLiteral(
    numerator: Long,
    denominator: Long,
    input: Expr
  ): Option[PatternBindings] =
    input match
      case Expr.RationalLit(otherNumerator, otherDenominator, _)
          if otherNumerator == numerator && otherDenominator == denominator =>
        Some(PatternBindings.empty)
      case _ =>
        None

  private def matchInexactLiteral(value: Double, input: Expr): Option[PatternBindings] =
    input match
      case Expr.InexactLit(other, _) if other == value =>
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
