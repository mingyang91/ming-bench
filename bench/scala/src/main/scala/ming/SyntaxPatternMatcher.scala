package ming

import MacroSyntax.*

private[ming] object SyntaxPatternMatcher:

  def matchPattern(pattern: Expr, input: Expr, literalNames: Set[String]): Option[PatternBindings] =
    pattern match
      case Expr.Symbol("_", _) =>
        Some(PatternBindings.empty)
      case Expr.Symbol(symbol, _) if literalNames.contains(symbol) =>
        matchLiteral(symbol, input)
      case Expr.Symbol(symbol, _) if symbol == Ellipsis =>
        None
      case Expr.Symbol(symbol, _) =>
        PatternBindings.empty.bindSingle(symbol, input)
      case Expr.VectorExpr(patternItems, _) =>
        matchVectorPattern(patternItems, input, literalNames)
      case Expr.ListExpr(patternItems, _) =>
        matchListPattern(patternItems, input, literalNames)
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

  private def matchSequence(
    patterns: List[Expr],
    inputs: List[Expr],
    literalNames: Set[String]
  ): Option[PatternBindings] =
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
            repeatedBindings <- matchRepeated(pattern, repeatedInputs, literalNames)
            restBindings     <- matchSequence(rest, remainingInputs, literalNames)
            merged           <- repeatedBindings.merge(restBindings)
          yield merged
      case pattern :: rest =>
        inputs match
          case head :: tail =>
            for
              current <- matchPattern(pattern, head, literalNames)
              next    <- matchSequence(rest, tail, literalNames)
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

  private def matchRepeated(
    pattern: Expr,
    inputs: List[Expr],
    literalNames: Set[String]
  ): Option[PatternBindings] =
    val initial = PatternBindings.withEmptyRepeated(repeatedPatternVariables(pattern, literalNames))
    inputs.foldLeft[Option[PatternBindings]](Some(initial)) { (acc, input) =>
      for
        current <- acc
        matched <- matchPattern(pattern, input, literalNames)
      yield current.appendRepeated(matched)
    }

  private def matchLiteral(symbol: String, input: Expr): Option[PatternBindings] =
    input match
      case Expr.Symbol(inputName, _) if inputName == symbol =>
        Some(PatternBindings.empty)
      case _ =>
        None

  private def matchListPattern(
    patternItems: List[Expr],
    input: Expr,
    literalNames: Set[String]
  ): Option[PatternBindings] =
    input match
      case Expr.ListExpr(inputItems, _) =>
        matchSequence(patternItems, inputItems, literalNames)
      case _ =>
        None

  private def matchVectorPattern(
    patternItems: List[Expr],
    input: Expr,
    literalNames: Set[String]
  ): Option[PatternBindings] =
    input match
      case Expr.VectorExpr(inputItems, _) =>
        matchSequence(patternItems, inputItems, literalNames)
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

  private def repeatedPatternVariables(pattern: Expr, literalNames: Set[String]): Set[String] =
    pattern match
      case Expr.Symbol(symbol, _) if symbol == "_" || literalNames.contains(symbol) || symbol == Ellipsis =>
        Set.empty
      case Expr.Symbol(symbol, _) =>
        Set(symbol)
      case Expr.VectorExpr(items, _) =>
        items.foldLeft(Set.empty[String]) { (acc, item) =>
          acc ++ repeatedPatternVariables(item, literalNames)
        }
      case Expr.ListExpr(items, _) =>
        items.foldLeft(Set.empty[String]) { (acc, item) =>
          acc ++ repeatedPatternVariables(item, literalNames)
        }
      case _ =>
        Set.empty
