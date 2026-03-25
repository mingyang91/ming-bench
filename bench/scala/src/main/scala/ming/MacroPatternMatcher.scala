package ming

private[ming] object MacroPatternMatcher:

  import MacroSupport.*

  def parseTransformer(expr: Expr, env: Env, pos: SourcePos): SyntaxMacro =
    expr match
      case Expr.ListExpr(Expr.Symbol("syntax-rules", _) :: Expr.ListExpr(literals, _) :: rules, _) if rules.nonEmpty =>
        val literalNames = literals.map {
          case Expr.Symbol(name, _) =>
            name

          case other =>
            throw EvalError.at(exprPos(other), "syntax-rules literal identifiers must be symbols")
        }.toSet

        val parsedRules = rules.map(parseRule)
        SyntaxMacro(literalNames, parsedRules, env)

      case _ =>
        throw EvalError.at(pos, "define-syntax expects a syntax-rules transformer")

  def expandMacro(
    name: String,
    expr: Expr,
    macroDef: SyntaxMacro,
    macros: MacroState,
    pos: SourcePos
  ): Expr =
    macroDef.rules.iterator
      .map(rule =>
        matchPattern(rule.pattern, expr, macroDef.literals + name).map(bindings =>
          MacroInstantiation.instantiate(rule.template, bindings, macroDef, macros, Map.empty, None)
        )
      )
      .collectFirst { case Some(expanded) => expanded }
      .getOrElse(throw EvalError.at(pos, s"no syntax-rules clause matched: $name"))

  private def parseRule(expr: Expr): SyntaxRule =
    expr match
      case Expr.ListExpr(pattern :: template :: Nil, _) =>
        SyntaxRule(pattern, template)

      case Expr.ListExpr(_, pos) =>
        throw EvalError.at(pos, "syntax-rules clauses must contain exactly a pattern and template")

      case other =>
        throw EvalError.at(exprPos(other), "syntax-rules clauses must be lists")

  def matchPattern(
    pattern: Expr,
    expr: Expr,
    literals: Set[String]
  ): Option[MatchBindings] =
    (pattern, expr) match
      case (Expr.IntAtom(left, _), Expr.IntAtom(right, _)) if left == right =>
        Some(MatchBindings())

      case (Expr.RationalAtom(leftNumerator, leftDenominator, _), Expr.RationalAtom(rightNumerator, rightDenominator, _))
          if leftNumerator == rightNumerator && leftDenominator == rightDenominator =>
        Some(MatchBindings())

      case (Expr.InexactAtom(left, _), Expr.InexactAtom(right, _)) if left == right =>
        Some(MatchBindings())

      case (Expr.BoolAtom(left, _), Expr.BoolAtom(right, _)) if left == right =>
        Some(MatchBindings())

      case (Expr.StringAtom(left, _), Expr.StringAtom(right, _)) if left == right =>
        Some(MatchBindings())

      case (Expr.CharAtom(left, _), Expr.CharAtom(right, _)) if left == right =>
        Some(MatchBindings())

      case (Expr.Symbol(name, _), Expr.Symbol(other, _)) if literals.contains(name) && name == other =>
        Some(MatchBindings())

      case (Expr.Symbol(name, _), _) if !literals.contains(name) && name != "..." =>
        Some(MatchBindings(single = Map(name -> expr)))

      case (Expr.ListExpr(patternItems, _), Expr.ListExpr(exprItems, _)) =>
        matchListPattern(patternItems, exprItems, literals)

      case _ =>
        None

  private def matchListPattern(
    patterns: List[Expr],
    exprs: List[Expr],
    literals: Set[String]
  ): Option[MatchBindings] =
    def loop(
      remainingPatterns: List[Expr],
      remainingExprs: List[Expr],
      acc: MatchBindings
    ): Option[MatchBindings] =
      remainingPatterns match
        case Nil =>
          Option.when(remainingExprs.isEmpty)(acc)

        case pattern :: Expr.Symbol("...", _) :: rest =>
          val minTail = minimumPatternLength(rest)
          if remainingExprs.length < minTail then None
          else
            val maxRepeats = remainingExprs.length - minTail
            (maxRepeats to 0 by -1).iterator
              .map { repeatCount =>
                val (repeatedExprs, tailExprs) = remainingExprs.splitAt(repeatCount)
                for
                  repeatedBindings <- matchRepeatedPattern(pattern, repeatedExprs, literals)
                  merged           <- acc.merge(repeatedBindings)
                  matchedTail      <- loop(rest, tailExprs, merged)
                yield matchedTail
              }
              .collectFirst { case Some(bindings) => bindings }

        case pattern :: rest =>
          remainingExprs match
            case expr :: tail =>
              for
                matched <- matchPattern(pattern, expr, literals)
                merged  <- acc.merge(matched)
                result  <- loop(rest, tail, merged)
              yield result

            case Nil =>
              None

    loop(patterns, exprs, MatchBindings())

  private def matchRepeatedPattern(
    pattern: Expr,
    exprs: List[Expr],
    literals: Set[String]
  ): Option[MatchBindings] =
    val initialRepeated = repeatedPatternVariables(pattern, literals).map(_ -> Vector.empty[Expr]).toMap
    exprs.foldLeft(Option(MatchBindings(repeated = initialRepeated))) { (accOpt, expr) =>
      for
        acc     <- accOpt
        matched <- matchPattern(pattern, expr, literals)
        next    <- acc.appendRepeated(matched)
      yield next
    }

  private def minimumPatternLength(patterns: List[Expr]): Int =
    patterns match
      case Nil =>
        0

      case _ :: Expr.Symbol("...", _) :: rest =>
        minimumPatternLength(rest)

      case _ :: rest =>
        1 + minimumPatternLength(rest)
