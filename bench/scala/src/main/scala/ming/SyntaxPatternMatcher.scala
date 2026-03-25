package ming

private[ming] object SyntaxPatternMatcher:

  import SchemeInterpreter.Expr
  import SyntaxRules.{Capture, Ellipsis}
  import Capture.*

  def matchRule(
    pattern: Expr.ListExpr,
    call: Expr.ListExpr,
    literals: Set[String]
  ): Option[Map[String, Capture]] =
    if pattern.items.isEmpty || call.items.isEmpty then None
    else matchItems(pattern.items.tail, call.items.tail, literals)

  def collectTemplateVariables(
    template: Expr,
    patternVariables: Set[String]
  ): Set[String] =
    template match
      case Expr.Symbol(name, _) if patternVariables.contains(name) =>
        Set(name)

      case Expr.Symbol(_, _) =>
        Set.empty

      case Expr.ListExpr(Expr.Symbol("quote", _) :: _, _) =>
        Set.empty

      case Expr.ListExpr(items, _) =>
        items.foldLeft(Set.empty[String]) { (acc, item) =>
          acc ++ collectTemplateVariables(item, patternVariables)
        }

      case _ =>
        Set.empty

  private def matchItems(
    patterns: List[Expr],
    inputs: List[Expr],
    literals: Set[String]
  ): Option[Map[String, Capture]] =
    patterns match
      case Nil =>
        Option.when(inputs.isEmpty)(Map.empty)

      case pattern :: Expr.Symbol(Ellipsis, _) :: rest =>
        val minRemaining = minPatternLength(rest)
        val maxRepeat    = inputs.length - minRemaining
        if maxRepeat < 0 then None
        else
          (maxRepeat to 0 by -1).iterator
            .flatMap { repeatCount =>
              val repeatedInputs = inputs.take(repeatCount)
              val remaining      = inputs.drop(repeatCount)

              for
                repeatedBindings <- matchRepeated(pattern, repeatedInputs, literals)
                restBindings     <- matchItems(rest, remaining, literals)
                merged           <- mergeBindings(repeatedBindings, restBindings)
              yield merged
            }
            .take(1)
            .toList
            .headOption

      case pattern :: rest =>
        inputs match
          case input :: remaining =>
            for
              current <- matchExpr(pattern, input, literals)
              next    <- matchItems(rest, remaining, literals)
              merged  <- mergeBindings(current, next)
            yield merged

          case Nil =>
            None

  private def matchRepeated(
    pattern: Expr,
    inputs: List[Expr],
    literals: Set[String]
  ): Option[Map[String, Capture]] =
    val variables = collectPatternVariables(pattern, literals)
    val perMatch = inputs.foldLeft(Option(List.empty[Map[String, Capture]])) {
      case (Some(acc), input) =>
        matchExpr(pattern, input, literals).map(acc :+ _)
      case (None, _) =>
        None
    }

    perMatch.map { matches =>
      variables.iterator.map { name =>
        name -> Repeated(matches.map(_.getOrElse(name, Repeated(Nil))))
      }.toMap
    }

  private def matchExpr(
    pattern: Expr,
    input: Expr,
    literals: Set[String]
  ): Option[Map[String, Capture]] =
    pattern match
      case Expr.Symbol("_", _) =>
        Some(Map.empty)

      case Expr.Symbol(name, _) if isPatternVariable(name, literals) =>
        Some(Map(name -> Single(input)))

      case Expr.Symbol(name, _) =>
        input match
          case Expr.Symbol(other, _) if other == name => Some(Map.empty)
          case _                                      => None

      case Expr.ListExpr(patternItems, _) =>
        input match
          case Expr.ListExpr(inputItems, _) => matchItems(patternItems, inputItems, literals)
          case _                            => None

      case Expr.Number(value, _) =>
        input match
          case Expr.Number(other, _) if other == value => Some(Map.empty)
          case _                                       => None

      case Expr.Bool(value, _) =>
        input match
          case Expr.Bool(other, _) if other == value => Some(Map.empty)
          case _                                     => None

      case Expr.StringLit(value, _) =>
        input match
          case Expr.StringLit(other, _) if other == value => Some(Map.empty)
          case _                                          => None

      case Expr.Character(value, _) =>
        input match
          case Expr.Character(other, _) if other == value => Some(Map.empty)
          case _                                          => None

  private def minPatternLength(patterns: List[Expr]): Int =
    patterns match
      case Nil =>
        0

      case _ :: Expr.Symbol(Ellipsis, _) :: rest =>
        minPatternLength(rest)

      case _ :: rest =>
        1 + minPatternLength(rest)

  private def collectPatternVariables(
    pattern: Expr,
    literals: Set[String]
  ): Set[String] =
    pattern match
      case Expr.Symbol("_", _) =>
        Set.empty

      case Expr.Symbol(name, _) if isPatternVariable(name, literals) =>
        Set(name)

      case Expr.Symbol(_, _) =>
        Set.empty

      case Expr.ListExpr(items, _) =>
        items.foldLeft(Set.empty[String]) { (acc, item) =>
          acc ++ collectPatternVariables(item, literals)
        }

      case _ =>
        Set.empty

  private def isPatternVariable(name: String, literals: Set[String]): Boolean =
    name != Ellipsis && name != "_" && !literals.contains(name)

  private def mergeBindings(
    left: Map[String, Capture],
    right: Map[String, Capture]
  ): Option[Map[String, Capture]] =
    right.foldLeft(Option(left)) {
      case (Some(acc), (name, value)) =>
        acc.get(name) match
          case Some(existing) if !sameCapture(existing, value) => None
          case Some(_)                                         => Some(acc)
          case None                                            => Some(acc.updated(name, value))

      case (None, _) =>
        None
    }

  private def sameCapture(left: Capture, right: Capture): Boolean =
    (left, right) match
      case (Single(a), Single(b)) =>
        sameSyntax(a, b)

      case (Repeated(a), Repeated(b)) =>
        a.length == b.length && a.zip(b).forall { case (leftValue, rightValue) =>
          sameCapture(leftValue, rightValue)
        }

      case _ =>
        false

  private def sameSyntax(left: Expr, right: Expr): Boolean =
    (left, right) match
      case (Expr.Number(a, _), Expr.Number(b, _))       => a == b
      case (Expr.Bool(a, _), Expr.Bool(b, _))           => a == b
      case (Expr.StringLit(a, _), Expr.StringLit(b, _)) => a == b
      case (Expr.Character(a, _), Expr.Character(b, _)) => a == b
      case (Expr.Symbol(a, _), Expr.Symbol(b, _))       => a == b
      case (Expr.ListExpr(a, _), Expr.ListExpr(b, _)) =>
        a.length == b.length && a.zip(b).forall { case (leftItem, rightItem) =>
          sameSyntax(leftItem, rightItem)
        }
      case _ => false
