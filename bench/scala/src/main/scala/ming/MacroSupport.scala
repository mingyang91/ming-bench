package ming

final private[ming] case class MatchBindings(
  single: Map[String, Expr] = Map.empty,
  repeated: Map[String, Vector[Expr]] = Map.empty
):

  def merge(other: MatchBindings): Option[MatchBindings] =
    val mergedSingleOpt = other.single.foldLeft(Option(single)) {
      case (Some(acc), (name, _)) if repeated.contains(name) =>
        None

      case (Some(acc), (name, expr)) =>
        acc.get(name) match
          case Some(existing) if !MacroSupport.sameExpr(existing, expr) =>
            None

          case Some(_) =>
            Some(acc)

          case None =>
            Some(acc.updated(name, expr))

      case (None, _) =>
        None
    }

    mergedSingleOpt.flatMap { mergedSingle =>
      val mergedRepeatedOpt = other.repeated.foldLeft(Option(repeated)) {
        case (Some(acc), (name, _)) if mergedSingle.contains(name) =>
          None

        case (Some(acc), (name, values)) =>
          acc.get(name) match
            case Some(existing) if existing != values =>
              None

            case Some(_) =>
              Some(acc)

            case None =>
              Some(acc.updated(name, values))

        case (None, _) =>
          None
      }

      mergedRepeatedOpt.map(mergedRepeated => MatchBindings(mergedSingle, mergedRepeated))
    }

  def appendRepeated(other: MatchBindings): Option[MatchBindings] =
    if other.repeated.nonEmpty || other.single.keySet.exists(single.contains) then None
    else
      val mergedRepeated = other.single.foldLeft(repeated) { case (acc, (name, expr)) =>
        acc.updated(name, acc.getOrElse(name, Vector.empty) :+ expr)
      }

      Some(copy(repeated = mergedRepeated))

private[ming] object MacroSupport:

  val syntaxKeywords = Set(
    "and",
    "begin",
    "cond",
    "define",
    "define-syntax",
    "else",
    "if",
    "lambda",
    "let",
    "or",
    "quote",
    "set!",
    "syntax-rules"
  )

  def repeatedPatternVariables(pattern: Expr, literals: Set[String]): Set[String] =
    pattern match
      case Expr.Symbol(name, _) if !literals.contains(name) && name != "..." =>
        Set(name)

      case Expr.ListExpr(items, _) =>
        items.foldLeft(Set.empty[String]) { (acc, item) =>
          acc ++ repeatedPatternVariables(item, literals)
        }

      case _ =>
        Set.empty

  def sameExpr(left: Expr, right: Expr): Boolean =
    (left, right) match
      case (Expr.IntAtom(leftValue, _), Expr.IntAtom(rightValue, _)) =>
        leftValue == rightValue

      case (
            Expr.RationalAtom(leftNumerator, leftDenominator, _),
            Expr.RationalAtom(rightNumerator, rightDenominator, _)
          ) =>
        leftNumerator == rightNumerator && leftDenominator == rightDenominator

      case (Expr.InexactAtom(leftValue, _), Expr.InexactAtom(rightValue, _)) =>
        leftValue == rightValue

      case (Expr.BoolAtom(leftValue, _), Expr.BoolAtom(rightValue, _)) =>
        leftValue == rightValue

      case (Expr.StringAtom(leftValue, _), Expr.StringAtom(rightValue, _)) =>
        leftValue == rightValue

      case (Expr.CharAtom(leftValue, _), Expr.CharAtom(rightValue, _)) =>
        leftValue == rightValue

      case (Expr.Symbol(leftName, _), Expr.Symbol(rightName, _)) =>
        leftName == rightName

      case (Expr.ListExpr(leftItems, _), Expr.ListExpr(rightItems, _)) =>
        leftItems.length == rightItems.length && leftItems.zip(rightItems).forall(sameExpr.tupled)

      case _ =>
        false

  def exprPos(expr: Expr): SourcePos =
    expr match
      case Expr.IntAtom(_, pos)         => pos
      case Expr.RationalAtom(_, _, pos) => pos
      case Expr.InexactAtom(_, pos)     => pos
      case Expr.BoolAtom(_, pos)        => pos
      case Expr.StringAtom(_, pos)      => pos
      case Expr.CharAtom(_, pos)        => pos
      case Expr.Symbol(_, pos)          => pos
      case Expr.ListExpr(_, pos)        => pos
