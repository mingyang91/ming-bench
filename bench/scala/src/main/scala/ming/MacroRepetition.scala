package ming

private[ming] object MacroRepetition:

  import MacroSupport.*

  def boundPatternExpr(
    name: String,
    pos: SourcePos,
    bindings: MatchBindings,
    repetitionIndex: Option[Int]
  ): Option[Expr] =
    bindings.single.get(name).orElse {
      bindings.repeated.get(name).map { matches =>
        repetitionIndex match
          case Some(index) if index >= 0 && index < matches.length =>
            matches(index)

          case Some(_) =>
            throw EvalError.at(pos, s"macro repetition index out of bounds for: $name")

          case None =>
            throw EvalError.at(pos, s"repeated pattern variable used outside ellipsis: $name")
      }
    }

  def repeatedCount(
    template: Expr,
    bindings: MatchBindings,
    shadowedNames: Set[String],
    repetitionIndex: Option[Int]
  ): Int =
    val repeatedNames = collectRepeatedNames(template, bindings, shadowedNames, repetitionIndex)
    if repeatedNames.isEmpty then
      throw EvalError.at(exprPos(template), "ellipsis template must reference a repeated pattern variable")

    val lengths = repeatedNames.map(name => bindings.repeated(name).length)
    if lengths.size != 1 then
      throw EvalError.at(exprPos(template), "ellipsis template variables must repeat the same number of times")

    lengths.head

  private def collectRepeatedNames(
    template: Expr,
    bindings: MatchBindings,
    shadowedNames: Set[String],
    repetitionIndex: Option[Int]
  ): Set[String] =
    template match
      case Expr.Symbol(name, _) if !shadowedNames.contains(name) && bindings.repeated.contains(name) =>
        Set(name)

      case Expr.ListExpr(Expr.Symbol("quote", _) :: _ :: Nil, _) =>
        Set.empty

      case Expr.ListExpr(Expr.Symbol("lambda", _) :: Expr.ListExpr(params, _) :: body, _) if body.nonEmpty =>
        val introduced = binderNames(params, bindings, repetitionIndex)
        body.foldLeft(Set.empty[String]) { (acc, expr) =>
          acc ++ collectRepeatedNames(expr, bindings, shadowedNames ++ introduced, repetitionIndex)
        }

      case Expr.ListExpr(Expr.Symbol("case-lambda", _) :: clauses, _) =>
        clauses.foldLeft(Set.empty[String]) { (acc, clause) =>
          acc ++ (clause match
            case Expr.ListExpr(Expr.ListExpr(params, _) :: body, _) if body.nonEmpty =>
              val introduced = binderNames(params, bindings, repetitionIndex)
              body.foldLeft(Set.empty[String]) { (bodyAcc, expr) =>
                bodyAcc ++ collectRepeatedNames(expr, bindings, shadowedNames ++ introduced, repetitionIndex)
              }

            case other =>
              collectRepeatedNames(other, bindings, shadowedNames, repetitionIndex))
        }

      case Expr.ListExpr(Expr.Symbol("let", _) :: Expr.ListExpr(letBindings, _) :: body, _) if body.nonEmpty =>
        val introduced = letBindings.flatMap {
          case Expr.ListExpr(nameExpr :: _ :: Nil, _) =>
            binderNames(List(nameExpr), bindings, repetitionIndex)

          case _ =>
            Nil
        }.toSet

        val bindingNames = letBindings.foldLeft(Set.empty[String]) {
          case (acc, Expr.ListExpr(_ :: valueExpr :: Nil, _)) =>
            acc ++ collectRepeatedNames(valueExpr, bindings, shadowedNames, repetitionIndex)

          case (acc, _) =>
            acc
        }

        bindingNames ++ body.foldLeft(Set.empty[String]) { (acc, expr) =>
          acc ++ collectRepeatedNames(expr, bindings, shadowedNames ++ introduced, repetitionIndex)
        }

      case Expr.ListExpr(items, _) =>
        items.foldLeft(Set.empty[String]) { (acc, item) =>
          acc ++ collectRepeatedNames(item, bindings, shadowedNames, repetitionIndex)
        }

      case _ =>
        Set.empty

  private def binderNames(
    binders: List[Expr],
    bindings: MatchBindings,
    repetitionIndex: Option[Int]
  ): List[String] =
    binders.flatMap {
      case Expr.Symbol(".", _) =>
        Nil

      case Expr.Symbol(name, pos) =>
        boundPatternExpr(name, pos, bindings, repetitionIndex) match
          case Some(_) =>
            Nil

          case None =>
            List(name)

      case _ =>
        Nil
    }
