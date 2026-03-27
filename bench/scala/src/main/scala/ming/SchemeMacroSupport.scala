package ming

private[ming] object MacroSyntax:

  val Ellipsis = "..."

  private val ReservedSyntaxNames = Set(
    "and",
    "begin",
    "cond",
    "define",
    "define-syntax",
    "guard",
    "if",
    "lambda",
    "let",
    "or",
    "quasiquote",
    "quote",
    "set!",
    "syntax-rules",
    "unquote",
    "unquote-splicing"
  )

  def isEllipsis(expr: Expr): Boolean =
    expr match
      case Expr.Symbol(`Ellipsis`, _) => true
      case _                          => false

  def freshIdentifier(base: String): String =
    val suffix = java.util.UUID.randomUUID().toString.replace('-', '_')
    s"__macro_${suffix}_$base"

  def shouldPreserveBinding(name: String, definitionEnv: Env): Boolean =
    ReservedSyntaxNames.contains(name) || definitionEnv.lookupSyntax(name).isDefined

  def syntaxEquals(left: Expr, right: Expr): Boolean =
    (left, right) match
      case (Expr.IntLit(leftValue, _), Expr.IntLit(rightValue, _)) => leftValue == rightValue
      case (Expr.RationalLit(leftN, leftD, _), Expr.RationalLit(rightN, rightD, _)) =>
        leftN == rightN && leftD == rightD
      case (Expr.InexactLit(leftValue, _), Expr.InexactLit(rightValue, _)) => leftValue == rightValue
      case (Expr.BoolLit(leftValue, _), Expr.BoolLit(rightValue, _))       => leftValue == rightValue
      case (Expr.StringLit(leftValue, _), Expr.StringLit(rightValue, _))   => leftValue == rightValue
      case (Expr.CharLit(leftValue, _), Expr.CharLit(rightValue, _))       => leftValue == rightValue
      case (Expr.Symbol(leftName, _), Expr.Symbol(rightName, _))           => leftName == rightName
      case (Expr.ListExpr(leftItems, _), Expr.ListExpr(rightItems, _)) =>
        leftItems.lengthCompare(rightItems.length) == 0 &&
        leftItems.zip(rightItems).forall { case (leftItem, rightItem) =>
          syntaxEquals(leftItem, rightItem)
        }
      case (Expr.VectorExpr(leftItems, _), Expr.VectorExpr(rightItems, _)) =>
        leftItems.lengthCompare(rightItems.length) == 0 &&
        leftItems.zip(rightItems).forall { case (leftItem, rightItem) =>
          syntaxEquals(leftItem, rightItem)
        }
      case _ =>
        false

final private[ming] case class SyntaxRule(pattern: Expr, template: Expr)

final private[ming] case class PatternBindings(
  single: Map[String, Expr],
  repeated: Map[String, Vector[Expr]]
):

  def bindSingle(name: String, expr: Expr): Option[PatternBindings] =
    single.get(name) match
      case Some(existing) if MacroSyntax.syntaxEquals(existing, expr) =>
        Some(this)
      case Some(_) =>
        None
      case None if repeated.contains(name) =>
        None
      case None =>
        Some(copy(single = single.updated(name, expr)))

  def merge(other: PatternBindings): Option[PatternBindings] =
    val mergedSingles = other.single.foldLeft[Option[Map[String, Expr]]](Some(single)) { case (acc, (name, expr)) =>
      acc.flatMap(mergeSingleValue(_, name, expr))
    }

    mergedSingles.flatMap { combinedSingles =>
      other.repeated
        .foldLeft[Option[Map[String, Vector[Expr]]]](Some(repeated)) { case (acc, (name, values)) =>
          acc.flatMap(mergeRepeatedValues(combinedSingles, _, name, values))
        }
        .map(PatternBindings(combinedSingles, _))
    }

  def appendRepeated(other: PatternBindings): PatternBindings =
    val appendedSingles = other.single.foldLeft(repeated) { case (acc, (name, expr)) =>
      acc.updated(name, acc.getOrElse(name, Vector.empty) :+ expr)
    }
    val appendedRepeated = other.repeated.foldLeft(appendedSingles) { case (acc, (name, values)) =>
      acc.updated(name, acc.getOrElse(name, Vector.empty) ++ values)
    }
    copy(repeated = appendedRepeated)

  private def mergeSingleValue(
    mergedSingle: Map[String, Expr],
    name: String,
    expr: Expr
  ): Option[Map[String, Expr]] =
    if repeated.contains(name) then None
    else
      mergedSingle.get(name) match
        case Some(existing) if MacroSyntax.syntaxEquals(existing, expr) =>
          Some(mergedSingle)
        case Some(_) =>
          None
        case None =>
          Some(mergedSingle.updated(name, expr))

  private def mergeRepeatedValues(
    mergedSingle: Map[String, Expr],
    mergedRepeated: Map[String, Vector[Expr]],
    name: String,
    values: Vector[Expr]
  ): Option[Map[String, Vector[Expr]]] =
    if mergedSingle.contains(name) then None
    else
      mergedRepeated.get(name) match
        case Some(existing) if existing == values =>
          Some(mergedRepeated)
        case Some(_) =>
          None
        case None =>
          Some(mergedRepeated.updated(name, values))

private[ming] object PatternBindings:

  val empty: PatternBindings = PatternBindings(Map.empty, Map.empty)

  def withEmptyRepeated(names: Set[String]): PatternBindings =
    PatternBindings(
      single = Map.empty,
      repeated = names.iterator.map(name => name -> Vector.empty[Expr]).toMap
    )

final private[ming] case class MacroHygiene(
  definitionEnv: Env,
  renames: Map[String, String]
):

  def rewriteSymbol(name: String, pos: SourcePos): (Expr, MacroHygiene) =
    if MacroSyntax.shouldPreserveBinding(name, definitionEnv) then (Expr.Symbol(name, pos), this)
    else
      renames.get(name) match
        case Some(rewritten) =>
          (Expr.Symbol(rewritten, pos), this)
        case None =>
          val fresh = MacroSyntax.freshIdentifier(name)
          aliasDefinition(originalName = name, freshName = fresh)
          (
            Expr.Symbol(fresh, pos),
            copy(renames = renames.updated(name, fresh))
          )

  private def aliasDefinition(originalName: String, freshName: String): Unit =
    definitionEnv.lookupValueCell(originalName) match
      case Some(cell) =>
        definitionEnv.defineAlias(freshName, cell)
      case None =>
        Builtins.resolve(originalName).foreach(value => definitionEnv.define(freshName, value))

private[ming] object MacroHygiene:

  def empty(definitionEnv: Env): MacroHygiene =
    MacroHygiene(definitionEnv, Map.empty)
