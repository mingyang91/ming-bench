package ming

import scala.collection.mutable

private[ming] object ExprListSupport:

  final case class ExprListParts(items: List[Expr], tail: Option[Expr])
  final case class ValueListParts(items: List[Value], tail: Option[Value])

  def parseItems(items: List[Expr], context: String): ExprListParts =
    val dotIndexes = items.zipWithIndex.collect { case (Expr.Symbol(".", _), index) => index }
    dotIndexes match
      case Nil =>
        ExprListParts(items, None)
      case index :: Nil =>
        val dotPos = items(index).pos
        if index == 0 then throw EvalError.at(dotPos, s"dot must follow at least one element in $context")
        else
          items.drop(index + 1) match
            case tail :: Nil =>
              ExprListParts(items.take(index), Some(tail))
            case Nil =>
              throw EvalError.at(dotPos, s"dot must be followed by a tail expression in $context")
            case _ =>
              throw EvalError.at(dotPos, s"dot must be followed by a single tail expression in $context")
      case _ :: second :: _ =>
        throw EvalError.at(items(second).pos, s"multiple dots in $context")

  def buildExpr(items: List[Expr], tail: Option[Expr], pos: SourcePos): Expr.ListExpr =
    tail match
      case Some(tailExpr) =>
        Expr.ListExpr(items :+ Expr.Symbol(".", pos) :+ tailExpr, pos)
      case None =>
        Expr.ListExpr(items, pos)

  def parseValueList(value: Value, pos: SourcePos, context: String): ValueListParts =
    val seen = mutable.HashSet.empty[Value.PairVal]

    @annotation.tailrec
    def loop(current: Value, acc: List[Value]): ValueListParts =
      current match
        case Value.EmptyList =>
          ValueListParts(acc.reverse, None)
        case pair: Value.PairVal =>
          if seen.contains(pair) then throw EvalError.at(pos, s"$context expected an acyclic list")
          seen += pair
          loop(pair.cdr, pair.car :: acc)
        case other =>
          ValueListParts(acc.reverse, Some(other))

    loop(value, Nil)
