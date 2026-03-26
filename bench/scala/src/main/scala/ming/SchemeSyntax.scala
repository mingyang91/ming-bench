package ming

final private[ming] case class Position(line: Int, column: Int)

sealed private[ming] trait Expr:
  def position: Position

final private[ming] case class IntExpr(value: BigInt, position: Position) extends Expr

final private[ming] case class BoolExpr(value: Boolean, position: Position) extends Expr

final private[ming] case class StringExpr(value: String, position: Position) extends Expr

final private[ming] case class CharExpr(value: Char, position: Position) extends Expr

final private[ming] case class SymbolExpr(name: String, position: Position) extends Expr

final private[ming] case class ListExpr(items: List[Expr], position: Position) extends Expr
