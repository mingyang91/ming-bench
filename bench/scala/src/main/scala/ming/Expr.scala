package ming

enum Expr:
  case Literal(value: Value, position: SourcePos)
  case Symbol(name: String, position: SourcePos)
  case ListExpr(items: List[Expr], position: SourcePos)

  def sourcePos: SourcePos = this match
    case Expr.Literal(_, position)  => position
    case Expr.Symbol(_, position)   => position
    case Expr.ListExpr(_, position) => position
