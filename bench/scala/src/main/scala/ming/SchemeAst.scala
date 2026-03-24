package ming

private[ming] enum Expr:
  case IntegerLiteral(value: Int)
  case BooleanLiteral(value: Boolean)
  case StringLiteral(value: String)
  case Symbol(name: String)
  case ListExpr(elements: List[Expr])
