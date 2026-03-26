package ming

private[ming] object SchemeSyntax:

  enum Expr:
    case IntegerLiteral(value: BigInt)
    case BooleanLiteral(value: Boolean)
    case StringLiteral(value: String)
    case Symbol(name: String)
    case ListExpr(items: List[Expr])
