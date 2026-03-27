package ming

private[ming] enum Expr:
  case IntLit(value: Int)
  case BoolLit(value: Boolean)
  case StringLit(value: String)
  case Symbol(name: String)
  case ListExpr(items: List[Expr])

private[ming] enum Value:
  case IntVal(value: Int)
  case BoolVal(value: Boolean)
  case StringVal(value: String)
  case SymbolVal(name: String)
  case EmptyList
  case PairVal(car: Value, cdr: Value)
  case BuiltinProc(name: String)
  case Closure(name: Option[String], params: List[String], body: List[Expr], env: Env)
  case Void
