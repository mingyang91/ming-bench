package ming

private[ming] enum Expr:
  def pos: SourcePos

  case IntLit(value: Int, pos: SourcePos)          extends Expr
  case BoolLit(value: Boolean, pos: SourcePos)     extends Expr
  case StringLit(value: String, pos: SourcePos)    extends Expr
  case Symbol(name: String, pos: SourcePos)        extends Expr
  case ListExpr(items: List[Expr], pos: SourcePos) extends Expr

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
