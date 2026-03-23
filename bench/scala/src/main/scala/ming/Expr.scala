package ming

/** Parsed S-expression. */
enum Expr:
  case Num(n: Long)
  case Bool(b: Boolean)
  case Str(s: String)
  case Sym(name: String)
  case SList(elements: List[Expr])
