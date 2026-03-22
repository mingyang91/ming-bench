package ming

enum SchemeValue:
  case IntVal(value: Long)
  case BoolVal(value: Boolean)
  case StringVal(value: String)
  case SymbolVal(name: String)
  case ListVal(elements: List[SchemeValue])
  case Void

  def display: String = this match
    case IntVal(n)    => n.toString
    case BoolVal(b)   => if b then "#t" else "#f"
    case StringVal(s) => "\"" + s + "\""
    case SymbolVal(n) => n
    case ListVal(es)  => "(" + es.map(_.display).mkString(" ") + ")"
    case Void         => ""

  def isTruthy: Boolean = this match
    case BoolVal(false) => false
    case _              => true
