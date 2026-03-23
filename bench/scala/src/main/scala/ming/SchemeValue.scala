package ming

/** AST / runtime value type for the Scheme interpreter. */
enum SchemeValue:
  case SInteger(value: Long)
  case SBoolean(value: Boolean)
  case SString(value: String)
  case SSymbol(name: String)
  case SList(elements: List[SchemeValue])

  def display: String = this match
    case SInteger(v)  => v.toString
    case SBoolean(v)  => if v then "#t" else "#f"
    case SString(v)   => "\"" + v + "\""
    case SSymbol(n)   => n
    case SList(elems) => "(" + elems.map(_.display).mkString(" ") + ")"
