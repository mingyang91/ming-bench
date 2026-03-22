package ming

/** Scheme value representation. */
enum SchemeValue:
  case SchemeInt(value: Long)
  case SchemeBool(value: Boolean)
  case SchemeString(value: String)
  case SchemeList(elements: List[SchemeValue])
  case SchemeSymbol(name: String)

  def display: String = this match
    case SchemeInt(v)    => v.toString
    case SchemeBool(v)   => if v then "#t" else "#f"
    case SchemeString(v) => s""""$v""""
    case SchemeSymbol(n) => n
    case SchemeList(es)  => s"(${es.map(_.display).mkString(" ")})"
