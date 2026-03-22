package ming

/** Scheme value representation. */
enum SchemeValue:
  case SchemeInt(value: Long)
  case SchemeBool(value: Boolean)
  case SchemeString(value: String)
  case SchemeSymbol(name: String)
  case SchemeList(elements: List[SchemeValue])
  case SchemeNil

  def display: String = this match
    case SchemeInt(v)    => v.toString
    case SchemeBool(v)   => if v then "#t" else "#f"
    case SchemeString(v) => s"\"$v\""
    case SchemeSymbol(n) => n
    case SchemeNil       => "()"
    case SchemeList(elems) =>
      elems.map(_.display).mkString("(", " ", ")")
