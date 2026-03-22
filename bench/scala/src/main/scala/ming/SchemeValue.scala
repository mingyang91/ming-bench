package ming

/** Scheme value representation. */
enum SchemeValue:
  case SchemeInt(value: Long)
  case SchemeBool(value: Boolean)
  case SchemeString(value: String)
  case SchemeSymbol(name: String)
  case SchemeList(elements: List[SchemeValue])
  case SchemeNil
  case SchemeChar(value: Char)
  case SchemeMutableString(chars: Array[Char])

  case SchemeLambda(
    params: List[String],
    body: List[SchemeValue],
    closure: Environment,
    name: Option[String] = None
  )
  case SchemeVoid
  case SchemeLocated(value: SchemeValue, line: Int, col: Int)

  def display: String = this match
    case SchemeInt(v)               => v.toString
    case SchemeBool(v)              => if v then "#t" else "#f"
    case SchemeString(v)            => s"\"$v\""
    case SchemeSymbol(n)            => n
    case SchemeNil                  => "()"
    case SchemeVoid                 => ""
    case SchemeChar(c)              => s"#\\$c"
    case SchemeMutableString(chars) => s"\"${String(chars)}\""
    case _: SchemeLambda            => "#<procedure>"
    case SchemeLocated(v, _, _)     => v.display
    case SchemeList(elems) =>
      elems.map(_.display).mkString("(", " ", ")")

  /** Display representation (no quotes on strings). */
  def toDisplayStr: String = this match
    case SchemeString(v)            => v
    case SchemeMutableString(chars) => String(chars)
    case SchemeList(elems)          => elems.map(_.toDisplayStr).mkString("(", " ", ")")
    case SchemeLocated(v, _, _)     => v.toDisplayStr
    case other                      => other.display
