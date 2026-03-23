package ming

/** AST / runtime value type for the Scheme interpreter. */
enum SchemeValue:
  case SInteger(value: Long)
  case SBoolean(value: Boolean)
  case SString(value: String)
  case SSymbol(name: String)
  case SList(elements: List[SchemeValue], pos: (Int, Int) = (0, 0))
  case SPair(car: SchemeValue, cdr: SchemeValue)
  case SNil
  case SLambda(params: List[String], body: List[SchemeValue], closure: Environment)
  case SBuiltin(name: String, fn: List[SchemeValue] => SchemeValue)
  case SVoid
  case SChar(value: Char)

  /** Write form — strings are quoted. Used by `write` and as the default representation. */
  def display: String = this match
    case SInteger(v)      => v.toString
    case SBoolean(v)      => if v then "#t" else "#f"
    case SString(v)       => "\"" + v + "\""
    case SSymbol(n)       => n
    case SList(elems, _)  => "(" + elems.map(_.display).mkString(" ") + ")"
    case SNil             => "()"
    case SPair(_, _)      => formatPair(this, quoted = true)
    case SLambda(_, _, _) => "#<procedure>"
    case SBuiltin(n, _)   => s"#<procedure:$n>"
    case SVoid            => "#<void>"
    case SChar(c)         => s"#\\$c"

  /** Display form — strings are unquoted. Used by Scheme `display`. */
  def displayForm: String = this match
    case SString(v)  => v
    case SPair(_, _) => formatPair(this, quoted = false)
    case _           => display

  private def formatPair(p: SchemeValue, quoted: Boolean): String =
    val sb = new StringBuilder("(")
    @scala.annotation.tailrec
    def loop(cur: SchemeValue, first: Boolean): Unit = cur match
      case SPair(a, d) =>
        if !first then sb.append(" ")
        sb.append(if quoted then a.display else a.displayForm)
        loop(d, false)
      case SNil => ()
      case other =>
        sb.append(" . ")
        sb.append(if quoted then other.display else other.displayForm)
    loop(p, true)
    sb.append(")")
    sb.toString
