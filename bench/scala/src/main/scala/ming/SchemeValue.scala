package ming

/** AST / runtime value type for the Scheme interpreter. */
enum SchemeValue:
  case SInteger(value: Long)
  case SBoolean(value: Boolean)
  case SString(value: String)
  case SSymbol(name: String)
  case SList(elements: List[SchemeValue])
  case SPair(car: SchemeValue, cdr: SchemeValue)
  case SNil
  case SLambda(params: List[String], body: List[SchemeValue], closure: Environment)
  case SBuiltin(name: String, fn: List[SchemeValue] => SchemeValue)
  case SVoid

  def display: String = this match
    case SInteger(v)      => v.toString
    case SBoolean(v)      => if v then "#t" else "#f"
    case SString(v)       => "\"" + v + "\""
    case SSymbol(n)       => n
    case SList(elems)     => "(" + elems.map(_.display).mkString(" ") + ")"
    case SNil             => "()"
    case SPair(_, _)      => displayPair(this)
    case SLambda(_, _, _) => "#<procedure>"
    case SBuiltin(n, _)   => s"#<procedure:$n>"
    case SVoid            => "#<void>"

  private def displayPair(p: SchemeValue): String =
    val sb = new StringBuilder("(")
    @scala.annotation.tailrec
    def loop(cur: SchemeValue, first: Boolean): Unit = cur match
      case SPair(a, d) =>
        if !first then sb.append(" ")
        sb.append(a.display)
        loop(d, false)
      case SNil => ()
      case other =>
        sb.append(" . ")
        sb.append(other.display)
    loop(p, true)
    sb.append(")")
    sb.toString
