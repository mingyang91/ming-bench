package ming

private[ming] object SchemeRenderer:

  def render(value: Value): String =
    value match
      case Value.IntVal(number)  => number.toString
      case Value.BoolVal(true)   => "#t"
      case Value.BoolVal(false)  => "#f"
      case Value.StringVal(text) => s""""${escapeString(text)}""""
      case Value.SymbolVal(name) => name
      case Value.EmptyList       => "()"
      case pair @ Value.PairVal(_, _) =>
        renderPair(pair)
      case Value.BuiltinProc(_) | Value.Closure(_, _, _, _) =>
        "#<procedure>"
      case Value.Void => ""

  private def renderPair(value: Value): String =
    val builder = new StringBuilder("(")

    def appendList(current: Value, first: Boolean): Unit =
      current match
        case Value.PairVal(car, cdr) =>
          if !first then builder.append(' ')
          builder.append(render(car))
          cdr match
            case Value.EmptyList => ()
            case next @ Value.PairVal(_, _) =>
              appendList(next, first = false)
            case other =>
              builder.append(" . ")
              builder.append(render(other))
        case _ =>
          ()

    appendList(value, first = true)
    builder.append(')')
    builder.toString

  private def escapeString(text: String): String =
    text.flatMap {
      case '\\' => "\\\\"
      case '"'  => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case ch   => ch.toString
    }
