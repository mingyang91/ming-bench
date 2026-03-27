package ming

private[ming] object SchemeRenderer:

  def render(value: Value): String =
    renderWithMode(value, displayStrings = false, displayChars = false)

  def renderForDisplay(value: Value): String =
    renderWithMode(value, displayStrings = true, displayChars = true)

  private def renderWithMode(value: Value, displayStrings: Boolean, displayChars: Boolean): String =
    value match
      case Value.IntVal(number)                      => number.toString
      case Value.RationalVal(numerator, denominator) => s"$numerator/$denominator"
      case Value.InexactVal(number)                  => java.lang.Double.toString(number)
      case Value.BoolVal(true)                       => "#t"
      case Value.BoolVal(false)                      => "#f"
      case Value.StringVal(text) =>
        if displayStrings then text.text
        else s""""${escapeString(text.text)}""""
      case Value.CharVal(ch) =>
        if displayChars then ch.toString
        else renderChar(ch)
      case Value.SymbolVal(name) => name
      case Value.EmptyList       => "()"
      case pair @ Value.PairVal(_, _) =>
        renderPair(pair, displayStrings, displayChars)
      case Value.BuiltinProc(_) | Value.RecordConstructor(_) | Value.RecordPredicate(_) |
          Value.RecordAccessor(_, _, _) | Value.Closure(_, _, _, _, _) =>
        "#<procedure>"
      case Value.RecordVal(instance) =>
        s"#<record ${instance.recordType.typeName}>"
      case Value.Void => ""

  private def renderPair(value: Value, displayStrings: Boolean, displayChars: Boolean): String =
    val builder = new StringBuilder("(")

    def appendList(current: Value, first: Boolean): Unit =
      current match
        case Value.PairVal(car, cdr) =>
          if !first then builder.append(' ')
          builder.append(renderWithMode(car, displayStrings, displayChars))
          cdr match
            case Value.EmptyList => ()
            case next @ Value.PairVal(_, _) =>
              appendList(next, first = false)
            case other =>
              builder.append(" . ")
              builder.append(renderWithMode(other, displayStrings, displayChars))
        case _ =>
          ()

    appendList(value, first = true)
    builder.append(')')
    builder.toString

  private def renderChar(ch: Char): String =
    ch match
      case ' '  => "#\\space"
      case '\n' => "#\\newline"
      case _    => s"#\\$ch"

  private def escapeString(text: String): String =
    text.flatMap {
      case '\\' => "\\\\"
      case '"'  => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case ch   => ch.toString
    }
