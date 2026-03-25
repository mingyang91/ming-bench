package ming

private[ming] object SchemeRendering:
  import SchemeInterpreter.{Expr, Procedure, Value}

  def render(value: Value): String =
    value match
      case Value.Number(number)  => number.toString
      case Value.Bool(true)      => "#t"
      case Value.Bool(false)     => "#f"
      case Value.StringLit(text) => "\"" + escapeString(text) + "\""
      case Value.MutableString(text) =>
        "\"" + escapeString(text) + "\""
      case Value.Character(character) => renderCharacter(character)
      case Value.Symbol(name)         => name
      case Value.ListValue(items)     => items.map(render).mkString("(", " ", ")")
      case _: Procedure               => "#<procedure>"
      case Value.Void                 => "#<void>"

  def renderDisplay(value: Value): String =
    value match
      case Value.StringLit(text)     => text
      case Value.MutableString(text) => text
      case Value.Character(value)    => value.toString
      case Value.ListValue(items)    => items.map(renderDisplay).mkString("(", " ", ")")
      case other                     => render(other)

  def renderExpr(expr: Expr): String =
    expr match
      case Expr.Number(number, _)       => number.toString
      case Expr.Bool(true, _)           => "#t"
      case Expr.Bool(false, _)          => "#f"
      case Expr.StringLit(text, _)      => "\"" + escapeString(text) + "\""
      case Expr.Character(character, _) => renderCharacter(character)
      case Expr.Symbol(name, _)         => name
      case Expr.ListExpr(items, _)      => items.map(renderExpr).mkString("(", " ", ")")

  private def escapeString(value: String): String =
    val builder = new StringBuilder
    value.foreach {
      case '"'  => builder.append("\\\"")
      case '\\' => builder.append("\\\\")
      case '\n' => builder.append("\\n")
      case '\t' => builder.append("\\t")
      case ch   => builder.append(ch)
    }
    builder.result()

  private def renderCharacter(value: Char): String =
    value match
      case ' '  => "#\\space"
      case '\n' => "#\\newline"
      case ch   => s"#\\$ch"
