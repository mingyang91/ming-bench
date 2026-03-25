package ming

import scala.annotation.tailrec

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
      case Value.EmptyList            => "()"
      case pair: Value.Pair           => renderPair(pair, render)
      case _: Procedure               => "#<procedure>"
      case Value.Void                 => "#<void>"

  def renderDisplay(value: Value): String =
    value match
      case Value.StringLit(text)     => text
      case Value.MutableString(text) => text
      case Value.Character(value)    => value.toString
      case Value.EmptyList           => "()"
      case pair: Value.Pair          => renderPair(pair, renderDisplay)
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

  private def renderPair(
    value: Value.Pair,
    renderValue: Value => String
  ): String =
    val builder = new StringBuilder("(")

    @tailrec
    def loop(current: Value, first: Boolean): Unit =
      current match
        case Value.Pair(car, cdr) =>
          if !first then builder.append(" ")
          builder.append(renderValue(car))
          cdr match
            case Value.EmptyList =>
              ()
            case next: Value.Pair =>
              loop(next, first = false)
            case other =>
              builder.append(" . ")
              builder.append(renderValue(other))
        case _ =>
          ()

    loop(value, first = true)
    builder.append(")")
    builder.result()
