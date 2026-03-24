package ming

private[ming] object ValueCodec:

  def render(value: Value): String =
    value match
      case Value.IntegerValue(number) => number.toString
      case Value.BooleanValue(true)   => "#t"
      case Value.BooleanValue(false)  => "#f"
      case Value.StringValue(text)    => quoteString(text)
      case Value.SymbolValue(name)    => name
      case Value.EmptyListValue       => "()"
      case pair @ Value.PairValue(_, _) =>
        s"(${renderPairContents(pair)})"
      case Value.BuiltinValue(name, _) =>
        s"#<procedure:$name>"
      case Value.ClosureValue(_, _, _) =>
        "#<procedure>"
      case Value.VoidValue =>
        "#<void>"

  def quoteExpr(expr: Expr): Value =
    expr match
      case Expr.IntegerLiteral(value) => Value.IntegerValue(value)
      case Expr.BooleanLiteral(value) => Value.BooleanValue(value)
      case Expr.StringLiteral(value)  => Value.StringValue(value)
      case Expr.Symbol(name)          => Value.SymbolValue(name)
      case Expr.ListExpr(elements)    => properList(elements.map(quoteExpr))

  private def renderPairContents(value: Value): String =
    value match
      case Value.PairValue(head, Value.EmptyListValue) =>
        render(head)
      case Value.PairValue(head, tail @ Value.PairValue(_, _)) =>
        s"${render(head)} ${renderPairContents(tail)}"
      case Value.PairValue(head, tail) =>
        s"${render(head)} . ${render(tail)}"
      case other =>
        render(other)

  private def quoteString(text: String): String =
    val escaped = text.flatMap {
      case '"'   => "\\\""
      case '\\'  => "\\\\"
      case '\n'  => "\\n"
      case '\r'  => "\\r"
      case '\t'  => "\\t"
      case other => other.toString
    }
    s"\"$escaped\""

  private def properList(values: List[Value]): Value =
    values.foldRight[Value](Value.EmptyListValue) { (value, tail) =>
      Value.PairValue(value, tail)
    }
