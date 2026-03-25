package ming

import java.util.IdentityHashMap

private[ming] object SchemeRendering:
  import SchemeInterpreter.{Expr, Procedure, Value}

  def render(value: Value): String =
    renderValue(value, displayMode = false, RenderState())

  def renderDisplay(value: Value): String =
    renderValue(value, displayMode = true, RenderState())

  def renderExpr(expr: Expr): String =
    expr match
      case Expr.Number(number, _)       => number.render
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

  private def renderValue(value: Value, displayMode: Boolean, state: RenderState): String =
    value match
      case Value.Number(number)      => number.render
      case Value.Bool(true)          => "#t"
      case Value.Bool(false)         => "#f"
      case Value.StringLit(text)     => if displayMode then text else "\"" + escapeString(text) + "\""
      case Value.MutableString(text) => if displayMode then text else "\"" + escapeString(text) + "\""
      case Value.Character(character) =>
        if displayMode then character.toString else renderCharacter(character)
      case Value.Symbol(name)      => name
      case Value.EmptyList         => "()"
      case pair: Value.Pair        => renderPair(pair, displayMode, state)
      case vector: Value.Vector    => renderVector(vector, displayMode, state)
      case record: Value.Record    => s"#<record ${record.typeName}>"
      case Value.MultipleValues(_) => "#<values>"
      case Value.SyntaxObject(_)   => "#<syntax>"
      case _: Procedure            => "#<procedure>"
      case Value.Void              => "#<void>"

  private def renderPair(
    value: Value.Pair,
    displayMode: Boolean,
    state: RenderState
  ): String =
    if state.activePairs.containsKey(value) then "#<cycle>"
    else
      val builder = new StringBuilder("(")

      def appendPair(current: Value.Pair): Unit =
        if state.activePairs.containsKey(current) then builder.append("#<cycle>")
        else
          state.activePairs.put(current, java.lang.Boolean.TRUE)
          try
            builder.append(renderValue(current.car, displayMode, state))
            current.cdr match
              case Value.EmptyList =>
                ()
              case next: Value.Pair if state.activePairs.containsKey(next) =>
                builder.append(" . ")
                builder.append("#<cycle>")
              case next: Value.Pair =>
                builder.append(" ")
                appendPair(next)
              case other =>
                builder.append(" . ")
                builder.append(renderValue(other, displayMode, state))
          finally state.activePairs.remove(current)

      appendPair(value)
      builder.append(")")
      builder.result()

  private def renderVector(
    value: Value.Vector,
    displayMode: Boolean,
    state: RenderState
  ): String =
    if state.activeVectors.containsKey(value) then "#<cycle>"
    else
      state.activeVectors.put(value, java.lang.Boolean.TRUE)
      try
        value.toList.map(renderValue(_, displayMode, state)).mkString("#(", " ", ")")
      finally
        state.activeVectors.remove(value)

  final private class RenderState private (
    val activePairs: IdentityHashMap[Value.Pair, java.lang.Boolean],
    val activeVectors: IdentityHashMap[Value.Vector, java.lang.Boolean]
  )

  private object RenderState:

    def apply(): RenderState =
      new RenderState(
        new IdentityHashMap[Value.Pair, java.lang.Boolean](),
        new IdentityHashMap[Value.Vector, java.lang.Boolean]()
      )
