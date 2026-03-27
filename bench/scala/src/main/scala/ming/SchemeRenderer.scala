package ming

import scala.collection.mutable

private[ming] object SchemeRenderer:

  def render(value: Value): String =
    renderWithMode(value, displayStrings = false, displayChars = false, mutable.HashSet.empty[AnyRef])

  def renderForDisplay(value: Value): String =
    renderWithMode(value, displayStrings = true, displayChars = true, mutable.HashSet.empty[AnyRef])

  private def renderWithMode(
    value: Value,
    displayStrings: Boolean,
    displayChars: Boolean,
    path: mutable.HashSet[AnyRef]
  ): String =
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
      case pair: Value.PairVal =>
        renderPair(pair, displayStrings, displayChars, path)
      case Value.VectorVal(instance) =>
        renderVector(instance, displayStrings, displayChars, path)
      case Value.MultiValues(values) =>
        values match
          case value :: Nil =>
            renderWithMode(value, displayStrings, displayChars, path)
          case _ =>
            "#<values>"
      case Value.BuiltinProc(_) | Value.RecordConstructor(_) | Value.RecordPredicate(_) |
          Value.RecordAccessor(_, _, _) | _: Value.ContinuationVal | Value.CaseClosure(_, _, _) |
          Value.Closure(_, _, _, _, _) =>
        "#<procedure>"
      case Value.RecordVal(instance) =>
        s"#<record ${instance.recordType.typeName}>"
      case Value.Void => ""

  private def renderPair(
    value: Value.PairVal,
    displayStrings: Boolean,
    displayChars: Boolean,
    path: mutable.HashSet[AnyRef]
  ): String =
    if path.contains(value) then "#<circular>"
    else
      val builder = new StringBuilder("(")
      val added   = mutable.ArrayBuffer.empty[AnyRef]

      try
        renderPairContents(value, builder, added, displayStrings, displayChars, path, isFirstElement = true)
        builder.append(')')
        builder.toString
      finally added.reverseIterator.foreach(path -= _)

  @annotation.tailrec
  private def renderPairContents(
    current: Value,
    builder: StringBuilder,
    added: mutable.ArrayBuffer[AnyRef],
    displayStrings: Boolean,
    displayChars: Boolean,
    path: mutable.HashSet[AnyRef],
    isFirstElement: Boolean
  ): Unit =
    current match
      case pair: Value.PairVal if path.contains(pair) =>
        appendDottedSeparator(builder, isFirstElement)
        builder.append("#<circular>")
      case pair: Value.PairVal =>
        path += pair
        added += pair
        appendElementSeparator(builder, isFirstElement)
        builder.append(renderWithMode(pair.car, displayStrings, displayChars, path))
        renderPairContents(pair.cdr, builder, added, displayStrings, displayChars, path, isFirstElement = false)
      case Value.EmptyList =>
        ()
      case other =>
        appendDottedSeparator(builder, isFirstElement)
        builder.append(renderWithMode(other, displayStrings, displayChars, path))

  private def renderVector(
    instance: VectorInstance,
    displayStrings: Boolean,
    displayChars: Boolean,
    path: mutable.HashSet[AnyRef]
  ): String =
    if path.contains(instance) then "#<circular>"
    else
      path += instance
      try
        instance.elements
          .map(renderWithMode(_, displayStrings, displayChars, path))
          .mkString("#(", " ", ")")
      finally
        path -= instance

  private def appendElementSeparator(builder: StringBuilder, isFirstElement: Boolean): Unit =
    if !isFirstElement then builder.append(' ')

  private def appendDottedSeparator(builder: StringBuilder, isFirstElement: Boolean): Unit =
    if !isFirstElement then builder.append(" . ")

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
