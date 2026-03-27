package ming

import java.util.Locale

private[ming] object Level1StringBuiltins:
  import RuntimeSupport.*

  val values: Map[String, Value] = Map(
    "display"          -> BuiltinValue("display", display),
    "write"            -> BuiltinValue("write", write),
    "newline"          -> BuiltinValue("newline", newline),
    "string-copy"      -> BuiltinValue("string-copy", stringCopy),
    "string-set!"      -> BuiltinValue("string-set!", stringSet),
    "string->list"     -> BuiltinValue("string->list", stringToList),
    "list->string"     -> BuiltinValue("list->string", listToString),
    "string-append"    -> BuiltinValue("string-append", stringAppend),
    "string-length"    -> BuiltinValue("string-length", stringLength),
    "substring"        -> BuiltinValue("substring", substring),
    "string->number"   -> BuiltinValue("string->number", stringToNumber),
    "number->string"   -> BuiltinValue("number->string", numberToString),
    "symbol->string"   -> BuiltinValue("symbol->string", symbolToString),
    "string->symbol"   -> BuiltinValue("string->symbol", stringToSymbol),
    "string-ref"       -> BuiltinValue("string-ref", stringRef),
    "string=?"         -> BuiltinValue("string=?", compareStrings("string=?")(_ == _)),
    "string<?"         -> BuiltinValue("string<?", compareStrings("string<?")(_ < _)),
    "string-ci=?"      -> BuiltinValue("string-ci=?", stringCiEqual),
    "string-upcase"    -> BuiltinValue("string-upcase", stringUpcase),
    "string-downcase"  -> BuiltinValue("string-downcase", stringDowncase),
    "char->integer"    -> BuiltinValue("char->integer", charToInteger),
    "integer->char"    -> BuiltinValue("integer->char", integerToChar),
    "char-alphabetic?" -> BuiltinValue("char-alphabetic?", unaryCharPredicate("char-alphabetic?")(_.isLetter)),
    "char-numeric?"    -> BuiltinValue("char-numeric?", unaryCharPredicate("char-numeric?")(_.isDigit)),
    "char-upcase"      -> BuiltinValue("char-upcase", charUpcase),
    "char-downcase"    -> BuiltinValue("char-downcase", charDowncase),
    "char=?"           -> BuiltinValue("char=?", compareChars("char=?")(_ == _)),
    "char<?"           -> BuiltinValue("char<?", compareChars("char<?")(_ < _))
  )

  private def display(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "display", position)
    OutputCapture.append(value.renderDisplay)
    VoidValue

  private def write(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "write", position)
    OutputCapture.append(value.render)
    VoidValue

  private def newline(arguments: List[Value], position: Position): Value =
    expectExact(arguments, 0, "newline", position)
    OutputCapture.append("\n")
    VoidValue

  private def stringCopy(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "string-copy", position)
    val text  = expectString(value, "string-copy", position)
    if BenchRuntime.stringsAreImmutable then StringValue(text)
    else MutableStringValue(text)

  private def stringSet(arguments: List[Value], position: Position): Value =
    expectExact(arguments, 3, "string-set!", position) match
      case stringValue :: indexValue :: charValue :: Nil =>
        if BenchRuntime.stringsAreImmutable then
          expectString(stringValue, "string-set!", position)
          SchemeFailure.raise("string-set! cannot mutate immutable strings", position)
        else
          val mutableString = expectMutableString(stringValue, "string-set!", position)
          val index         = expectIndex(indexValue, "string-set!", position)
          val char          = expectChar(charValue, "string-set!", position)
          if index >= mutableString.text.length then SchemeFailure.raise("string-set! index out of bounds", position)

          mutableString.update(index, char)
          VoidValue
      case _ =>
        throw new IllegalStateException("validated three-argument list")

  private def stringToList(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "string->list", position)
    buildList(expectString(value, "string->list", position).toList.map(CharValue(_)))

  private def listToString(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "list->string", position)
    StringValue(expectProperList(value, "list->string", position).map(expectChar(_, "list->string", position)).mkString)

  private def stringAppend(arguments: List[Value], position: Position): Value =
    StringValue(arguments.map(expectString(_, "string-append", position)).mkString)

  private def stringLength(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "string-length", position)
    IntValue(expectString(value, "string-length", position).length)

  private def substring(arguments: List[Value], position: Position): Value =
    expectExact(arguments, 3, "substring", position) match
      case stringValue :: startValue :: endValue :: Nil =>
        val text  = expectString(stringValue, "substring", position)
        val start = expectIndex(startValue, "substring", position)
        val end   = expectIndex(endValue, "substring", position)
        if start > end || end > text.length then SchemeFailure.raise("substring indices out of bounds", position)

        StringValue(text.substring(start, end))
      case _ =>
        throw new IllegalStateException("validated three-argument list")

  private def stringToNumber(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "string->number", position)
    val text  = expectString(value, "string->number", position)
    NumericSupport.parseNumberToken(text, position) match
      case Some(number) => NumericSupport.valueForLiteral(number)
      case None         => BoolValue(false)

  private def numberToString(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "number->string", position)
    StringValue(expectNumber(value, "number->string", position).render)

  private def symbolToString(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "symbol->string", position)
    StringValue(expectSymbol(value, "symbol->string", position))

  private def stringToSymbol(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "string->symbol", position)
    SymbolValue(expectString(value, "string->symbol", position))

  private def stringRef(arguments: List[Value], position: Position): Value =
    val (stringValue, indexValue) = expectTwoArguments(arguments, "string-ref", position)
    val text                      = expectString(stringValue, "string-ref", position)
    val index                     = expectIndex(indexValue, "string-ref", position)
    if index >= text.length then SchemeFailure.raise("string-ref index out of bounds", position)

    CharValue(text.charAt(index))

  private def stringCiEqual(arguments: List[Value], position: Position): Value =
    val normalized = expectAtLeast(arguments, 2, "string-ci=?", position).map: value =>
      expectString(value, "string-ci=?", position).toUpperCase(Locale.ROOT)
    BoolValue(normalized.zip(normalized.tail).forall((left, right) => left == right))

  private def stringUpcase(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "string-upcase", position)
    StringValue(expectString(value, "string-upcase", position).toUpperCase(Locale.ROOT))

  private def stringDowncase(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "string-downcase", position)
    StringValue(expectString(value, "string-downcase", position).toLowerCase(Locale.ROOT))

  private def charToInteger(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "char->integer", position)
    IntValue(expectChar(value, "char->integer", position).toInt)

  private def integerToChar(arguments: List[Value], position: Position): Value =
    val value    = expectSingleArgument(arguments, "integer->char", position)
    val codeUnit = expectInteger(value, "integer->char", position)
    if codeUnit < Char.MinValue.toInt || codeUnit > Char.MaxValue.toInt then
      SchemeFailure.raise("integer->char expected a valid character code", position)

    CharValue(codeUnit.toInt.toChar)

  private def charUpcase(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "char-upcase", position)
    CharValue(Character.toUpperCase(expectChar(value, "char-upcase", position)))

  private def charDowncase(arguments: List[Value], position: Position): Value =
    val value = expectSingleArgument(arguments, "char-downcase", position)
    CharValue(Character.toLowerCase(expectChar(value, "char-downcase", position)))

  private def unaryCharPredicate(
    name: String
  )(predicate: Char => Boolean): (List[Value], Position) => Value =
    (arguments, position) =>
      BoolValue(predicate(expectChar(expectSingleArgument(arguments, name, position), name, position)))

  private def compareStrings(
    name: String
  )(predicate: (String, String) => Boolean): (List[Value], Position) => Value =
    (arguments, position) =>
      val strings = expectAtLeast(arguments, 2, name, position).map(expectString(_, name, position))
      BoolValue(strings.zip(strings.tail).forall((left, right) => predicate(left, right)))

  private def compareChars(
    name: String
  )(predicate: (Char, Char) => Boolean): (List[Value], Position) => Value =
    (arguments, position) =>
      val chars = expectAtLeast(arguments, 2, name, position).map(expectChar(_, name, position))
      BoolValue(chars.zip(chars.tail).forall((left, right) => predicate(left, right)))
