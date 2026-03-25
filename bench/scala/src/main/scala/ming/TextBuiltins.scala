package ming

private[ming] object TextBuiltins extends BuiltinSupport:

  def entries(runtime: RuntimeContext): Map[String, Value] = Map(
    "display"          -> Value.Builtin("display", display(runtime)),
    "write"            -> Value.Builtin("write", write(runtime)),
    "newline"          -> Value.Builtin("newline", newline(runtime)),
    "string-append"    -> Value.Builtin("string-append", stringAppend),
    "string-copy"      -> Value.Builtin("string-copy", stringCopy),
    "string-length"    -> Value.Builtin("string-length", stringLength),
    "substring"        -> Value.Builtin("substring", substring),
    "string->number"   -> Value.Builtin("string->number", stringToNumber),
    "number->string"   -> Value.Builtin("number->string", numberToString),
    "symbol->string"   -> Value.Builtin("symbol->string", symbolToString),
    "string->symbol"   -> Value.Builtin("string->symbol", stringToSymbol),
    "string-ref"       -> Value.Builtin("string-ref", stringRef),
    "string=?"         -> stringComparison("string=?", _ == _),
    "string<?"         -> stringComparison("string<?", (left, right) => left.compareTo(right) < 0),
    "string-ci=?"      -> stringComparison("string-ci=?", (left, right) => left.equalsIgnoreCase(right)),
    "string-upcase"    -> Value.Builtin("string-upcase", stringUpcase),
    "string-downcase"  -> Value.Builtin("string-downcase", stringDowncase),
    "string-set!"      -> Value.Builtin("string-set!", stringSet),
    "char-alphabetic?" -> charPredicate("char-alphabetic?", ch => Character.isLetter(ch)),
    "char-numeric?"    -> charPredicate("char-numeric?", ch => Character.isDigit(ch)),
    "char-upcase"      -> Value.Builtin("char-upcase", charUpcase),
    "char-downcase"    -> Value.Builtin("char-downcase", charDowncase),
    "char=?"           -> charComparison("char=?", _ == _),
    "char<?"           -> charComparison("char<?", _ < _)
  )

  private def display(runtime: RuntimeContext)(args: List[Value], pos: SourcePos): Value =
    runtime.appendOutput(expectSingleArg(args, pos, "display").renderDisplay)
    Value.VoidVal

  private def write(runtime: RuntimeContext)(args: List[Value], pos: SourcePos): Value =
    runtime.appendOutput(expectSingleArg(args, pos, "write").render)
    Value.VoidVal

  private def newline(runtime: RuntimeContext)(args: List[Value], pos: SourcePos): Value =
    if args.nonEmpty then throw EvalError.at(pos, "newline expects exactly 0 arguments")
    runtime.appendOutput("\n")
    Value.VoidVal

  private def stringAppend(args: List[Value], pos: SourcePos): Value =
    Value.StringVal(args.map(arg => expectString(arg, pos, "string-append")).mkString.toCharArray)

  private def stringCopy(args: List[Value], pos: SourcePos): Value =
    Value.StringVal(expectStringValue(expectSingleArg(args, pos, "string-copy"), pos, "string-copy").clone())

  private def stringLength(args: List[Value], pos: SourcePos): Value =
    Value.IntVal(expectString(expectSingleArg(args, pos, "string-length"), pos, "string-length").length)

  private def substring(args: List[Value], pos: SourcePos): Value =
    args match
      case stringValue :: startValue :: endValue :: Nil =>
        val text  = expectString(stringValue, pos, "substring")
        val start = expectIndex(startValue, pos, "substring")
        val end   = expectIndex(endValue, pos, "substring")
        if start > end || end > text.length then throw EvalError.at(pos, "substring indices are out of bounds")

        Value.StringVal(text.substring(start, end).toCharArray)

      case _ =>
        throw EvalError.at(pos, "substring expects exactly 3 arguments")

  private def stringToNumber(args: List[Value], pos: SourcePos): Value =
    val text = expectString(expectSingleArg(args, pos, "string->number"), pos, "string->number").trim
    if text.isEmpty then Value.BoolVal(false)
    else
      try Value.IntVal(BigInt(text))
      catch
        case _: NumberFormatException =>
          Value.BoolVal(false)

  private def numberToString(args: List[Value], pos: SourcePos): Value =
    Value.StringVal(
      expectNumber(expectSingleArg(args, pos, "number->string"), pos, "number->string").toString.toCharArray
    )

  private def symbolToString(args: List[Value], pos: SourcePos): Value =
    Value.StringVal(expectSymbol(expectSingleArg(args, pos, "symbol->string"), pos, "symbol->string").toCharArray)

  private def stringToSymbol(args: List[Value], pos: SourcePos): Value =
    Value.SymbolVal(expectString(expectSingleArg(args, pos, "string->symbol"), pos, "string->symbol"))

  private def stringRef(args: List[Value], pos: SourcePos): Value =
    args match
      case stringValue :: indexValue :: Nil =>
        val text  = expectString(stringValue, pos, "string-ref")
        val index = expectIndex(indexValue, pos, "string-ref")
        if index >= text.length then throw EvalError.at(pos, "string-ref index is out of bounds")

        Value.CharVal(text.charAt(index))

      case _ =>
        throw EvalError.at(pos, "string-ref expects exactly 2 arguments")

  private def stringUpcase(args: List[Value], pos: SourcePos): Value =
    Value.StringVal(
      expectStringValue(expectSingleArg(args, pos, "string-upcase"), pos, "string-upcase").map(ch =>
        Character.toUpperCase(ch)
      )
    )

  private def stringDowncase(args: List[Value], pos: SourcePos): Value =
    Value.StringVal(
      expectStringValue(expectSingleArg(args, pos, "string-downcase"), pos, "string-downcase").map(ch =>
        Character.toLowerCase(ch)
      )
    )

  private def stringSet(args: List[Value], pos: SourcePos): Value =
    args match
      case stringValue :: indexValue :: charValue :: Nil =>
        val text  = expectStringValue(stringValue, pos, "string-set!")
        val index = expectIndex(indexValue, pos, "string-set!")
        if index >= text.length then throw EvalError.at(pos, "string-set! index is out of bounds")

        text(index) = expectChar(charValue, pos, "string-set!")
        Value.VoidVal

      case _ =>
        throw EvalError.at(pos, "string-set! expects exactly 3 arguments")

  private def charUpcase(args: List[Value], pos: SourcePos): Value =
    Value.CharVal(Character.toUpperCase(expectChar(expectSingleArg(args, pos, "char-upcase"), pos, "char-upcase")))

  private def charDowncase(args: List[Value], pos: SourcePos): Value =
    Value.CharVal(
      Character.toLowerCase(expectChar(expectSingleArg(args, pos, "char-downcase"), pos, "char-downcase"))
    )

  private def charPredicate(name: String, test: Char => Boolean): Value =
    Value.Builtin(
      name,
      (args, pos) => Value.BoolVal(test(expectChar(expectSingleArg(args, pos, name), pos, name)))
    )

  private def charComparison(name: String, relation: (Char, Char) => Boolean): Value =
    Value.Builtin(
      name,
      (args, pos) =>
        val chars = args.map(arg => expectChar(arg, pos, name))
        if chars.lengthCompare(2) < 0 then throw EvalError.at(pos, s"$name expects at least 2 arguments")

        Value.BoolVal(chars.zip(chars.tail).forall(relation.tupled))
    )

  private def stringComparison(name: String, relation: (String, String) => Boolean): Value =
    Value.Builtin(
      name,
      (args, pos) =>
        val strings = args.map(arg => expectString(arg, pos, name))
        if strings.lengthCompare(2) < 0 then throw EvalError.at(pos, s"$name expects at least 2 arguments")

        Value.BoolVal(strings.zip(strings.tail).forall(relation.tupled))
    )
