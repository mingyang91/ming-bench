package ming

private[ming] object StringBuiltins:

  val names: Set[String] = Set(
    "string-append",
    "string-length",
    "substring",
    "string->number",
    "number->string",
    "symbol->string",
    "string->symbol",
    "string-ref"
  )

  def handles(name: String): Boolean =
    names.contains(name)

  def invoke(name: String, args: List[Value], pos: SourcePos): Value =
    name match
      case "string-append" =>
        Value.StringVal(args.map(arg => requireString(name, arg, pos)).mkString)
      case "string-length" =>
        Value.IntVal(requireString(name, requireSingleArg(name, args, pos), pos).length)
      case "substring" =>
        substring(name, args, pos)
      case "string->number" =>
        stringToNumber(name, args, pos)
      case "number->string" =>
        Value.StringVal(requireIndex(name, requireSingleArg(name, args, pos), pos).toString)
      case "symbol->string" =>
        Value.StringVal(requireSymbol(name, requireSingleArg(name, args, pos), pos))
      case "string->symbol" =>
        Value.SymbolVal(requireString(name, requireSingleArg(name, args, pos), pos))
      case "string-ref" =>
        stringRef(name, args, pos)
      case _ =>
        unknownProcedure(name, pos)

  private def substring(name: String, args: List[Value], pos: SourcePos): Value =
    val values = requireArgCount(name, args, expected = 3, pos)
    val text   = requireString(name, values.head, pos)
    val start  = requireIndex(name, values(1), pos)
    val end    = requireIndex(name, values(2), pos)
    if start < 0 || end < start || end > text.length then throw EvalError.at(pos, s"$name indices out of range")
    Value.StringVal(text.substring(start, end))

  private def stringToNumber(name: String, args: List[Value], pos: SourcePos): Value =
    val text = requireString(name, requireSingleArg(name, args, pos), pos)
    text.toIntOption match
      case Some(number) =>
        Value.IntVal(number)
      case None =>
        Value.BoolVal(false)

  private def stringRef(name: String, args: List[Value], pos: SourcePos): Value =
    val values = requireArgCount(name, args, expected = 2, pos)
    val text   = requireString(name, values.head, pos)
    val index  = requireIndex(name, values(1), pos)
    if index < 0 || index >= text.length then throw EvalError.at(pos, s"$name index out of range")
    Value.CharVal(text.charAt(index))

  private def requireString(name: String, value: Value, pos: SourcePos): String =
    value match
      case Value.StringVal(text) => text
      case other =>
        throw EvalError.at(pos, s"$name expected a string, got ${ValueSemantics.typeName(other)}")

  private def requireSymbol(name: String, value: Value, pos: SourcePos): String =
    value match
      case Value.SymbolVal(symbol) => symbol
      case other =>
        throw EvalError.at(pos, s"$name expected a symbol, got ${ValueSemantics.typeName(other)}")

  private def requireIndex(name: String, value: Value, pos: SourcePos): Int =
    value match
      case Value.IntVal(number) => number
      case other =>
        throw EvalError.at(pos, s"$name expected a number, got ${ValueSemantics.typeName(other)}")

  private def requireSingleArg(name: String, args: List[Value], pos: SourcePos): Value =
    requireArgCount(name, args, expected = 1, pos).head

  private def requireArgCount[T](name: String, args: List[T], expected: Int, pos: SourcePos): List[T] =
    if args.lengthCompare(expected) != 0 then
      throw EvalError.at(pos, s"$name expects $expected argument(s), got ${args.length}")
    args

  private def unknownProcedure(name: String, pos: SourcePos): Nothing =
    throw EvalError.at(pos, s"unknown procedure: $name")
