package ming

import java.util.Locale

import BuiltinSupport.*

private[ming] object StringBuiltins:

  val names: Set[String] = Set(
    "string-append",
    "string-length",
    "substring",
    "string->number",
    "number->string",
    "string->list",
    "list->string",
    "symbol->string",
    "string->symbol",
    "string-ref",
    "string-copy",
    "string-set!",
    "char->integer",
    "integer->char",
    "string=?",
    "string<?",
    "string-ci=?",
    "string-upcase",
    "string-downcase"
  )

  def handles(name: String): Boolean =
    names.contains(name)

  def invoke(name: String, args: List[Value], pos: SourcePos): Value =
    name match
      case "string-append" =>
        Value.StringVal(MutableString.from(args.map(arg => requireString(name, arg, pos).text).mkString))
      case "string-length" =>
        Value.IntVal(requireString(name, requireSingleArg(name, args, pos), pos).length)
      case "substring" =>
        substring(name, args, pos)
      case "string->number" =>
        stringToNumber(name, args, pos)
      case "number->string" =>
        Value.StringVal(
          MutableString.from(SchemeNumber.render(requireNumber(name, requireSingleArg(name, args, pos), pos)))
        )
      case "string->list" =>
        stringToList(name, args, pos)
      case "list->string" =>
        listToString(name, args, pos)
      case "symbol->string" =>
        Value.StringVal(MutableString.from(requireSymbol(name, requireSingleArg(name, args, pos), pos)))
      case "string->symbol" =>
        Value.SymbolVal(requireString(name, requireSingleArg(name, args, pos), pos).text)
      case "string-ref" =>
        stringRef(name, args, pos)
      case "string-copy" =>
        Value.StringVal(MutableString.from(requireString(name, requireSingleArg(name, args, pos), pos).text))
      case "string-set!" =>
        stringSet(name, args, pos)
      case "char->integer" =>
        Value.IntVal(requireChar(name, requireSingleArg(name, args, pos), pos).toLong)
      case "integer->char" =>
        integerToChar(name, args, pos)
      case "string=?" =>
        compareStrings(name, args, pos)(_ == _)
      case "string<?" =>
        compareStrings(name, args, pos)(_ < _)
      case "string-ci=?" =>
        compareStrings(name, args, pos)(_.equalsIgnoreCase(_))
      case "string-upcase" =>
        Value.StringVal(
          MutableString.from(requireString(name, requireSingleArg(name, args, pos), pos).text.toUpperCase(Locale.ROOT))
        )
      case "string-downcase" =>
        Value.StringVal(
          MutableString.from(requireString(name, requireSingleArg(name, args, pos), pos).text.toLowerCase(Locale.ROOT))
        )
      case _ =>
        unknownProcedure(name, pos)

  private def substring(name: String, args: List[Value], pos: SourcePos): Value =
    val values = requireArgCount(name, args, expected = 3, pos)
    val text   = requireString(name, values.head, pos)
    val start  = requireIndex(name, values(1), pos)
    val end    = requireIndex(name, values(2), pos)
    if start < 0 || end < start || end > text.length then throw EvalError.at(pos, s"$name indices out of range")
    Value.StringVal(MutableString.from(text.text.substring(start, end)))

  private def stringToNumber(name: String, args: List[Value], pos: SourcePos): Value =
    val text = requireString(name, requireSingleArg(name, args, pos), pos)
    SchemeNumber.parseToken(text.text).map(_.toValue).getOrElse(Value.BoolVal(false))

  private def stringToList(name: String, args: List[Value], pos: SourcePos): Value =
    val text = requireString(name, requireSingleArg(name, args, pos), pos)
    ValueSemantics.listFrom(text.text.toList.map(Value.CharVal(_)))

  private def listToString(name: String, args: List[Value], pos: SourcePos): Value =
    val chars = ValueSemantics
      .toProperList(name, requireSingleArg(name, args, pos), pos)
      .map(value => requireChar(name, value, pos))
      .mkString
    Value.StringVal(MutableString.from(chars))

  private def stringRef(name: String, args: List[Value], pos: SourcePos): Value =
    val values = requireArgCount(name, args, expected = 2, pos)
    val text   = requireString(name, values.head, pos)
    val index  = requireIndex(name, values(1), pos)
    if index < 0 || index >= text.length then throw EvalError.at(pos, s"$name index out of range")
    Value.CharVal(text.charAt(index))

  private def stringSet(name: String, args: List[Value], pos: SourcePos): Value =
    val values = requireArgCount(name, args, expected = 3, pos)
    val text   = requireString(name, values.head, pos)
    val index  = requireIndex(name, values(1), pos)
    val ch     = requireChar(name, values(2), pos)
    if !text.isMutable then throw EvalError.at(pos, s"$name cannot mutate immutable strings")
    if index < 0 || index >= text.length then throw EvalError.at(pos, s"$name index out of range")
    text.setCharAt(index, ch)
    Value.Void

  private def integerToChar(name: String, args: List[Value], pos: SourcePos): Value =
    val codePoint = requireExactInteger(name, requireSingleArg(name, args, pos), pos)
    if codePoint < Character.MIN_VALUE.toLong ||
      codePoint > Character.MAX_VALUE.toLong ||
      Character.isSurrogate(codePoint.toChar)
    then throw EvalError.at(pos, s"$name expected a valid character code")
    Value.CharVal(codePoint.toChar)
