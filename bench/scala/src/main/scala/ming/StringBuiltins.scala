package ming

import scala.util.{Failure, Success, Try}

private[ming] object StringBuiltins:

  import BuiltinSupport.*
  import SchemeInterpreter.Value

  def all: List[Value.Builtin] =
    List(
      stringAppendBuiltin,
      stringLengthBuiltin,
      substringBuiltin,
      stringToNumberBuiltin,
      numberToStringBuiltin,
      symbolToStringBuiltin,
      stringToSymbolBuiltin,
      stringRefBuiltin
    )

  private val stringAppendBuiltin: Value.Builtin =
    Value.Builtin(
      "string-append",
      (args, pos) => Value.StringLit(args.map(asString(_, "string-append", pos)).mkString)
    )

  private val stringLengthBuiltin: Value.Builtin =
    Value.Builtin(
      "string-length",
      (args, pos) =>
        val value = asString(singleArg("string-length", args, pos), "string-length", pos)
        Value.Number(BigInt(value.length))
    )

  private val substringBuiltin: Value.Builtin =
    Value.Builtin(
      "substring",
      (args, pos) =>
        val (stringValue, startValue, endValue) = threeArgs("substring", args, pos)
        val text                                = asString(stringValue, "substring", pos)
        val start                               = asIndex(startValue, "substring", pos)
        val end                                 = asIndex(endValue, "substring", pos)
        if start > end || end > text.length then fail(pos, "substring index out of bounds")
        Value.StringLit(text.substring(start, end))
    )

  private val stringToNumberBuiltin: Value.Builtin =
    Value.Builtin(
      "string->number",
      (args, pos) =>
        val text = asString(singleArg("string->number", args, pos), "string->number", pos)
        parseInteger(text)
    )

  private val numberToStringBuiltin: Value.Builtin =
    Value.Builtin(
      "number->string",
      (args, pos) =>
        val value = asNumber(singleArg("number->string", args, pos), "number->string", pos)
        Value.StringLit(value.toString)
    )

  private val symbolToStringBuiltin: Value.Builtin =
    Value.Builtin(
      "symbol->string",
      (args, pos) =>
        val value = asSymbol(singleArg("symbol->string", args, pos), "symbol->string", pos)
        Value.StringLit(value)
    )

  private val stringToSymbolBuiltin: Value.Builtin =
    Value.Builtin(
      "string->symbol",
      (args, pos) =>
        val value = asString(singleArg("string->symbol", args, pos), "string->symbol", pos)
        Value.Symbol(value)
    )

  private val stringRefBuiltin: Value.Builtin =
    Value.Builtin(
      "string-ref",
      (args, pos) =>
        val (stringValue, indexValue) = twoArgs("string-ref", args, pos)
        val text                      = asString(stringValue, "string-ref", pos)
        val index                     = asIndex(indexValue, "string-ref", pos)
        if index >= text.length then fail(pos, "string-ref index out of bounds")
        Value.Character(text.charAt(index))
    )

  private def parseInteger(value: String): Value =
    Try(BigInt(value)) match
      case Success(number) => Value.Number(number)
      case Failure(_)      => Value.Bool(false)
