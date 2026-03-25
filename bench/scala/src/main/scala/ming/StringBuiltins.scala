package ming

import java.util.Locale

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
      stringRefBuiltin,
      stringCopyBuiltin,
      stringSetBuiltin,
      stringEqualsBuiltin,
      stringLessBuiltin,
      stringCiEqualsBuiltin,
      stringUpcaseBuiltin,
      stringDowncaseBuiltin
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
        Value.Number(SchemeNumber.exact(BigInt(value.length)))
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
        parseNumber(text)
    )

  private val numberToStringBuiltin: Value.Builtin =
    Value.Builtin(
      "number->string",
      (args, pos) =>
        val value = asNumber(singleArg("number->string", args, pos), "number->string", pos)
        Value.StringLit(value.render)
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

  private val stringCopyBuiltin: Value.Builtin =
    Value.Builtin(
      "string-copy",
      (args, pos) =>
        val text = asString(singleArg("string-copy", args, pos), "string-copy", pos)
        Value.MutableString(text)
    )

  private val stringSetBuiltin: Value.Builtin =
    Value.Builtin(
      "string-set!",
      (args, pos) =>
        val (stringValue, indexValue, charValue) = threeArgs("string-set!", args, pos)
        val text                                 = asMutableString(stringValue, "string-set!", pos)
        val index                                = asIndex(indexValue, "string-set!", pos)
        if index >= text.length then fail(pos, "string-set! index out of bounds")
        text.set(index, asCharacter(charValue, "string-set!", pos))
        Value.Void
    )

  private def parseNumber(value: String): Value =
    SchemeNumber.parseToken(value) match
      case Some(number) => Value.Number(number)
      case None         => Value.Bool(false)

  private val stringEqualsBuiltin: Value.Builtin =
    stringComparisonBuiltin("string=?")(_ == _)

  private val stringLessBuiltin: Value.Builtin =
    stringComparisonBuiltin("string<?")(_ < _)

  private val stringCiEqualsBuiltin: Value.Builtin =
    Value.Builtin(
      "string-ci=?",
      (args, pos) =>
        Value.Bool(compareStrings("string-ci=?", args, pos) { (left, right) =>
          left.toLowerCase(Locale.ROOT) == right.toLowerCase(Locale.ROOT)
        })
    )

  private val stringUpcaseBuiltin: Value.Builtin =
    Value.Builtin(
      "string-upcase",
      (args, pos) =>
        val text = asString(singleArg("string-upcase", args, pos), "string-upcase", pos)
        Value.StringLit(text.toUpperCase(Locale.ROOT))
    )

  private val stringDowncaseBuiltin: Value.Builtin =
    Value.Builtin(
      "string-downcase",
      (args, pos) =>
        val text = asString(singleArg("string-downcase", args, pos), "string-downcase", pos)
        Value.StringLit(text.toLowerCase(Locale.ROOT))
    )

  private def stringComparisonBuiltin(
    name: String
  )(predicate: (String, String) => Boolean): Value.Builtin =
    Value.Builtin(
      name,
      (args, pos) => Value.Bool(compareStrings(name, args, pos)(predicate))
    )

  private def compareStrings(
    name: String,
    args: List[Value],
    pos: SourcePos
  )(predicate: (String, String) => Boolean): Boolean =
    requireAtLeast(name, args, expected = 2, pos)
    val strings = args.map(asString(_, name, pos))
    strings.zip(strings.tail).forall { case (left, right) =>
      predicate(left, right)
    }
