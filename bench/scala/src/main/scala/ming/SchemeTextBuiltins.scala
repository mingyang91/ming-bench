package ming

import java.util.Locale

import SchemeBuiltinSupport.*
import SchemeModel.*
import SchemeNumbers.*
import SchemeRuntime.*

private[ming] object SchemeTextBuiltins:

  val bindings: List[(String, Value)] = List(
    "string-append" -> Value.Builtin(
      "string-append",
      args =>
        Value.StringValue(
          SchemeString.fromText(args.map(arg => requireString("string-append", arg).text).mkString)
        )
    ),
    "string-length" -> unaryStringBuiltin("string-length") { text =>
      Value.IntegerValue(BigInt(text.length))
    },
    "substring" -> Value.Builtin(
      "substring",
      args =>
        requireArgCount("substring", args, 3)
        val text = requireString("substring", args.head)
        buildSubstring(text, args(1), args(2))
    ),
    "string->number" -> unaryStringBuiltin("string->number") { text =>
      parseNumber(text)
    },
    "number->string" -> Value.Builtin(
      "number->string",
      args =>
        requireArgCount("number->string", args, 1)
        Value.StringValue(SchemeString.fromText(SchemeNumbers.render(requireNumber("number->string", args.head))))
    ),
    "symbol->string" -> Value.Builtin(
      "symbol->string",
      args =>
        requireArgCount("symbol->string", args, 1)
        Value.StringValue(SchemeString.fromText(requireSymbol("symbol->string", args.head)))
    ),
    "string->symbol" -> Value.Builtin(
      "string->symbol",
      args =>
        requireArgCount("string->symbol", args, 1)
        Value.SymbolValue(requireString("string->symbol", args.head).text)
    ),
    "string-ref" -> Value.Builtin(
      "string-ref",
      args =>
        requireArgCount("string-ref", args, 2)
        val text  = requireString("string-ref", args.head)
        val index = requireIndex("string-ref", args(1), text.length)
        Value.CharValue(text.codePointAt(index))
    ),
    "string->list" -> unaryStringBuiltin("string->list") { text =>
      makeList(stringCodePoints(text).iterator.map(Value.CharValue.apply).toList)
    },
    "list->string" -> Value.Builtin(
      "list->string",
      args =>
        requireArgCount("list->string", args, 1)
        val chars = properListElements("list->string", args.head).map(requireChar("list->string", _))
        Value.StringValue(SchemeString.fromCodePoints(chars.toArray))
    ),
    "string-set!" -> Value.Builtin(
      "string-set!",
      args =>
        requireArgCount("string-set!", args, 3)
        val text = requireString("string-set!", args.head)
        if !text.isMutable then throw new EvalError("string-set! is not supported on immutable strings")
        val index = requireIndex("string-set!", args(1), text.length)
        text.setCodePoint(index, requireChar("string-set!", args(2)))
        Value.VoidValue
    ),
    "string-copy" -> Value.Builtin(
      "string-copy",
      args =>
        requireArgCount("string-copy", args, 1)
        Value.StringValue(requireString("string-copy", args.head).copyString())
    ),
    "string=?" -> stringComparator("string=?")(_ == _),
    "string<?" -> stringComparator("string<?")(_ < _),
    "string-ci=?" ->
      stringComparator("string-ci=?", _.toLowerCase(Locale.ROOT))(_ == _),
    "string-upcase" -> unaryStringBuiltin("string-upcase") { text =>
      Value.StringValue(SchemeString.fromText(text.text.toUpperCase(Locale.ROOT)))
    },
    "string-downcase" -> unaryStringBuiltin("string-downcase") { text =>
      Value.StringValue(SchemeString.fromText(text.text.toLowerCase(Locale.ROOT)))
    },
    "char-alphabetic?" -> unaryCharPredicate("char-alphabetic?")(codePoint => Character.isLetter(codePoint)),
    "char-numeric?"    -> unaryCharPredicate("char-numeric?")(codePoint => Character.isDigit(codePoint)),
    "char->integer" -> Value.Builtin(
      "char->integer",
      args =>
        requireArgCount("char->integer", args, 1)
        Value.IntegerValue(BigInt(requireChar("char->integer", args.head)))
    ),
    "integer->char" -> Value.Builtin(
      "integer->char",
      args =>
        requireArgCount("integer->char", args, 1)
        val codePoint = requireExactInteger("integer->char", args.head)
        if !codePoint.isValidInt || !isScalarValue(codePoint.toInt) then
          throw new EvalError("integer->char expected a valid Unicode scalar value")
        Value.CharValue(codePoint.toInt)
    ),
    "char-upcase"   -> unaryCharTransform("char-upcase")(codePoint => Character.toUpperCase(codePoint)),
    "char-downcase" -> unaryCharTransform("char-downcase")(codePoint => Character.toLowerCase(codePoint)),
    "char=?"        -> charComparator("char=?")(_ == _),
    "char<?"        -> charComparator("char<?")(_ < _)
  )

  private def stringComparator(
    name: String,
    normalize: String => String = identity
  )(predicate: (String, String) => Boolean): Value =
    Value.Builtin(
      name,
      args =>
        requireMinArgCount(name, args, 2)
        val strings = args.map(arg => normalize(requireString(name, arg).text))
        Value.BooleanValue(strings.zip(strings.tail).forall(predicate.tupled))
    )

  private def unaryCharPredicate(name: String)(predicate: Int => Boolean): Value =
    Value.Builtin(
      name,
      args =>
        requireArgCount(name, args, 1)
        Value.BooleanValue(predicate(requireChar(name, args.head)))
    )

  private def unaryCharTransform(name: String)(transform: Int => Int): Value =
    Value.Builtin(
      name,
      args =>
        requireArgCount(name, args, 1)
        Value.CharValue(transform(requireChar(name, args.head)))
    )

  private def charComparator(name: String)(predicate: (Int, Int) => Boolean): Value =
    Value.Builtin(
      name,
      args =>
        requireMinArgCount(name, args, 2)
        val chars = args.map(arg => requireChar(name, arg))
        Value.BooleanValue(chars.zip(chars.tail).forall(predicate.tupled))
    )

  private def isScalarValue(codePoint: Int): Boolean =
    Character.isValidCodePoint(codePoint) &&
      !(codePoint >= Character.MIN_SURROGATE.toInt && codePoint <= Character.MAX_SURROGATE.toInt)
