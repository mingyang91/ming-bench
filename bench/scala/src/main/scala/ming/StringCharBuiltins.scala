package ming

import SchemeValue.*

/** String and character built-in procedures. */
object StringCharBuiltins:

  def evalStringAppend(args: List[SchemeValue]): SchemeValue =
    val sb = args.map {
      case SchemeString(s) => s
      case other           => throw new EvalError(s"string-append: not a string: ${other.display}")
    }
    SchemeString(sb.mkString)

  def evalStringLength(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("string-length: expected 1 argument")
    args.head match
      case SchemeString(s) => SchemeInt(s.length.toLong)
      case other           => throw new EvalError(s"string-length: not a string: ${other.display}")

  def evalSubstring(args: List[SchemeValue]): SchemeValue =
    if args.length != 3 then throw new EvalError("substring: expected 3 arguments")
    (args.head, args(1), args(2)) match
      case (SchemeString(s), SchemeInt(start), SchemeInt(end)) =>
        SchemeString(s.substring(start.toInt, end.toInt))
      case _ => throw new EvalError("substring: invalid arguments")

  def evalStringToNumber(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("string->number: expected 1 argument")
    args.head match
      case SchemeString(s) =>
        s.toLongOption match
          case Some(n) => SchemeInt(n)
          case None    => SchemeBool(false)
      case other => throw new EvalError(s"string->number: not a string: ${other.display}")

  def evalNumberToString(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("number->string: expected 1 argument")
    args.head match
      case SchemeInt(n) => SchemeString(n.toString)
      case other        => throw new EvalError(s"number->string: not a number: ${other.display}")

  def evalSymbolToString(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("symbol->string: expected 1 argument")
    args.head match
      case SchemeSymbol(name) => SchemeString(name)
      case other              => throw new EvalError(s"symbol->string: not a symbol: ${other.display}")

  def evalStringToSymbol(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("string->symbol: expected 1 argument")
    args.head match
      case SchemeString(s) => SchemeSymbol(s)
      case other           => throw new EvalError(s"string->symbol: not a string: ${other.display}")

  def evalStringRef(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("string-ref: expected 2 arguments")
    (args.head, args(1)) match
      case (SchemeString(s), SchemeInt(idx)) =>
        if idx < 0 || idx >= s.length then throw new EvalError("string-ref: index out of bounds")
        SchemeChar(s.charAt(idx.toInt))
      case _ => throw new EvalError("string-ref: invalid arguments")

  def evalStringCopy(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("string-copy: expected 1 argument")
    args.head match
      case SchemeString(s)         => new SchemeMutableString(s.toCharArray)
      case ms: SchemeMutableString => new SchemeMutableString(ms.chars.clone())
      case other                   => throw new EvalError(s"string-copy: not a string: ${other.display}")

  def evalStringSet(args: List[SchemeValue]): SchemeValue =
    if args.length != 3 then throw new EvalError("string-set!: expected 3 arguments")
    (args.head, args(1), args(2)) match
      case (ms: SchemeMutableString, SchemeInt(idx), SchemeChar(c)) =>
        if idx < 0 || idx >= ms.chars.length then throw new EvalError("string-set!: index out of bounds")
        ms.chars(idx.toInt) = c
        SchemeVoid
      case (_: SchemeString, _, _) =>
        throw new EvalError("string-set!: string is immutable")
      case _ => throw new EvalError("string-set!: invalid arguments")

  def charPred(args: List[SchemeValue], pred: Char => Boolean): SchemeValue =
    if args.length != 1 then throw new EvalError("char predicate: expected 1 argument")
    args.head match
      case SchemeChar(c) => SchemeBool(pred(c))
      case other         => throw new EvalError(s"char predicate: not a char: ${other.display}")

  def evalCharUpcase(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("char-upcase: expected 1 argument")
    args.head match
      case SchemeChar(c) => SchemeChar(c.toUpper)
      case other         => throw new EvalError(s"char-upcase: not a char: ${other.display}")

  def evalCharDowncase(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("char-downcase: expected 1 argument")
    args.head match
      case SchemeChar(c) => SchemeChar(c.toLower)
      case other         => throw new EvalError(s"char-downcase: not a char: ${other.display}")

  def charCmp(args: List[SchemeValue], cmp: (Char, Char) => Boolean): SchemeValue =
    if args.length != 2 then throw new EvalError("char comparison: expected 2 arguments")
    (args.head, args(1)) match
      case (SchemeChar(a), SchemeChar(b)) => SchemeBool(cmp(a, b))
      case _                              => throw new EvalError("char comparison: not chars")

  def strCmp(args: List[SchemeValue], cmp: (String, String) => Boolean): SchemeValue =
    if args.length != 2 then throw new EvalError("string comparison: expected 2 arguments")
    (args.head, args(1)) match
      case (SchemeString(a), SchemeString(b)) => SchemeBool(cmp(a, b))
      case _                                  => throw new EvalError("string comparison: not strings")

  def evalStringCiEq(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("string-ci=?: expected 2 arguments")
    (args.head, args(1)) match
      case (SchemeString(a), SchemeString(b)) => SchemeBool(a.equalsIgnoreCase(b))
      case _                                  => throw new EvalError("string-ci=?: not strings")

  def evalStringUpcase(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("string-upcase: expected 1 argument")
    args.head match
      case SchemeString(s) => SchemeString(s.toUpperCase)
      case other           => throw new EvalError(s"string-upcase: not a string: ${other.display}")

  def evalStringDowncase(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("string-downcase: expected 1 argument")
    args.head match
      case SchemeString(s) => SchemeString(s.toLowerCase)
      case other           => throw new EvalError(s"string-downcase: not a string: ${other.display}")
