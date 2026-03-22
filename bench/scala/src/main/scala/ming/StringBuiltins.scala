package ming

/** String-related built-in procedure implementations. */
object StringBuiltins:

  import SchemeValue.*

  def requireString(v: SchemeValue): String = v match
    case SchemeString(s)            => s
    case SchemeMutableString(chars) => String(chars)
    case _ =>
      throw new EvalError(s"expected string, got: ${v.display}")

  def evalStringAppend(args: List[SchemeValue]): SchemeValue =
    SchemeString(args.map(requireString).mkString)

  def evalStringLength(args: List[SchemeValue]): SchemeValue =
    args match
      case List(v) => SchemeInt(requireString(v).length.toLong)
      case _       => throw new EvalError("string-length expects 1 argument")

  def evalSubstring(args: List[SchemeValue]): SchemeValue =
    args match
      case List(s, start, end) =>
        val startIdx = Builtins.requireInt(start).toInt
        val endIdx   = Builtins.requireInt(end).toInt
        SchemeString(requireString(s).substring(startIdx, endIdx))
      case _ => throw new EvalError("substring expects 3 arguments")

  def evalStringToNumber(args: List[SchemeValue]): SchemeValue =
    args match
      case List(v) =>
        requireString(v).toLongOption match
          case Some(n) => SchemeInt(n)
          case None    => SchemeBool(false)
      case _ =>
        throw new EvalError("string->number expects 1 argument")

  def evalNumberToString(args: List[SchemeValue]): SchemeValue =
    args match
      case List(v) => SchemeString(Builtins.requireInt(v).toString)
      case _ =>
        throw new EvalError("number->string expects 1 argument")

  def evalSymbolToString(args: List[SchemeValue]): SchemeValue =
    args match
      case List(SchemeSymbol(name)) => SchemeString(name)
      case _ =>
        throw new EvalError("symbol->string expects a symbol")

  def evalStringToSymbol(args: List[SchemeValue]): SchemeValue =
    args match
      case List(v) => SchemeSymbol(requireString(v))
      case _ =>
        throw new EvalError("string->symbol expects 1 argument")

  def evalStringRef(args: List[SchemeValue]): SchemeValue =
    args match
      case List(s, idx) =>
        val str = requireString(s)
        val i   = Builtins.requireInt(idx).toInt
        SchemeChar(str.charAt(i))
      case _ =>
        throw new EvalError("string-ref expects 2 arguments")

  def evalStringCopy(args: List[SchemeValue]): SchemeValue =
    args match
      case List(v) =>
        val str = requireString(v)
        SchemeMutableString(str.toCharArray)
      case _ =>
        throw new EvalError("string-copy expects 1 argument")

  def evalStringSet(args: List[SchemeValue]): SchemeValue =
    args match
      case List(SchemeMutableString(chars), idx, SchemeChar(c)) =>
        val i = Builtins.requireInt(idx).toInt
        chars(i) = c
        SchemeVoid
      case List(SchemeString(_), _, _) =>
        throw new EvalError("string-set!: string is immutable")
      case _ =>
        throw new EvalError("string-set! expects a mutable string, index, and character")
