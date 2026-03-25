package ming

/** String built-in operations. */
object StringOps:

  private def asInt(v: SchemeVal): Long = v match
    case SchemeVal.SInt(n) => n
    case other             => throw new EvalError(s"expected number, got ${other.display}")

  private def asChar(v: SchemeVal): Char = v match
    case SchemeVal.SChar(c) => c
    case other              => throw new EvalError(s"expected char, got ${other.display}")

  private def asString(v: SchemeVal): StringBuilder = v match
    case SchemeVal.SString(s, _) => s
    case other                   => throw new EvalError(s"expected string, got ${other.display}")

  private def requireOne(name: String, args: List[SchemeVal]): SchemeVal =
    if args.length != 1 then throw new EvalError(s"$name: expected 1 argument")
    args.head

  private def requireTwo(
    name: String,
    args: List[SchemeVal]
  ): (SchemeVal, SchemeVal) =
    if args.length != 2 then throw new EvalError(s"$name: expected 2 arguments")
    (args(0), args(1))

  def apply(name: String, args: List[SchemeVal]): SchemeVal =
    name match
      case "string-append" | "string-length" | "substring" | "string-ref" | "string-copy" | "string-set!" =>
        applyCore(name, args)
      case "string->number" | "number->string" | "symbol->string" | "string->symbol" | "string->list" |
          "list->string" =>
        applyConversion(name, args)
      case "string=?" | "string<?" | "string-ci=?" | "string-upcase" | "string-downcase" =>
        applyCompareCase(name, args)
      case _ => throw new EvalError(s"unknown string op: $name")

  private def applyCore(name: String, args: List[SchemeVal]): SchemeVal =
    name match
      case "string-append" =>
        val strs = args.map {
          case SchemeVal.SString(s, _) => s.toString
          case other =>
            throw new EvalError(
              s"string-append: expected string, got ${other.display}"
            )
        }
        SchemeVal.SString(new StringBuilder(strs.mkString), true)
      case "string-length" =>
        requireOne("string-length", args) match
          case SchemeVal.SString(s, _) => SchemeVal.SInt(s.length.toLong)
          case other =>
            throw new EvalError(
              s"string-length: expected string, got ${other.display}"
            )
      case "substring" =>
        if args.length != 3 then throw new EvalError("substring: expected 3 arguments")
        (args(0), args(1), args(2)) match
          case (SchemeVal.SString(s, _), SchemeVal.SInt(start), SchemeVal.SInt(end)) =>
            SchemeVal.SString(
              new StringBuilder(s.toString.substring(start.toInt, end.toInt)),
              true
            )
          case _ =>
            throw new EvalError("substring: expected string and two integers")
      case "string-ref" =>
        if args.length != 2 then throw new EvalError("string-ref: expected 2 arguments")
        (args(0), args(1)) match
          case (SchemeVal.SString(s, _), SchemeVal.SInt(i)) =>
            SchemeVal.SChar(s.charAt(i.toInt))
          case _ =>
            throw new EvalError("string-ref: expected string and integer")
      case "string-copy" =>
        requireOne("string-copy", args) match
          case SchemeVal.SString(s, _) => SchemeVal.SString(new StringBuilder(s.toString), true)
          case other =>
            throw new EvalError(
              s"string-copy: expected string, got ${other.display}"
            )
      case "string-set!" =>
        if args.length != 3 then throw new EvalError("string-set!: expected 3 arguments")
        args(0) match
          case SchemeVal.SString(s, true) =>
            val idx = asInt(args(1)).toInt
            val ch  = asChar(args(2))
            s.setCharAt(idx, ch)
            SchemeVal.SVoid
          case SchemeVal.SString(_, false) =>
            throw new EvalError("string-set!: strings are immutable")
          case other =>
            throw new EvalError(s"string-set!: expected string, got ${other.display}")
      case _ => throw new EvalError(s"unknown string op: $name")

  private def applyConversion(name: String, args: List[SchemeVal]): SchemeVal =
    name match
      case "string->number" =>
        requireOne("string->number", args) match
          case SchemeVal.SString(s, _) =>
            s.toString.toLongOption match
              case Some(n) => SchemeVal.SInt(n)
              case None    => SchemeVal.SBool(false)
          case other =>
            throw new EvalError(s"string->number: expected string, got ${other.display}")
      case "number->string" =>
        val arg = requireOne("number->string", args)
        arg match
          case SchemeVal.SInt(n)         => SchemeVal.SString(new StringBuilder(n.toString), true)
          case SchemeVal.SFloat(d)       => SchemeVal.SString(new StringBuilder(arg.display), true)
          case SchemeVal.SRational(n, d) => SchemeVal.SString(new StringBuilder(arg.display), true)
          case other =>
            throw new EvalError(s"number->string: expected number, got ${other.display}")
      case "symbol->string" =>
        requireOne("symbol->string", args) match
          case SchemeVal.SSymbol(n) => SchemeVal.SString(new StringBuilder(n), true)
          case other =>
            throw new EvalError(s"symbol->string: expected symbol, got ${other.display}")
      case "string->symbol" =>
        requireOne("string->symbol", args) match
          case SchemeVal.SString(s, _) => SchemeVal.SSymbol(s.toString)
          case other =>
            throw new EvalError(s"string->symbol: expected string, got ${other.display}")
      case "string->list" =>
        requireOne("string->list", args) match
          case SchemeVal.SString(s, _) =>
            val chars = s.toString.toList.map(SchemeVal.SChar(_))
            if chars.isEmpty then SchemeVal.SList(Nil)
            else SchemeVal.buildList(chars)
          case other =>
            throw new EvalError(s"string->list: expected string, got ${other.display}")
      case "list->string" =>
        val elems = SchemeVal
          .toScalaList(requireOne("list->string", args))
          .getOrElse(
            throw new EvalError("list->string: expected list")
          )
        val chars = elems.map {
          case SchemeVal.SChar(c) => c
          case other              => throw new EvalError(s"list->string: expected char, got ${other.display}")
        }
        SchemeVal.SString(new StringBuilder(chars.mkString), true)
      case "make-string" =>
        args match
          case SchemeVal.SInt(n) :: Nil =>
            SchemeVal.SString(new StringBuilder(" " * n.toInt), true)
          case SchemeVal.SInt(n) :: SchemeVal.SChar(c) :: Nil =>
            SchemeVal.SString(new StringBuilder(c.toString * n.toInt), true)
          case _ => throw new EvalError("make-string: expected (length) or (length char)")
      case "string" =>
        val chars = args.map {
          case SchemeVal.SChar(c) => c
          case other              => throw new EvalError(s"string: expected char, got ${other.display}")
        }
        SchemeVal.SString(new StringBuilder(chars.mkString), true)
      case _ => throw new EvalError(s"unknown string op: $name")

  private def applyCompareCase(name: String, args: List[SchemeVal]): SchemeVal =
    name match
      case "string=?" =>
        val (a, b) = requireTwo(name, args)
        SchemeVal.SBool(asString(a).toString == asString(b).toString)
      case "string<?" =>
        val (a, b) = requireTwo(name, args)
        SchemeVal.SBool(asString(a).toString < asString(b).toString)
      case "string>?" =>
        val (a, b) = requireTwo(name, args)
        SchemeVal.SBool(asString(a).toString > asString(b).toString)
      case "string<=?" =>
        val (a, b) = requireTwo(name, args)
        SchemeVal.SBool(asString(a).toString <= asString(b).toString)
      case "string>=?" =>
        val (a, b) = requireTwo(name, args)
        SchemeVal.SBool(asString(a).toString >= asString(b).toString)
      case "string-ci=?" =>
        val (a, b) = requireTwo(name, args)
        SchemeVal.SBool(
          asString(a).toString.equalsIgnoreCase(asString(b).toString)
        )
      case "string-upcase" =>
        val s = asString(requireOne(name, args))
        SchemeVal.SString(new StringBuilder(s.toString.toUpperCase), true)
      case "string-downcase" =>
        val s = asString(requireOne(name, args))
        SchemeVal.SString(new StringBuilder(s.toString.toLowerCase), true)
      case _ => throw new EvalError(s"unknown string op: $name")
