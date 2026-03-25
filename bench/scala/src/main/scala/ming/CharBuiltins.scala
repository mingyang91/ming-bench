package ming

object CharBuiltins:

  def register(env: Env): Unit =
    registerCharOps(env)
    registerStringCompare(env)

  private def registerCharOps(env: Env): Unit =
    env.define(
      "char-alphabetic?",
      SchemeVal.BuiltinProc(
        "char-alphabetic?",
        args =>
          if args.size != 1 then throw new EvalError("char-alphabetic?: expected 1 argument")
          args.head match
            case SchemeVal.CharVal(c) => SchemeVal.BoolVal(c.isLetter)
            case other =>
              throw new EvalError(
                s"char-alphabetic?: expected char, got ${SchemeVal.display(other)}"
              )
      )
    )
    env.define(
      "char-numeric?",
      SchemeVal.BuiltinProc(
        "char-numeric?",
        args =>
          if args.size != 1 then throw new EvalError("char-numeric?: expected 1 argument")
          args.head match
            case SchemeVal.CharVal(c) => SchemeVal.BoolVal(c.isDigit)
            case other =>
              throw new EvalError(
                s"char-numeric?: expected char, got ${SchemeVal.display(other)}"
              )
      )
    )
    env.define(
      "char-upcase",
      SchemeVal.BuiltinProc(
        "char-upcase",
        args =>
          if args.size != 1 then throw new EvalError("char-upcase: expected 1 argument")
          args.head match
            case SchemeVal.CharVal(c) => SchemeVal.CharVal(c.toUpper)
            case other =>
              throw new EvalError(
                s"char-upcase: expected char, got ${SchemeVal.display(other)}"
              )
      )
    )
    env.define(
      "char-downcase",
      SchemeVal.BuiltinProc(
        "char-downcase",
        args =>
          if args.size != 1 then throw new EvalError("char-downcase: expected 1 argument")
          args.head match
            case SchemeVal.CharVal(c) => SchemeVal.CharVal(c.toLower)
            case other =>
              throw new EvalError(
                s"char-downcase: expected char, got ${SchemeVal.display(other)}"
              )
      )
    )
    env.define(
      "char=?",
      SchemeVal.BuiltinProc(
        "char=?",
        args =>
          if args.size != 2 then throw new EvalError("char=?: expected 2 arguments")
          (args(0), args(1)) match
            case (SchemeVal.CharVal(a), SchemeVal.CharVal(b)) => SchemeVal.BoolVal(a == b)
            case _                                            => throw new EvalError("char=?: expected chars")
      )
    )
    env.define(
      "char<?",
      SchemeVal.BuiltinProc(
        "char<?",
        args =>
          if args.size != 2 then throw new EvalError("char<?: expected 2 arguments")
          (args(0), args(1)) match
            case (SchemeVal.CharVal(a), SchemeVal.CharVal(b)) => SchemeVal.BoolVal(a < b)
            case _                                            => throw new EvalError("char<?: expected chars")
      )
    )

  private def registerStringCompare(env: Env): Unit =
    env.define(
      "string=?",
      SchemeVal.BuiltinProc(
        "string=?",
        args =>
          if args.size != 2 then throw new EvalError("string=?: expected 2 arguments")
          (args(0), args(1)) match
            case (SchemeVal.StringVal(a), SchemeVal.StringVal(b)) =>
              SchemeVal.BoolVal(java.util.Arrays.equals(a, b))
            case _ => throw new EvalError("string=?: expected strings")
      )
    )
    env.define(
      "string<?",
      SchemeVal.BuiltinProc(
        "string<?",
        args =>
          if args.size != 2 then throw new EvalError("string<?: expected 2 arguments")
          (args(0), args(1)) match
            case (SchemeVal.StringVal(a), SchemeVal.StringVal(b)) =>
              SchemeVal.BoolVal(new String(a).compareTo(new String(b)) < 0)
            case _ => throw new EvalError("string<?: expected strings")
      )
    )
    env.define(
      "string-ci=?",
      SchemeVal.BuiltinProc(
        "string-ci=?",
        args =>
          if args.size != 2 then throw new EvalError("string-ci=?: expected 2 arguments")
          (args(0), args(1)) match
            case (SchemeVal.StringVal(a), SchemeVal.StringVal(b)) =>
              SchemeVal.BoolVal(new String(a).equalsIgnoreCase(new String(b)))
            case _ => throw new EvalError("string-ci=?: expected strings")
      )
    )
    env.define(
      "string-upcase",
      SchemeVal.BuiltinProc(
        "string-upcase",
        args =>
          if args.size != 1 then throw new EvalError("string-upcase: expected 1 argument")
          args.head match
            case SchemeVal.StringVal(chars) => SchemeVal.str(new String(chars).toUpperCase)
            case other =>
              throw new EvalError(
                s"string-upcase: expected string, got ${SchemeVal.display(other)}"
              )
      )
    )
    env.define(
      "string-downcase",
      SchemeVal.BuiltinProc(
        "string-downcase",
        args =>
          if args.size != 1 then throw new EvalError("string-downcase: expected 1 argument")
          args.head match
            case SchemeVal.StringVal(chars) => SchemeVal.str(new String(chars).toLowerCase)
            case other =>
              throw new EvalError(
                s"string-downcase: expected string, got ${SchemeVal.display(other)}"
              )
      )
    )
