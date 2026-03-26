package ming

private[ming] object CharStringBuiltins:

  import Builtins.typeCheck

  def install(env: Env): Unit =
    installCharUtils(env)
    installStringUtils(env)

  private def installCharUtils(env: Env): Unit =
    env.set(
      "char-alphabetic?",
      typeCheck("char-alphabetic?") { case SchemeChar(c) => c.isLetter; case _ => false }
    )
    env.set(
      "char-numeric?",
      typeCheck("char-numeric?") { case SchemeChar(c) => c.isDigit; case _ => false }
    )
    env.set(
      "char-upcase",
      SchemeBuiltin(
        "char-upcase",
        args =>
          if args.size != 1 then throw new EvalError("char-upcase: expected 1 argument")
          args.head match
            case SchemeChar(c) => SchemeChar(c.toUpper)
            case _             => throw new EvalError("char-upcase: expected char")
      )
    )
    env.set(
      "char-downcase",
      SchemeBuiltin(
        "char-downcase",
        args =>
          if args.size != 1 then throw new EvalError("char-downcase: expected 1 argument")
          args.head match
            case SchemeChar(c) => SchemeChar(c.toLower)
            case _             => throw new EvalError("char-downcase: expected char")
      )
    )
    env.set(
      "char=?",
      SchemeBuiltin(
        "char=?",
        args =>
          if args.size != 2 then throw new EvalError("char=?: expected 2 arguments")
          (args(0), args(1)) match
            case (SchemeChar(a), SchemeChar(b)) => SchemeBool(a == b)
            case _                              => throw new EvalError("char=?: expected chars")
      )
    )
    env.set(
      "char<?",
      SchemeBuiltin(
        "char<?",
        args =>
          if args.size != 2 then throw new EvalError("char<?: expected 2 arguments")
          (args(0), args(1)) match
            case (SchemeChar(a), SchemeChar(b)) => SchemeBool(a < b)
            case _                              => throw new EvalError("char<?: expected chars")
      )
    )

  private def installStringUtils(env: Env): Unit =
    env.set(
      "string=?",
      SchemeBuiltin(
        "string=?",
        args =>
          if args.size != 2 then throw new EvalError("string=?: expected 2 arguments")
          (args(0), args(1)) match
            case (SchemeString(a), SchemeString(b)) => SchemeBool(a == b)
            case _                                  => throw new EvalError("string=?: expected strings")
      )
    )
    env.set(
      "string<?",
      SchemeBuiltin(
        "string<?",
        args =>
          if args.size != 2 then throw new EvalError("string<?: expected 2 arguments")
          (args(0), args(1)) match
            case (SchemeString(a), SchemeString(b)) => SchemeBool(a < b)
            case _                                  => throw new EvalError("string<?: expected strings")
      )
    )
    env.set(
      "string>?",
      SchemeBuiltin(
        "string>?",
        args =>
          if args.size != 2 then throw new EvalError("string>?: expected 2 arguments")
          (args(0), args(1)) match
            case (SchemeString(a), SchemeString(b)) => SchemeBool(a > b)
            case _                                  => throw new EvalError("string>?: expected strings")
      )
    )
    env.set(
      "string<=?",
      SchemeBuiltin(
        "string<=?",
        args =>
          if args.size != 2 then throw new EvalError("string<=?: expected 2 arguments")
          (args(0), args(1)) match
            case (SchemeString(a), SchemeString(b)) => SchemeBool(a <= b)
            case _                                  => throw new EvalError("string<=?: expected strings")
      )
    )
    env.set(
      "string>=?",
      SchemeBuiltin(
        "string>=?",
        args =>
          if args.size != 2 then throw new EvalError("string>=?: expected 2 arguments")
          (args(0), args(1)) match
            case (SchemeString(a), SchemeString(b)) => SchemeBool(a >= b)
            case _                                  => throw new EvalError("string>=?: expected strings")
      )
    )
    env.set(
      "string-ci=?",
      SchemeBuiltin(
        "string-ci=?",
        args =>
          if args.size != 2 then throw new EvalError("string-ci=?: expected 2 arguments")
          (args(0), args(1)) match
            case (SchemeString(a), SchemeString(b)) => SchemeBool(a.equalsIgnoreCase(b))
            case _                                  => throw new EvalError("string-ci=?: expected strings")
      )
    )
    env.set(
      "string-upcase",
      SchemeBuiltin(
        "string-upcase",
        args =>
          if args.size != 1 then throw new EvalError("string-upcase: expected 1 argument")
          args.head match
            case SchemeString(s) => SchemeString(s.toUpperCase)
            case _               => throw new EvalError("string-upcase: expected string")
      )
    )
    env.set(
      "string-downcase",
      SchemeBuiltin(
        "string-downcase",
        args =>
          if args.size != 1 then throw new EvalError("string-downcase: expected 1 argument")
          args.head match
            case SchemeString(s) => SchemeString(s.toLowerCase)
            case _               => throw new EvalError("string-downcase: expected string")
      )
    )
