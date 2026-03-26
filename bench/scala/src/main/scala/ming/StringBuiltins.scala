package ming

private[ming] object StringBuiltins:

  private def installStringOps(env: Env): Unit =
    env.set(
      "string-length",
      SchemeBuiltin(
        "string-length",
        args =>
          if args.size != 1 then throw new EvalError("string-length: expected 1 argument")
          args.head match
            case SchemeString(s) => SchemeInt(s.length.toLong)
            case _               => throw new EvalError("string-length: expected string")
      )
    )
    env.set(
      "string-append",
      SchemeBuiltin(
        "string-append",
        args =>
          val parts = args.map {
            case SchemeString(s) => s
            case _               => throw new EvalError("string-append: expected string")
          }
          SchemeString(parts.mkString)
      )
    )
    env.set(
      "substring",
      SchemeBuiltin(
        "substring",
        args =>
          if args.size != 3 then throw new EvalError("substring: expected 3 arguments")
          (args(0), args(1), args(2)) match
            case (SchemeString(s), SchemeInt(start), SchemeInt(end)) =>
              SchemeString(s.substring(start.toInt, end.toInt))
            case _ => throw new EvalError("substring: expected string, int, int")
      )
    )
    env.set(
      "string-ref",
      SchemeBuiltin(
        "string-ref",
        args =>
          if args.size != 2 then throw new EvalError("string-ref: expected 2 arguments")
          (args(0), args(1)) match
            case (SchemeString(s), SchemeInt(i)) => SchemeChar(s.charAt(i.toInt))
            case _                               => throw new EvalError("string-ref: expected string and int")
      )
    )
    env.set(
      "string->number",
      SchemeBuiltin(
        "string->number",
        args =>
          if args.size != 1 then throw new EvalError("string->number: expected 1 argument")
          args.head match
            case SchemeString(s) =>
              try SchemeInt(s.toLong)
              catch case _: NumberFormatException => SchemeBool(false)
            case _ => throw new EvalError("string->number: expected string")
      )
    )
    env.set(
      "number->string",
      SchemeBuiltin(
        "number->string",
        args =>
          if args.size != 1 then throw new EvalError("number->string: expected 1 argument")
          args.head match
            case SchemeInt(n) => SchemeString(n.toString)
            case _            => throw new EvalError("number->string: expected number")
      )
    )
    env.set(
      "string-copy",
      SchemeBuiltin(
        "string-copy",
        args =>
          if args.size != 1 then throw new EvalError("string-copy: expected 1 argument")
          args.head match
            case ss: SchemeString => new SchemeString(ss.chars.clone())
            case _                => throw new EvalError("string-copy: expected string")
      )
    )
    env.set(
      "string-set!",
      SchemeBuiltin(
        "string-set!",
        args =>
          if args.size != 3 then throw new EvalError("string-set!: expected 3 arguments")
          (args(0), args(1), args(2)) match
            case (ss: SchemeString, SchemeInt(i), SchemeChar(c)) =>
              ss.chars(i.toInt) = c
              SchemeVoid
            case _ => throw new EvalError("string-set!: expected string, int, char")
      )
    )

  private def installSymbolOps(env: Env): Unit =
    env.set(
      "symbol->string",
      SchemeBuiltin(
        "symbol->string",
        args =>
          if args.size != 1 then throw new EvalError("symbol->string: expected 1 argument")
          args.head match
            case SchemeSymbol(name) => SchemeString(name)
            case _                  => throw new EvalError("symbol->string: expected symbol")
      )
    )
    env.set(
      "string->symbol",
      SchemeBuiltin(
        "string->symbol",
        args =>
          if args.size != 1 then throw new EvalError("string->symbol: expected 1 argument")
          args.head match
            case SchemeString(s) => SchemeSymbol(s)
            case _               => throw new EvalError("string->symbol: expected string")
      )
    )

  def install(env: Env): Unit =
    installStringOps(env)
    installSymbolOps(env)
