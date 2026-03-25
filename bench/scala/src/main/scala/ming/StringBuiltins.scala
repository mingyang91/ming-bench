package ming

object StringBuiltins:

  def register(env: Env, output: StringBuilder): Unit =
    registerIO(env, output)
    registerStringOps(env)
    registerSymbolOps(env)

  private def registerIO(env: Env, output: StringBuilder): Unit =
    env.define(
      "display",
      SchemeVal.BuiltinProc(
        "display",
        args =>
          if args.size != 1 then throw new EvalError("display: expected 1 argument")
          output.append(SchemeVal.displayOutput(args.head))
          SchemeVal.Void
      )
    )
    env.define(
      "write",
      SchemeVal.BuiltinProc(
        "write",
        args =>
          if args.size != 1 then throw new EvalError("write: expected 1 argument")
          output.append(SchemeVal.display(args.head))
          SchemeVal.Void
      )
    )
    env.define(
      "newline",
      SchemeVal.BuiltinProc(
        "newline",
        args =>
          if args.nonEmpty then throw new EvalError("newline: expected 0 arguments")
          output.append("\n")
          SchemeVal.Void
      )
    )

  private def registerStringOps(env: Env): Unit =
    env.define(
      "string-append",
      SchemeVal.BuiltinProc(
        "string-append",
        args =>
          val strs = args.map {
            case SchemeVal.StringVal(s) => s
            case other => throw new EvalError(s"string-append: expected string, got ${SchemeVal.display(other)}")
          }
          SchemeVal.StringVal(strs.mkString)
      )
    )
    env.define(
      "string-length",
      SchemeVal.BuiltinProc(
        "string-length",
        args =>
          if args.size != 1 then throw new EvalError("string-length: expected 1 argument")
          args.head match
            case SchemeVal.StringVal(s) => SchemeVal.IntVal(s.length.toLong)
            case other => throw new EvalError(s"string-length: expected string, got ${SchemeVal.display(other)}")
      )
    )
    env.define(
      "substring",
      SchemeVal.BuiltinProc(
        "substring",
        args =>
          if args.size != 3 then throw new EvalError("substring: expected 3 arguments")
          (args(0), args(1), args(2)) match
            case (SchemeVal.StringVal(s), SchemeVal.IntVal(start), SchemeVal.IntVal(end)) =>
              SchemeVal.StringVal(s.substring(start.toInt, end.toInt))
            case _ => throw new EvalError("substring: invalid arguments")
      )
    )
    env.define(
      "string->number",
      SchemeVal.BuiltinProc(
        "string->number",
        args =>
          if args.size != 1 then throw new EvalError("string->number: expected 1 argument")
          args.head match
            case SchemeVal.StringVal(s) =>
              try SchemeVal.IntVal(s.toLong)
              catch case _: NumberFormatException => SchemeVal.BoolVal(false)
            case other => throw new EvalError(s"string->number: expected string, got ${SchemeVal.display(other)}")
      )
    )
    env.define(
      "number->string",
      SchemeVal.BuiltinProc(
        "number->string",
        args =>
          if args.size != 1 then throw new EvalError("number->string: expected 1 argument")
          args.head match
            case SchemeVal.IntVal(n) => SchemeVal.StringVal(n.toString)
            case other => throw new EvalError(s"number->string: expected number, got ${SchemeVal.display(other)}")
      )
    )
    env.define(
      "string-ref",
      SchemeVal.BuiltinProc(
        "string-ref",
        args =>
          if args.size != 2 then throw new EvalError("string-ref: expected 2 arguments")
          (args(0), args(1)) match
            case (SchemeVal.StringVal(s), SchemeVal.IntVal(i)) =>
              SchemeVal.CharVal(s.charAt(i.toInt))
            case _ => throw new EvalError("string-ref: invalid arguments")
      )
    )

  private def registerSymbolOps(env: Env): Unit =
    env.define(
      "symbol->string",
      SchemeVal.BuiltinProc(
        "symbol->string",
        args =>
          if args.size != 1 then throw new EvalError("symbol->string: expected 1 argument")
          args.head match
            case SchemeVal.Symbol(name) => SchemeVal.StringVal(name)
            case other => throw new EvalError(s"symbol->string: expected symbol, got ${SchemeVal.display(other)}")
      )
    )
    env.define(
      "string->symbol",
      SchemeVal.BuiltinProc(
        "string->symbol",
        args =>
          if args.size != 1 then throw new EvalError("string->symbol: expected 1 argument")
          args.head match
            case SchemeVal.StringVal(s) => SchemeVal.Symbol(s)
            case other => throw new EvalError(s"string->symbol: expected string, got ${SchemeVal.display(other)}")
      )
    )
