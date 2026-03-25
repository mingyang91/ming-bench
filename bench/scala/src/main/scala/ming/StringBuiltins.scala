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

  private def asString(v: SchemeVal, name: String): String = v match
    case SchemeVal.StringVal(chars) => new String(chars)
    case other                      => throw new EvalError(s"$name: expected string, got ${SchemeVal.display(other)}")

  private def registerStringOps(env: Env): Unit =
    registerStringCore(env)
    registerStringConversion(env)
    registerCharOps(env)

  private def registerStringCore(env: Env): Unit =
    env.define(
      "make-string",
      SchemeVal.BuiltinProc(
        "make-string",
        args =>
          if args.isEmpty || args.size > 2 then throw new EvalError("make-string: expected 1-2 arguments")
          val k = args(0) match
            case SchemeVal.IntVal(n) => n.toInt
            case _                   => throw new EvalError("make-string: expected integer")
          val fill =
            if args.size == 2 then
              args(1) match
                case SchemeVal.CharVal(c) => c
                case _                    => throw new EvalError("make-string: expected character")
            else '\u0000'
          SchemeVal.StringVal(Array.fill(k)(fill))
      )
    )
    env.define(
      "string",
      SchemeVal.BuiltinProc(
        "string",
        args =>
          val chars = args.map {
            case SchemeVal.CharVal(c) => c
            case other => throw new EvalError(s"string: expected character, got ${SchemeVal.display(other)}")
          }
          SchemeVal.StringVal(chars.toArray)
      )
    )
    env.define(
      "string-append",
      SchemeVal.BuiltinProc(
        "string-append",
        args =>
          val strs = args.map(a => asString(a, "string-append"))
          SchemeVal.str(strs.mkString)
      )
    )
    env.define(
      "string-length",
      SchemeVal.BuiltinProc(
        "string-length",
        args =>
          if args.size != 1 then throw new EvalError("string-length: expected 1 argument")
          args.head match
            case SchemeVal.StringVal(chars) => SchemeVal.IntVal(chars.length.toLong)
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
            case (SchemeVal.StringVal(chars), SchemeVal.IntVal(start), SchemeVal.IntVal(end)) =>
              SchemeVal.str(new String(chars, start.toInt, end.toInt - start.toInt))
            case _ => throw new EvalError("substring: invalid arguments")
      )
    )
    env.define(
      "string-ref",
      SchemeVal.BuiltinProc(
        "string-ref",
        args =>
          if args.size != 2 then throw new EvalError("string-ref: expected 2 arguments")
          (args(0), args(1)) match
            case (SchemeVal.StringVal(chars), SchemeVal.IntVal(i)) =>
              SchemeVal.CharVal(chars(i.toInt))
            case _ => throw new EvalError("string-ref: invalid arguments")
      )
    )
    env.define(
      "string-copy",
      SchemeVal.BuiltinProc(
        "string-copy",
        args =>
          if args.size != 1 then throw new EvalError("string-copy: expected 1 argument")
          args.head match
            case SchemeVal.StringVal(chars) => SchemeVal.StringVal(chars.clone())
            case other => throw new EvalError(s"string-copy: expected string, got ${SchemeVal.display(other)}")
      )
    )
    env.define(
      "string-set!",
      SchemeVal.BuiltinProc(
        "string-set!",
        args =>
          if args.size != 3 then throw new EvalError("string-set!: expected 3 arguments")
          (args(0), args(1), args(2)) match
            case (sv @ SchemeVal.StringVal(chars), SchemeVal.IntVal(i), SchemeVal.CharVal(c)) =>
              if sv.immutable then throw new EvalError("string-set!: string is immutable")
              chars(i.toInt) = c
              SchemeVal.Void
            case _ => throw new EvalError("string-set!: invalid arguments")
      )
    )

  private def registerStringConversion(env: Env): Unit =
    env.define(
      "string->number",
      SchemeVal.BuiltinProc(
        "string->number",
        args =>
          if args.size != 1 then throw new EvalError("string->number: expected 1 argument")
          args.head match
            case SchemeVal.StringVal(chars) =>
              val s = new String(chars)
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
            case SchemeVal.IntVal(n) => SchemeVal.str(n.toString)
            case other => throw new EvalError(s"number->string: expected number, got ${SchemeVal.display(other)}")
      )
    )
    env.define(
      "string->list",
      SchemeVal.BuiltinProc(
        "string->list",
        args =>
          if args.size != 1 then throw new EvalError("string->list: expected 1 argument")
          args.head match
            case SchemeVal.StringVal(chars) =>
              SchemeVal.SList(chars.toList.map(SchemeVal.CharVal(_)))
            case other => throw new EvalError(s"string->list: expected string, got ${SchemeVal.display(other)}")
      )
    )
    env.define(
      "list->string",
      SchemeVal.BuiltinProc(
        "list->string",
        args =>
          if args.size != 1 then throw new EvalError("list->string: expected 1 argument")
          val elems = args.head match
            case SchemeVal.SList(elems) => elems
            case SchemeVal.Pair(_)      => SchemeVal.toScalaList(args.head)
            case other => throw new EvalError(s"list->string: expected list, got ${SchemeVal.display(other)}")
          val chars = elems.map {
            case SchemeVal.CharVal(c) => c
            case other => throw new EvalError(s"list->string: expected character, got ${SchemeVal.display(other)}")
          }
          SchemeVal.StringVal(chars.toArray)
      )
    )

  private def registerCharOps(env: Env): Unit =
    env.define(
      "char->integer",
      SchemeVal.BuiltinProc(
        "char->integer",
        args =>
          if args.size != 1 then throw new EvalError("char->integer: expected 1 argument")
          args.head match
            case SchemeVal.CharVal(c) => SchemeVal.IntVal(c.toLong)
            case other => throw new EvalError(s"char->integer: expected character, got ${SchemeVal.display(other)}")
      )
    )
    env.define(
      "integer->char",
      SchemeVal.BuiltinProc(
        "integer->char",
        args =>
          if args.size != 1 then throw new EvalError("integer->char: expected 1 argument")
          args.head match
            case SchemeVal.IntVal(n) => SchemeVal.CharVal(n.toChar)
            case other => throw new EvalError(s"integer->char: expected integer, got ${SchemeVal.display(other)}")
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
            case SchemeVal.Symbol(name) => SchemeVal.str(name)
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
            case SchemeVal.StringVal(chars) => SchemeVal.Symbol(new String(chars))
            case other => throw new EvalError(s"string->symbol: expected string, got ${SchemeVal.display(other)}")
      )
    )
