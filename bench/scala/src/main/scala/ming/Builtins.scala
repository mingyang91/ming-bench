package ming

import scala.collection.mutable

private[ming] object Builtins:

  private def asLong(v: SchemeVal, op: String): Long = v match
    case SchemeInt(n) => n
    case _            => throw new EvalError(s"$op: expected number, got ${v.display}")

  private def cmp(name: String, op: (Long, Long) => Boolean): SchemeBuiltin =
    SchemeBuiltin(
      name,
      args =>
        if args.size < 2 then throw new EvalError(s"$name: expected at least 2 arguments")
        val nums = args.map(a => asLong(a, name))
        SchemeBool(nums.sliding(2).forall(w => op(w(0), w(1))))
    )

  private def typeCheck(name: String)(pred: SchemeVal => Boolean): SchemeBuiltin =
    SchemeBuiltin(
      name,
      args =>
        if args.size != 1 then throw new EvalError(s"$name: expected 1 argument")
        SchemeBool(pred(args.head))
    )

  def install(env: Env, output: StringBuilder = new StringBuilder): Unit =
    env.set("+", SchemeBuiltin("+", args => SchemeInt(args.map(a => asLong(a, "+")).sum)))
    env.set(
      "-",
      SchemeBuiltin(
        "-",
        args =>
          if args.isEmpty then throw new EvalError("-: expected at least 1 argument")
          else if args.size == 1 then SchemeInt(-asLong(args.head, "-"))
          else SchemeInt(args.map(a => asLong(a, "-")).reduce(_ - _))
      )
    )
    env.set("*", SchemeBuiltin("*", args => SchemeInt(args.map(a => asLong(a, "*")).product)))
    env.set(
      "/",
      SchemeBuiltin(
        "/",
        args =>
          if args.size < 2 then throw new EvalError("/: expected at least 2 arguments")
          else
            val nums = args.map(a => asLong(a, "/"))
            if nums.tail.exists(_ == 0) then throw new EvalError("/: division by zero")
            SchemeInt(nums.reduce(_ / _))
      )
    )

    env.set("<", cmp("<", _ < _))
    env.set(">", cmp(">", _ > _))
    env.set("=", cmp("=", _ == _))
    env.set("<=", cmp("<=", _ <= _))
    env.set(">=", cmp(">=", _ >= _))

    env.set(
      "not",
      SchemeBuiltin(
        "not",
        args =>
          if args.size != 1 then throw new EvalError("not: expected 1 argument")
          SchemeBool(args.head match
            case SchemeBool(false) => true
            case _                 => false)
      )
    )

    installList(env)
    installPredicates(env)
    installIO(env, output)
    installStringOps(env)
    installSymbolOps(env)

  private def installList(env: Env): Unit =
    env.set(
      "cons",
      SchemeBuiltin(
        "cons",
        args =>
          if args.size != 2 then throw new EvalError("cons: expected 2 arguments")
          args(1) match
            case SchemeList(elems) => SchemeList(args(0) :: elems)
            case _                 => throw new EvalError("cons: second argument must be a list")
      )
    )

    env.set(
      "car",
      SchemeBuiltin(
        "car",
        args =>
          if args.size != 1 then throw new EvalError("car: expected 1 argument")
          args.head match
            case SchemeList(h :: _) => h
            case SchemeList(Nil)    => throw new EvalError("car: empty list")
            case _                  => throw new EvalError("car: expected pair")
      )
    )

    env.set(
      "cdr",
      SchemeBuiltin(
        "cdr",
        args =>
          if args.size != 1 then throw new EvalError("cdr: expected 1 argument")
          args.head match
            case SchemeList(_ :: t) => SchemeList(t)
            case SchemeList(Nil)    => throw new EvalError("cdr: empty list")
            case _                  => throw new EvalError("cdr: expected pair")
      )
    )

    env.set(
      "null?",
      typeCheck("null?") {
        case SchemeList(Nil) => true
        case _               => false
      }
    )

    env.set("list", SchemeBuiltin("list", args => SchemeList(args)))

    env.set(
      "length",
      SchemeBuiltin(
        "length",
        args =>
          if args.size != 1 then throw new EvalError("length: expected 1 argument")
          args.head match
            case SchemeList(elems) => SchemeInt(elems.size.toLong)
            case _                 => throw new EvalError("length: expected list")
      )
    )

    env.set(
      "append",
      SchemeBuiltin(
        "append",
        args =>
          val result = args.foldLeft(List.empty[SchemeVal]) { (acc, v) =>
            v match
              case SchemeList(elems) => acc ++ elems
              case _                 => throw new EvalError("append: expected list")
          }
          SchemeList(result)
      )
    )

  private def installPredicates(env: Env): Unit =
    env.set("number?", typeCheck("number?") { case _: SchemeInt => true; case _ => false })
    env.set("string?", typeCheck("string?") { case _: SchemeString => true; case _ => false })
    env.set("boolean?", typeCheck("boolean?") { case _: SchemeBool => true; case _ => false })
    env.set(
      "pair?",
      typeCheck("pair?") {
        case SchemeList(_ :: _) => true
        case _                  => false
      }
    )
    env.set("symbol?", typeCheck("symbol?") { case _: SchemeSymbol => true; case _ => false })
    env.set("char?", typeCheck("char?") { case _: SchemeChar => true; case _ => false })

  private def displayVal(v: SchemeVal): String = v match
    case SchemeString(s)   => s
    case SchemeList(elems) => "(" + elems.map(displayVal).mkString(" ") + ")"
    case other             => other.display

  private def installIO(env: Env, output: StringBuilder): Unit =
    env.set(
      "display",
      SchemeBuiltin(
        "display",
        args =>
          if args.size != 1 then throw new EvalError("display: expected 1 argument")
          output.append(displayVal(args.head))
          SchemeVoid
      )
    )
    env.set(
      "write",
      SchemeBuiltin(
        "write",
        args =>
          if args.size != 1 then throw new EvalError("write: expected 1 argument")
          output.append(args.head.display)
          SchemeVoid
      )
    )
    env.set(
      "newline",
      SchemeBuiltin(
        "newline",
        args =>
          if args.nonEmpty then throw new EvalError("newline: expected 0 arguments")
          output.append("\n")
          SchemeVoid
      )
    )

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
