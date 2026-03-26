package ming

private[ming] object Builtins:

  private[ming] def asLong(v: SchemeVal, op: String): Long = v match
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

  private[ming] def typeCheck(name: String)(pred: SchemeVal => Boolean): SchemeBuiltin =
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

    installPredicates(env)
    installIO(env, output)
    installApply(env)
    ListBuiltins.install(env)
    StringBuiltins.install(env)
    NumericBuiltins.install(env)
    CharStringBuiltins.install(env)

  private def installApply(env: Env): Unit =
    env.set(
      "apply",
      SchemeBuiltin(
        "apply",
        args =>
          if args.size < 2 then throw new EvalError("apply: expected at least 2 arguments")
          val proc    = args.head
          val lastArg = args.last
          val lastList = lastArg match
            case SchemeList(elems) => elems
            case _                 => throw new EvalError("apply: last argument must be a list")
          val prefixArgs = args.slice(1, args.size - 1)
          val allArgs    = prefixArgs ++ lastList
          Evaluator.applyProc(proc, allArgs)
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
        case _: SchemePair      => true
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
