package ming

private[ming] object NumericBuiltins:

  import Builtins.{asLong, typeCheck}

  def install(env: Env): Unit =
    env.set(
      "abs",
      SchemeBuiltin(
        "abs",
        args =>
          if args.size != 1 then throw new EvalError("abs: expected 1 argument")
          SchemeInt(math.abs(asLong(args.head, "abs")))
      )
    )
    env.set(
      "quotient",
      SchemeBuiltin(
        "quotient",
        args =>
          if args.size != 2 then throw new EvalError("quotient: expected 2 arguments")
          val a = asLong(args(0), "quotient")
          val b = asLong(args(1), "quotient")
          if b == 0 then throw new EvalError("quotient: division by zero")
          SchemeInt(a / b)
      )
    )
    env.set(
      "remainder",
      SchemeBuiltin(
        "remainder",
        args =>
          if args.size != 2 then throw new EvalError("remainder: expected 2 arguments")
          val a = asLong(args(0), "remainder")
          val b = asLong(args(1), "remainder")
          if b == 0 then throw new EvalError("remainder: division by zero")
          SchemeInt(a % b)
      )
    )
    env.set(
      "modulo",
      SchemeBuiltin(
        "modulo",
        args =>
          if args.size != 2 then throw new EvalError("modulo: expected 2 arguments")
          val a = asLong(args(0), "modulo")
          val b = asLong(args(1), "modulo")
          if b == 0 then throw new EvalError("modulo: division by zero")
          val r = a % b
          SchemeInt(if r != 0 && ((r ^ b) < 0) then r + b else r)
      )
    )
    env.set(
      "min",
      SchemeBuiltin(
        "min",
        args =>
          if args.isEmpty then throw new EvalError("min: expected at least 1 argument")
          SchemeInt(args.map(a => asLong(a, "min")).min)
      )
    )
    env.set(
      "max",
      SchemeBuiltin(
        "max",
        args =>
          if args.isEmpty then throw new EvalError("max: expected at least 1 argument")
          SchemeInt(args.map(a => asLong(a, "max")).max)
      )
    )
    env.set(
      "expt",
      SchemeBuiltin(
        "expt",
        args =>
          if args.size != 2 then throw new EvalError("expt: expected 2 arguments")
          val base = asLong(args(0), "expt")
          val exp  = asLong(args(1), "expt")
          SchemeInt(math.pow(base.toDouble, exp.toDouble).toLong)
      )
    )
    env.set("zero?", typeCheck("zero?") { case SchemeInt(n) => n == 0; case _ => false })
    env.set("positive?", typeCheck("positive?") { case SchemeInt(n) => n > 0; case _ => false })
    env.set("negative?", typeCheck("negative?") { case SchemeInt(n) => n < 0; case _ => false })
    env.set("odd?", typeCheck("odd?") { case SchemeInt(n) => n % 2 != 0; case _ => false })
    env.set("even?", typeCheck("even?") { case SchemeInt(n) => n % 2 == 0; case _ => false })
