package ming

private[ming] object NumericBuiltins:

  import Builtins.{asLong, typeCheck}

  def install(env: Env): Unit =
    installArithmetic(env)
    installPredicates(env)
    installConversions(env)
    installRationalAccessors(env)

  private def installArithmetic(env: Env): Unit =
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

  private def installPredicates(env: Env): Unit =
    env.set(
      "zero?",
      typeCheck("zero?") {
        case SchemeInt(n)         => n == 0
        case SchemeRational(n, _) => n == 0
        case SchemeFloat(f)       => f == 0.0
        case _                    => false
      }
    )
    env.set(
      "positive?",
      typeCheck("positive?") {
        case SchemeInt(n)         => n > 0
        case SchemeRational(n, _) => n > 0
        case SchemeFloat(f)       => f > 0.0
        case _                    => false
      }
    )
    env.set(
      "negative?",
      typeCheck("negative?") {
        case SchemeInt(n)         => n < 0
        case SchemeRational(n, _) => n < 0
        case SchemeFloat(f)       => f < 0.0
        case _                    => false
      }
    )
    env.set("odd?", typeCheck("odd?") { case SchemeInt(n) => n % 2 != 0; case _ => false })
    env.set("even?", typeCheck("even?") { case SchemeInt(n) => n % 2 == 0; case _ => false })

  private def installConversions(env: Env): Unit =
    env.set(
      "exact->inexact",
      SchemeBuiltin(
        "exact->inexact",
        args =>
          if args.size != 1 then throw new EvalError("exact->inexact: expected 1 argument")
          args.head match
            case SchemeInt(n)         => SchemeFloat(n.toDouble)
            case SchemeRational(n, d) => SchemeFloat(n.toDouble / d.toDouble)
            case f: SchemeFloat       => f
            case v                    => throw new EvalError(s"exact->inexact: expected number, got ${v.display}")
      )
    )
    env.set(
      "inexact->exact",
      SchemeBuiltin(
        "inexact->exact",
        args =>
          if args.size != 1 then throw new EvalError("inexact->exact: expected 1 argument")
          args.head match
            case i: SchemeInt      => i
            case r: SchemeRational => r
            case SchemeFloat(f) =>
              if f == math.floor(f) && !f.isInfinite then SchemeInt(f.toLong)
              else
                val bits = java.lang.Double.doubleToLongBits(f)
                val mant = bits & 0xfffffffffffffL | 0x10000000000000L
                val exp  = ((bits >> 52) & 0x7ff).toInt - 1023 - 52
                if exp >= 0 then SchemeInt(mant * (1L << exp))
                else Builtins.makeRational(mant, 1L << (-exp))
            case v => throw new EvalError(s"inexact->exact: expected number, got ${v.display}")
      )
    )

  private def installRationalAccessors(env: Env): Unit =
    env.set(
      "numerator",
      SchemeBuiltin(
        "numerator",
        args =>
          if args.size != 1 then throw new EvalError("numerator: expected 1 argument")
          args.head match
            case SchemeInt(n)         => SchemeInt(n)
            case SchemeRational(n, _) => SchemeInt(n)
            case v                    => throw new EvalError(s"numerator: expected rational, got ${v.display}")
      )
    )
    env.set(
      "denominator",
      SchemeBuiltin(
        "denominator",
        args =>
          if args.size != 1 then throw new EvalError("denominator: expected 1 argument")
          args.head match
            case SchemeInt(_)         => SchemeInt(1)
            case SchemeRational(_, d) => SchemeInt(d)
            case v                    => throw new EvalError(s"denominator: expected rational, got ${v.display}")
      )
    )
