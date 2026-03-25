package ming

object RationalBuiltins:

  def register(env: Env): Unit =
    env.define(
      "exact?",
      SchemeVal.BuiltinProc(
        "exact?",
        args =>
          if args.size != 1 then throw new EvalError("exact?: expected 1 argument")
          SchemeVal.BoolVal(SchemeVal.isExact(args.head))
      )
    )
    env.define(
      "inexact?",
      SchemeVal.BuiltinProc(
        "inexact?",
        args =>
          if args.size != 1 then throw new EvalError("inexact?: expected 1 argument")
          args.head match
            case SchemeVal.FloatVal(_) => SchemeVal.BoolVal(true)
            case _                     => SchemeVal.BoolVal(false)
      )
    )
    env.define(
      "exact->inexact",
      SchemeVal.BuiltinProc(
        "exact->inexact",
        args =>
          if args.size != 1 then throw new EvalError("exact->inexact: expected 1 argument")
          SchemeVal.FloatVal(SchemeVal.toDouble(args.head))
      )
    )
    env.define(
      "inexact->exact",
      SchemeVal.BuiltinProc(
        "inexact->exact",
        args =>
          if args.size != 1 then throw new EvalError("inexact->exact: expected 1 argument")
          args.head match
            case v if SchemeVal.isExact(v) => v
            case SchemeVal.FloatVal(d) =>
              if d == d.toLong.toDouble then SchemeVal.IntVal(d.toLong)
              else
                val denom = 1L << 53
                val num   = (d * denom).toLong
                SchemeVal.makeRational(num, denom)
            case other =>
              throw new EvalError(s"inexact->exact: expected number, got ${SchemeVal.display(other)}")
      )
    )
    env.define(
      "numerator",
      SchemeVal.BuiltinProc(
        "numerator",
        args =>
          if args.size != 1 then throw new EvalError("numerator: expected 1 argument")
          args.head match
            case SchemeVal.IntVal(n)         => SchemeVal.IntVal(n)
            case SchemeVal.RationalVal(n, _) => SchemeVal.IntVal(n)
            case other =>
              throw new EvalError(s"numerator: expected rational, got ${SchemeVal.display(other)}")
      )
    )
    env.define(
      "denominator",
      SchemeVal.BuiltinProc(
        "denominator",
        args =>
          if args.size != 1 then throw new EvalError("denominator: expected 1 argument")
          args.head match
            case SchemeVal.IntVal(_)         => SchemeVal.IntVal(1)
            case SchemeVal.RationalVal(_, d) => SchemeVal.IntVal(d)
            case other =>
              throw new EvalError(s"denominator: expected rational, got ${SchemeVal.display(other)}")
      )
    )
    env.define(
      "integer?",
      SchemeVal.BuiltinProc(
        "integer?",
        args =>
          if args.size != 1 then throw new EvalError("integer?: expected 1 argument")
          args.head match
            case SchemeVal.IntVal(_)   => SchemeVal.BoolVal(true)
            case SchemeVal.FloatVal(d) => SchemeVal.BoolVal(d == d.toLong.toDouble && !d.isInfinite)
            case SchemeVal.RationalVal(_, _) =>
              SchemeVal.BoolVal(false)
            case _ => SchemeVal.BoolVal(false)
      )
    )
    env.define(
      "rational?",
      SchemeVal.BuiltinProc(
        "rational?",
        args =>
          if args.size != 1 then throw new EvalError("rational?: expected 1 argument")
          args.head match
            case SchemeVal.IntVal(_) | SchemeVal.RationalVal(_, _) => SchemeVal.BoolVal(true)
            case SchemeVal.FloatVal(d)                             => SchemeVal.BoolVal(!d.isNaN && !d.isInfinite)
            case _                                                 => SchemeVal.BoolVal(false)
      )
    )
