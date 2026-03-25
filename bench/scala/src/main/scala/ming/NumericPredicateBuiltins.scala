package ming

/** Numeric predicates (zero?, positive?, negative?, odd?, even?) and utilities (gcd, lcm, truncate, round). */
object NumericPredicateBuiltins:

  def register(env: Env): Unit =
    registerSignPredicates(env)
    registerParityPredicates(env)
    registerGcdLcm(env)
    registerRounding(env)

  private def registerSignPredicates(env: Env): Unit =
    env.define(
      "zero?",
      SchemeVal.BuiltinProc(
        "zero?",
        args =>
          if args.size != 1 then throw new EvalError("zero?: expected 1 argument")
          args.head match
            case SchemeVal.IntVal(n)         => SchemeVal.BoolVal(n == 0)
            case SchemeVal.FloatVal(d)       => SchemeVal.BoolVal(d == 0.0)
            case SchemeVal.RationalVal(n, _) => SchemeVal.BoolVal(n == 0)
            case other =>
              throw new EvalError(s"zero?: expected number, got ${SchemeVal.display(other)}")
      )
    )
    env.define(
      "positive?",
      SchemeVal.BuiltinProc(
        "positive?",
        args =>
          if args.size != 1 then throw new EvalError("positive?: expected 1 argument")
          args.head match
            case SchemeVal.IntVal(n)         => SchemeVal.BoolVal(n > 0)
            case SchemeVal.FloatVal(d)       => SchemeVal.BoolVal(d > 0)
            case SchemeVal.RationalVal(n, _) => SchemeVal.BoolVal(n > 0)
            case other =>
              throw new EvalError(s"positive?: expected number, got ${SchemeVal.display(other)}")
      )
    )
    env.define(
      "negative?",
      SchemeVal.BuiltinProc(
        "negative?",
        args =>
          if args.size != 1 then throw new EvalError("negative?: expected 1 argument")
          args.head match
            case SchemeVal.IntVal(n)         => SchemeVal.BoolVal(n < 0)
            case SchemeVal.FloatVal(d)       => SchemeVal.BoolVal(d < 0)
            case SchemeVal.RationalVal(n, _) => SchemeVal.BoolVal(n < 0)
            case other =>
              throw new EvalError(s"negative?: expected number, got ${SchemeVal.display(other)}")
      )
    )

  private def registerParityPredicates(env: Env): Unit =
    env.define(
      "odd?",
      SchemeVal.BuiltinProc(
        "odd?",
        args =>
          if args.size != 1 then throw new EvalError("odd?: expected 1 argument")
          args.head match
            case SchemeVal.IntVal(n) => SchemeVal.BoolVal(n % 2 != 0)
            case other =>
              throw new EvalError(s"odd?: expected number, got ${SchemeVal.display(other)}")
      )
    )
    env.define(
      "even?",
      SchemeVal.BuiltinProc(
        "even?",
        args =>
          if args.size != 1 then throw new EvalError("even?: expected 1 argument")
          args.head match
            case SchemeVal.IntVal(n) => SchemeVal.BoolVal(n % 2 == 0)
            case other =>
              throw new EvalError(s"even?: expected number, got ${SchemeVal.display(other)}")
      )
    )

  private def requireNums(args: List[SchemeVal], name: String): List[Long] =
    args.map {
      case SchemeVal.IntVal(n) => n
      case other =>
        throw new EvalError(s"$name: expected number, got ${SchemeVal.display(other)}")
    }

  private def registerGcdLcm(env: Env): Unit =
    env.define(
      "gcd",
      SchemeVal.BuiltinProc(
        "gcd",
        args =>
          if args.isEmpty then SchemeVal.IntVal(0)
          else
            val nums   = requireNums(args, "gcd")
            val result = nums.map(math.abs).reduce((a, b) => gcdLong(a, b))
            SchemeVal.IntVal(result)
      )
    )
    env.define(
      "lcm",
      SchemeVal.BuiltinProc(
        "lcm",
        args =>
          if args.isEmpty then SchemeVal.IntVal(1)
          else
            val nums = requireNums(args, "lcm")
            val result = nums.map(math.abs).reduce { (a, b) =>
              if a == 0 || b == 0 then 0 else math.abs(a / gcdLong(a, b) * b)
            }
            SchemeVal.IntVal(result)
      )
    )

  private def registerRounding(env: Env): Unit =
    env.define(
      "truncate",
      SchemeVal.BuiltinProc(
        "truncate",
        args =>
          if args.size != 1 then throw new EvalError("truncate: expected 1 argument")
          args.head match
            case SchemeVal.IntVal(_)         => args.head
            case SchemeVal.FloatVal(d)       => SchemeVal.IntVal(d.toLong)
            case SchemeVal.RationalVal(n, d) => SchemeVal.IntVal(n / d)
            case other =>
              throw new EvalError(s"truncate: expected number, got ${SchemeVal.display(other)}")
      )
    )
    env.define(
      "round",
      SchemeVal.BuiltinProc(
        "round",
        args =>
          if args.size != 1 then throw new EvalError("round: expected 1 argument")
          args.head match
            case SchemeVal.IntVal(_)   => args.head
            case SchemeVal.FloatVal(d) => SchemeVal.IntVal(math.round(d))
            case SchemeVal.RationalVal(n, d) =>
              SchemeVal.IntVal(math.round(n.toDouble / d.toDouble))
            case other =>
              throw new EvalError(s"round: expected number, got ${SchemeVal.display(other)}")
      )
    )

  private def gcdLong(a: Long, b: Long): Long =
    val aa = math.abs(a); val bb = math.abs(b)
    if bb == 0 then aa else gcdLong(bb, aa % bb)
