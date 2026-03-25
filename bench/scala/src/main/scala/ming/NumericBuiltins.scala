package ming

object NumericBuiltins:

  private def requireNums(args: List[SchemeVal], name: String): List[Long] =
    args.map {
      case SchemeVal.IntVal(n) => n
      case other =>
        throw new EvalError(s"$name: expected number, got ${SchemeVal.display(other)}")
    }

  private def requireNumeric(args: List[SchemeVal], name: String): Unit =
    args.foreach { v =>
      if !SchemeVal.isNumeric(v) then throw new EvalError(s"$name: expected number, got ${SchemeVal.display(v)}")
    }

  // Convert any numeric SchemeVal to (numerator, denominator) for exact arithmetic
  private def toRational(v: SchemeVal): (Long, Long) = v match
    case SchemeVal.IntVal(n)         => (n, 1L)
    case SchemeVal.RationalVal(n, d) => (n, d)
    case other                       => throw new EvalError(s"expected exact number, got ${SchemeVal.display(other)}")

  private def hasInexact(args: List[SchemeVal]): Boolean =
    args.exists(_.isInstanceOf[SchemeVal.FloatVal])

  private def addExact(a: SchemeVal, b: SchemeVal): SchemeVal =
    val (an, ad) = toRational(a)
    val (bn, bd) = toRational(b)
    SchemeVal.makeRational(an * bd + bn * ad, ad * bd)

  private def subExact(a: SchemeVal, b: SchemeVal): SchemeVal =
    val (an, ad) = toRational(a)
    val (bn, bd) = toRational(b)
    SchemeVal.makeRational(an * bd - bn * ad, ad * bd)

  private def mulExact(a: SchemeVal, b: SchemeVal): SchemeVal =
    val (an, ad) = toRational(a)
    val (bn, bd) = toRational(b)
    SchemeVal.makeRational(an * bn, ad * bd)

  private def divExact(a: SchemeVal, b: SchemeVal): SchemeVal =
    val (an, ad) = toRational(a)
    val (bn, bd) = toRational(b)
    if bn == 0 then throw new EvalError("division by zero")
    SchemeVal.makeRational(an * bd, ad * bn)

  def register(env: Env): Unit =
    registerArithmetic(env)
    registerComparison(env)
    registerMathOps(env)
    registerNumericPredicates(env)
    RationalBuiltins.register(env)

  private def registerArithmetic(env: Env): Unit =
    env.define(
      "+",
      SchemeVal.BuiltinProc(
        "+",
        args =>
          requireNumeric(args, "+")
          if args.isEmpty then SchemeVal.IntVal(0)
          else if hasInexact(args) then SchemeVal.FloatVal(args.map(SchemeVal.toDouble).sum)
          else args.reduce(addExact)
      )
    )
    env.define(
      "-",
      SchemeVal.BuiltinProc(
        "-",
        args =>
          requireNumeric(args, "-")
          if args.isEmpty then throw new EvalError("-: expected at least 1 argument")
          else if hasInexact(args) then
            val ds = args.map(SchemeVal.toDouble)
            if ds.size == 1 then SchemeVal.FloatVal(-ds.head)
            else SchemeVal.FloatVal(ds.tail.foldLeft(ds.head)(_ - _))
          else if args.size == 1 then
            args.head match
              case SchemeVal.IntVal(n)         => SchemeVal.IntVal(-n)
              case SchemeVal.RationalVal(n, d) => SchemeVal.RationalVal(-n, d)
              case _                           => throw new EvalError("-: expected number")
          else args.tail.foldLeft(args.head)(subExact)
      )
    )
    env.define(
      "*",
      SchemeVal.BuiltinProc(
        "*",
        args =>
          requireNumeric(args, "*")
          if args.isEmpty then SchemeVal.IntVal(1)
          else if hasInexact(args) then SchemeVal.FloatVal(args.map(SchemeVal.toDouble).product)
          else args.reduce(mulExact)
      )
    )
    env.define(
      "/",
      SchemeVal.BuiltinProc(
        "/",
        args =>
          requireNumeric(args, "/")
          if args.size < 2 then throw new EvalError("/: expected at least 2 arguments")
          if hasInexact(args) then
            val ds = args.map(SchemeVal.toDouble)
            if ds.tail.exists(_ == 0.0) then throw new EvalError("division by zero")
            SchemeVal.FloatVal(ds.tail.foldLeft(ds.head)(_ / _))
          else args.tail.foldLeft(args.head)(divExact)
      )
    )

  private def registerComparison(env: Env): Unit =
    def cmpNums(args: List[SchemeVal], name: String, op: (Double, Double) => Boolean): SchemeVal =
      requireNumeric(args, name)
      val ds = args.map(SchemeVal.toDouble)
      SchemeVal.BoolVal(ds.zip(ds.tail).forall((a, b) => op(a, b)))

    env.define("<", SchemeVal.BuiltinProc("<", args => cmpNums(args, "<", _ < _)))
    env.define(">", SchemeVal.BuiltinProc(">", args => cmpNums(args, ">", _ > _)))
    env.define("=", SchemeVal.BuiltinProc("=", args => cmpNums(args, "=", _ == _)))
    env.define("<=", SchemeVal.BuiltinProc("<=", args => cmpNums(args, "<=", _ <= _)))
    env.define(">=", SchemeVal.BuiltinProc(">=", args => cmpNums(args, ">=", _ >= _)))

  private def registerMathOps(env: Env): Unit =
    env.define(
      "abs",
      SchemeVal.BuiltinProc(
        "abs",
        args =>
          if args.size != 1 then throw new EvalError("abs: expected 1 argument")
          args.head match
            case SchemeVal.IntVal(n)         => SchemeVal.IntVal(math.abs(n))
            case SchemeVal.RationalVal(n, d) => SchemeVal.makeRational(math.abs(n), d)
            case SchemeVal.FloatVal(d)       => SchemeVal.FloatVal(math.abs(d))
            case other =>
              throw new EvalError(s"abs: expected number, got ${SchemeVal.display(other)}")
      )
    )
    env.define(
      "modulo",
      SchemeVal.BuiltinProc(
        "modulo",
        args =>
          val nums = requireNums(args, "modulo")
          if nums.size != 2 then throw new EvalError("modulo: expected 2 arguments")
          val (a, b) = (nums(0), nums(1))
          if b == 0 then throw new EvalError("modulo: division by zero")
          SchemeVal.IntVal(java.lang.Math.floorMod(a, b))
      )
    )
    env.define(
      "remainder",
      SchemeVal.BuiltinProc(
        "remainder",
        args =>
          val nums = requireNums(args, "remainder")
          if nums.size != 2 then throw new EvalError("remainder: expected 2 arguments")
          val (a, b) = (nums(0), nums(1))
          if b == 0 then throw new EvalError("remainder: division by zero")
          SchemeVal.IntVal(a % b)
      )
    )
    env.define(
      "quotient",
      SchemeVal.BuiltinProc(
        "quotient",
        args =>
          val nums = requireNums(args, "quotient")
          if nums.size != 2 then throw new EvalError("quotient: expected 2 arguments")
          val (a, b) = (nums(0), nums(1))
          if b == 0 then throw new EvalError("quotient: division by zero")
          SchemeVal.IntVal((a.toDouble / b.toDouble).toLong)
      )
    )
    env.define(
      "min",
      SchemeVal.BuiltinProc(
        "min",
        args =>
          requireNumeric(args, "min")
          if args.isEmpty then throw new EvalError("min: expected at least 1 argument")
          args.minBy(SchemeVal.toDouble)
      )
    )
    env.define(
      "max",
      SchemeVal.BuiltinProc(
        "max",
        args =>
          requireNumeric(args, "max")
          if args.isEmpty then throw new EvalError("max: expected at least 1 argument")
          args.maxBy(SchemeVal.toDouble)
      )
    )
    env.define(
      "expt",
      SchemeVal.BuiltinProc(
        "expt",
        args =>
          val nums = requireNums(args, "expt")
          if nums.size != 2 then throw new EvalError("expt: expected 2 arguments")
          SchemeVal.IntVal(math.pow(nums(0).toDouble, nums(1).toDouble).toLong)
      )
    )

  private def registerNumericPredicates(env: Env): Unit =
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
