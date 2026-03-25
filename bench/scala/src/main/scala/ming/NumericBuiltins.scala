package ming

object NumericBuiltins:

  private def requireNums(args: List[SchemeVal], name: String): List[Long] =
    args.map {
      case SchemeVal.IntVal(n) => n
      case other =>
        throw new EvalError(s"$name: expected number, got ${SchemeVal.display(other)}")
    }

  def register(env: Env): Unit =
    registerArithmetic(env)
    registerComparison(env)
    registerMathOps(env)
    registerNumericPredicates(env)

  private def registerArithmetic(env: Env): Unit =
    env.define(
      "+",
      SchemeVal.BuiltinProc(
        "+",
        args =>
          val nums = requireNums(args, "+")
          SchemeVal.IntVal(nums.sum)
      )
    )
    env.define(
      "-",
      SchemeVal.BuiltinProc(
        "-",
        args =>
          val nums = requireNums(args, "-")
          if nums.isEmpty then throw new EvalError("-: expected at least 1 argument")
          else if nums.size == 1 then SchemeVal.IntVal(-nums.head)
          else SchemeVal.IntVal(nums.tail.foldLeft(nums.head)(_ - _))
      )
    )
    env.define(
      "*",
      SchemeVal.BuiltinProc(
        "*",
        args =>
          val nums = requireNums(args, "*")
          SchemeVal.IntVal(nums.product)
      )
    )
    env.define(
      "/",
      SchemeVal.BuiltinProc(
        "/",
        args =>
          val nums = requireNums(args, "/")
          if nums.size < 2 then throw new EvalError("/: expected at least 2 arguments")
          if nums.tail.contains(0L) then throw new EvalError("division by zero")
          SchemeVal.IntVal(nums.tail.foldLeft(nums.head)(_ / _))
      )
    )

  private def registerComparison(env: Env): Unit =
    env.define(
      "<",
      SchemeVal.BuiltinProc(
        "<",
        args =>
          val nums = requireNums(args, "<")
          SchemeVal.BoolVal(nums.zip(nums.tail).forall((a, b) => a < b))
      )
    )
    env.define(
      ">",
      SchemeVal.BuiltinProc(
        ">",
        args =>
          val nums = requireNums(args, ">")
          SchemeVal.BoolVal(nums.zip(nums.tail).forall((a, b) => a > b))
      )
    )
    env.define(
      "=",
      SchemeVal.BuiltinProc(
        "=",
        args =>
          val nums = requireNums(args, "=")
          SchemeVal.BoolVal(nums.zip(nums.tail).forall((a, b) => a == b))
      )
    )
    env.define(
      "<=",
      SchemeVal.BuiltinProc(
        "<=",
        args =>
          val nums = requireNums(args, "<=")
          SchemeVal.BoolVal(nums.zip(nums.tail).forall((a, b) => a <= b))
      )
    )
    env.define(
      ">=",
      SchemeVal.BuiltinProc(
        ">=",
        args =>
          val nums = requireNums(args, ">=")
          SchemeVal.BoolVal(nums.zip(nums.tail).forall((a, b) => a >= b))
      )
    )

  private def registerMathOps(env: Env): Unit =
    env.define(
      "abs",
      SchemeVal.BuiltinProc(
        "abs",
        args =>
          if args.size != 1 then throw new EvalError("abs: expected 1 argument")
          args.head match
            case SchemeVal.IntVal(n) => SchemeVal.IntVal(math.abs(n))
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
          val nums = requireNums(args, "min")
          if nums.isEmpty then throw new EvalError("min: expected at least 1 argument")
          SchemeVal.IntVal(nums.min)
      )
    )
    env.define(
      "max",
      SchemeVal.BuiltinProc(
        "max",
        args =>
          val nums = requireNums(args, "max")
          if nums.isEmpty then throw new EvalError("max: expected at least 1 argument")
          SchemeVal.IntVal(nums.max)
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
            case SchemeVal.IntVal(n) => SchemeVal.BoolVal(n == 0)
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
            case SchemeVal.IntVal(n) => SchemeVal.BoolVal(n > 0)
            case other =>
              throw new EvalError(
                s"positive?: expected number, got ${SchemeVal.display(other)}"
              )
      )
    )
    env.define(
      "negative?",
      SchemeVal.BuiltinProc(
        "negative?",
        args =>
          if args.size != 1 then throw new EvalError("negative?: expected 1 argument")
          args.head match
            case SchemeVal.IntVal(n) => SchemeVal.BoolVal(n < 0)
            case other =>
              throw new EvalError(
                s"negative?: expected number, got ${SchemeVal.display(other)}"
              )
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
