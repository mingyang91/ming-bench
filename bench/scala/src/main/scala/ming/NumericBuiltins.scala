package ming

object NumericBuiltins:

  private def requireNums(name: String, args: List[SchemeVal]): List[Long] =
    args.map {
      case SchemeVal.IntVal(n)    => n
      case SchemeVal.FloatVal(d)  => d.toLong
      case SchemeVal.RatVal(n, d) => n / d
      case other                  => throw new EvalError(s"$name: expected number, got ${other.display}")
    }

  private def typePredicate(
    name: String,
    test: SchemeVal => Boolean
  ): (String, SchemeVal) =
    name -> SchemeVal.BuiltinProc(
      name,
      {
        case List(v) => SchemeVal.BoolVal(test(v))
        case args =>
          throw new EvalError(s"$name: expected 1 argument, got ${args.length}")
      }
    )

  def numericBuiltins: List[(String, SchemeVal)] = List(
    "abs" -> SchemeVal.BuiltinProc(
      "abs",
      {
        case List(SchemeVal.IntVal(n))    => SchemeVal.IntVal(math.abs(n))
        case List(SchemeVal.FloatVal(d))  => SchemeVal.FloatVal(math.abs(d))
        case List(SchemeVal.RatVal(n, d)) => SchemeNum.makeRational(math.abs(n), d)
        case List(other)                  => throw new EvalError(s"abs: expected number, got ${other.display}")
        case args                         => throw new EvalError(s"abs: expected 1 argument, got ${args.length}")
      }
    ),
    "modulo" -> SchemeVal.BuiltinProc(
      "modulo",
      {
        case List(SchemeVal.IntVal(a), SchemeVal.IntVal(b)) => SchemeVal.IntVal(Math.floorMod(a, b))
        case _                                              => throw new EvalError("modulo: expected 2 numbers")
      }
    ),
    "remainder" -> SchemeVal.BuiltinProc(
      "remainder",
      {
        case List(SchemeVal.IntVal(a), SchemeVal.IntVal(b)) => SchemeVal.IntVal(a % b)
        case _                                              => throw new EvalError("remainder: expected 2 numbers")
      }
    ),
    "quotient" -> SchemeVal.BuiltinProc(
      "quotient",
      {
        case List(SchemeVal.IntVal(a), SchemeVal.IntVal(b)) => SchemeVal.IntVal(a / b)
        case _                                              => throw new EvalError("quotient: expected 2 numbers")
      }
    ),
    "min" -> SchemeVal.BuiltinProc(
      "min",
      args =>
        val nums = requireNums("min", args)
        if nums.isEmpty then throw new EvalError("min: expected at least 1 argument")
        SchemeVal.IntVal(nums.min)
    ),
    "max" -> SchemeVal.BuiltinProc(
      "max",
      args =>
        val nums = requireNums("max", args)
        if nums.isEmpty then throw new EvalError("max: expected at least 1 argument")
        SchemeVal.IntVal(nums.max)
    ),
    "expt" -> SchemeVal.BuiltinProc(
      "expt",
      {
        case List(SchemeVal.IntVal(base), SchemeVal.IntVal(exp)) =>
          var result = 1L
          for _ <- 0L until exp do result *= base
          SchemeVal.IntVal(result)
        case _ => throw new EvalError("expt: expected 2 numbers")
      }
    ),
    "gcd" -> SchemeVal.BuiltinProc(
      "gcd",
      args =>
        def gcd2(a: Long, b: Long): Long = if b == 0 then a else gcd2(b, a % b)
        val nums = args.map {
          case SchemeVal.IntVal(n) => math.abs(n)
          case other               => throw new EvalError(s"gcd: expected integer, got ${other.display}")
        }
        if nums.isEmpty then SchemeVal.IntVal(0)
        else SchemeVal.IntVal(nums.reduce(gcd2))
    ),
    "lcm" -> SchemeVal.BuiltinProc(
      "lcm",
      args =>
        def gcd2(a: Long, b: Long): Long = if b == 0 then a else gcd2(b, a % b)
        val nums = args.map {
          case SchemeVal.IntVal(n) => math.abs(n)
          case other               => throw new EvalError(s"lcm: expected integer, got ${other.display}")
        }
        if nums.isEmpty then SchemeVal.IntVal(1)
        else SchemeVal.IntVal(nums.reduce((a, b) => if a == 0 || b == 0 then 0 else a / gcd2(a, b) * b))
    )
  )

  def mathBuiltins: List[(String, SchemeVal)] = List(
    "truncate" -> SchemeVal.BuiltinProc(
      "truncate",
      {
        case List(SchemeVal.IntVal(n)) => SchemeVal.IntVal(n)
        case List(SchemeVal.FloatVal(d)) =>
          SchemeVal.FloatVal(
            if d >= 0 then math.floor(d) else math.ceil(d)
          )
        case List(other) =>
          throw new EvalError(
            s"truncate: expected number, got ${other.display}"
          )
        case args =>
          throw new EvalError(
            s"truncate: expected 1 argument, got ${args.length}"
          )
      }
    ),
    "floor" -> SchemeVal.BuiltinProc(
      "floor",
      {
        case List(SchemeVal.IntVal(n))   => SchemeVal.IntVal(n)
        case List(SchemeVal.FloatVal(d)) => SchemeVal.FloatVal(math.floor(d))
        case List(other) =>
          throw new EvalError(
            s"floor: expected number, got ${other.display}"
          )
        case args =>
          throw new EvalError(
            s"floor: expected 1 argument, got ${args.length}"
          )
      }
    ),
    "ceiling" -> SchemeVal.BuiltinProc(
      "ceiling",
      {
        case List(SchemeVal.IntVal(n))   => SchemeVal.IntVal(n)
        case List(SchemeVal.FloatVal(d)) => SchemeVal.FloatVal(math.ceil(d))
        case List(other) =>
          throw new EvalError(
            s"ceiling: expected number, got ${other.display}"
          )
        case args =>
          throw new EvalError(
            s"ceiling: expected 1 argument, got ${args.length}"
          )
      }
    ),
    "round" -> SchemeVal.BuiltinProc(
      "round",
      {
        case List(SchemeVal.IntVal(n))   => SchemeVal.IntVal(n)
        case List(SchemeVal.FloatVal(d)) => SchemeVal.FloatVal(math.rint(d))
        case List(other) =>
          throw new EvalError(
            s"round: expected number, got ${other.display}"
          )
        case args =>
          throw new EvalError(
            s"round: expected 1 argument, got ${args.length}"
          )
      }
    ),
    typePredicate(
      "zero?",
      {
        case v if v.isNumber => SchemeNum.toDouble(v) == 0.0; case _ => false
      }
    ),
    typePredicate(
      "positive?",
      {
        case v if v.isNumber => SchemeNum.toDouble(v) > 0.0; case _ => false
      }
    ),
    typePredicate(
      "negative?",
      {
        case v if v.isNumber => SchemeNum.toDouble(v) < 0.0; case _ => false
      }
    ),
    typePredicate(
      "odd?",
      {
        case SchemeVal.IntVal(n) => n % 2 != 0; case _ => false
      }
    ),
    typePredicate(
      "even?",
      {
        case SchemeVal.IntVal(n) => n % 2 == 0; case _ => false
      }
    )
  )

  def exactnessBuiltins: List[(String, SchemeVal)] = List(
    typePredicate(
      "exact?",
      v =>
        SchemeNum.requireNum("exact?", v); SchemeNum.isExact(v)
    ),
    typePredicate(
      "inexact?",
      v =>
        SchemeNum.requireNum("inexact?", v); !SchemeNum.isExact(v)
    ),
    "exact->inexact" -> SchemeVal.BuiltinProc(
      "exact->inexact",
      {
        case List(v) =>
          SchemeNum.requireNum("exact->inexact", v)
          SchemeVal.FloatVal(SchemeNum.toDouble(v))
        case args =>
          throw new EvalError(s"exact->inexact: expected 1 argument, got ${args.length}")
      }
    ),
    "inexact->exact" -> SchemeVal.BuiltinProc(
      "inexact->exact",
      {
        case List(SchemeVal.FloatVal(d)) =>
          if d == d.toLong.toDouble && !d.isInfinite then SchemeVal.IntVal(d.toLong)
          else
            val bits     = java.lang.Double.doubleToLongBits(d)
            val mantissa = (bits & 0xfffffffffffffL) | 0x10000000000000L
            val exponent = ((bits >> 52) & 0x7ff).toInt - 1023 - 52
            val sign     = if (bits >> 63) != 0 then -1L else 1L
            if exponent >= 0 then SchemeVal.IntVal(sign * mantissa * (1L << exponent))
            else SchemeNum.makeRational(sign * mantissa, 1L << (-exponent))
        case List(v @ (SchemeVal.IntVal(_) | SchemeVal.RatVal(_, _))) => v
        case List(other) =>
          throw new EvalError(s"inexact->exact: expected number, got ${other.display}")
        case args =>
          throw new EvalError(s"inexact->exact: expected 1 argument, got ${args.length}")
      }
    ),
    "numerator" -> SchemeVal.BuiltinProc(
      "numerator",
      {
        case List(SchemeVal.IntVal(n))    => SchemeVal.IntVal(n)
        case List(SchemeVal.RatVal(n, _)) => SchemeVal.IntVal(n)
        case List(other) =>
          throw new EvalError(s"numerator: expected rational, got ${other.display}")
        case args =>
          throw new EvalError(s"numerator: expected 1 argument, got ${args.length}")
      }
    ),
    "denominator" -> SchemeVal.BuiltinProc(
      "denominator",
      {
        case List(SchemeVal.IntVal(_))    => SchemeVal.IntVal(1)
        case List(SchemeVal.RatVal(_, d)) => SchemeVal.IntVal(d)
        case List(other) =>
          throw new EvalError(s"denominator: expected rational, got ${other.display}")
        case args =>
          throw new EvalError(s"denominator: expected 1 argument, got ${args.length}")
      }
    )
  )
