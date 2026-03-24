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
    typePredicate("zero?", { case v if v.isNumber => SchemeNum.toDouble(v) == 0.0; case _ => false }),
    typePredicate("positive?", { case v if v.isNumber => SchemeNum.toDouble(v) > 0.0; case _ => false }),
    typePredicate("negative?", { case v if v.isNumber => SchemeNum.toDouble(v) < 0.0; case _ => false }),
    typePredicate("odd?", { case SchemeVal.IntVal(n) => n % 2 != 0; case _ => false }),
    typePredicate("even?", { case SchemeVal.IntVal(n) => n % 2 == 0; case _ => false })
  )

  private def schemeEqual(a: SchemeVal, b: SchemeVal): Boolean = (a, b) match
    case (SchemeVal.IntVal(x), SchemeVal.IntVal(y))   => x == y
    case (x, y) if x.isNumber && y.isNumber           => SchemeNum.numEq(x, y)
    case (SchemeVal.BoolVal(x), SchemeVal.BoolVal(y)) => x == y
    case (SchemeVal.StrVal(x), SchemeVal.StrVal(y))   => java.util.Arrays.equals(x, y)
    case (SchemeVal.SymVal(x), SchemeVal.SymVal(y))   => x == y
    case (SchemeVal.CharVal(x), SchemeVal.CharVal(y)) => x == y
    case (SchemeVal.ListVal(xs), SchemeVal.ListVal(ys)) =>
      xs.length == ys.length && xs.zip(ys).forall((a, b) => schemeEqual(a, b))
    case (SchemeVal.PairVal(a1, d1), SchemeVal.PairVal(a2, d2)) =>
      schemeEqual(a1, a2) && schemeEqual(d1, d2)
    case (SchemeVal.Void, SchemeVal.Void) => true
    case _                                => false

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

  def listBuiltins: List[(String, SchemeVal)] = List(
    "list-ref" -> SchemeVal.BuiltinProc(
      "list-ref",
      {
        case List(SchemeVal.ListVal(elems), SchemeVal.IntVal(i)) => elems(i.toInt)
        case _ => throw new EvalError("list-ref: expected (list, index)")
      }
    ),
    "list-tail" -> SchemeVal.BuiltinProc(
      "list-tail",
      {
        case List(SchemeVal.ListVal(elems), SchemeVal.IntVal(i)) => SchemeVal.ListVal(elems.drop(i.toInt))
        case _ => throw new EvalError("list-tail: expected (list, index)")
      }
    ),
    typePredicate(
      "list?",
      {
        case SchemeVal.ListVal(_) => true
        case _                    => false
      }
    ),
    "assoc" -> SchemeVal.BuiltinProc(
      "assoc",
      {
        case List(key, SchemeVal.ListVal(alist)) =>
          alist
            .find {
              case SchemeVal.ListVal(k :: _) => schemeEqual(key, k)
              case SchemeVal.PairVal(k, _)   => schemeEqual(key, k)
              case _                         => false
            }
            .getOrElse(SchemeVal.BoolVal(false))
        case _ => throw new EvalError("assoc: expected (key, alist)")
      }
    ),
    "equal?" -> SchemeVal.BuiltinProc(
      "equal?",
      {
        case List(a, b) => SchemeVal.BoolVal(schemeEqual(a, b))
        case args       => throw new EvalError(s"equal?: expected 2 arguments, got ${args.length}")
      }
    ),
    "eq?" -> SchemeVal.BuiltinProc(
      "eq?",
      {
        case List(a, b) =>
          SchemeVal.BoolVal(
            (a, b) match
              case (SchemeVal.SymVal(x), SchemeVal.SymVal(y))       => x == y
              case (SchemeVal.IntVal(x), SchemeVal.IntVal(y))       => x == y
              case (SchemeVal.BoolVal(x), SchemeVal.BoolVal(y))     => x == y
              case (SchemeVal.CharVal(x), SchemeVal.CharVal(y))     => x == y
              case (SchemeVal.ListVal(Nil), SchemeVal.ListVal(Nil)) => true
              case _                                                => a eq b
          )
        case args => throw new EvalError(s"eq?: expected 2 arguments, got ${args.length}")
      }
    ),
    "map" -> SchemeVal.BuiltinProc(
      "map",
      args =>
        if args.length < 2 then throw new EvalError("map: expected at least 2 arguments")
        val fn = args.head
        val lists = args.tail.map {
          case SchemeVal.ListVal(elems) => elems
          case other                    => throw new EvalError(s"map: expected list, got ${other.display}")
        }
        val len = lists.head.length
        val result = (0 until len).map { i =>
          val callArgs = lists.map(_(i))
          Evaluator.applyProc(fn, callArgs)
        }.toList
        SchemeVal.ListVal(result)
    )
  )
