package ming

private[ming] object MathBuiltins:

  def applyMathBuiltin(name: String, args: List[Expr]): Expr = name match
    case "gcd"      => applyGcd(args)
    case "lcm"      => applyLcm(args)
    case "floor"    => Builtins.unary(name, args)(applyFloor)
    case "ceiling"  => Builtins.unary(name, args)(applyCeiling)
    case "truncate" => Builtins.unary(name, args)(applyTruncate)
    case "round"    => Builtins.unary(name, args)(applyRound)
    case "sqrt"     => Builtins.unary(name, args)(applySqrt)
    case _          => throw EvalError(s"unknown math procedure: $name")

  private def applyGcd(args: List[Expr]): Expr =
    if args.isEmpty then return Expr.Num(0)
    val nums = args.map(Builtins.asNum)
    Expr.Num(nums.map(math.abs).reduceLeft((a, b) => gcdLong(a, b)))

  private def gcdLong(a: Long, b: Long): Long =
    if b == 0 then a else gcdLong(b, a % b)

  private def applyLcm(args: List[Expr]): Expr =
    if args.isEmpty then return Expr.Num(1)
    val nums = args.map(Builtins.asNum)
    Expr.Num(nums.map(math.abs).reduceLeft((a, b) => a / gcdLong(a, b) * b))

  private def applyFloor(e: Expr): Expr = e match
    case Expr.Num(n)  => Expr.Num(n)
    case Expr.Real(v) => Expr.Num(math.floor(v).toLong)
    case _            => throw EvalError("floor: not a number")

  private def applyCeiling(e: Expr): Expr = e match
    case Expr.Num(n)  => Expr.Num(n)
    case Expr.Real(v) => Expr.Num(math.ceil(v).toLong)
    case _            => throw EvalError("ceiling: not a number")

  private def applyTruncate(e: Expr): Expr = e match
    case Expr.Num(n)  => Expr.Num(n)
    case Expr.Real(v) => Expr.Num(v.toLong)
    case _            => throw EvalError("truncate: not a number")

  private def applyRound(e: Expr): Expr = e match
    case Expr.Num(n)  => Expr.Num(n)
    case Expr.Real(v) => Expr.Num(math.round(v))
    case _            => throw EvalError("round: not a number")

  private def applySqrt(e: Expr): Expr = e match
    case Expr.Num(n) =>
      val s = math.sqrt(n.toDouble)
      if s == s.floor && s * s == n.toDouble then Expr.Num(s.toLong)
      else Expr.Real(s)
    case Expr.Real(v) => Expr.Real(math.sqrt(v))
    case _            => throw EvalError("sqrt: not a number")
