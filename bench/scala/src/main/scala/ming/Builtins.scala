package ming

/** Built-in primitive procedures. */
object Builtins:

  val names: List[String] = List("+", "-", "*", "/", "=", "<", ">", "<=", ">=", "not")

  private def isTruthy(v: SchemeVal): Boolean = v match
    case SchemeVal.SBool(false) => false
    case _                      => true

  private def asInt(v: SchemeVal): Long = v match
    case SchemeVal.SInt(n) => n
    case other             => throw new EvalError(s"expected number, got ${other.display}")

  private def requireComparison(name: String, args: List[SchemeVal]): List[Long] =
    if args.length < 2 then throw new EvalError(s"$name: expected at least 2 arguments")
    args.map(asInt)

  def applyBuiltin(name: String, args: List[SchemeVal]): SchemeVal =
    name match
      case "+" =>
        SchemeVal.SInt(args.map(asInt).sum)
      case "-" =>
        if args.isEmpty then throw new EvalError("-: expected at least 1 argument")
        val nums = args.map(asInt)
        if nums.length == 1 then SchemeVal.SInt(-nums.head)
        else SchemeVal.SInt(nums.head - nums.tail.sum)
      case "*" =>
        SchemeVal.SInt(args.map(asInt).product)
      case "/" =>
        if args.length < 2 then throw new EvalError("/: expected at least 2 arguments")
        val nums = args.map(asInt)
        if nums.tail.contains(0L) then throw new EvalError("division by zero")
        SchemeVal.SInt(nums.head / nums.tail.product)
      case "=" =>
        val nums = requireComparison("=", args)
        SchemeVal.SBool(nums.forall(_ == nums.head))
      case "<" =>
        val nums = requireComparison("<", args)
        SchemeVal.SBool(nums.zip(nums.tail).forall((a, b) => a < b))
      case ">" =>
        val nums = requireComparison(">", args)
        SchemeVal.SBool(nums.zip(nums.tail).forall((a, b) => a > b))
      case "<=" =>
        val nums = requireComparison("<=", args)
        SchemeVal.SBool(nums.zip(nums.tail).forall((a, b) => a <= b))
      case ">=" =>
        val nums = requireComparison(">=", args)
        SchemeVal.SBool(nums.zip(nums.tail).forall((a, b) => a >= b))
      case "not" =>
        if args.length != 1 then throw new EvalError("not: expected 1 argument")
        SchemeVal.SBool(!isTruthy(args.head))
      case other =>
        throw new EvalError(s"unknown procedure: $other")
