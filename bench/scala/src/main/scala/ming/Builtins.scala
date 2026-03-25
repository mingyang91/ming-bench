package ming

/** Built-in primitive procedures. */
object Builtins:

  val names: List[String] = List(
    "+",
    "-",
    "*",
    "/",
    "=",
    "<",
    ">",
    "<=",
    ">=",
    "not",
    "cons",
    "car",
    "cdr",
    "null?",
    "list",
    "length",
    "append",
    "pair?",
    "string?",
    "number?",
    "boolean?",
    "symbol?"
  )

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
      case "cons" =>
        if args.length != 2 then throw new EvalError("cons: expected 2 arguments")
        args(1) match
          case SchemeVal.SList(elems) => SchemeVal.SList(args(0) :: elems)
          case other => throw new EvalError(s"cons: expected list as second argument, got ${other.display}")
      case "car" =>
        if args.length != 1 then throw new EvalError("car: expected 1 argument")
        args(0) match
          case SchemeVal.SList(head :: _) => head
          case _                          => throw new EvalError("car: expected pair")
      case "cdr" =>
        if args.length != 1 then throw new EvalError("cdr: expected 1 argument")
        args(0) match
          case SchemeVal.SList(_ :: tail) => SchemeVal.SList(tail)
          case _                          => throw new EvalError("cdr: expected pair")
      case "null?" =>
        if args.length != 1 then throw new EvalError("null?: expected 1 argument")
        SchemeVal.SBool(args(0) == SchemeVal.SList(Nil))
      case "list" =>
        SchemeVal.SList(args)
      case "length" =>
        if args.length != 1 then throw new EvalError("length: expected 1 argument")
        args(0) match
          case SchemeVal.SList(elems) => SchemeVal.SInt(elems.length.toLong)
          case _                      => throw new EvalError("length: expected list")
      case "append" =>
        val lists = args.map {
          case SchemeVal.SList(elems) => elems
          case other                  => throw new EvalError(s"append: expected list, got ${other.display}")
        }
        SchemeVal.SList(lists.flatten)
      case "pair?" =>
        if args.length != 1 then throw new EvalError("pair?: expected 1 argument")
        SchemeVal.SBool(args(0) match
          case SchemeVal.SList(elems) => elems.nonEmpty
          case _                      => false)
      case "string?" =>
        if args.length != 1 then throw new EvalError("string?: expected 1 argument")
        SchemeVal.SBool(args(0) match
          case SchemeVal.SString(_) => true
          case _                    => false)
      case "number?" =>
        if args.length != 1 then throw new EvalError("number?: expected 1 argument")
        SchemeVal.SBool(args(0) match
          case SchemeVal.SInt(_) => true
          case _                 => false)
      case "boolean?" =>
        if args.length != 1 then throw new EvalError("boolean?: expected 1 argument")
        SchemeVal.SBool(args(0) match
          case SchemeVal.SBool(_) => true
          case _                  => false)
      case "symbol?" =>
        if args.length != 1 then throw new EvalError("symbol?: expected 1 argument")
        SchemeVal.SBool(args(0) match
          case SchemeVal.SSymbol(_) => true
          case _                    => false)
      case other =>
        throw new EvalError(s"unknown procedure: $other")
