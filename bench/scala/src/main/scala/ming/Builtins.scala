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
    "symbol?",
    "display",
    "write",
    "newline",
    "string-append",
    "string-length",
    "substring",
    "string->number",
    "number->string",
    "symbol->string",
    "string->symbol",
    "string-ref",
    "string-copy",
    "string-set!",
    "char?"
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

  private def requireOne(name: String, args: List[SchemeVal]): SchemeVal =
    if args.length != 1 then throw new EvalError(s"$name: expected 1 argument")
    args.head

  private def applyArithmetic(name: String, args: List[SchemeVal]): SchemeVal =
    name match
      case "+" => SchemeVal.SInt(args.map(asInt).sum)
      case "-" =>
        if args.isEmpty then throw new EvalError("-: expected at least 1 argument")
        val nums = args.map(asInt)
        if nums.length == 1 then SchemeVal.SInt(-nums.head)
        else SchemeVal.SInt(nums.head - nums.tail.sum)
      case "*" => SchemeVal.SInt(args.map(asInt).product)
      case "/" =>
        if args.length < 2 then throw new EvalError("/: expected at least 2 arguments")
        val nums = args.map(asInt)
        if nums.tail.contains(0L) then throw new EvalError("division by zero")
        SchemeVal.SInt(nums.head / nums.tail.product)
      case _ => throw new EvalError(s"unknown arithmetic op: $name")

  private def applyComparison(name: String, args: List[SchemeVal]): SchemeVal =
    val nums  = requireComparison(name, args)
    val pairs = nums.zip(nums.tail)
    SchemeVal.SBool(name match
      case "="  => nums.forall(_ == nums.head)
      case "<"  => pairs.forall((a, b) => a < b)
      case ">"  => pairs.forall((a, b) => a > b)
      case "<=" => pairs.forall((a, b) => a <= b)
      case ">=" => pairs.forall((a, b) => a >= b)
      case _    => throw new EvalError(s"unknown comparison: $name"))

  private def applyListOp(name: String, args: List[SchemeVal]): SchemeVal =
    name match
      case "cons" =>
        if args.length != 2 then throw new EvalError("cons: expected 2 arguments")
        args(1) match
          case SchemeVal.SList(elems) => SchemeVal.SList(args(0) :: elems)
          case other => throw new EvalError(s"cons: expected list as second argument, got ${other.display}")
      case "car" =>
        requireOne("car", args) match
          case SchemeVal.SList(head :: _) => head
          case _                          => throw new EvalError("car: expected pair")
      case "cdr" =>
        requireOne("cdr", args) match
          case SchemeVal.SList(_ :: tail) => SchemeVal.SList(tail)
          case _                          => throw new EvalError("cdr: expected pair")
      case "null?" =>
        SchemeVal.SBool(requireOne("null?", args) == SchemeVal.SList(Nil))
      case "list" => SchemeVal.SList(args)
      case "length" =>
        requireOne("length", args) match
          case SchemeVal.SList(elems) => SchemeVal.SInt(elems.length.toLong)
          case _                      => throw new EvalError("length: expected list")
      case "append" =>
        val lists = args.map {
          case SchemeVal.SList(elems) => elems
          case other                  => throw new EvalError(s"append: expected list, got ${other.display}")
        }
        SchemeVal.SList(lists.flatten)
      case "pair?" =>
        SchemeVal.SBool(requireOne("pair?", args) match
          case SchemeVal.SList(elems) => elems.nonEmpty
          case _                      => false)
      case _ => throw new EvalError(s"unknown list op: $name")

  private def applyTypePredicate(name: String, args: List[SchemeVal]): SchemeVal =
    val arg = requireOne(name, args)
    SchemeVal.SBool((name, arg) match
      case ("string?", SchemeVal.SString(_))                             => true
      case ("number?", SchemeVal.SInt(_))                                => true
      case ("boolean?", SchemeVal.SBool(_))                              => true
      case ("symbol?", SchemeVal.SSymbol(_))                             => true
      case ("char?", SchemeVal.SChar(_))                                 => true
      case ("string?" | "number?" | "boolean?" | "symbol?" | "char?", _) => false
      case _ => throw new EvalError(s"unknown predicate: $name"))

  private def applyStringOp(name: String, args: List[SchemeVal]): SchemeVal =
    name match
      case "string-append" =>
        val strs = args.map {
          case SchemeVal.SString(s) => s.toString
          case other                => throw new EvalError(s"string-append: expected string, got ${other.display}")
        }
        SchemeVal.SString(new StringBuilder(strs.mkString))
      case "string-length" =>
        requireOne("string-length", args) match
          case SchemeVal.SString(s) => SchemeVal.SInt(s.length.toLong)
          case other                => throw new EvalError(s"string-length: expected string, got ${other.display}")
      case "substring" =>
        if args.length != 3 then throw new EvalError("substring: expected 3 arguments")
        (args(0), args(1), args(2)) match
          case (SchemeVal.SString(s), SchemeVal.SInt(start), SchemeVal.SInt(end)) =>
            SchemeVal.SString(new StringBuilder(s.toString.substring(start.toInt, end.toInt)))
          case _ => throw new EvalError("substring: expected string and two integers")
      case "string->number" =>
        requireOne("string->number", args) match
          case SchemeVal.SString(s) =>
            s.toString.toLongOption match
              case Some(n) => SchemeVal.SInt(n)
              case None    => SchemeVal.SBool(false)
          case other => throw new EvalError(s"string->number: expected string, got ${other.display}")
      case "number->string" =>
        requireOne("number->string", args) match
          case SchemeVal.SInt(n) => SchemeVal.SString(new StringBuilder(n.toString))
          case other             => throw new EvalError(s"number->string: expected number, got ${other.display}")
      case "symbol->string" =>
        requireOne("symbol->string", args) match
          case SchemeVal.SSymbol(n) => SchemeVal.SString(new StringBuilder(n))
          case other                => throw new EvalError(s"symbol->string: expected symbol, got ${other.display}")
      case "string->symbol" =>
        requireOne("string->symbol", args) match
          case SchemeVal.SString(s) => SchemeVal.SSymbol(s.toString)
          case other                => throw new EvalError(s"string->symbol: expected string, got ${other.display}")
      case "string-ref" =>
        if args.length != 2 then throw new EvalError("string-ref: expected 2 arguments")
        (args(0), args(1)) match
          case (SchemeVal.SString(s), SchemeVal.SInt(i)) => SchemeVal.SChar(s.charAt(i.toInt))
          case _ => throw new EvalError("string-ref: expected string and integer")
      case "string-copy" =>
        requireOne("string-copy", args) match
          case SchemeVal.SString(s) => SchemeVal.SString(new StringBuilder(s.toString))
          case other                => throw new EvalError(s"string-copy: expected string, got ${other.display}")
      case "string-set!" =>
        if args.length != 3 then throw new EvalError("string-set!: expected 3 arguments")
        (args(0), args(1), args(2)) match
          case (SchemeVal.SString(s), SchemeVal.SInt(i), SchemeVal.SChar(c)) =>
            s.setCharAt(i.toInt, c)
            SchemeVal.SVoid
          case _ => throw new EvalError("string-set!: expected string, integer, and character")
      case _ => throw new EvalError(s"unknown string op: $name")

  def applyBuiltin(name: String, args: List[SchemeVal]): SchemeVal =
    name match
      case "+" | "-" | "*" | "/"         => applyArithmetic(name, args)
      case "=" | "<" | ">" | "<=" | ">=" => applyComparison(name, args)
      case "not" =>
        if args.length != 1 then throw new EvalError("not: expected 1 argument")
        SchemeVal.SBool(!isTruthy(args.head))
      case "cons" | "car" | "cdr" | "null?" | "list" | "length" | "append" | "pair?" =>
        applyListOp(name, args)
      case "string?" | "number?" | "boolean?" | "symbol?" | "char?" =>
        applyTypePredicate(name, args)
      case "string-append" | "string-length" | "substring" | "string->number" | "number->string" | "symbol->string" |
          "string->symbol" | "string-ref" | "string-copy" | "string-set!" =>
        applyStringOp(name, args)
      case other =>
        throw new EvalError(s"unknown procedure: $other")
