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
    "char?",
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
    "apply",
    "map",
    "for-each",
    // L09
    "abs",
    "modulo",
    "remainder",
    "quotient",
    "min",
    "max",
    "expt",
    "zero?",
    "positive?",
    "negative?",
    "odd?",
    "even?",
    "list-ref",
    "list-tail",
    "list?",
    "assoc",
    "eq?",
    "equal?",
    "char-alphabetic?",
    "char-numeric?",
    "char-upcase",
    "char-downcase",
    "char=?",
    "char<?",
    "string=?",
    "string<?",
    "string-ci=?",
    "string-upcase",
    "string-downcase"
  )

  private def isTruthy(v: SchemeVal): Boolean = v match
    case SchemeVal.SBool(false) => false
    case _                      => true

  private def asInt(v: SchemeVal): Long = v match
    case SchemeVal.SInt(n) => n
    case other =>
      throw new EvalError(s"expected number, got ${other.display}")

  private def asChar(v: SchemeVal): Char = v match
    case SchemeVal.SChar(c) => c
    case other =>
      throw new EvalError(s"expected char, got ${other.display}")

  private def requireComparison(
    name: String,
    args: List[SchemeVal]
  ): List[Long] =
    if args.length < 2 then throw new EvalError(s"$name: expected at least 2 arguments")
    args.map(asInt)

  private def requireOne(
    name: String,
    args: List[SchemeVal]
  ): SchemeVal =
    if args.length != 1 then throw new EvalError(s"$name: expected 1 argument")
    args.head

  private def requireTwo(
    name: String,
    args: List[SchemeVal]
  ): (SchemeVal, SchemeVal) =
    if args.length != 2 then throw new EvalError(s"$name: expected 2 arguments")
    (args(0), args(1))

  def schemeEqual(a: SchemeVal, b: SchemeVal): Boolean = (a, b) match
    case (SchemeVal.SInt(x), SchemeVal.SInt(y))       => x == y
    case (SchemeVal.SBool(x), SchemeVal.SBool(y))     => x == y
    case (SchemeVal.SString(x), SchemeVal.SString(y)) => x.toString == y.toString
    case (SchemeVal.SSymbol(x), SchemeVal.SSymbol(y)) => x == y
    case (SchemeVal.SChar(x), SchemeVal.SChar(y))     => x == y
    case (SchemeVal.SList(xs), SchemeVal.SList(ys)) =>
      xs.length == ys.length && xs.zip(ys).forall((x, y) => schemeEqual(x, y))
    case (SchemeVal.SPair(a1, d1), SchemeVal.SPair(a2, d2)) =>
      schemeEqual(a1, a2) && schemeEqual(d1, d2)
    case (SchemeVal.SVoid, SchemeVal.SVoid) => true
    case _                                  => false

  def schemeEq(a: SchemeVal, b: SchemeVal): Boolean = (a, b) match
    case (SchemeVal.SInt(x), SchemeVal.SInt(y))       => x == y
    case (SchemeVal.SBool(x), SchemeVal.SBool(y))     => x == y
    case (SchemeVal.SSymbol(x), SchemeVal.SSymbol(y)) => x == y
    case (SchemeVal.SChar(x), SchemeVal.SChar(y))     => x == y
    case (SchemeVal.SVoid, SchemeVal.SVoid)           => true
    case (SchemeVal.SList(Nil), SchemeVal.SList(Nil)) => true
    case _                                            => a eq b

  private def applyArithmetic(
    name: String,
    args: List[SchemeVal]
  ): SchemeVal =
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
      case "abs" =>
        SchemeVal.SInt(Math.abs(asInt(requireOne("abs", args))))
      case "modulo" =>
        val (a, b) = requireTwo("modulo", args)
        val x      = asInt(a); val y = asInt(b)
        SchemeVal.SInt(Math.floorMod(x, y))
      case "remainder" =>
        val (a, b) = requireTwo("remainder", args)
        val x      = asInt(a); val y = asInt(b)
        SchemeVal.SInt(x % y)
      case "quotient" =>
        val (a, b) = requireTwo("quotient", args)
        val x      = asInt(a); val y = asInt(b)
        SchemeVal.SInt(x / y)
      case "min" =>
        if args.isEmpty then throw new EvalError("min: expected at least 1 argument")
        SchemeVal.SInt(args.map(asInt).min)
      case "max" =>
        if args.isEmpty then throw new EvalError("max: expected at least 1 argument")
        SchemeVal.SInt(args.map(asInt).max)
      case "expt" =>
        val (a, b) = requireTwo("expt", args)
        val base   = asInt(a); val exp = asInt(b)
        SchemeVal.SInt(Math.pow(base.toDouble, exp.toDouble).toLong)
      case _ => throw new EvalError(s"unknown arithmetic op: $name")

  private def applyComparison(
    name: String,
    args: List[SchemeVal]
  ): SchemeVal =
    val nums  = requireComparison(name, args)
    val pairs = nums.zip(nums.tail)
    SchemeVal.SBool(name match
      case "="  => nums.forall(_ == nums.head)
      case "<"  => pairs.forall((a, b) => a < b)
      case ">"  => pairs.forall((a, b) => a > b)
      case "<=" => pairs.forall((a, b) => a <= b)
      case ">=" => pairs.forall((a, b) => a >= b)
      case _    => throw new EvalError(s"unknown comparison: $name"))

  private def applyTypePredicate(
    name: String,
    args: List[SchemeVal]
  ): SchemeVal =
    val arg = requireOne(name, args)
    SchemeVal.SBool((name, arg) match
      case ("string?", SchemeVal.SString(_)) => true
      case ("number?", SchemeVal.SInt(_))    => true
      case ("boolean?", SchemeVal.SBool(_))  => true
      case ("symbol?", SchemeVal.SSymbol(_)) => true
      case ("char?", SchemeVal.SChar(_))     => true
      case ("string?" | "number?" | "boolean?" | "symbol?" | "char?", _) =>
        false
      case _ => throw new EvalError(s"unknown predicate: $name"))

  private def applyNumericPredicate(
    name: String,
    args: List[SchemeVal]
  ): SchemeVal =
    val n = asInt(requireOne(name, args))
    SchemeVal.SBool(name match
      case "zero?"     => n == 0
      case "positive?" => n > 0
      case "negative?" => n < 0
      case "odd?"      => n % 2 != 0
      case "even?"     => n % 2 == 0
      case _           => throw new EvalError(s"unknown predicate: $name"))

  private def applyCharOp(
    name: String,
    args: List[SchemeVal]
  ): SchemeVal =
    name match
      case "char-alphabetic?" =>
        SchemeVal.SBool(asChar(requireOne(name, args)).isLetter)
      case "char-numeric?" =>
        SchemeVal.SBool(asChar(requireOne(name, args)).isDigit)
      case "char-upcase" =>
        SchemeVal.SChar(asChar(requireOne(name, args)).toUpper)
      case "char-downcase" =>
        SchemeVal.SChar(asChar(requireOne(name, args)).toLower)
      case "char=?" =>
        val (a, b) = requireTwo(name, args)
        SchemeVal.SBool(asChar(a) == asChar(b))
      case "char<?" =>
        val (a, b) = requireTwo(name, args)
        SchemeVal.SBool(asChar(a) < asChar(b))
      case _ => throw new EvalError(s"unknown char op: $name")

  def applyBuiltin(name: String, args: List[SchemeVal]): SchemeVal =
    name match
      case "+" | "-" | "*" | "/" | "abs" | "modulo" | "remainder" | "quotient" | "min" | "max" | "expt" =>
        applyArithmetic(name, args)
      case "=" | "<" | ">" | "<=" | ">=" => applyComparison(name, args)
      case "not" =>
        if args.length != 1 then throw new EvalError("not: expected 1 argument")
        SchemeVal.SBool(!isTruthy(args.head))
      case "cons" | "car" | "cdr" | "null?" | "list" | "length" | "append" | "pair?" | "list-ref" | "list-tail" |
          "list?" | "assoc" =>
        ListOps(name, args)
      case "string?" | "number?" | "boolean?" | "symbol?" | "char?" =>
        applyTypePredicate(name, args)
      case "zero?" | "positive?" | "negative?" | "odd?" | "even?" =>
        applyNumericPredicate(name, args)
      case "char-alphabetic?" | "char-numeric?" | "char-upcase" | "char-downcase" | "char=?" | "char<?" =>
        applyCharOp(name, args)
      case "string-append" | "string-length" | "substring" | "string->number" | "number->string" | "symbol->string" |
          "string->symbol" | "string-ref" | "string-copy" | "string-set!" | "string=?" | "string<?" | "string-ci=?" |
          "string-upcase" | "string-downcase" =>
        StringOps(name, args)
      case "eq?" =>
        val (a, b) = requireTwo("eq?", args)
        SchemeVal.SBool(schemeEq(a, b))
      case "equal?" =>
        val (a, b) = requireTwo("equal?", args)
        SchemeVal.SBool(schemeEqual(a, b))
      case other =>
        throw new EvalError(s"unknown procedure: $other")
