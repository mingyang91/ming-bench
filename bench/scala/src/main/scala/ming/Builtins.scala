package ming

/** Built-in primitive procedures — dispatches to specialized modules. */
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
    "string-downcase",
    // L11
    "exact?",
    "inexact?",
    "exact->inexact",
    "inexact->exact",
    "numerator",
    "denominator",
    "integer?",
    "rational?",
    // L13
    "procedure?",
    // L14
    "eqv?",
    "vector",
    "make-vector",
    "vector-ref",
    "vector-set!",
    "vector-length",
    "vector?",
    "vector->list",
    "list->vector",
    // L15
    "string->list",
    "list->string",
    "char->integer",
    "integer->char",
    // L17
    "set-car!",
    "set-cdr!",
    "caar",
    "cadr",
    "cdar",
    "cddr",
    "caddr",
    "error",
    "reverse",
    "member",
    "assv",
    "gcd",
    "lcm",
    "truncate",
    "round",
    "make-string",
    "string",
    "string>?",
    "string<=?",
    "string>=?",
    // L18
    "call/cc",
    "call-with-current-continuation"
  )

  private def isTruthy(v: SchemeVal): Boolean = v match
    case SchemeVal.SBool(false) => false
    case _                      => true

  private def isNumeric(v: SchemeVal): Boolean = v match
    case SchemeVal.SInt(_) | SchemeVal.SFloat(_) | SchemeVal.SRational(_, _) => true
    case _                                                                   => false

  private def toDouble(v: SchemeVal): Double = v match
    case SchemeVal.SInt(n)         => n.toDouble
    case SchemeVal.SFloat(d)       => d
    case SchemeVal.SRational(n, d) => n.toDouble / d.toDouble
    case other                     => throw new EvalError(s"expected number, got ${other.display}")

  private def requireTwo(name: String, args: List[SchemeVal]): (SchemeVal, SchemeVal) =
    if args.length != 2 then throw new EvalError(s"$name: expected 2 arguments")
    (args(0), args(1))

  def schemeEqual(a: SchemeVal, b: SchemeVal): Boolean = (a, b) match
    case (SchemeVal.SInt(x), SchemeVal.SInt(y))                     => x == y
    case (SchemeVal.SFloat(x), SchemeVal.SFloat(y))                 => x == y
    case (SchemeVal.SRational(n1, d1), SchemeVal.SRational(n2, d2)) => n1 == n2 && d1 == d2
    case _ if isNumeric(a) && isNumeric(b)                          => toDouble(a) == toDouble(b)
    case (SchemeVal.SBool(x), SchemeVal.SBool(y))                   => x == y
    case (SchemeVal.SString(x, _), SchemeVal.SString(y, _))         => x.toString == y.toString
    case (SchemeVal.SSymbol(x), SchemeVal.SSymbol(y))               => x == y
    case (SchemeVal.SChar(x), SchemeVal.SChar(y))                   => x == y
    case (SchemeVal.SList(Nil), SchemeVal.SList(Nil))               => true
    case (SchemeVal.SVector(xs), SchemeVal.SVector(ys)) =>
      xs.length == ys.length && xs.zip(ys).forall((x, y) => schemeEqual(x, y))
    case (SchemeVal.SVoid, SchemeVal.SVoid) => true
    // Handle pairs and non-empty lists uniformly
    case (aa, bb) if SchemeVal.isPairLike(aa) && SchemeVal.isPairLike(bb) =>
      schemeEqual(SchemeVal.pairCar(aa), SchemeVal.pairCar(bb)) &&
      schemeEqual(SchemeVal.pairCdr(aa), SchemeVal.pairCdr(bb))
    case _ => false

  def schemeEqv(a: SchemeVal, b: SchemeVal): Boolean = (a, b) match
    case (SchemeVal.SInt(x), SchemeVal.SInt(y))                     => x == y
    case (SchemeVal.SFloat(x), SchemeVal.SFloat(y))                 => x == y
    case (SchemeVal.SRational(n1, d1), SchemeVal.SRational(n2, d2)) => n1 == n2 && d1 == d2
    case (SchemeVal.SBool(x), SchemeVal.SBool(y))                   => x == y
    case (SchemeVal.SSymbol(x), SchemeVal.SSymbol(y))               => x == y
    case (SchemeVal.SChar(x), SchemeVal.SChar(y))                   => x == y
    case (SchemeVal.SVoid, SchemeVal.SVoid)                         => true
    case (SchemeVal.SList(Nil), SchemeVal.SList(Nil))               => true
    case _                                                          => a eq b

  def schemeEq(a: SchemeVal, b: SchemeVal): Boolean = (a, b) match
    case (SchemeVal.SInt(x), SchemeVal.SInt(y))       => x == y
    case (SchemeVal.SBool(x), SchemeVal.SBool(y))     => x == y
    case (SchemeVal.SSymbol(x), SchemeVal.SSymbol(y)) => x == y
    case (SchemeVal.SChar(x), SchemeVal.SChar(y))     => x == y
    case (SchemeVal.SVoid, SchemeVal.SVoid)           => true
    case (SchemeVal.SList(Nil), SchemeVal.SList(Nil)) => true
    case _                                            => a eq b

  def applyBuiltin(name: String, args: List[SchemeVal]): SchemeVal =
    name match
      case "+" | "-" | "*" | "/" | "abs" | "modulo" | "remainder" | "quotient" | "min" | "max" | "expt" | "gcd" |
          "lcm" | "truncate" | "round" =>
        NumericOps.applyArithmetic(name, args)
      case "=" | "<" | ">" | "<=" | ">=" =>
        NumericOps.applyComparison(name, args)
      case "not" =>
        if args.length != 1 then throw new EvalError("not: expected 1 argument")
        SchemeVal.SBool(!isTruthy(args.head))
      case "cons" | "car" | "cdr" | "null?" | "list" | "length" | "append" | "pair?" | "list-ref" | "list-tail" |
          "list?" | "assoc" | "set-car!" | "set-cdr!" | "caar" | "cadr" | "cdar" | "cddr" | "caddr" | "reverse" |
          "member" | "assv" =>
        ListOps(name, args)
      case "string?" | "number?" | "boolean?" | "symbol?" | "char?" | "integer?" | "rational?" =>
        NumericOps.applyTypePredicate(name, args)
      case "procedure?" =>
        if args.length != 1 then throw new EvalError("procedure?: expected 1 argument")
        val isProcedure = args.head match
          case _: SchemeVal.SLambda       => true
          case _: SchemeVal.SCaseLambda   => true
          case _: SchemeVal.SContinuation => true
          case SchemeVal.SSymbol(n)       => names.contains(n) || n.startsWith("__record-")
          case _                          => false
        SchemeVal.SBool(isProcedure)
      case "zero?" | "positive?" | "negative?" | "odd?" | "even?" =>
        NumericOps.applyNumericPredicate(name, args)
      case "char-alphabetic?" | "char-numeric?" | "char-upcase" | "char-downcase" | "char=?" | "char<?" =>
        NumericOps.applyCharOp(name, args)
      case "string-append" | "string-length" | "substring" | "string->number" | "number->string" | "symbol->string" |
          "string->symbol" | "string-ref" | "string-copy" | "string-set!" | "string=?" | "string<?" | "string-ci=?" |
          "string-upcase" | "string-downcase" | "string->list" | "list->string" | "make-string" | "string" |
          "string>?" | "string<=?" | "string>=?" =>
        StringOps(name, args)
      case "char->integer" =>
        if args.length != 1 then throw new EvalError("char->integer: expected 1 argument")
        args.head match
          case SchemeVal.SChar(c) => SchemeVal.SInt(c.toLong)
          case other              => throw new EvalError(s"char->integer: expected char, got ${other.display}")
      case "integer->char" =>
        if args.length != 1 then throw new EvalError("integer->char: expected 1 argument")
        args.head match
          case SchemeVal.SInt(n) => SchemeVal.SChar(n.toChar)
          case other             => throw new EvalError(s"integer->char: expected integer, got ${other.display}")
      case "eq?" =>
        val (a, b) = requireTwo("eq?", args)
        SchemeVal.SBool(schemeEq(a, b))
      case "equal?" =>
        val (a, b) = requireTwo("equal?", args)
        SchemeVal.SBool(schemeEqual(a, b))
      case "exact?" | "inexact?" | "exact->inexact" | "inexact->exact" | "numerator" | "denominator" =>
        NumericOps.applyExactness(name, args)
      case "eqv?" =>
        val (a, b) = requireTwo("eqv?", args)
        SchemeVal.SBool(schemeEqv(a, b))
      case "vector" | "make-vector" | "vector-ref" | "vector-set!" | "vector-length" | "vector?" | "vector->list" |
          "list->vector" =>
        VectorOps(name, args)
      case "error" =>
        if args.isEmpty then throw new EvalError("error")
        val msg = args.map(_.displayRepr).mkString(" ")
        throw new EvalError(msg)
      case other =>
        throw new EvalError(s"unknown procedure: $other")
