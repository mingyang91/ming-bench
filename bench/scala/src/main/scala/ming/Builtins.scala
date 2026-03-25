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
    "list->vector"
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
    case (SchemeVal.SString(x), SchemeVal.SString(y))               => x.toString == y.toString
    case (SchemeVal.SSymbol(x), SchemeVal.SSymbol(y))               => x == y
    case (SchemeVal.SChar(x), SchemeVal.SChar(y))                   => x == y
    case (SchemeVal.SList(xs), SchemeVal.SList(ys)) =>
      xs.length == ys.length && xs.zip(ys).forall((x, y) => schemeEqual(x, y))
    case (SchemeVal.SPair(a1, d1), SchemeVal.SPair(a2, d2)) =>
      schemeEqual(a1, a2) && schemeEqual(d1, d2)
    case (SchemeVal.SVector(xs), SchemeVal.SVector(ys)) =>
      xs.length == ys.length && xs.zip(ys).forall((x, y) => schemeEqual(x, y))
    case (SchemeVal.SVoid, SchemeVal.SVoid) => true
    case _                                  => false

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
      case "+" | "-" | "*" | "/" | "abs" | "modulo" | "remainder" | "quotient" | "min" | "max" | "expt" =>
        NumericOps.applyArithmetic(name, args)
      case "=" | "<" | ">" | "<=" | ">=" =>
        NumericOps.applyComparison(name, args)
      case "not" =>
        if args.length != 1 then throw new EvalError("not: expected 1 argument")
        SchemeVal.SBool(!isTruthy(args.head))
      case "cons" | "car" | "cdr" | "null?" | "list" | "length" | "append" | "pair?" | "list-ref" | "list-tail" |
          "list?" | "assoc" =>
        ListOps(name, args)
      case "string?" | "number?" | "boolean?" | "symbol?" | "char?" | "integer?" | "rational?" =>
        NumericOps.applyTypePredicate(name, args)
      case "procedure?" =>
        if args.length != 1 then throw new EvalError("procedure?: expected 1 argument")
        val isProcedure = args.head match
          case _: SchemeVal.SLambda     => true
          case _: SchemeVal.SCaseLambda => true
          case SchemeVal.SSymbol(n)     => names.contains(n) || n.startsWith("__record-")
          case _                        => false
        SchemeVal.SBool(isProcedure)
      case "zero?" | "positive?" | "negative?" | "odd?" | "even?" =>
        NumericOps.applyNumericPredicate(name, args)
      case "char-alphabetic?" | "char-numeric?" | "char-upcase" | "char-downcase" | "char=?" | "char<?" =>
        NumericOps.applyCharOp(name, args)
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
      case "exact?" | "inexact?" | "exact->inexact" | "inexact->exact" | "numerator" | "denominator" =>
        NumericOps.applyExactness(name, args)
      case "eqv?" =>
        val (a, b) = requireTwo("eqv?", args)
        SchemeVal.SBool(schemeEqv(a, b))
      case "vector" =>
        SchemeVal.SVector(args.toArray)
      case "make-vector" =>
        args match
          case SchemeVal.SInt(n) :: Nil =>
            SchemeVal.SVector(Array.fill(n.toInt)(SchemeVal.SInt(0)))
          case SchemeVal.SInt(n) :: fill :: Nil =>
            SchemeVal.SVector(Array.fill(n.toInt)(fill))
          case _ => throw new EvalError("make-vector: expected (size) or (size fill)")
      case "vector-ref" =>
        val (v, idx) = requireTwo("vector-ref", args)
        (v, idx) match
          case (SchemeVal.SVector(elems), SchemeVal.SInt(n)) =>
            val i = n.toInt
            if i < 0 || i >= elems.length then throw new EvalError("vector-ref: index out of range")
            elems(i)
          case (SchemeVal.SVector(_), _) => throw new EvalError("vector-ref: expected integer index")
          case _                         => throw new EvalError("vector-ref: expected vector")
      case "vector-set!" =>
        if args.length != 3 then throw new EvalError("vector-set!: expected 3 arguments")
        (args(0), args(1)) match
          case (SchemeVal.SVector(elems), SchemeVal.SInt(n)) =>
            val i = n.toInt
            if i < 0 || i >= elems.length then throw new EvalError("vector-set!: index out of range")
            elems(i) = args(2)
            SchemeVal.SVoid
          case (SchemeVal.SVector(_), _) => throw new EvalError("vector-set!: expected integer index")
          case _                         => throw new EvalError("vector-set!: expected vector")
      case "vector-length" =>
        if args.length != 1 then throw new EvalError("vector-length: expected 1 argument")
        args.head match
          case SchemeVal.SVector(elems) => SchemeVal.SInt(elems.length.toLong)
          case _                        => throw new EvalError("vector-length: expected vector")
      case "vector?" =>
        if args.length != 1 then throw new EvalError("vector?: expected 1 argument")
        val isVec = args.head match
          case _: SchemeVal.SVector => true
          case _                    => false
        SchemeVal.SBool(isVec)
      case "vector->list" =>
        if args.length != 1 then throw new EvalError("vector->list: expected 1 argument")
        args.head match
          case SchemeVal.SVector(elems) => SchemeVal.SList(elems.toList)
          case _                        => throw new EvalError("vector->list: expected vector")
      case "list->vector" =>
        if args.length != 1 then throw new EvalError("list->vector: expected 1 argument")
        args.head match
          case SchemeVal.SList(elems) => SchemeVal.SVector(elems.toArray)
          case _                      => throw new EvalError("list->vector: expected list")
      case other =>
        throw new EvalError(s"unknown procedure: $other")
