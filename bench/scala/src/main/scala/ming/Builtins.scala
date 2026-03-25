package ming

private[ming] object Builtins:

  val outputBuffer: ThreadLocal[StringBuilder] = new ThreadLocal[StringBuilder]

  def applyBuiltin(name: String, args: List[Expr]): Expr = name match
    case "+"        => applyAdd(args)
    case "-"        => applyMinus(args)
    case "*"        => applyMul(args)
    case "/"        => applyDiv(args)
    case "<"        => numCmp(name, args, _ < _)
    case ">"        => numCmp(name, args, _ > _)
    case "="        => numCmp(name, args, _ == _)
    case "<="       => numCmp(name, args, _ <= _)
    case ">="       => numCmp(name, args, _ >= _)
    case "not"      => unary(name, args)(e => Expr.Bool(isFalsy(e)))
    case "cons"     => applyCons(args)
    case "car"      => unary(name, args)(PairOps.carOf)
    case "cdr"      => unary(name, args)(PairOps.cdrOf)
    case "caar"     => unary(name, args)(e => PairOps.carOf(PairOps.carOf(e)))
    case "cadr"     => unary(name, args)(e => PairOps.carOf(PairOps.cdrOf(e)))
    case "cdar"     => unary(name, args)(e => PairOps.cdrOf(PairOps.carOf(e)))
    case "cddr"     => unary(name, args)(e => PairOps.cdrOf(PairOps.cdrOf(e)))
    case "null?"    => unary(name, args)(e => Expr.Bool(PairOps.isNull(e)))
    case "list"     => PairOps.makeList(args)
    case "length"   => unary(name, args)(e => Expr.Num(PairOps.lengthOf(e)))
    case "number?"  => unary(name, args)(e => Expr.Bool(NumericUtils.isNumber(e)))
    case "string?"  => unary(name, args)(e => Expr.Bool(e.isInstanceOf[Expr.Str]))
    case "boolean?" => unary(name, args)(e => Expr.Bool(e.isInstanceOf[Expr.Bool]))
    case "pair?"    => unary(name, args)(e => Expr.Bool(PairOps.isPair(e)))
    case "symbol?"  => unary(name, args)(e => Expr.Bool(e.isInstanceOf[Expr.Sym]))
    case "append"   => NumericListBuiltins.applyAppend(args)
    case "display"  => applyIO(name, args, displayOutput)
    case "write"    => applyIO(name, args, display)
    case "newline"  => applyOutput(args, "\n")
    case "string-append" | "string-length" | "string-set!" | "string-copy" | "substring" | "string->number" |
        "number->string" | "symbol->string" | "string->symbol" | "string-ref" | "char?" | "string->list" |
        "list->string" | "char->integer" | "integer->char" | "make-string" | "string" =>
      StringBuiltins.applyStringBuiltin(name, args)
    case "abs" =>
      unary(name, args) {
        case Expr.Num(n)         => Expr.Num(math.abs(n))
        case Expr.Rational(n, d) => Expr.Rational(math.abs(n), d)
        case Expr.Real(v)        => Expr.Real(math.abs(v))
        case _                   => throw EvalError("abs: not a number")
      }
    case "modulo"    => NumericListBuiltins.applyModulo(args)
    case "remainder" => NumericListBuiltins.applyRemainder(args)
    case "quotient"  => NumericListBuiltins.applyQuotient(args)
    case "min"       => NumericListBuiltins.applyMinMax(name, args, _ < _)
    case "max"       => NumericListBuiltins.applyMinMax(name, args, _ > _)
    case "expt"      => NumericListBuiltins.applyExpt(args)
    case "zero?" | "positive?" | "negative?" | "odd?" | "even?" =>
      NumericListBuiltins.applyNumericPredicate(name, args)
    case "list-ref"     => NumericListBuiltins.applyListRef(args)
    case "list-tail"    => NumericListBuiltins.applyListTail(args)
    case "list?"        => unary(name, args)(e => Expr.Bool(PairOps.isList(e)))
    case "assoc"        => NumericListBuiltins.applyAssoc(args)
    case "map"          => throw EvalError("map: should be handled by applyProc")
    case "for-each"     => throw EvalError("for-each: should be handled by applyProc")
    case "eq?" | "eqv?" => applyEqv(name, args)
    case "equal?" =>
      if args.length != 2 then throw EvalError("equal?: need exactly 2 arguments")
      Expr.Bool(EqualityOps.schemeEqual(args(0), args(1)))
    case "char-alphabetic?" | "char-numeric?" | "char-upcase" | "char-downcase" | "char=?" | "char<?" | "char<=?" |
        "char>=?" | "char>?" =>
      StringBuiltins.applyCharBuiltin(name, args)
    case "string=?" | "string<?" | "string-ci=?" | "string-upcase" | "string-downcase" | "string<=?" | "string>=?" |
        "string>?" =>
      StringBuiltins.applyStringBuiltin(name, args)
    case "exact?" | "inexact?" | "integer?" | "rational?" | "exact->inexact" | "inexact->exact" | "numerator" |
        "denominator" =>
      RationalBuiltins.applyRationalBuiltin(name, args)
    case "procedure?" => unary(name, args)(BuiltinRegistry.isProcedure)
    case "vector" | "make-vector" | "vector-ref" | "vector-set!" | "vector-length" | "vector?" | "vector->list" |
        "list->vector" =>
      VectorBuiltins.applyVectorBuiltin(name, args)
    case "set-car!" => applySetCar(args)
    case "set-cdr!" => applySetCdr(args)
    case "reverse" =>
      unary(name, args)(e => PairOps.makeList(PairOps.toScalaList(e).reverse))
    case "member" | "memv" | "memq" | "assv" | "assq" =>
      ListSearchBuiltins.applyListSearch(name, args)
    case "gcd" | "lcm" | "floor" | "ceiling" | "truncate" | "round" | "sqrt" =>
      MathBuiltins.applyMathBuiltin(name, args)
    case "void"          => Expr.Bool(false)
    case "apply"         => throw EvalError("apply: should be handled by applyProc")
    case "syntax->datum" => unary(name, args)(v => v)
    case "datum->syntax" =>
      if args.length != 2 then throw EvalError("datum->syntax: need exactly 2 arguments")
      args(1) // return the datum as-is (lexical context ignored in this impl)
    case _ => throw EvalError(s"unknown procedure: $name")

  private def applyAdd(args: List[Expr]): Expr =
    if args.isEmpty then return Expr.Num(0)
    if args.exists(_.isInstanceOf[Expr.Real]) then Expr.Real(args.map(NumericUtils.toDouble).sum)
    else args.map(toExact).reduceLeft(NumericUtils.addExact)

  private def applyMinus(args: List[Expr]): Expr =
    if args.isEmpty then throw EvalError("-: need at least 1 argument")
    if args.exists(_.isInstanceOf[Expr.Real]) then
      val nums = args.map(NumericUtils.toDouble)
      if nums.length == 1 then Expr.Real(-nums.head)
      else Expr.Real(nums.reduceLeft(_ - _))
    else
      val exacts = args.map(toExact)
      if exacts.length == 1 then NumericUtils.negateExact(exacts.head)
      else exacts.reduceLeft(NumericUtils.subExact)

  private def applyMul(args: List[Expr]): Expr =
    if args.isEmpty then return Expr.Num(1)
    if args.exists(_.isInstanceOf[Expr.Real]) then Expr.Real(args.map(NumericUtils.toDouble).product)
    else args.map(toExact).reduceLeft(NumericUtils.mulExact)

  private def applyDiv(args: List[Expr]): Expr =
    if args.length < 2 then throw EvalError("/: need at least 2 arguments")
    if args.exists(_.isInstanceOf[Expr.Real]) then
      val nums = args.map(NumericUtils.toDouble)
      if nums.tail.contains(0.0) then throw EvalError("division by zero")
      Expr.Real(nums.reduceLeft(_ / _))
    else
      val exacts = args.map(toExact)
      exacts.reduceLeft(NumericUtils.divExact)

  private def toExact(e: Expr): Expr = e match
    case Expr.Num(_) | Expr.Rational(_, _) => e
    case _                                 => throw EvalError(s"expected number, got ${display(e)}")

  private def applyEqv(name: String, args: List[Expr]): Expr =
    if args.length != 2 then throw EvalError(s"$name: need exactly 2 arguments")
    Expr.Bool(EqualityOps.eqv(args(0), args(1)))

  private def applyCons(args: List[Expr]): Expr =
    if args.length != 2 then throw EvalError("cons: need exactly 2 arguments")
    PairOps.cons(args(0), args(1))

  private def applySetCar(args: List[Expr]): Expr =
    if args.length != 2 then throw EvalError("set-car!: need exactly 2 arguments")
    args(0) match
      case Expr.Pair(cell) =>
        cell.car = args(1)
        Expr.Bool(false)
      case _ => throw EvalError("set-car!: not a mutable pair")

  private def applySetCdr(args: List[Expr]): Expr =
    if args.length != 2 then throw EvalError("set-cdr!: need exactly 2 arguments")
    args(0) match
      case Expr.Pair(cell) =>
        cell.cdr = args(1)
        Expr.Bool(false)
      case _ => throw EvalError("set-cdr!: not a mutable pair")

  def unary(name: String, args: List[Expr])(f: Expr => Expr): Expr =
    if args.length != 1 then throw EvalError(s"$name: need exactly 1 argument")
    f(args.head)

  private def numCmp(
    name: String,
    args: List[Expr],
    op: (Double, Double) => Boolean
  ): Expr =
    if args.length != 2 then throw EvalError(s"$name: need exactly 2 arguments")
    Expr.Bool(op(NumericUtils.toDouble(args(0)), NumericUtils.toDouble(args(1))))

  def asNum(e: Expr): Long = e match
    case Expr.Num(n) => n
    case _           => throw EvalError(s"expected integer, got ${display(e)}")

  def isFalsy(e: Expr): Boolean = e match
    case Expr.Bool(false) => true
    case _                => false

  def display(e: Expr): String = Display.display(e)

  private def displayOutput(e: Expr): String = Display.displayOutput(e)

  private def applyIO(name: String, args: List[Expr], fmt: Expr => String): Expr =
    unary(name, args) { e =>
      val buf = outputBuffer.get()
      if buf != null then buf.append(fmt(e))
      Expr.Bool(false)
    }

  private def applyOutput(args: List[Expr], text: String): Expr =
    if args.nonEmpty then throw EvalError("newline: need exactly 0 arguments")
    val buf = outputBuffer.get()
    if buf != null then buf.append(text)
    Expr.Bool(false)
