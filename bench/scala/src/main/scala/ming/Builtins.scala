package ming

private[ming] object Builtins:

  val outputBuffer: ThreadLocal[StringBuilder] = new ThreadLocal[StringBuilder]

  def applyBuiltin(name: String, args: List[Expr]): Expr = name match
    case "+"    => applyAdd(args)
    case "-"    => applyMinus(args)
    case "*"    => applyMul(args)
    case "/"    => applyDiv(args)
    case "<"    => numCmp(name, args, _ < _)
    case ">"    => numCmp(name, args, _ > _)
    case "="    => numCmp(name, args, _ == _)
    case "<="   => numCmp(name, args, _ <= _)
    case ">="   => numCmp(name, args, _ >= _)
    case "not"  => unary(name, args)(e => Expr.Bool(isFalsy(e)))
    case "cons" => applyCons(args)
    case "car"  => unary(name, args)(carOf)
    case "cdr"  => unary(name, args)(cdrOf)
    case "null?" =>
      unary(name, args)(e => Expr.Bool(e == Expr.Lst(Nil)))
    case "list"     => Expr.Lst(args)
    case "length"   => unary(name, args)(lengthOf)
    case "number?"  => unary(name, args)(e => Expr.Bool(NumericUtils.isNumber(e)))
    case "string?"  => unary(name, args)(e => Expr.Bool(e.isInstanceOf[Expr.Str]))
    case "boolean?" => unary(name, args)(e => Expr.Bool(e.isInstanceOf[Expr.Bool]))
    case "pair?" =>
      unary(name, args) {
        case Expr.Lst(_ :: _) => Expr.Bool(true)
        case Expr.Pair(_, _)  => Expr.Bool(true)
        case _                => Expr.Bool(false)
      }
    case "symbol?" => unary(name, args)(e => Expr.Bool(e.isInstanceOf[Expr.Sym]))
    case "append"  => NumericListBuiltins.applyAppend(args)
    case "display" =>
      unary(name, args) { e =>
        val buf = outputBuffer.get()
        if buf != null then buf.append(displayOutput(e))
        Expr.Bool(false)
      }
    case "write" =>
      unary(name, args) { e =>
        val buf = outputBuffer.get()
        if buf != null then buf.append(display(e))
        Expr.Bool(false)
      }
    case "newline" =>
      if args.nonEmpty then throw EvalError("newline: need exactly 0 arguments")
      val buf = outputBuffer.get()
      if buf != null then buf.append("\n")
      Expr.Bool(false)
    case "string-append" | "string-length" | "string-set!" | "string-copy" | "substring" | "string->number" |
        "number->string" | "symbol->string" | "string->symbol" | "string-ref" | "char?" =>
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
    case "list-ref"  => NumericListBuiltins.applyListRef(args)
    case "list-tail" => NumericListBuiltins.applyListTail(args)
    case "list?"     => unary(name, args)(e => Expr.Bool(NumericListBuiltins.isList(e)))
    case "assoc"     => NumericListBuiltins.applyAssoc(args)
    case "map"       => throw EvalError("map: should be handled by applyProc")
    case "eq?" =>
      if args.length != 2 then throw EvalError("eq?: need exactly 2 arguments")
      Expr.Bool(EqualityOps.eqv(args(0), args(1)))
    case "equal?" =>
      if args.length != 2 then throw EvalError("equal?: need exactly 2 arguments")
      Expr.Bool(EqualityOps.schemeEqual(args(0), args(1)))
    case "char-alphabetic?" | "char-numeric?" | "char-upcase" | "char-downcase" | "char=?" | "char<?" =>
      StringBuiltins.applyCharBuiltin(name, args)
    case "string=?"    => StringBuiltins.strCmp(name, args, _ == _)
    case "string<?"    => StringBuiltins.strCmp(name, args, _ < _)
    case "string-ci=?" => StringBuiltins.strCiCmp(name, args, _ == _)
    case "string-upcase" =>
      unary(name, args) {
        case Expr.Str(s) => Expr.Str(new String(s).toUpperCase.toCharArray);
        case _           => throw EvalError("string-upcase: not a string")
      }
    case "string-downcase" =>
      unary(name, args) {
        case Expr.Str(s) => Expr.Str(new String(s).toLowerCase.toCharArray);
        case _           => throw EvalError("string-downcase: not a string")
      }
    case "exact?" | "inexact?" | "integer?" | "rational?" | "exact->inexact" | "inexact->exact" | "numerator" |
        "denominator" =>
      RationalBuiltins.applyRationalBuiltin(name, args)
    case "apply" => throw EvalError("apply: should be handled by applyProc")
    case _       => throw EvalError(s"unknown procedure: $name")

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

  private def applyCons(args: List[Expr]): Expr =
    if args.length != 2 then throw EvalError("cons: need exactly 2 arguments")
    args(1) match
      case Expr.Lst(elems) => Expr.Lst(args(0) :: elems)
      case _               => Expr.Pair(args(0), args(1))

  private def carOf(e: Expr): Expr = e match
    case Expr.Lst(h :: _) => h
    case Expr.Pair(a, _)  => a
    case _                => throw EvalError("car: not a pair")

  private def cdrOf(e: Expr): Expr = e match
    case Expr.Lst(_ :: t) => Expr.Lst(t)
    case Expr.Pair(_, d)  => d
    case _                => throw EvalError("cdr: not a pair")

  private def lengthOf(e: Expr): Expr = e match
    case Expr.Lst(elems) => Expr.Num(elems.length.toLong)
    case _               => throw EvalError("length: not a list")

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

  def display(e: Expr): String = e match
    case Expr.Num(n)         => n.toString
    case Expr.Rational(n, d) => s"$n/$d"
    case Expr.Real(v) =>
      if v == v.floor && !v.isInfinite && !v.isNaN then
        val l = v.toLong
        if l.toDouble == v then s"$l.0"
        else v.toString
      else v.toString
    case Expr.Bool(true)            => "#t"
    case Expr.Bool(false)           => "#f"
    case Expr.Str(s)                => "\"" + new String(s) + "\""
    case Expr.Chr(c)                => s"#\\$c"
    case Expr.Sym(name)             => name
    case Expr.Lst(elems)            => "(" + elems.map(display).mkString(" ") + ")"
    case Expr.Pair(a, d)            => s"(${display(a)} . ${display(d)})"
    case Expr.Lambda(_, _, _, _)    => "#<procedure>"
    case Expr.Macro(_, _, _)        => "#<macro>"
    case Expr.Record(name, _, _, _) => s"#<record:$name>"

  private def displayOutput(e: Expr): String = e match
    case Expr.Str(s) => new String(s)
    case other       => display(other)

  def eqv(a: Expr, b: Expr): Boolean = EqualityOps.eqv(a, b)

  def schemeEqual(a: Expr, b: Expr): Boolean = EqualityOps.schemeEqual(a, b)

  val builtinNames: List[String] = List(
    "+",
    "-",
    "*",
    "/",
    "<",
    ">",
    "=",
    "<=",
    ">=",
    "not",
    "cons",
    "car",
    "cdr",
    "null?",
    "list",
    "length",
    "number?",
    "string?",
    "boolean?",
    "pair?",
    "symbol?",
    "append",
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
    "string-set!",
    "string-copy",
    "char?",
    "apply",
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
    "map",
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
    "exact?",
    "inexact?",
    "integer?",
    "rational?",
    "exact->inexact",
    "inexact->exact",
    "numerator",
    "denominator"
  )

  def makeTopLevelEnv(): Env =
    val env = Env(scala.collection.mutable.Map.empty, None)
    for name <- builtinNames do env.define(name, Expr.Sym(name))
    env
