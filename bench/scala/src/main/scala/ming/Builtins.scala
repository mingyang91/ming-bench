package ming

import scala.collection.mutable

private[ming] object Builtins:

  val outputBuffer: ThreadLocal[StringBuilder] = new ThreadLocal[StringBuilder]

  def applyBuiltin(name: String, args: List[Expr]): Expr = name match
    case "+"    => Expr.Num(args.map(asNum).sum)
    case "-"    => applyMinus(args)
    case "*"    => Expr.Num(args.map(asNum).product)
    case "/"    => applyDiv(args)
    case "<"    => binaryCmp(name, args, _ < _)
    case ">"    => binaryCmp(name, args, _ > _)
    case "="    => binaryCmp(name, args, _ == _)
    case "<="   => binaryCmp(name, args, _ <= _)
    case ">="   => binaryCmp(name, args, _ >= _)
    case "not"  => unary(name, args)(e => Expr.Bool(isFalsy(e)))
    case "cons" => applyCons(args)
    case "car"  => unary(name, args)(carOf)
    case "cdr"  => unary(name, args)(cdrOf)
    case "null?" =>
      unary(name, args)(e => Expr.Bool(e == Expr.Lst(Nil)))
    case "list"     => Expr.Lst(args)
    case "length"   => unary(name, args)(lengthOf)
    case "number?"  => unary(name, args)(e => Expr.Bool(e.isInstanceOf[Expr.Num]))
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
      unary(name, args) { case Expr.Num(n) => Expr.Num(math.abs(n)); case _ => throw EvalError("abs: not a number") }
    case "modulo"    => NumericListBuiltins.applyModulo(args)
    case "remainder" => NumericListBuiltins.applyRemainder(args)
    case "quotient"  => NumericListBuiltins.applyQuotient(args)
    case "min"       => NumericListBuiltins.applyMinMax(name, args, _ < _)
    case "max"       => NumericListBuiltins.applyMinMax(name, args, _ > _)
    case "expt"      => NumericListBuiltins.applyExpt(args)
    case "zero?" =>
      unary(name, args) { case Expr.Num(n) => Expr.Bool(n == 0); case _ => throw EvalError("zero?: not a number") }
    case "positive?" =>
      unary(name, args) { case Expr.Num(n) => Expr.Bool(n > 0); case _ => throw EvalError("positive?: not a number") }
    case "negative?" =>
      unary(name, args) { case Expr.Num(n) => Expr.Bool(n < 0); case _ => throw EvalError("negative?: not a number") }
    case "odd?" =>
      unary(name, args) { case Expr.Num(n) => Expr.Bool(n % 2 != 0); case _ => throw EvalError("odd?: not a number") }
    case "even?" =>
      unary(name, args) { case Expr.Num(n) => Expr.Bool(n % 2 == 0); case _ => throw EvalError("even?: not a number") }
    case "list-ref"  => NumericListBuiltins.applyListRef(args)
    case "list-tail" => NumericListBuiltins.applyListTail(args)
    case "list?"     => unary(name, args)(e => Expr.Bool(NumericListBuiltins.isList(e)))
    case "assoc"     => NumericListBuiltins.applyAssoc(args)
    case "map"       => throw EvalError("map: should be handled by applyProc")
    case "eq?" =>
      if args.length != 2 then throw EvalError("eq?: need exactly 2 arguments"); Expr.Bool(eqv(args(0), args(1)))
    case "equal?" =>
      if args.length != 2 then throw EvalError("equal?: need exactly 2 arguments");
      Expr.Bool(schemeEqual(args(0), args(1)))
    case "char-alphabetic?" =>
      unary(name, args) {
        case Expr.Chr(c) => Expr.Bool(c.isLetter); case _ => throw EvalError("char-alphabetic?: not a char")
      }
    case "char-numeric?" =>
      unary(name, args) {
        case Expr.Chr(c) => Expr.Bool(c.isDigit); case _ => throw EvalError("char-numeric?: not a char")
      }
    case "char-upcase" =>
      unary(name, args) {
        case Expr.Chr(c) => Expr.Chr(c.toUpper); case _ => throw EvalError("char-upcase: not a char")
      }
    case "char-downcase" =>
      unary(name, args) {
        case Expr.Chr(c) => Expr.Chr(c.toLower); case _ => throw EvalError("char-downcase: not a char")
      }
    case "char=?"      => StringBuiltins.charCmp(name, args, _ == _)
    case "char<?"      => StringBuiltins.charCmp(name, args, _ < _)
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
    case "apply" => throw EvalError("apply: should be handled by applyProc")
    case _       => throw EvalError(s"unknown procedure: $name")

  private def applyMinus(args: List[Expr]): Expr =
    if args.isEmpty then throw EvalError("-: need at least 1 argument")
    val nums = args.map(asNum)
    if nums.length == 1 then Expr.Num(-nums.head)
    else Expr.Num(nums.reduceLeft(_ - _))

  private def applyDiv(args: List[Expr]): Expr =
    if args.length < 2 then throw EvalError("/: need at least 2 arguments")
    val nums = args.map(asNum)
    if nums.tail.contains(0L) then throw EvalError("division by zero")
    Expr.Num(nums.reduceLeft(_ / _))

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

  private def binaryCmp(
    name: String,
    args: List[Expr],
    op: (Long, Long) => Boolean
  ): Expr =
    if args.length != 2 then throw EvalError(s"$name: need exactly 2 arguments")
    Expr.Bool(op(asNum(args(0)), asNum(args(1))))

  def asNum(e: Expr): Long = e match
    case Expr.Num(n) => n
    case _           => throw EvalError(s"expected number, got ${display(e)}")

  def isFalsy(e: Expr): Boolean = e match
    case Expr.Bool(false) => true
    case _                => false

  def display(e: Expr): String = e match
    case Expr.Num(n)             => n.toString
    case Expr.Bool(true)         => "#t"
    case Expr.Bool(false)        => "#f"
    case Expr.Str(s)             => "\"" + new String(s) + "\""
    case Expr.Chr(c)             => s"#\\$c"
    case Expr.Sym(name)          => name
    case Expr.Lst(elems)         => "(" + elems.map(display).mkString(" ") + ")"
    case Expr.Pair(a, d)         => s"(${display(a)} . ${display(d)})"
    case Expr.Lambda(_, _, _, _) => "#<procedure>"

  private def displayOutput(e: Expr): String = e match
    case Expr.Str(s) => new String(s)
    case other       => display(other)

  def eqv(a: Expr, b: Expr): Boolean = (a, b) match
    case (Expr.Num(x), Expr.Num(y))     => x == y
    case (Expr.Bool(x), Expr.Bool(y))   => x == y
    case (Expr.Sym(x), Expr.Sym(y))     => x == y
    case (Expr.Chr(x), Expr.Chr(y))     => x == y
    case (Expr.Lst(Nil), Expr.Lst(Nil)) => true
    case _                              => a eq b

  def schemeEqual(a: Expr, b: Expr): Boolean = (a, b) match
    case (Expr.Num(x), Expr.Num(y))   => x == y
    case (Expr.Bool(x), Expr.Bool(y)) => x == y
    case (Expr.Sym(x), Expr.Sym(y))   => x == y
    case (Expr.Chr(x), Expr.Chr(y))   => x == y
    case (Expr.Str(x), Expr.Str(y))   => java.util.Arrays.equals(x, y)
    case (Expr.Lst(xs), Expr.Lst(ys)) =>
      xs.length == ys.length && xs.zip(ys).forall((a, b) => schemeEqual(a, b))
    case (Expr.Pair(a1, d1), Expr.Pair(a2, d2)) =>
      schemeEqual(a1, a2) && schemeEqual(d1, d2)
    case _ => false

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
    "string-downcase"
  )

  def makeTopLevelEnv(): Env =
    val env = Env(scala.collection.mutable.Map.empty, None)
    for name <- builtinNames do env.define(name, Expr.Sym(name))
    env
