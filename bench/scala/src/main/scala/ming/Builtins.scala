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
        case _                => Expr.Bool(false)
      }
    case "symbol?" => unary(name, args)(e => Expr.Bool(e.isInstanceOf[Expr.Sym]))
    case "append"  => applyAppend(args)
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
      applyStringBuiltin(name, args)
    case "apply" => throw EvalError("apply: should be handled by applyProc")
    case _       => throw EvalError(s"unknown procedure: $name")

  private def applyStringBuiltin(name: String, args: List[Expr]): Expr = name match
    case "string-append" =>
      val strs = args.map {
        case Expr.Str(s) => new String(s)
        case other       => throw EvalError(s"string-append: not a string: ${display(other)}")
      }
      Expr.Str(strs.mkString.toCharArray)
    case "string-length" =>
      unary(name, args) {
        case Expr.Str(s) => Expr.Num(s.length.toLong)
        case other       => throw EvalError(s"string-length: not a string: ${display(other)}")
      }
    case "string-set!" =>
      if args.length != 3 then throw EvalError("string-set!: need exactly 3 arguments")
      args match
        case List(Expr.Str(s), Expr.Num(idx), Expr.Chr(c)) =>
          s(idx.toInt) = c
          Expr.Bool(false)
        case _ => throw EvalError("string-set!: invalid arguments")
    case "string-copy" =>
      unary(name, args) {
        case Expr.Str(s) => Expr.Str(s.clone())
        case other       => throw EvalError(s"string-copy: not a string: ${display(other)}")
      }
    case "substring" =>
      if args.length != 3 then throw EvalError("substring: need exactly 3 arguments")
      args match
        case List(Expr.Str(s), Expr.Num(start), Expr.Num(end)) =>
          Expr.Str(new String(s).substring(start.toInt, end.toInt).toCharArray)
        case _ => throw EvalError("substring: invalid arguments")
    case "string->number" =>
      unary(name, args) {
        case Expr.Str(s) =>
          try Expr.Num(new String(s).toLong)
          catch case _: NumberFormatException => Expr.Bool(false)
        case other => throw EvalError(s"string->number: not a string: ${display(other)}")
      }
    case "number->string" =>
      unary(name, args) {
        case Expr.Num(n) => Expr.Str(n.toString.toCharArray)
        case other       => throw EvalError(s"number->string: not a number: ${display(other)}")
      }
    case "symbol->string" =>
      unary(name, args) {
        case Expr.Sym(s) => Expr.Str(s.toCharArray)
        case other       => throw EvalError(s"symbol->string: not a symbol: ${display(other)}")
      }
    case "string->symbol" =>
      unary(name, args) {
        case Expr.Str(s) => Expr.Sym(new String(s))
        case other       => throw EvalError(s"string->symbol: not a string: ${display(other)}")
      }
    case "string-ref" =>
      if args.length != 2 then throw EvalError("string-ref: need exactly 2 arguments")
      args match
        case List(Expr.Str(s), Expr.Num(idx)) => Expr.Chr(s(idx.toInt))
        case _                                => throw EvalError("string-ref: invalid arguments")
    case "char?" =>
      unary(name, args)(e => Expr.Bool(e.isInstanceOf[Expr.Chr]))
    case _ => throw EvalError(s"unknown procedure: $name")

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
      case _               => throw EvalError("cons: second argument must be a list")

  private def carOf(e: Expr): Expr = e match
    case Expr.Lst(h :: _) => h
    case _                => throw EvalError("car: not a pair")

  private def cdrOf(e: Expr): Expr = e match
    case Expr.Lst(_ :: t) => Expr.Lst(t)
    case _                => throw EvalError("cdr: not a pair")

  private def lengthOf(e: Expr): Expr = e match
    case Expr.Lst(elems) => Expr.Num(elems.length.toLong)
    case _               => throw EvalError("length: not a list")

  private def applyAppend(args: List[Expr]): Expr =
    if args.isEmpty then Expr.Lst(Nil)
    else
      val lists = args.map {
        case Expr.Lst(elems) => elems
        case other           => throw EvalError(s"append: not a list: ${display(other)}")
      }
      Expr.Lst(lists.flatten)

  private def unary(name: String, args: List[Expr])(f: Expr => Expr): Expr =
    if args.length != 1 then throw EvalError(s"$name: need exactly 1 argument")
    f(args.head)

  private def binaryCmp(
    name: String,
    args: List[Expr],
    op: (Long, Long) => Boolean
  ): Expr =
    if args.length != 2 then throw EvalError(s"$name: need exactly 2 arguments")
    Expr.Bool(op(asNum(args(0)), asNum(args(1))))

  private def asNum(e: Expr): Long = e match
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
    case Expr.Lambda(_, _, _, _) => "#<procedure>"

  private def displayOutput(e: Expr): String = e match
    case Expr.Str(s) => new String(s)
    case other       => display(other)

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
    "apply"
  )

  def makeTopLevelEnv(): Env =
    val env = Env(scala.collection.mutable.Map.empty, None)
    for name <- builtinNames do env.define(name, Expr.Sym(name))
    env
