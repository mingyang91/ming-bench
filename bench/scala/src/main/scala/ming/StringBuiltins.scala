package ming

private[ming] object StringBuiltins:

  def applyStringBuiltin(name: String, args: List[Expr]): Expr = name match
    case "string-append" =>
      val strs = args.map {
        case Expr.Str(s) => new String(s)
        case other       => throw EvalError(s"string-append: not a string: ${Builtins.display(other)}")
      }
      Expr.Str(strs.mkString.toCharArray)
    case "string-length" =>
      Builtins.unary(name, args) {
        case Expr.Str(s) => Expr.Num(s.length.toLong)
        case other       => throw EvalError(s"string-length: not a string: ${Builtins.display(other)}")
      }
    case "string-set!" =>
      if args.length != 3 then throw EvalError("string-set!: need exactly 3 arguments")
      args match
        case List(Expr.Str(s), Expr.Num(idx), Expr.Chr(c)) =>
          s(idx.toInt) = c
          Expr.Bool(false)
        case _ => throw EvalError("string-set!: invalid arguments")
    case "string-copy" =>
      Builtins.unary(name, args) {
        case Expr.Str(s) => Expr.Str(s.clone())
        case other       => throw EvalError(s"string-copy: not a string: ${Builtins.display(other)}")
      }
    case "substring" =>
      if args.length != 3 then throw EvalError("substring: need exactly 3 arguments")
      args match
        case List(Expr.Str(s), Expr.Num(start), Expr.Num(end)) =>
          Expr.Str(new String(s).substring(start.toInt, end.toInt).toCharArray)
        case _ => throw EvalError("substring: invalid arguments")
    case "string->number" =>
      Builtins.unary(name, args) {
        case Expr.Str(s) =>
          try Expr.Num(new String(s).toLong)
          catch case _: NumberFormatException => Expr.Bool(false)
        case other => throw EvalError(s"string->number: not a string: ${Builtins.display(other)}")
      }
    case "number->string" =>
      Builtins.unary(name, args) {
        case Expr.Num(n) => Expr.Str(n.toString.toCharArray)
        case other       => throw EvalError(s"number->string: not a number: ${Builtins.display(other)}")
      }
    case "symbol->string" =>
      Builtins.unary(name, args) {
        case Expr.Sym(s) => Expr.Str(s.toCharArray)
        case other       => throw EvalError(s"symbol->string: not a symbol: ${Builtins.display(other)}")
      }
    case "string->symbol" =>
      Builtins.unary(name, args) {
        case Expr.Str(s) => Expr.Sym(new String(s))
        case other       => throw EvalError(s"string->symbol: not a string: ${Builtins.display(other)}")
      }
    case "string-ref" =>
      if args.length != 2 then throw EvalError("string-ref: need exactly 2 arguments")
      args match
        case List(Expr.Str(s), Expr.Num(idx)) => Expr.Chr(s(idx.toInt))
        case _                                => throw EvalError("string-ref: invalid arguments")
    case "char?" =>
      Builtins.unary(name, args)(e => Expr.Bool(e.isInstanceOf[Expr.Chr]))
    case _ => throw EvalError(s"unknown procedure: $name")

  def charCmp(name: String, args: List[Expr], op: (Char, Char) => Boolean): Expr =
    if args.length != 2 then throw EvalError(s"$name: need exactly 2 arguments")
    (args(0), args(1)) match
      case (Expr.Chr(a), Expr.Chr(b)) => Expr.Bool(op(a, b))
      case _                          => throw EvalError(s"$name: not characters")

  def strCmp(name: String, args: List[Expr], op: (String, String) => Boolean): Expr =
    if args.length != 2 then throw EvalError(s"$name: need exactly 2 arguments")
    (args(0), args(1)) match
      case (Expr.Str(a), Expr.Str(b)) => Expr.Bool(op(new String(a), new String(b)))
      case _                          => throw EvalError(s"$name: not strings")

  def strCiCmp(name: String, args: List[Expr], op: (String, String) => Boolean): Expr =
    if args.length != 2 then throw EvalError(s"$name: need exactly 2 arguments")
    (args(0), args(1)) match
      case (Expr.Str(a), Expr.Str(b)) =>
        Expr.Bool(op(new String(a).toLowerCase, new String(b).toLowerCase))
      case _ => throw EvalError(s"$name: not strings")
