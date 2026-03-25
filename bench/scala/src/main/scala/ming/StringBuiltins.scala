package ming

private[ming] object StringBuiltins:

  def applyStringBuiltin(name: String, args: List[Expr]): Expr = name match
    case "string-append" =>
      val strs = args.map {
        case Expr.Str(s, _) => new String(s)
        case other          => throw EvalError(s"string-append: not a string: ${Builtins.display(other)}")
      }
      Expr.Str(strs.mkString.toCharArray)
    case "string-length" =>
      Builtins.unary(name, args) {
        case Expr.Str(s, _) => Expr.Num(s.length.toLong)
        case other          => throw EvalError(s"string-length: not a string: ${Builtins.display(other)}")
      }
    case "string-set!" =>
      if args.length != 3 then throw EvalError("string-set!: need exactly 3 arguments")
      args match
        case List(Expr.Str(s, mutable), Expr.Num(idx), Expr.Chr(c)) =>
          if !mutable then throw EvalError("string-set!: strings are immutable")
          s(idx.toInt) = c
          Expr.Bool(false)
        case _ => throw EvalError("string-set!: invalid arguments")
    case "string-copy" =>
      Builtins.unary(name, args) {
        case Expr.Str(s, _) => Expr.Str(s.clone())
        case other          => throw EvalError(s"string-copy: not a string: ${Builtins.display(other)}")
      }
    case "substring" =>
      if args.length != 3 then throw EvalError("substring: need exactly 3 arguments")
      args match
        case List(Expr.Str(s, _), Expr.Num(start), Expr.Num(end)) =>
          Expr.Str(new String(s).substring(start.toInt, end.toInt).toCharArray)
        case _ => throw EvalError("substring: invalid arguments")
    case "string-ref" =>
      if args.length != 2 then throw EvalError("string-ref: need exactly 2 arguments")
      args match
        case List(Expr.Str(s, _), Expr.Num(idx)) => Expr.Chr(s(idx.toInt))
        case _                                   => throw EvalError("string-ref: invalid arguments")
    case "char?" =>
      Builtins.unary(name, args)(e => Expr.Bool(e.isInstanceOf[Expr.Chr]))
    case "string=?"    => strCmp(name, args, _ == _)
    case "string<?"    => strCmp(name, args, _ < _)
    case "string<=?"   => strCmp(name, args, _ <= _)
    case "string>?"    => strCmp(name, args, _ > _)
    case "string>=?"   => strCmp(name, args, _ >= _)
    case "string-ci=?" => strCiCmp(name, args, _ == _)
    case "string-upcase" =>
      Builtins.unary(name, args) {
        case Expr.Str(s, _) => Expr.Str(new String(s).toUpperCase.toCharArray)
        case _              => throw EvalError("string-upcase: not a string")
      }
    case "string-downcase" =>
      Builtins.unary(name, args) {
        case Expr.Str(s, _) => Expr.Str(new String(s).toLowerCase.toCharArray)
        case _              => throw EvalError("string-downcase: not a string")
      }
    case "string->list" | "list->string" | "string" | "make-string" =>
      applyStringListOps(name, args)
    case "string->number" | "number->string" | "symbol->string" | "string->symbol" =>
      applyStringConversion(name, args)
    case "char->integer" | "integer->char" => applyCharConversion(name, args)
    case _                                 => throw EvalError(s"unknown procedure: $name")

  private def applyStringListOps(name: String, args: List[Expr]): Expr = name match
    case "string->list" =>
      Builtins.unary(name, args) {
        case Expr.Str(s, _) => PairOps.makeList(s.map(c => Expr.Chr(c)).toList)
        case other          => throw EvalError(s"string->list: not a string: ${Builtins.display(other)}")
      }
    case "list->string" =>
      Builtins.unary(name, args) { e =>
        val elems = PairOps.toScalaList(e)
        val chars = elems.map {
          case Expr.Chr(c) => c
          case other       => throw EvalError(s"list->string: not a character: ${Builtins.display(other)}")
        }
        Expr.Str(chars.toArray)
      }
    case "string" =>
      val chars = args.map {
        case Expr.Chr(c) => c
        case other       => throw EvalError(s"string: not a character: ${Builtins.display(other)}")
      }
      Expr.Str(chars.toArray)
    case "make-string" =>
      if args.length < 1 || args.length > 2 then throw EvalError("make-string: need 1 or 2 arguments")
      val len = Builtins.asNum(args.head).toInt
      val ch =
        if args.length == 2 then
          args(1) match
            case Expr.Chr(c) => c
            case _           => throw EvalError("make-string: second argument must be a character")
        else '\u0000'
      Expr.Str(Array.fill(len)(ch))
    case _ => throw EvalError(s"unknown procedure: $name")

  private def applyStringConversion(name: String, args: List[Expr]): Expr = name match
    case "string->number" =>
      Builtins.unary(name, args) {
        case Expr.Str(s, _) =>
          val str = new String(s)
          str.toLongOption match
            case Some(n) => Expr.Num(n)
            case None =>
              str.toDoubleOption match
                case Some(d) => Expr.Real(d)
                case None    => Expr.Bool(false)
        case other => throw EvalError(s"string->number: not a string: ${Builtins.display(other)}")
      }
    case "number->string" =>
      Builtins.unary(name, args) { e =>
        if NumericUtils.isNumber(e) then Expr.Str(Builtins.display(e).toCharArray)
        else throw EvalError(s"number->string: not a number: ${Builtins.display(e)}")
      }
    case "symbol->string" =>
      Builtins.unary(name, args) {
        case Expr.Sym(s) => Expr.Str(s.toCharArray)
        case other       => throw EvalError(s"symbol->string: not a symbol: ${Builtins.display(other)}")
      }
    case "string->symbol" =>
      Builtins.unary(name, args) {
        case Expr.Str(s, _) => Expr.Sym(new String(s))
        case other          => throw EvalError(s"string->symbol: not a string: ${Builtins.display(other)}")
      }
    case _ => throw EvalError(s"unknown procedure: $name")

  def applyCharBuiltin(name: String, args: List[Expr]): Expr = name match
    case "char-alphabetic?" =>
      Builtins.unary(name, args) {
        case Expr.Chr(c) => Expr.Bool(c.isLetter); case _ => throw EvalError("char-alphabetic?: not a char")
      }
    case "char-numeric?" =>
      Builtins.unary(name, args) {
        case Expr.Chr(c) => Expr.Bool(c.isDigit); case _ => throw EvalError("char-numeric?: not a char")
      }
    case "char-upcase" =>
      Builtins.unary(name, args) {
        case Expr.Chr(c) => Expr.Chr(c.toUpper); case _ => throw EvalError("char-upcase: not a char")
      }
    case "char-downcase" =>
      Builtins.unary(name, args) {
        case Expr.Chr(c) => Expr.Chr(c.toLower); case _ => throw EvalError("char-downcase: not a char")
      }
    case "char=?"  => charCmp(name, args, _ == _)
    case "char<?"  => charCmp(name, args, _ < _)
    case "char<=?" => charCmp(name, args, _ <= _)
    case "char>?"  => charCmp(name, args, _ > _)
    case "char>=?" => charCmp(name, args, _ >= _)
    case _         => throw EvalError(s"unknown char procedure: $name")

  private def applyCharConversion(name: String, args: List[Expr]): Expr = name match
    case "char->integer" =>
      Builtins.unary(name, args) {
        case Expr.Chr(c) => Expr.Num(c.toLong)
        case other       => throw EvalError(s"char->integer: not a character: ${Builtins.display(other)}")
      }
    case "integer->char" =>
      Builtins.unary(name, args) {
        case Expr.Num(n) => Expr.Chr(n.toChar)
        case other       => throw EvalError(s"integer->char: not an integer: ${Builtins.display(other)}")
      }
    case _ => throw EvalError(s"unknown procedure: $name")

  def charCmp(name: String, args: List[Expr], op: (Char, Char) => Boolean): Expr =
    if args.length != 2 then throw EvalError(s"$name: need exactly 2 arguments")
    (args(0), args(1)) match
      case (Expr.Chr(a), Expr.Chr(b)) => Expr.Bool(op(a, b))
      case _                          => throw EvalError(s"$name: not characters")

  def strCmp(name: String, args: List[Expr], op: (String, String) => Boolean): Expr =
    if args.length != 2 then throw EvalError(s"$name: need exactly 2 arguments")
    (args(0), args(1)) match
      case (Expr.Str(a, _), Expr.Str(b, _)) => Expr.Bool(op(new String(a), new String(b)))
      case _                                => throw EvalError(s"$name: not strings")

  def strCiCmp(name: String, args: List[Expr], op: (String, String) => Boolean): Expr =
    if args.length != 2 then throw EvalError(s"$name: need exactly 2 arguments")
    (args(0), args(1)) match
      case (Expr.Str(a, _), Expr.Str(b, _)) =>
        Expr.Bool(op(new String(a).toLowerCase, new String(b).toLowerCase))
      case _ => throw EvalError(s"$name: not strings")
