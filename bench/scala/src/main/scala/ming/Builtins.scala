package ming

import SchemeValue.*

/** Built-in procedure implementations. */
object Builtins:

  val knownNames: Set[String] = Set(
    "+",
    "-",
    "*",
    "/",
    "<",
    ">",
    "=",
    "<=",
    ">=",
    "cons",
    "car",
    "cdr",
    "null?",
    "list",
    "append",
    "length",
    "string?",
    "number?",
    "boolean?",
    "pair?",
    "symbol?",
    "char?",
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
    "display",
    "write",
    "newline",
    "apply",
    "map",
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
    "string->list",
    "list->string",
    "char->integer",
    "integer->char"
  )

  def evalBuiltin(
    op: String,
    args: List[SchemeValue]
  ): (SchemeValue, String) = op match
    case "display" => evalDisplay(args)
    case "write"   => evalWrite(args)
    case "newline" => (SchemeVoid, "\n")
    case _         => (evalPure(op, args), "")

  private def evalPure(
    op: String,
    args: List[SchemeValue]
  ): SchemeValue = op match
    case "+"                => SchemeInt(args.map(asInt).sum)
    case "-"                => NumericBuiltins.evalMinus(args)
    case "*"                => SchemeInt(args.map(asInt).product)
    case "/"                => NumericBuiltins.evalDivide(args)
    case "<"                => NumericBuiltins.compareOp(args, _ < _)
    case ">"                => NumericBuiltins.compareOp(args, _ > _)
    case "="                => NumericBuiltins.compareOp(args, _ == _)
    case "<="               => NumericBuiltins.compareOp(args, _ <= _)
    case ">="               => NumericBuiltins.compareOp(args, _ >= _)
    case "cons"             => evalCons(args)
    case "car"              => evalCar(args)
    case "cdr"              => evalCdr(args)
    case "null?"            => evalNullQ(args)
    case "list"             => SchemeList(args)
    case "append"           => evalAppend(args)
    case "length"           => evalLength(args)
    case "string?"          => typeCheck(args, _.isInstanceOf[SchemeString])
    case "number?"          => typeCheck(args, _.isInstanceOf[SchemeInt])
    case "boolean?"         => typeCheck(args, _.isInstanceOf[SchemeBool])
    case "pair?"            => evalPairQ(args)
    case "symbol?"          => typeCheck(args, _.isInstanceOf[SchemeSymbol])
    case "char?"            => typeCheck(args, _.isInstanceOf[SchemeChar])
    case "string-append"    => StringCharBuiltins.evalStringAppend(args)
    case "string-length"    => StringCharBuiltins.evalStringLength(args)
    case "substring"        => StringCharBuiltins.evalSubstring(args)
    case "string->number"   => StringCharBuiltins.evalStringToNumber(args)
    case "number->string"   => StringCharBuiltins.evalNumberToString(args)
    case "symbol->string"   => StringCharBuiltins.evalSymbolToString(args)
    case "string->symbol"   => StringCharBuiltins.evalStringToSymbol(args)
    case "string-ref"       => StringCharBuiltins.evalStringRef(args)
    case "string-copy"      => StringCharBuiltins.evalStringCopy(args)
    case "string-set!"      => StringCharBuiltins.evalStringSet(args)
    case "abs"              => NumericBuiltins.evalAbs(args)
    case "modulo"           => NumericBuiltins.evalModulo(args)
    case "remainder"        => NumericBuiltins.evalRemainder(args)
    case "quotient"         => NumericBuiltins.evalQuotient(args)
    case "min"              => NumericBuiltins.evalMinMax(args, min = true)
    case "max"              => NumericBuiltins.evalMinMax(args, min = false)
    case "expt"             => NumericBuiltins.evalExpt(args)
    case "zero?"            => NumericBuiltins.numPred(args, _ == 0)
    case "positive?"        => NumericBuiltins.numPred(args, _ > 0)
    case "negative?"        => NumericBuiltins.numPred(args, _ < 0)
    case "odd?"             => NumericBuiltins.numPred(args, n => math.abs(n % 2) == 1)
    case "even?"            => NumericBuiltins.numPred(args, _ % 2 == 0)
    case "list-ref"         => evalListRef(args)
    case "list-tail"        => evalListTail(args)
    case "list?"            => evalListPred(args)
    case "assoc"            => evalAssoc(args)
    case "eq?"              => evalEqQ(args)
    case "equal?"           => evalEqualQ(args)
    case "char-alphabetic?" => StringCharBuiltins.charPred(args, _.isLetter)
    case "char-numeric?"    => StringCharBuiltins.charPred(args, _.isDigit)
    case "char-upcase"      => StringCharBuiltins.evalCharUpcase(args)
    case "char-downcase"    => StringCharBuiltins.evalCharDowncase(args)
    case "char=?"           => StringCharBuiltins.charCmp(args, _ == _)
    case "char<?"           => StringCharBuiltins.charCmp(args, _ < _)
    case "string=?"         => StringCharBuiltins.strCmp(args, _ == _)
    case "string<?"         => StringCharBuiltins.strCmp(args, (a, b) => a.compareTo(b) < 0)
    case "string-ci=?"      => StringCharBuiltins.evalStringCiEq(args)
    case "string-upcase"    => StringCharBuiltins.evalStringUpcase(args)
    case "string-downcase"  => StringCharBuiltins.evalStringDowncase(args)
    case "string->list"     => StringCharBuiltins.evalStringToList(args)
    case "list->string"     => StringCharBuiltins.evalListToString(args)
    case "char->integer"    => StringCharBuiltins.evalCharToInteger(args)
    case "integer->char"    => StringCharBuiltins.evalIntegerToChar(args)
    case _                  => throw new EvalError(s"unknown procedure: $op")

  private def evalDisplay(args: List[SchemeValue]): (SchemeValue, String) =
    if args.length != 1 then throw new EvalError("display: expected 1 argument")
    (SchemeVoid, args.head.displayOutput)

  private def evalWrite(args: List[SchemeValue]): (SchemeValue, String) =
    if args.length != 1 then throw new EvalError("write: expected 1 argument")
    (SchemeVoid, args.head.display)

  private def evalCons(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("cons: expected 2 arguments")
    args(1) match
      case SchemeList(es) => SchemeList(args.head :: es)
      case other          => SchemePair(args.head, other)

  private def evalCar(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("car: expected 1 argument")
    args.head match
      case SchemeList(h :: _) => h
      case SchemePair(h, _)   => h
      case other              => throw new EvalError(s"car: not a pair: ${other.display}")

  private def evalCdr(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("cdr: expected 1 argument")
    args.head match
      case SchemeList(_ :: t) => SchemeList(t)
      case SchemePair(_, t)   => t
      case other              => throw new EvalError(s"cdr: not a pair: ${other.display}")

  private def evalNullQ(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("null?: expected 1 argument")
    SchemeBool(args.head == SchemeList(Nil))

  private def evalAppend(args: List[SchemeValue]): SchemeValue =
    val combined = args.foldLeft(List.empty[SchemeValue]) { (acc, arg) =>
      arg match
        case SchemeList(es) => acc ++ es
        case other =>
          throw new EvalError(s"append: not a list: ${other.display}")
    }
    SchemeList(combined)

  private def evalLength(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("length: expected 1 argument")
    args.head match
      case SchemeList(es) => SchemeInt(es.length.toLong)
      case other =>
        throw new EvalError(s"length: not a list: ${other.display}")

  private def evalPairQ(args: List[SchemeValue]): SchemeValue =
    SchemeBool(args.length == 1 && (args.head match
      case SchemeList(_ :: _) => true
      case _: SchemePair      => true
      case _                  => false))

  private def typeCheck(
    args: List[SchemeValue],
    pred: SchemeValue => Boolean
  ): SchemeValue =
    SchemeBool(args.length == 1 && pred(args.head))

  private def evalListRef(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("list-ref: expected 2 arguments")
    (args.head, args(1)) match
      case (SchemeList(es), SchemeInt(idx)) =>
        if idx < 0 || idx >= es.length then throw new EvalError("list-ref: index out of bounds")
        es(idx.toInt)
      case _ => throw new EvalError("list-ref: invalid arguments")

  private def evalListTail(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("list-tail: expected 2 arguments")
    (args.head, args(1)) match
      case (SchemeList(es), SchemeInt(idx)) =>
        if idx < 0 || idx > es.length then throw new EvalError("list-tail: index out of bounds")
        SchemeList(es.drop(idx.toInt))
      case _ => throw new EvalError("list-tail: invalid arguments")

  private def evalListPred(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("list?: expected 1 argument")
    SchemeBool(args.head match
      case SchemeList(_) => true
      case _             => false)

  private def evalAssoc(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("assoc: expected 2 arguments")
    val key = args.head
    args(1) match
      case SchemeList(alist) =>
        alist
          .collectFirst {
            case pair @ SchemeList(k :: _) if schemeEqual(k, key) => pair
          }
          .getOrElse(SchemeBool(false))
      case other => throw new EvalError(s"assoc: not a list: ${other.display}")

  private def evalEqQ(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("eq?: expected 2 arguments")
    SchemeBool(schemeEq(args.head, args(1)))

  private def schemeEq(a: SchemeValue, b: SchemeValue): Boolean = (a, b) match
    case (SchemeInt(x), SchemeInt(y))       => x == y
    case (SchemeBool(x), SchemeBool(y))     => x == y
    case (SchemeSymbol(x), SchemeSymbol(y)) => x == y
    case (SchemeChar(x), SchemeChar(y))     => x == y
    case (SchemeList(Nil), SchemeList(Nil)) => true
    case _                                  => a eq b

  private def evalEqualQ(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("equal?: expected 2 arguments")
    SchemeBool(schemeEqual(args.head, args(1)))

  private def schemeEqual(a: SchemeValue, b: SchemeValue): Boolean = (a, b) match
    case (SchemeInt(x), SchemeInt(y))             => x == y
    case (SchemeBool(x), SchemeBool(y))           => x == y
    case (SchemeSymbol(x), SchemeSymbol(y))       => x == y
    case (SchemeChar(x), SchemeChar(y))           => x == y
    case (SchemeString(x), SchemeString(y))       => x == y
    case (SchemeList(xs), SchemeList(ys))         => xs.length == ys.length && xs.zip(ys).forall(schemeEqual(_, _))
    case (SchemePair(a1, d1), SchemePair(a2, d2)) => schemeEqual(a1, a2) && schemeEqual(d1, d2)
    case _                                        => a eq b

  def asInt(v: SchemeValue): Long = v match
    case SchemeInt(n) => n
    case other =>
      throw new EvalError(s"expected integer, got: ${other.display}")
