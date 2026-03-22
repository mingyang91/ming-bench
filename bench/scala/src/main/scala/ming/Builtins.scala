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
    "not",
    "eqv?",
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
    "for-each",
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
    "set-car!",
    "set-cdr!",
    "reverse",
    "error",
    "vector",
    "make-vector",
    "vector-ref",
    "vector-set!",
    "vector-length",
    "vector?",
    "vector->list",
    "list->vector",
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
    "integer->char",
    "member",
    "assv",
    "integer?",
    "gcd",
    "lcm",
    "truncate",
    "round",
    "make-string",
    "string",
    "string>?",
    "string<=?",
    "string>=?",
    "procedure?",
    "dynamic-wind",
    "raise",
    "with-exception-handler"
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
    case "cons"             => CollectionBuiltins.evalCons(args)
    case "car"              => CollectionBuiltins.evalCar(args)
    case "cdr"              => CollectionBuiltins.evalCdr(args)
    case "null?"            => CollectionBuiltins.evalNullQ(args)
    case "list"             => SchemeList(args)
    case "append"           => CollectionBuiltins.evalAppend(args)
    case "length"           => CollectionBuiltins.evalLength(args)
    case "string?"          => typeCheck(args, isSchemeString)
    case "number?"          => typeCheck(args, _.isInstanceOf[SchemeInt])
    case "boolean?"         => typeCheck(args, _.isInstanceOf[SchemeBool])
    case "pair?"            => CollectionBuiltins.evalPairQ(args)
    case "symbol?"          => typeCheck(args, _.isInstanceOf[SchemeSymbol])
    case "char?"            => typeCheck(args, _.isInstanceOf[SchemeChar])
    case "not"              => CollectionBuiltins.evalNot(args)
    case "eqv?"             => CollectionBuiltins.evalEqQ(args)
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
    case "list-ref"         => CollectionBuiltins.evalListRef(args)
    case "list-tail"        => CollectionBuiltins.evalListTail(args)
    case "list?"            => CollectionBuiltins.evalListPred(args)
    case "assoc"            => CollectionBuiltins.evalAssoc(args)
    case "eq?"              => CollectionBuiltins.evalEqQ(args)
    case "equal?"           => CollectionBuiltins.evalEqualQ(args)
    case "set-car!"         => CollectionBuiltins.evalSetCar(args)
    case "set-cdr!"         => CollectionBuiltins.evalSetCdr(args)
    case "reverse"          => CollectionBuiltins.evalReverse(args)
    case "error"            => CollectionBuiltins.evalError(args)
    case "vector"           => new SchemeVector(args.toArray)
    case "make-vector"      => CollectionBuiltins.evalMakeVector(args)
    case "vector-ref"       => CollectionBuiltins.evalVectorRef(args)
    case "vector-set!"      => CollectionBuiltins.evalVectorSet(args)
    case "vector-length"    => CollectionBuiltins.evalVectorLength(args)
    case "vector?"          => typeCheck(args, _.isInstanceOf[SchemeVector])
    case "vector->list"     => CollectionBuiltins.evalVectorToList(args)
    case "list->vector"     => CollectionBuiltins.evalListToVector(args)
    case "member"           => CollectionBuiltins.evalMember(args)
    case "assv"             => CollectionBuiltins.evalAssv(args)
    case "integer?"         => typeCheck(args, _.isInstanceOf[SchemeInt])
    case "gcd"              => NumericBuiltins.evalGcd(args)
    case "lcm"              => NumericBuiltins.evalLcm(args)
    case "truncate"         => CollectionBuiltins.evalTruncate(args)
    case "round"            => CollectionBuiltins.evalTruncate(args)
    case "make-string"      => StringCharBuiltins.evalMakeString(args)
    case "string"           => StringCharBuiltins.evalString(args)
    case "string>?"         => StringCharBuiltins.strCmp(args, (a, b) => a.compareTo(b) > 0)
    case "string<=?"        => StringCharBuiltins.strCmp(args, (a, b) => a.compareTo(b) <= 0)
    case "string>=?"        => StringCharBuiltins.strCmp(args, (a, b) => a.compareTo(b) >= 0)
    case "procedure?"       => CollectionBuiltins.evalProcedureQ(args)
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

  private def typeCheck(
    args: List[SchemeValue],
    pred: SchemeValue => Boolean
  ): SchemeValue =
    SchemeBool(args.length == 1 && pred(args.head))

  def asList(v: SchemeValue): List[SchemeValue] =
    @scala.annotation.tailrec
    def loop(v: SchemeValue, acc: List[SchemeValue]): List[SchemeValue] = v match
      case SchemeList(es) => acc.reverse ++ es
      case p: SchemePair  => loop(p.cdr, p.car :: acc)
      case _              => throw new EvalError(s"not a proper list: ${v.display}")
    loop(v, Nil)

  def schemeEqv(a: SchemeValue, b: SchemeValue): Boolean =
    CollectionBuiltins.schemeEqv(a, b)

  private def isSchemeString(v: SchemeValue): Boolean = v match
    case _: SchemeString        => true
    case _: SchemeMutableString => true
    case _                      => false

  def asInt(v: SchemeValue): Long = v match
    case SchemeInt(n) => n
    case other =>
      throw new EvalError(s"expected integer, got: ${other.display}")
