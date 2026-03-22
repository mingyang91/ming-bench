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
    "string-downcase"
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
    case "+"              => SchemeInt(args.map(asInt).sum)
    case "-"              => evalMinus(args)
    case "*"              => SchemeInt(args.map(asInt).product)
    case "/"              => evalDivide(args)
    case "<"              => compareOp(args, _ < _)
    case ">"              => compareOp(args, _ > _)
    case "="              => compareOp(args, _ == _)
    case "<="             => compareOp(args, _ <= _)
    case ">="             => compareOp(args, _ >= _)
    case "cons"           => evalCons(args)
    case "car"            => evalCar(args)
    case "cdr"            => evalCdr(args)
    case "null?"          => evalNullQ(args)
    case "list"           => SchemeList(args)
    case "append"         => evalAppend(args)
    case "length"         => evalLength(args)
    case "string?"        => typeCheck(args, _.isInstanceOf[SchemeString])
    case "number?"        => typeCheck(args, _.isInstanceOf[SchemeInt])
    case "boolean?"       => typeCheck(args, _.isInstanceOf[SchemeBool])
    case "pair?"          => evalPairQ(args)
    case "symbol?"        => typeCheck(args, _.isInstanceOf[SchemeSymbol])
    case "char?"          => typeCheck(args, _.isInstanceOf[SchemeChar])
    case "string-append"  => evalStringAppend(args)
    case "string-length"  => evalStringLength(args)
    case "substring"      => evalSubstring(args)
    case "string->number" => evalStringToNumber(args)
    case "number->string" => evalNumberToString(args)
    case "symbol->string" => evalSymbolToString(args)
    case "string->symbol" => evalStringToSymbol(args)
    case "string-ref"     => evalStringRef(args)
    case "string-copy"    => evalStringCopy(args)
    case "string-set!"       => evalStringSet(args)
    case "abs"               => evalAbs(args)
    case "modulo"            => evalModulo(args)
    case "remainder"         => evalRemainder(args)
    case "quotient"          => evalQuotient(args)
    case "min"               => evalMinMax(args, min = true)
    case "max"               => evalMinMax(args, min = false)
    case "expt"              => evalExpt(args)
    case "zero?"             => numPred(args, _ == 0)
    case "positive?"         => numPred(args, _ > 0)
    case "negative?"         => numPred(args, _ < 0)
    case "odd?"              => numPred(args, n => math.abs(n % 2) == 1)
    case "even?"             => numPred(args, _ % 2 == 0)
    case "list-ref"          => evalListRef(args)
    case "list-tail"         => evalListTail(args)
    case "list?"             => evalListPred(args)
    case "assoc"             => evalAssoc(args)
    case "eq?"               => evalEqQ(args)
    case "equal?"            => evalEqualQ(args)
    case "char-alphabetic?"  => charPred(args, _.isLetter)
    case "char-numeric?"     => charPred(args, _.isDigit)
    case "char-upcase"       => evalCharUpcase(args)
    case "char-downcase"     => evalCharDowncase(args)
    case "char=?"            => charCmp(args, _ == _)
    case "char<?"            => charCmp(args, _ < _)
    case "string=?"          => strCmp(args, _ == _)
    case "string<?"          => strCmp(args, (a, b) => a.compareTo(b) < 0)
    case "string-ci=?"       => evalStringCiEq(args)
    case "string-upcase"     => evalStringUpcase(args)
    case "string-downcase"   => evalStringDowncase(args)
    case _                   => throw new EvalError(s"unknown procedure: $op")

  private def evalDisplay(args: List[SchemeValue]): (SchemeValue, String) =
    if args.length != 1 then throw new EvalError("display: expected 1 argument")
    (SchemeVoid, args.head.displayOutput)

  private def evalWrite(args: List[SchemeValue]): (SchemeValue, String) =
    if args.length != 1 then throw new EvalError("write: expected 1 argument")
    (SchemeVoid, args.head.display)

  private def evalMinus(args: List[SchemeValue]): SchemeValue =
    if args.isEmpty then throw new EvalError("-: expected at least 1 argument")
    else if args.length == 1 then SchemeInt(-asInt(args.head))
    else SchemeInt(args.map(asInt).reduceLeft(_ - _))

  private def evalDivide(args: List[SchemeValue]): SchemeValue =
    if args.length < 2 then throw new EvalError("/: expected at least 2 arguments")
    else
      val nums = args.map(asInt)
      if nums.tail.contains(0L) then throw new EvalError("/: division by zero")
      else SchemeInt(nums.reduceLeft(_ / _))

  private def evalCons(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("cons: expected 2 arguments")
    args(1) match
      case SchemeList(es) => SchemeList(args.head :: es)
      case other          => SchemePair(args.head, other)

  private def evalCar(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("car: expected 1 argument")
    args.head match
      case SchemeList(h :: _) => h
      case SchemePair(h, _)  => h
      case other              => throw new EvalError(s"car: not a pair: ${other.display}")

  private def evalCdr(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("cdr: expected 1 argument")
    args.head match
      case SchemeList(_ :: t) => SchemeList(t)
      case SchemePair(_, t)  => t
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
      case _: SchemePair     => true
      case _                  => false))

  private def evalStringAppend(args: List[SchemeValue]): SchemeValue =
    val sb = args.map {
      case SchemeString(s) => s
      case other           => throw new EvalError(s"string-append: not a string: ${other.display}")
    }
    SchemeString(sb.mkString)

  private def evalStringLength(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("string-length: expected 1 argument")
    args.head match
      case SchemeString(s) => SchemeInt(s.length.toLong)
      case other           => throw new EvalError(s"string-length: not a string: ${other.display}")

  private def evalSubstring(args: List[SchemeValue]): SchemeValue =
    if args.length != 3 then throw new EvalError("substring: expected 3 arguments")
    (args.head, args(1), args(2)) match
      case (SchemeString(s), SchemeInt(start), SchemeInt(end)) =>
        SchemeString(s.substring(start.toInt, end.toInt))
      case _ => throw new EvalError("substring: invalid arguments")

  private def evalStringToNumber(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("string->number: expected 1 argument")
    args.head match
      case SchemeString(s) =>
        s.toLongOption match
          case Some(n) => SchemeInt(n)
          case None    => SchemeBool(false)
      case other => throw new EvalError(s"string->number: not a string: ${other.display}")

  private def evalNumberToString(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("number->string: expected 1 argument")
    args.head match
      case SchemeInt(n) => SchemeString(n.toString)
      case other        => throw new EvalError(s"number->string: not a number: ${other.display}")

  private def evalSymbolToString(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("symbol->string: expected 1 argument")
    args.head match
      case SchemeSymbol(name) => SchemeString(name)
      case other              => throw new EvalError(s"symbol->string: not a symbol: ${other.display}")

  private def evalStringToSymbol(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("string->symbol: expected 1 argument")
    args.head match
      case SchemeString(s) => SchemeSymbol(s)
      case other           => throw new EvalError(s"string->symbol: not a string: ${other.display}")

  private def evalStringRef(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("string-ref: expected 2 arguments")
    (args.head, args(1)) match
      case (SchemeString(s), SchemeInt(idx)) =>
        if idx < 0 || idx >= s.length then throw new EvalError("string-ref: index out of bounds")
        SchemeChar(s.charAt(idx.toInt))
      case _ => throw new EvalError("string-ref: invalid arguments")

  private def evalStringCopy(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("string-copy: expected 1 argument")
    args.head match
      case SchemeString(s)         => new SchemeMutableString(s.toCharArray)
      case ms: SchemeMutableString => new SchemeMutableString(ms.chars.clone())
      case other                   => throw new EvalError(s"string-copy: not a string: ${other.display}")

  private def evalStringSet(args: List[SchemeValue]): SchemeValue =
    if args.length != 3 then throw new EvalError("string-set!: expected 3 arguments")
    (args.head, args(1), args(2)) match
      case (ms: SchemeMutableString, SchemeInt(idx), SchemeChar(c)) =>
        if idx < 0 || idx >= ms.chars.length then throw new EvalError("string-set!: index out of bounds")
        ms.chars(idx.toInt) = c
        SchemeVoid
      case (_: SchemeString, _, _) =>
        throw new EvalError("string-set!: string is immutable")
      case _ => throw new EvalError("string-set!: invalid arguments")

  private def typeCheck(
    args: List[SchemeValue],
    pred: SchemeValue => Boolean
  ): SchemeValue =
    SchemeBool(args.length == 1 && pred(args.head))

  private def compareOp(
    args: List[SchemeValue],
    cmp: (Long, Long) => Boolean
  ): SchemeValue =
    if args.length != 2 then throw new EvalError("comparison: expected 2 arguments")
    SchemeBool(cmp(asInt(args.head), asInt(args(1))))

  private def evalAbs(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("abs: expected 1 argument")
    SchemeInt(math.abs(asInt(args.head)))

  private def evalModulo(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("modulo: expected 2 arguments")
    val (a, b) = (asInt(args.head), asInt(args(1)))
    if b == 0 then throw new EvalError("modulo: division by zero")
    SchemeInt(math.floorMod(a, b))

  private def evalRemainder(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("remainder: expected 2 arguments")
    val (a, b) = (asInt(args.head), asInt(args(1)))
    if b == 0 then throw new EvalError("remainder: division by zero")
    SchemeInt(a % b)

  private def evalQuotient(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("quotient: expected 2 arguments")
    val (a, b) = (asInt(args.head), asInt(args(1)))
    if b == 0 then throw new EvalError("quotient: division by zero")
    SchemeInt(a / b)

  private def evalMinMax(args: List[SchemeValue], min: Boolean): SchemeValue =
    if args.isEmpty then throw new EvalError(s"${if min then "min" else "max"}: expected at least 1 argument")
    val nums = args.map(asInt)
    SchemeInt(if min then nums.min else nums.max)

  private def evalExpt(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("expt: expected 2 arguments")
    val (base, exp) = (asInt(args.head), asInt(args(1)))
    SchemeInt(longPow(base, exp))

  @scala.annotation.tailrec
  private def longPow(base: Long, exp: Long, acc: Long = 1L): Long =
    if exp <= 0 then acc
    else longPow(base, exp - 1, acc * base)

  private def numPred(args: List[SchemeValue], pred: Long => Boolean): SchemeValue =
    if args.length != 1 then throw new EvalError("numeric predicate: expected 1 argument")
    SchemeBool(pred(asInt(args.head)))

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
        alist.collectFirst {
          case pair @ SchemeList(k :: _) if schemeEqual(k, key) => pair
        }.getOrElse(SchemeBool(false))
      case other => throw new EvalError(s"assoc: not a list: ${other.display}")

  private def evalEqQ(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("eq?: expected 2 arguments")
    SchemeBool(schemeEq(args.head, args(1)))

  private def schemeEq(a: SchemeValue, b: SchemeValue): Boolean = (a, b) match
    case (SchemeInt(x), SchemeInt(y))       => x == y
    case (SchemeBool(x), SchemeBool(y))     => x == y
    case (SchemeSymbol(x), SchemeSymbol(y)) => x == y
    case (SchemeChar(x), SchemeChar(y))     => x == y
    case (SchemeList(Nil), SchemeList(Nil))  => true
    case _                                   => a eq b

  private def evalEqualQ(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("equal?: expected 2 arguments")
    SchemeBool(schemeEqual(args.head, args(1)))

  private def schemeEqual(a: SchemeValue, b: SchemeValue): Boolean = (a, b) match
    case (SchemeInt(x), SchemeInt(y))         => x == y
    case (SchemeBool(x), SchemeBool(y))       => x == y
    case (SchemeSymbol(x), SchemeSymbol(y))   => x == y
    case (SchemeChar(x), SchemeChar(y))       => x == y
    case (SchemeString(x), SchemeString(y))   => x == y
    case (SchemeList(xs), SchemeList(ys))     => xs.length == ys.length && xs.zip(ys).forall(schemeEqual(_, _))
    case (SchemePair(a1, d1), SchemePair(a2, d2)) => schemeEqual(a1, a2) && schemeEqual(d1, d2)
    case _                                     => a eq b

  private def charPred(args: List[SchemeValue], pred: Char => Boolean): SchemeValue =
    if args.length != 1 then throw new EvalError("char predicate: expected 1 argument")
    args.head match
      case SchemeChar(c) => SchemeBool(pred(c))
      case other         => throw new EvalError(s"char predicate: not a char: ${other.display}")

  private def evalCharUpcase(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("char-upcase: expected 1 argument")
    args.head match
      case SchemeChar(c) => SchemeChar(c.toUpper)
      case other         => throw new EvalError(s"char-upcase: not a char: ${other.display}")

  private def evalCharDowncase(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("char-downcase: expected 1 argument")
    args.head match
      case SchemeChar(c) => SchemeChar(c.toLower)
      case other         => throw new EvalError(s"char-downcase: not a char: ${other.display}")

  private def charCmp(args: List[SchemeValue], cmp: (Char, Char) => Boolean): SchemeValue =
    if args.length != 2 then throw new EvalError("char comparison: expected 2 arguments")
    (args.head, args(1)) match
      case (SchemeChar(a), SchemeChar(b)) => SchemeBool(cmp(a, b))
      case _ => throw new EvalError("char comparison: not chars")

  private def strCmp(args: List[SchemeValue], cmp: (String, String) => Boolean): SchemeValue =
    if args.length != 2 then throw new EvalError("string comparison: expected 2 arguments")
    (args.head, args(1)) match
      case (SchemeString(a), SchemeString(b)) => SchemeBool(cmp(a, b))
      case _ => throw new EvalError("string comparison: not strings")

  private def evalStringCiEq(args: List[SchemeValue]): SchemeValue =
    if args.length != 2 then throw new EvalError("string-ci=?: expected 2 arguments")
    (args.head, args(1)) match
      case (SchemeString(a), SchemeString(b)) => SchemeBool(a.equalsIgnoreCase(b))
      case _ => throw new EvalError("string-ci=?: not strings")

  private def evalStringUpcase(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("string-upcase: expected 1 argument")
    args.head match
      case SchemeString(s) => SchemeString(s.toUpperCase)
      case other           => throw new EvalError(s"string-upcase: not a string: ${other.display}")

  private def evalStringDowncase(args: List[SchemeValue]): SchemeValue =
    if args.length != 1 then throw new EvalError("string-downcase: expected 1 argument")
    args.head match
      case SchemeString(s) => SchemeString(s.toLowerCase)
      case other           => throw new EvalError(s"string-downcase: not a string: ${other.display}")

  def asInt(v: SchemeValue): Long = v match
    case SchemeInt(n) => n
    case other =>
      throw new EvalError(s"expected integer, got: ${other.display}")
