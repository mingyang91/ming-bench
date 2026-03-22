package ming

import SchemeValue.*

object Builtins:

  def applyBuiltin(
    name: String,
    args: List[SchemeValue]
  ): (SchemeValue, String) =
    name match
      case "+" => (arithOp(args, 0L, _ + _), "")
      case "-" =>
        val r = args match
          case Nil              => throw new EvalError("-: need at least 1 argument")
          case IntVal(n) :: Nil => IntVal(-n)
          case _                => arithOp(args.tail, asInt(args.head), _ - _)
        (r, "")
      case "*" => (arithOp(args, 1L, _ * _), "")
      case "/" =>
        val r = args match
          case Nil => throw new EvalError("/: need at least 1 argument")
          case _ =>
            val result = args.tail.foldLeft(asInt(args.head)) { (acc, v) =>
              val d = asInt(v)
              if d == 0 then throw new EvalError("division by zero")
              else acc / d
            }
            IntVal(result)
        (r, "")
      case "<"  => (cmpOp(args, _ < _), "")
      case ">"  => (cmpOp(args, _ > _), "")
      case "="  => (cmpOp(args, _ == _), "")
      case "<=" => (cmpOp(args, _ <= _), "")
      case ">=" => (cmpOp(args, _ >= _), "")
      case "not" =>
        args match
          case v :: Nil => (BoolVal(!v.isTruthy), "")
          case _        => throw new EvalError("not: expects 1 argument")
      case "cons" =>
        args match
          case a :: b :: Nil => (PairVal(a, b), "")
          case _             => throw new EvalError("cons: expects 2 arguments")
      case "car" =>
        args match
          case PairVal(a, _) :: Nil      => (a, "")
          case ListVal(h :: _, _) :: Nil => (h, "")
          case _                         => throw new EvalError("car: expects a pair")
      case "cdr" =>
        args match
          case PairVal(_, d) :: Nil      => (d, "")
          case ListVal(_ :: t, _) :: Nil => (listToPairs(t), "")
          case _                         => throw new EvalError("cdr: expects a pair")
      case "null?" =>
        args match
          case v :: Nil => (BoolVal(isNull(v)), "")
          case _        => throw new EvalError("null?: expects 1 argument")
      case "list" => (listToPairs(args), "")
      case "length" =>
        args match
          case v :: Nil => (IntVal(pairLength(v)), "")
          case _        => throw new EvalError("length: expects 1 argument")
      case "pair?" =>
        args match
          case v :: Nil => (BoolVal(isPair(v)), "")
          case _        => throw new EvalError("pair?: expects 1 argument")
      case "number?" | "boolean?" | "string?" | "symbol?" =>
        (typeCheck(name, args), "")
      case "append" =>
        args match
          case a :: b :: Nil => (appendLists(a, b), "")
          case _             => throw new EvalError("append: expects 2 arguments")
      // L05: I/O
      case "display" =>
        args match
          case v :: Nil => (Void, v.displayOut)
          case _        => throw new EvalError("display: expects 1 argument")
      case "write" =>
        args match
          case v :: Nil => (Void, v.display)
          case _        => throw new EvalError("write: expects 1 argument")
      case "newline" =>
        args match
          case Nil => (Void, "\n")
          case _   => throw new EvalError("newline: expects 0 arguments")
      // L05: string operations
      case "string-append"  => (stringAppend(args), "")
      case "string-length"  => (stringLength(args), "")
      case "substring"      => (substringOp(args), "")
      case "string->number" => (stringToNumber(args), "")
      case "number->string" => (numberToString(args), "")
      case "symbol->string" => (symbolToString(args), "")
      case "string->symbol" => (stringToSymbol(args), "")
      case "string-ref"     => (stringRef(args), "")
      case "string-copy"    => (stringCopy(args), "")
      case "string-set!"    => (stringSet(args), "")
      case "char?"          => (typeCheck(name, args), "")
      // L13: equality
      case "eq?"    => (eqOp(args), "")
      case "eqv?"   => (eqvOp(args), "")
      case "equal?" => (equalOp(args), "")
      // L13: numeric utilities
      case "abs"       => (absOp(args), "")
      case "modulo"    => (moduloOp(args), "")
      case "remainder" => (remainderOp(args), "")
      case "quotient"  => (quotientOp(args), "")
      case "min"       => (minMaxOp(args, math.min), "")
      case "max"       => (minMaxOp(args, math.max), "")
      case "expt"      => (exptOp(args), "")
      case "zero?"     => (numPred1(args, _ == 0, "zero?"), "")
      case "positive?" => (numPred1(args, _ > 0, "positive?"), "")
      case "negative?" => (numPred1(args, _ < 0, "negative?"), "")
      case "odd?"      => (numPred1(args, n => math.abs(n % 2) == 1, "odd?"), "")
      case "even?"     => (numPred1(args, _ % 2 == 0, "even?"), "")
      // L13: list utilities
      case "list-ref"  => (listRefOp(args), "")
      case "list-tail" => (listTailOp(args), "")
      case "list?"     => (listPredOp(args), "")
      case "assoc"     => (assocOp(args), "")
      // L13: character utilities
      case "char-alphabetic?" => (charPred1(args, _.isLetter, "char-alphabetic?"), "")
      case "char-numeric?"    => (charPred1(args, _.isDigit, "char-numeric?"), "")
      case "char-upcase"      => (charTransform(args, _.toUpper, "char-upcase"), "")
      case "char-downcase"    => (charTransform(args, _.toLower, "char-downcase"), "")
      case "char=?"           => (charCmp(args, _ == _, "char=?"), "")
      case "char<?"           => (charCmp(args, _ < _, "char<?"), "")
      // L13: string utilities
      case "string=?"      => (strCmp(args, _ == _, "string=?"), "")
      case "string<?"      => (strCmp(args, _ < _, "string<?"), "")
      case "string-ci=?"   => (strCiEq(args), "")
      case "string-upcase"   => (strCase(args, _.toUpperCase, "string-upcase"), "")
      case "string-downcase" => (strCase(args, _.toLowerCase, "string-downcase"), "")
      case _                => throw new EvalError(s"unknown procedure: $name")

  private def typeCheck(name: String, args: List[SchemeValue]): SchemeValue =
    val result = (name, args) match
      case ("number?", (_: IntVal) :: Nil)           => true
      case ("boolean?", (_: BoolVal) :: Nil)         => true
      case ("string?", (_: StringVal) :: Nil)        => true
      case ("string?", (_: MutableStringVal) :: Nil) => true
      case ("symbol?", (_: SymbolVal) :: Nil)        => true
      case ("char?", (_: CharVal) :: Nil)            => true
      case (_, _ :: Nil)                             => false
      case _                                         => throw new EvalError(s"$name: expects 1 argument")
    BoolVal(result)

  def isNull(v: SchemeValue): Boolean = v match
    case ListVal(Nil, _) => true
    case _               => false

  def isPair(v: SchemeValue): Boolean = v match
    case _: PairVal         => true
    case ListVal(_ :: _, _) => true
    case _                  => false

  def listToPairs(elements: List[SchemeValue]): SchemeValue =
    elements.foldRight(ListVal(Nil): SchemeValue)((el, acc) => PairVal(el, acc))

  private def pairLength(v: SchemeValue): Long = v match
    case ListVal(Nil, _) => 0
    case ListVal(es, _)  => es.length.toLong
    case PairVal(_, cdr) => 1 + pairLength(cdr)
    case _               => throw new EvalError("length: not a proper list")

  private def appendLists(a: SchemeValue, b: SchemeValue): SchemeValue =
    a match
      case ListVal(Nil, _)    => b
      case PairVal(h, t)      => PairVal(h, appendLists(t, b))
      case ListVal(h :: t, _) => PairVal(h, appendLists(listToPairs(t), b))
      case _                  => throw new EvalError("append: not a proper list")

  private def asInt(v: SchemeValue): Long = v match
    case IntVal(n) => n
    case other     => throw new EvalError(s"expected number, got: ${other.display}")

  private def asString(v: SchemeValue): String = v match
    case StringVal(s)         => s
    case MutableStringVal(cs) => String(cs)
    case other                => throw new EvalError(s"expected string, got: ${other.display}")

  private def arithOp(
    args: List[SchemeValue],
    init: Long,
    op: (Long, Long) => Long
  ): SchemeValue =
    IntVal(args.foldLeft(init)((acc, v) => op(acc, asInt(v))))

  private def cmpOp(
    args: List[SchemeValue],
    op: (Long, Long) => Boolean
  ): SchemeValue =
    args match
      case a :: b :: Nil => BoolVal(op(asInt(a), asInt(b)))
      case _             => throw new EvalError("comparison expects 2 arguments")

  private def stringAppend(args: List[SchemeValue]): SchemeValue =
    StringVal(args.map(asString).mkString)

  private def stringLength(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil => IntVal(asString(v).length.toLong)
      case _        => throw new EvalError("string-length: expects 1 argument")

  private def substringOp(args: List[SchemeValue]): SchemeValue =
    args match
      case s :: start :: end :: Nil =>
        StringVal(asString(s).substring(asInt(start).toInt, asInt(end).toInt))
      case _ => throw new EvalError("substring: expects 3 arguments")

  private def stringToNumber(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil =>
        val s = asString(v)
        try IntVal(s.toLong)
        catch case _: NumberFormatException => BoolVal(false)
      case _ => throw new EvalError("string->number: expects 1 argument")

  private def numberToString(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil => StringVal(asInt(v).toString)
      case _        => throw new EvalError("number->string: expects 1 argument")

  private def symbolToString(args: List[SchemeValue]): SchemeValue =
    args match
      case SymbolVal(name, _) :: Nil => StringVal(name)
      case _ :: Nil                  => throw new EvalError("symbol->string: not a symbol")
      case _                         => throw new EvalError("symbol->string: expects 1 argument")

  private def stringToSymbol(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil => SymbolVal(asString(v))
      case _        => throw new EvalError("string->symbol: expects 1 argument")

  private def stringRef(args: List[SchemeValue]): SchemeValue =
    args match
      case s :: idx :: Nil =>
        val (str, len) = asStringContent(s)
        val i          = asInt(idx).toInt
        if i < 0 || i >= len then throw new EvalError("string-ref: index out of range")
        CharVal(str.charAt(i))
      case _ => throw new EvalError("string-ref: expects 2 arguments")

  private def stringCopy(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil =>
        v match
          case StringVal(s)         => MutableStringVal(s.toCharArray)
          case MutableStringVal(cs) => MutableStringVal(cs.clone())
          case other                => throw new EvalError(s"string-copy: expected string, got: ${other.display}")
      case _ => throw new EvalError("string-copy: expects 1 argument")

  private def stringSet(args: List[SchemeValue]): SchemeValue =
    args match
      case s :: idx :: c :: Nil =>
        s match
          case MutableStringVal(cs) =>
            val i = asInt(idx).toInt
            if i < 0 || i >= cs.length then throw new EvalError("string-set!: index out of range")
            val ch = c match
              case CharVal(ch) => ch
              case other       => throw new EvalError(s"string-set!: expected char, got: ${other.display}")
            cs(i) = ch
            Void
          case _ => throw new EvalError("string-set!: expected mutable string")
      case _ => throw new EvalError("string-set!: expects 3 arguments")

  private def asStringContent(v: SchemeValue): (String, Int) = v match
    case StringVal(s)         => (s, s.length)
    case MutableStringVal(cs) => (String(cs), cs.length)
    case other                => throw new EvalError(s"expected string, got: ${other.display}")

  // --- L13: equality ---

  private def eqOp(args: List[SchemeValue]): SchemeValue =
    args match
      case a :: b :: Nil => BoolVal(schemeEq(a, b))
      case _             => throw new EvalError("eq?: expects 2 arguments")

  private def eqvOp(args: List[SchemeValue]): SchemeValue =
    args match
      case a :: b :: Nil => BoolVal(schemeEqv(a, b))
      case _             => throw new EvalError("eqv?: expects 2 arguments")

  private def equalOp(args: List[SchemeValue]): SchemeValue =
    args match
      case a :: b :: Nil => BoolVal(schemeEqual(a, b))
      case _             => throw new EvalError("equal?: expects 2 arguments")

  private def schemeEq(a: SchemeValue, b: SchemeValue): Boolean =
    (a, b) match
      case (IntVal(x), IntVal(y))                   => x == y
      case (BoolVal(x), BoolVal(y))                 => x == y
      case (CharVal(x), CharVal(y))                 => x == y
      case (SymbolVal(x, _), SymbolVal(y, _))       => x == y
      case (StringVal(x), StringVal(y))              => x eq y
      case (ListVal(Nil, _), ListVal(Nil, _))        => true
      case (Void, Void)                              => true
      case _                                         => a eq b

  private def schemeEqv(a: SchemeValue, b: SchemeValue): Boolean =
    (a, b) match
      case (IntVal(x), IntVal(y))             => x == y
      case (BoolVal(x), BoolVal(y))           => x == y
      case (CharVal(x), CharVal(y))           => x == y
      case (SymbolVal(x, _), SymbolVal(y, _)) => x == y
      case (StringVal(x), StringVal(y))        => x == y
      case (ListVal(Nil, _), ListVal(Nil, _))  => true
      case (Void, Void)                        => true
      case _                                   => a eq b

  def schemeEqual(a: SchemeValue, b: SchemeValue): Boolean =
    (a, b) match
      case (IntVal(x), IntVal(y))             => x == y
      case (BoolVal(x), BoolVal(y))           => x == y
      case (CharVal(x), CharVal(y))           => x == y
      case (SymbolVal(x, _), SymbolVal(y, _)) => x == y
      case (StringVal(x), StringVal(y))        => x == y
      case (MutableStringVal(x), MutableStringVal(y)) => java.util.Arrays.equals(x, y)
      case (StringVal(x), MutableStringVal(y))         => x == String(y)
      case (MutableStringVal(x), StringVal(y))         => String(x) == y
      case (ListVal(Nil, _), ListVal(Nil, _))          => true
      case (Void, Void)                                => true
      case (PairVal(a1, d1), PairVal(a2, d2))          => schemeEqual(a1, a2) && schemeEqual(d1, d2)
      case (ListVal(es1, _), ListVal(es2, _)) =>
        es1.length == es2.length && es1.zip(es2).forall((x, y) => schemeEqual(x, y))
      case (PairVal(_, _), ListVal(_, _)) =>
        schemeEqual(a, normalizeToCanonical(a)) || equalAsPairs(a, b)
      case (ListVal(_, _), PairVal(_, _)) =>
        equalAsPairs(a, b)
      case _ => false

  private def equalAsPairs(a: SchemeValue, b: SchemeValue): Boolean =
    (a, b) match
      case (PairVal(a1, d1), PairVal(a2, d2)) => schemeEqual(a1, a2) && schemeEqual(d1, d2)
      case (ListVal(Nil, _), ListVal(Nil, _)) => true
      case (ListVal(h :: t, _), _)            => equalAsPairs(PairVal(h, listToPairs(t)), b)
      case (_, ListVal(h :: t, _))            => equalAsPairs(a, PairVal(h, listToPairs(t)))
      case _                                  => schemeEqual(a, b)

  private def normalizeToCanonical(v: SchemeValue): SchemeValue = v match
    case PairVal(car, cdr) =>
      cdr match
        case ListVal(Nil, _)   => PairVal(car, ListVal(Nil))
        case PairVal(_, _)     => PairVal(car, normalizeToCanonical(cdr))
        case ListVal(es, _)    => PairVal(car, normalizeToCanonical(listToPairs(es)))
        case other             => PairVal(car, other)
    case other => other

  // --- L13: numeric utilities ---

  private def absOp(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil => IntVal(math.abs(asInt(v)))
      case _        => throw new EvalError("abs: expects 1 argument")

  private def moduloOp(args: List[SchemeValue]): SchemeValue =
    args match
      case a :: b :: Nil =>
        val (x, y) = (asInt(a), asInt(b))
        if y == 0 then throw new EvalError("modulo: division by zero")
        IntVal(Math.floorMod(x, y))
      case _ => throw new EvalError("modulo: expects 2 arguments")

  private def remainderOp(args: List[SchemeValue]): SchemeValue =
    args match
      case a :: b :: Nil =>
        val (x, y) = (asInt(a), asInt(b))
        if y == 0 then throw new EvalError("remainder: division by zero")
        IntVal(x % y)
      case _ => throw new EvalError("remainder: expects 2 arguments")

  private def quotientOp(args: List[SchemeValue]): SchemeValue =
    args match
      case a :: b :: Nil =>
        val (x, y) = (asInt(a), asInt(b))
        if y == 0 then throw new EvalError("quotient: division by zero")
        IntVal((x.toDouble / y.toDouble).toLong)
      case _ => throw new EvalError("quotient: expects 2 arguments")

  private def minMaxOp(
    args: List[SchemeValue],
    op: (Long, Long) => Long
  ): SchemeValue =
    args match
      case Nil => throw new EvalError("min/max: need at least 1 argument")
      case _   => IntVal(args.map(asInt).reduce(op))

  private def exptOp(args: List[SchemeValue]): SchemeValue =
    args match
      case base :: exp :: Nil =>
        val (b, e) = (asInt(base), asInt(exp))
        if e < 0 then throw new EvalError("expt: negative exponent")
        IntVal(power(b, e))
      case _ => throw new EvalError("expt: expects 2 arguments")

  @scala.annotation.tailrec
  private def power(base: Long, exp: Long, acc: Long = 1L): Long =
    if exp == 0 then acc
    else power(base, exp - 1, acc * base)

  private def numPred1(
    args: List[SchemeValue],
    pred: Long => Boolean,
    name: String
  ): SchemeValue =
    args match
      case v :: Nil => BoolVal(pred(asInt(v)))
      case _        => throw new EvalError(s"$name: expects 1 argument")

  // --- L13: list utilities ---

  private def listRefOp(args: List[SchemeValue]): SchemeValue =
    args match
      case lst :: idx :: Nil =>
        val i = asInt(idx).toInt
        listRefHelper(lst, i, 0)
      case _ => throw new EvalError("list-ref: expects 2 arguments")

  @scala.annotation.tailrec
  private def listRefHelper(v: SchemeValue, target: Int, current: Int): SchemeValue =
    v match
      case PairVal(car, cdr) =>
        if current == target then car
        else listRefHelper(cdr, target, current + 1)
      case ListVal(es, _) if es.nonEmpty =>
        val localIdx = target - current
        if localIdx >= 0 && localIdx < es.length then es(localIdx)
        else throw new EvalError("list-ref: index out of range")
      case _ => throw new EvalError("list-ref: index out of range")

  private def listTailOp(args: List[SchemeValue]): SchemeValue =
    args match
      case lst :: idx :: Nil =>
        val i = asInt(idx).toInt
        listTailHelper(lst, i, 0)
      case _ => throw new EvalError("list-tail: expects 2 arguments")

  @scala.annotation.tailrec
  private def listTailHelper(v: SchemeValue, target: Int, current: Int): SchemeValue =
    if current == target then v
    else
      v match
        case PairVal(_, cdr)               => listTailHelper(cdr, target, current + 1)
        case ListVal(es, _) if es.nonEmpty =>
          val localIdx = target - current
          if localIdx <= es.length then listToPairs(es.drop(localIdx))
          else throw new EvalError("list-tail: index out of range")
        case _ => throw new EvalError("list-tail: index out of range")

  private def listPredOp(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil => BoolVal(isProperList(v))
      case _        => throw new EvalError("list?: expects 1 argument")

  @scala.annotation.tailrec
  private def isProperList(v: SchemeValue): Boolean = v match
    case ListVal(Nil, _)   => true
    case ListVal(_, _)     => true
    case PairVal(_, cdr)   => isProperList(cdr)
    case _                 => false

  private def assocOp(args: List[SchemeValue]): SchemeValue =
    args match
      case key :: lst :: Nil => assocSearch(key, lst)
      case _                 => throw new EvalError("assoc: expects 2 arguments")

  @scala.annotation.tailrec
  private def assocSearch(key: SchemeValue, lst: SchemeValue): SchemeValue =
    lst match
      case ListVal(Nil, _) => BoolVal(false)
      case PairVal(pair, rest) =>
        pair match
          case PairVal(k, _) if schemeEqual(k, key)              => pair
          case ListVal(k :: _, _) if schemeEqual(k, key)         => pair
          case _                                                  => assocSearch(key, rest)
      case ListVal(elems, _) if elems.nonEmpty =>
        assocSearch(key, listToPairs(elems))
      case _ => BoolVal(false)

  // --- L13: character utilities ---

  private def asChar(v: SchemeValue): Char = v match
    case CharVal(c) => c
    case other      => throw new EvalError(s"expected char, got: ${other.display}")

  private def charPred1(
    args: List[SchemeValue],
    pred: Char => Boolean,
    name: String
  ): SchemeValue =
    args match
      case v :: Nil => BoolVal(pred(asChar(v)))
      case _        => throw new EvalError(s"$name: expects 1 argument")

  private def charTransform(
    args: List[SchemeValue],
    f: Char => Char,
    name: String
  ): SchemeValue =
    args match
      case v :: Nil => CharVal(f(asChar(v)))
      case _        => throw new EvalError(s"$name: expects 1 argument")

  private def charCmp(
    args: List[SchemeValue],
    op: (Char, Char) => Boolean,
    name: String
  ): SchemeValue =
    args match
      case a :: b :: Nil => BoolVal(op(asChar(a), asChar(b)))
      case _             => throw new EvalError(s"$name: expects 2 arguments")

  // --- L13: string utilities ---

  private def strCmp(
    args: List[SchemeValue],
    op: (String, String) => Boolean,
    name: String
  ): SchemeValue =
    args match
      case a :: b :: Nil => BoolVal(op(asString(a), asString(b)))
      case _             => throw new EvalError(s"$name: expects 2 arguments")

  private def strCiEq(args: List[SchemeValue]): SchemeValue =
    args match
      case a :: b :: Nil =>
        BoolVal(asString(a).equalsIgnoreCase(asString(b)))
      case _ => throw new EvalError("string-ci=?: expects 2 arguments")

  private def strCase(
    args: List[SchemeValue],
    f: String => String,
    name: String
  ): SchemeValue =
    args match
      case v :: Nil => StringVal(f(asString(v)))
      case _        => throw new EvalError(s"$name: expects 1 argument")
