package ming

import SchemeValue.*

object BuiltinsL13:

  def applyL13(
    name: String,
    args: List[SchemeValue]
  ): (SchemeValue, String) =
    name match
      // equality
      case "eq?"    => (BuiltinsEquality.eqOp(args), "")
      case "eqv?"   => (BuiltinsEquality.eqvOp(args), "")
      case "equal?" => (BuiltinsEquality.equalOp(args), "")
      // numeric utilities
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
      // list utilities
      case "list-ref"  => (listRefOp(args), "")
      case "list-tail" => (listTailOp(args), "")
      case "list?"     => (listPredOp(args), "")
      case "assoc"     => (assocOp(args), "")
      // character utilities
      case "char-alphabetic?" => (charPred1(args, _.isLetter, "char-alphabetic?"), "")
      case "char-numeric?"    => (charPred1(args, _.isDigit, "char-numeric?"), "")
      case "char-upcase"      => (charTransform(args, _.toUpper, "char-upcase"), "")
      case "char-downcase"    => (charTransform(args, _.toLower, "char-downcase"), "")
      case "char=?"           => (charCmp(args, _ == _, "char=?"), "")
      case "char<?"           => (charCmp(args, _ < _, "char<?"), "")
      // string utilities
      case "string=?"        => (strCmp(args, _ == _, "string=?"), "")
      case "string<?"        => (strCmp(args, _ < _, "string<?"), "")
      case "string-ci=?"     => (strCiEq(args), "")
      case "string-upcase"   => (strCase(args, _.toUpperCase, "string-upcase"), "")
      case "string-downcase" => (strCase(args, _.toLowerCase, "string-downcase"), "")
      // L14: string/list conversion and char/integer conversion
      case "string->list"  => (stringToList(args), "")
      case "list->string"  => (listToString(args), "")
      case "char->integer" => (charToInteger(args), "")
      case "integer->char" => (integerToChar(args), "")
      case _ => BuiltinsL15.applyL15(name, args)

  // --- numeric utilities ---

  private def asInt(v: SchemeValue): Long = v match
    case IntVal(n) => n
    case other     => throw new EvalError(s"expected number, got: ${other.display}")

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

  // --- list utilities ---

  private def listRefOp(args: List[SchemeValue]): SchemeValue =
    args match
      case lst :: idx :: Nil =>
        val i = asInt(idx).toInt
        listRefHelper(lst, i, 0)
      case _ => throw new EvalError("list-ref: expects 2 arguments")

  @scala.annotation.tailrec
  private def listRefHelper(v: SchemeValue, target: Int, current: Int): SchemeValue =
    v match
      case AnyPair(car, cdr) =>
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
        case AnyPair(_, cdr) => listTailHelper(cdr, target, current + 1)
        case ListVal(es, _) if es.nonEmpty =>
          val localIdx = target - current
          if localIdx <= es.length then Builtins.listToPairs(es.drop(localIdx))
          else throw new EvalError("list-tail: index out of range")
        case _ => throw new EvalError("list-tail: index out of range")

  private def listPredOp(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil => BoolVal(isProperList(v))
      case _        => throw new EvalError("list?: expects 1 argument")

  @scala.annotation.tailrec
  private def isProperList(v: SchemeValue): Boolean = v match
    case ListVal(Nil, _) => true
    case ListVal(_, _)   => true
    case AnyPair(_, cdr) => isProperList(cdr)
    case _               => false

  private def assocOp(args: List[SchemeValue]): SchemeValue =
    args match
      case key :: lst :: Nil => assocSearch(key, lst)
      case _                 => throw new EvalError("assoc: expects 2 arguments")

  @scala.annotation.tailrec
  private def assocSearch(key: SchemeValue, lst: SchemeValue): SchemeValue =
    lst match
      case ListVal(Nil, _) => BoolVal(false)
      case AnyPair(pair, rest) =>
        pair match
          case AnyPair(k, _) if BuiltinsEquality.schemeEqual(k, key)      => pair
          case ListVal(k :: _, _) if BuiltinsEquality.schemeEqual(k, key) => pair
          case _                                                          => assocSearch(key, rest)
      case ListVal(elems, _) if elems.nonEmpty =>
        assocSearch(key, Builtins.listToPairs(elems))
      case _ => BoolVal(false)

  // --- character utilities ---

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

  // --- string utilities ---

  private def asString(v: SchemeValue): String = v match
    case StringVal(s)         => s
    case MutableStringVal(cs) => String(cs)
    case other                => throw new EvalError(s"expected string, got: ${other.display}")

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

  // --- L14: string/list and char/integer conversion ---

  private def stringToList(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil =>
        val chars = asString(v).toList.map(CharVal(_))
        Builtins.listToPairs(chars)
      case _ => throw new EvalError("string->list: expects 1 argument")

  private def listToString(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil =>
        val chars = collectChars(v)
        StringVal(chars.mkString)
      case _ => throw new EvalError("list->string: expects 1 argument")

  @scala.annotation.tailrec
  private def collectChars(v: SchemeValue, acc: List[Char] = Nil): List[Char] =
    v match
      case ListVal(Nil, _)           => acc.reverse
      case AnyPair(CharVal(c), rest) => collectChars(rest, c :: acc)
      case AnyPair(other, _) =>
        throw new EvalError(s"list->string: expected char, got: ${other.display}")
      case ListVal(elems, _) =>
        collectChars(Builtins.listToPairs(elems), acc)
      case _ => throw new EvalError("list->string: not a proper list of characters")

  private def charToInteger(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil => IntVal(asChar(v).toLong)
      case _        => throw new EvalError("char->integer: expects 1 argument")

  private def integerToChar(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil => CharVal(asInt(v).toChar)
      case _        => throw new EvalError("integer->char: expects 1 argument")
