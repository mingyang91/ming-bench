package ming

import SchemeValue.*

/** Extended builtin operations: strings, numerics, chars, list utilities. */
object BuiltinsExt:

  // --- String operations ---

  def stringAppendOp(args: List[SchemeValue]): SchemeValue =
    val sb = StringBuilder()
    args.foreach {
      case StringVal(s, _) => sb.append(s)
      case _               => throw new EvalError("string-append: expected string")
    }
    StringVal(sb.toString)

  def stringLengthOp(args: List[SchemeValue]): SchemeValue =
    args match
      case StringVal(s, _) :: Nil         => IntVal(s.length.toLong)
      case MutableStringVal(cs, _) :: Nil => IntVal(cs.length.toLong)
      case _                              => throw new EvalError("string-length: expected string")

  def substringOp(args: List[SchemeValue]): SchemeValue =
    args match
      case StringVal(s, _) :: IntVal(start, _) :: IntVal(end, _) :: Nil =>
        StringVal(s.substring(start.toInt, end.toInt))
      case _ => throw new EvalError("substring: expected (string start end)")

  def stringToNumberOp(args: List[SchemeValue]): SchemeValue =
    args match
      case StringVal(s, _) :: Nil =>
        try IntVal(s.toLong)
        catch case _: NumberFormatException => BoolVal(false)
      case _ => throw new EvalError("string->number: expected string")

  def numberToStringOp(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil => StringVal(n.toString)
      case _                   => throw new EvalError("number->string: expected number")

  def symbolToStringOp(args: List[SchemeValue]): SchemeValue =
    args match
      case SymbolVal(name, _) :: Nil => StringVal(name)
      case _                         => throw new EvalError("symbol->string: expected symbol")

  def stringToSymbolOp(args: List[SchemeValue]): SchemeValue =
    args match
      case StringVal(s, _) :: Nil => SymbolVal(s)
      case _                      => throw new EvalError("string->symbol: expected string")

  def stringRefOp(args: List[SchemeValue]): SchemeValue =
    args match
      case StringVal(s, _) :: IntVal(idx, _) :: Nil =>
        if idx < 0 || idx >= s.length then throw new EvalError("string-ref: index out of bounds")
        CharVal(s.charAt(idx.toInt))
      case MutableStringVal(cs, _) :: IntVal(idx, _) :: Nil =>
        if idx < 0 || idx >= cs.length then throw new EvalError("string-ref: index out of bounds")
        CharVal(cs(idx.toInt))
      case _ => throw new EvalError("string-ref: expected (string index)")

  def stringSetOp(args: List[SchemeValue]): SchemeValue =
    args match
      case MutableStringVal(cs, _) :: IntVal(idx, _) :: CharVal(c, _) :: Nil =>
        if idx < 0 || idx >= cs.length then throw new EvalError("string-set!: index out of bounds")
        cs(idx.toInt) = c
        Void
      case StringVal(_, _) :: _ :: _ :: Nil =>
        throw new EvalError("string-set!: string is immutable")
      case _ => throw new EvalError("string-set!: expected (mutable-string index char)")

  def stringCopyOp(args: List[SchemeValue]): SchemeValue =
    args match
      case StringVal(s, _) :: Nil         => MutableStringVal(s.toCharArray)
      case MutableStringVal(cs, _) :: Nil => MutableStringVal(cs.clone())
      case _                              => throw new EvalError("string-copy: expected string")

  // --- String/Char conversions (L14) ---

  def stringToListOp(args: List[SchemeValue]): SchemeValue =
    args match
      case StringVal(s, _) :: Nil =>
        ListVal(s.toList.map(c => CharVal(c)))
      case MutableStringVal(cs, _) :: Nil =>
        ListVal(cs.toList.map(c => CharVal(c)))
      case _ => throw new EvalError("string->list: expected string")

  def listToStringOp(args: List[SchemeValue]): SchemeValue =
    args match
      case ListVal(elems, _) :: Nil =>
        val chars = elems.map:
          case CharVal(c, _) => c
          case other         => throw new EvalError(s"list->string: expected char, got ${other.display}")
        StringVal(String(chars.toArray))
      case _ => throw new EvalError("list->string: expected list")

  def charToIntegerOp(args: List[SchemeValue]): SchemeValue =
    args match
      case CharVal(c, _) :: Nil => IntVal(c.toLong)
      case _                    => throw new EvalError("char->integer: expected char")

  def integerToCharOp(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil => CharVal(n.toChar)
      case _                   => throw new EvalError("integer->char: expected integer")

  // --- Numeric utilities (L13) ---

  def absOp(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil => IntVal(math.abs(n))
      case _                   => throw new EvalError("abs: expected 1 number")

  def moduloOp(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(a, _) :: IntVal(b, _) :: Nil =>
        if b == 0 then throw new EvalError("modulo: division by zero")
        IntVal(Math.floorMod(a, b))
      case _ => throw new EvalError("modulo: expected 2 numbers")

  def remainderOp(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(a, _) :: IntVal(b, _) :: Nil =>
        if b == 0 then throw new EvalError("remainder: division by zero")
        IntVal(a % b)
      case _ => throw new EvalError("remainder: expected 2 numbers")

  def quotientOp(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(a, _) :: IntVal(b, _) :: Nil =>
        if b == 0 then throw new EvalError("quotient: division by zero")
        IntVal(a / b) // truncates toward zero in Scala
      case _ => throw new EvalError("quotient: expected 2 numbers")

  def minOp(args: List[SchemeValue]): SchemeValue =
    if args.isEmpty then throw new EvalError("min: requires at least 1 argument")
    IntVal(args.map {
      case IntVal(n, _) => n
      case _            => throw new EvalError("min: expected number")
    }.min)

  def maxOp(args: List[SchemeValue]): SchemeValue =
    if args.isEmpty then throw new EvalError("max: requires at least 1 argument")
    IntVal(args.map {
      case IntVal(n, _) => n
      case _            => throw new EvalError("max: expected number")
    }.max)

  def exptOp(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(base, _) :: IntVal(exp, _) :: Nil =>
        if exp < 0 then throw new EvalError("expt: negative exponent")
        IntVal(math.pow(base.toDouble, exp.toDouble).toLong)
      case _ => throw new EvalError("expt: expected 2 numbers")

  def zeroCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil => BoolVal(n == 0)
      case _                   => throw new EvalError("zero?: expected 1 number")

  def positiveCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil => BoolVal(n > 0)
      case _                   => throw new EvalError("positive?: expected 1 number")

  def negativeCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil => BoolVal(n < 0)
      case _                   => throw new EvalError("negative?: expected 1 number")

  def oddCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil => BoolVal(n % 2 != 0)
      case _                   => throw new EvalError("odd?: expected 1 number")

  def evenCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil => BoolVal(n % 2 == 0)
      case _                   => throw new EvalError("even?: expected 1 number")

  // --- List utilities (L13) ---

  def listRefOp(args: List[SchemeValue]): SchemeValue =
    args match
      case ListVal(es, _) :: IntVal(idx, _) :: Nil =>
        if idx < 0 || idx >= es.length then throw new EvalError("list-ref: index out of bounds")
        es(idx.toInt)
      case _ => throw new EvalError("list-ref: expected (list index)")

  def listTailOp(args: List[SchemeValue]): SchemeValue =
    args match
      case ListVal(es, _) :: IntVal(idx, _) :: Nil =>
        if idx < 0 || idx > es.length then throw new EvalError("list-tail: index out of bounds")
        ListVal(es.drop(idx.toInt))
      case _ => throw new EvalError("list-tail: expected (list index)")

  def listCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil => BoolVal(isProperList(v))
      case _        => throw new EvalError("list?: requires 1 argument")

  private def isProperList(v: SchemeValue): Boolean = v match
    case ListVal(_, _)   => true
    case PairVal(_, cdr) => isProperList(cdr)
    case _               => false

  def assocOp(args: List[SchemeValue]): SchemeValue =
    args match
      case key :: ListVal(alist, _) :: Nil =>
        alist
          .collectFirst {
            case entry @ ListVal(k :: _, _) if Builtins.schemeEqual(key, k) => entry
          }
          .getOrElse(BoolVal(false))
      case _ => throw new EvalError("assoc: expected (key alist)")

  def mapOp(args: List[SchemeValue]): SchemeValue =
    if args.length < 2 then throw new EvalError("map: requires at least 2 arguments")
    val proc = args.head
    val lists = args.tail.map {
      case ListVal(es, _) => es
      case _              => throw new EvalError("map: expected list")
    }
    val len = lists.head.length
    if !lists.forall(_.length == len) then throw new EvalError("map: lists must have same length")
    val result = (0 until len).toList.map { i =>
      val elemArgs = lists.map(_(i))
      Interpreter.applyProc(proc, elemArgs)
    }
    ListVal(result)

  // --- Character utilities (L13) ---

  def charAlphabeticCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case CharVal(c, _) :: Nil => BoolVal(c.isLetter)
      case _                    => throw new EvalError("char-alphabetic?: expected 1 char")

  def charNumericCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case CharVal(c, _) :: Nil => BoolVal(c.isDigit)
      case _                    => throw new EvalError("char-numeric?: expected 1 char")

  def charUpcaseOp(args: List[SchemeValue]): SchemeValue =
    args match
      case CharVal(c, _) :: Nil => CharVal(c.toUpper)
      case _                    => throw new EvalError("char-upcase: expected 1 char")

  def charDowncaseOp(args: List[SchemeValue]): SchemeValue =
    args match
      case CharVal(c, _) :: Nil => CharVal(c.toLower)
      case _                    => throw new EvalError("char-downcase: expected 1 char")

  def charEqualCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case CharVal(a, _) :: CharVal(b, _) :: Nil => BoolVal(a == b)
      case _                                     => throw new EvalError("char=?: expected 2 chars")

  def charLessCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case CharVal(a, _) :: CharVal(b, _) :: Nil => BoolVal(a < b)
      case _                                     => throw new EvalError("char<?: expected 2 chars")

  // --- String comparison/case utilities (L13) ---

  private def extractString(v: SchemeValue): String = v match
    case StringVal(s, _)         => s
    case MutableStringVal(cs, _) => String(cs)
    case _                       => throw new EvalError("expected string")

  def stringEqualCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case a :: b :: Nil => BoolVal(extractString(a) == extractString(b))
      case _             => throw new EvalError("string=?: expected 2 strings")

  def stringLessCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case a :: b :: Nil => BoolVal(extractString(a) < extractString(b))
      case _             => throw new EvalError("string<?: expected 2 strings")

  def stringCiEqualCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case a :: b :: Nil => BoolVal(extractString(a).equalsIgnoreCase(extractString(b)))
      case _             => throw new EvalError("string-ci=?: expected 2 strings")

  def stringUpcaseOp(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil => StringVal(extractString(v).toUpperCase)
      case _        => throw new EvalError("string-upcase: expected 1 string")

  def stringDowncaseOp(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil => StringVal(extractString(v).toLowerCase)
      case _        => throw new EvalError("string-downcase: expected 1 string")
