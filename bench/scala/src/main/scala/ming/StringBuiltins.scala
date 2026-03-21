package ming

/** String, character, and equality built-in procedures. */
object StringBuiltins:

  private[ming] def asString(v: Value): String =
    v.stringContent.getOrElse(
      throw new EvalError(s"expected string, got: ${v.display}")
    )

  def evalStringAppend(args: List[Value]): Value =
    Value.StringVal(args.map(asString).mkString)

  def evalStringLength(args: List[Value]): Value =
    args match
      case v :: Nil =>
        val s = asString(v)
        Value.IntVal(s.length.toLong)
      case _ =>
        throw new EvalError("string-length requires 1 string argument")

  def evalSubstring(args: List[Value]): Value =
    args match
      case v :: Value.IntVal(start) :: Value.IntVal(end) :: Nil =>
        val s = asString(v)
        Value.StringVal(s.substring(start.toInt, end.toInt))
      case _ =>
        throw new EvalError("substring requires string, start, end")

  def evalStringToNumber(args: List[Value]): Value =
    args match
      case v :: Nil =>
        val s = asString(v)
        s.toLongOption match
          case Some(n) => Value.IntVal(n)
          case None    => Value.BoolVal(false)
      case _ =>
        throw new EvalError("string->number requires 1 string argument")

  def evalNumberToString(args: List[Value]): Value =
    args match
      case Value.IntVal(n) :: Nil => Value.StringVal(n.toString)
      case _ =>
        throw new EvalError("number->string requires 1 integer argument")

  def evalSymbolToString(args: List[Value]): Value =
    args match
      case Value.Symbol(name, _) :: Nil => Value.StringVal(name)
      case _ =>
        throw new EvalError("symbol->string requires 1 symbol argument")

  def evalStringToSymbol(args: List[Value]): Value =
    args match
      case v :: Nil =>
        val s = asString(v)
        Value.Symbol(s)
      case _ =>
        throw new EvalError("string->symbol requires 1 string argument")

  def evalStringRef(args: List[Value]): Value =
    args match
      case v :: Value.IntVal(i) :: Nil =>
        val s = asString(v)
        Value.CharVal(s.charAt(i.toInt))
      case _ =>
        throw new EvalError("string-ref requires string and index")

  def evalStringCopy(args: List[Value]): Value =
    args match
      case v :: Nil =>
        val s = asString(v)
        Value.MutableStringVal(s.toCharArray)
      case _ =>
        throw new EvalError("string-copy requires 1 string argument")

  def evalStringSet(args: List[Value]): Value =
    args match
      case Value.MutableStringVal(chars) :: Value.IntVal(i) :: Value.CharVal(
            c
          ) :: Nil =>
        chars(i.toInt) = c
        Value.VoidVal
      case _ =>
        throw new EvalError(
          "string-set! requires mutable string, index, and char"
        )

  def evalEq(args: List[Value]): Value =
    args match
      case a :: b :: Nil => Value.BoolVal(schemeEq(a, b))
      case _             => throw new EvalError("eq? requires exactly 2 arguments")

  private[ming] def schemeEq(a: Value, b: Value): Boolean =
    (a, b) match
      case (Value.IntVal(x), Value.IntVal(y))       => x == y
      case (Value.BoolVal(x), Value.BoolVal(y))     => x == y
      case (Value.Symbol(x, _), Value.Symbol(y, _)) => x == y
      case (Value.CharVal(x), Value.CharVal(y))     => x == y
      case (Value.NilVal, Value.NilVal)             => true
      case (Value.VoidVal, Value.VoidVal)           => true
      case _                                        => a eq b

  def evalEqual(args: List[Value]): Value =
    args match
      case a :: b :: Nil => Value.BoolVal(schemeEqual(a, b))
      case _             => throw new EvalError("equal? requires exactly 2 arguments")

  private[ming] def schemeEqual(a: Value, b: Value): Boolean =
    (a, b) match
      case (Value.PairVal(ca, cd, _), Value.PairVal(cb, dd, _)) =>
        schemeEqual(ca, cb) && schemeEqual(cd, dd)
      case (Value.NilVal, Value.NilVal) => true
      case (sa, sb) if sa.stringContent.isDefined && sb.stringContent.isDefined =>
        sa.stringContent == sb.stringContent
      case _ => schemeEq(a, b)

  def charPred(args: List[Value], pred: Char => Boolean): Value =
    args match
      case Value.CharVal(c) :: Nil => Value.BoolVal(pred(c))
      case _ =>
        throw new EvalError("char predicate requires 1 char argument")

  def charTransform(args: List[Value], f: Char => Char): Value =
    args match
      case Value.CharVal(c) :: Nil => Value.CharVal(f(c))
      case _ =>
        throw new EvalError("char transform requires 1 char argument")

  def charCmp(
    args: List[Value],
    op: (Char, Char) => Boolean
  ): Value =
    args match
      case Value.CharVal(a) :: Value.CharVal(b) :: Nil =>
        Value.BoolVal(op(a, b))
      case _ =>
        throw new EvalError("char comparison requires 2 char arguments")

  def strCmp(
    args: List[Value],
    op: (String, String) => Boolean
  ): Value =
    args match
      case a :: b :: Nil =>
        Value.BoolVal(op(asString(a), asString(b)))
      case _ =>
        throw new EvalError(
          "string comparison requires 2 string arguments"
        )

  def evalStringToList(args: List[Value]): Value =
    args match
      case v :: Nil =>
        val s = asString(v)
        s.foldRight(Value.NilVal: Value)((c, acc) => Value.PairVal(Value.CharVal(c), acc))
      case _ =>
        throw new EvalError("string->list requires 1 string argument")

  def evalListToString(args: List[Value]): Value =
    args match
      case v :: Nil =>
        val chars = collectChars(v, List.empty)
        Value.StringVal(chars.mkString)
      case _ =>
        throw new EvalError("list->string requires 1 argument")

  @scala.annotation.tailrec
  private def collectChars(v: Value, acc: List[Char]): List[Char] =
    v match
      case Value.NilVal => acc.reverse
      case Value.PairVal(Value.CharVal(c), cdr, _) =>
        collectChars(cdr, c :: acc)
      case _ =>
        throw new EvalError("list->string: expected list of characters")

  def evalCharToInteger(args: List[Value]): Value =
    args match
      case Value.CharVal(c) :: Nil => Value.IntVal(c.toLong)
      case _ =>
        throw new EvalError("char->integer requires 1 char argument")

  def evalIntegerToChar(args: List[Value]): Value =
    args match
      case Value.IntVal(n) :: Nil => Value.CharVal(n.toChar)
      case _ =>
        throw new EvalError("integer->char requires 1 integer argument")

  def strCase(args: List[Value], f: String => String): Value =
    args match
      case v :: Nil => Value.StringVal(f(asString(v)))
      case _ =>
        throw new EvalError("string case requires 1 string argument")
