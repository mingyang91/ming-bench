package ming

/** String and char built-in operations. */
object StringOps:

  private def asStr(v: Value): String = v match
    case Value.Str(s)        => s
    case Value.MutStr(chars) => new String(chars)
    case _                   => throw new EvalError(s"expected string, got: ${v.display}")

  def stringAppendOp(args: List[Value]): Value =
    Value.Str(args.map(asStr).mkString)

  def stringLengthOp(args: List[Value]): Value = args match
    case Value.Str(s) :: Nil        => Value.Integer(s.length.toLong)
    case Value.MutStr(chars) :: Nil => Value.Integer(chars.length.toLong)
    case _ :: Nil                   => throw new EvalError("string-length: not a string")
    case _                          => throw new EvalError("string-length requires 1 argument")

  def substringOp(args: List[Value]): Value = args match
    case Value.Str(s) :: Value.Integer(start) :: Value.Integer(end) :: Nil =>
      Value.Str(s.substring(start.toInt, end.toInt))
    case _ => throw new EvalError("substring requires string, start, end")

  def stringToNumberOp(args: List[Value]): Value = args match
    case Value.Str(s) :: Nil =>
      try Value.Integer(s.toLong)
      catch case _: NumberFormatException => Value.Bool(false)
    case _ => throw new EvalError("string->number requires 1 string argument")

  def numberToStringOp(args: List[Value]): Value = args match
    case Value.Integer(n) :: Nil    => Value.Str(n.toString)
    case (v: Value.Rational) :: Nil => Value.Str(v.display)
    case (v: Value.Float) :: Nil    => Value.Str(v.display)
    case _ :: Nil                   => throw new EvalError("number->string: not a number")
    case _                          => throw new EvalError("number->string requires 1 argument")

  def symbolToStringOp(args: List[Value]): Value = args match
    case Value.Symbol(name) :: Nil => Value.Str(name)
    case _ :: Nil                  => throw new EvalError("symbol->string: not a symbol")
    case _                         => throw new EvalError("symbol->string requires 1 argument")

  def stringToSymbolOp(args: List[Value]): Value = args match
    case Value.Str(s) :: Nil => Value.Symbol(s)
    case _ :: Nil            => throw new EvalError("string->symbol: not a string")
    case _                   => throw new EvalError("string->symbol requires 1 argument")

  def stringRefOp(args: List[Value]): Value = args match
    case Value.Str(s) :: Value.Integer(idx) :: Nil =>
      if idx < 0 || idx >= s.length then throw new EvalError("string-ref: index out of bounds")
      else Value.Char(s.charAt(idx.toInt))
    case Value.MutStr(chars) :: Value.Integer(idx) :: Nil =>
      if idx < 0 || idx >= chars.length then throw new EvalError("string-ref: index out of bounds")
      else Value.Char(chars(idx.toInt))
    case _ => throw new EvalError("string-ref requires string and index")

  def stringCopyOp(args: List[Value]): Value = args match
    case Value.Str(s) :: Nil        => Value.MutStr(s.toCharArray)
    case Value.MutStr(chars) :: Nil => Value.MutStr(chars.clone())
    case _ :: Nil                   => throw new EvalError("string-copy: not a string")
    case _                          => throw new EvalError("string-copy requires 1 argument")

  def stringSetOp(args: List[Value]): Value = args match
    case Value.MutStr(chars) :: Value.Integer(idx) :: Value.Char(c) :: Nil =>
      if idx < 0 || idx >= chars.length then throw new EvalError("string-set!: index out of bounds")
      else
        chars(idx.toInt) = c
        Value.Void
    case Value.Str(_) :: _ :: _ :: Nil =>
      throw new EvalError("string-set!: string is immutable")
    case _ => throw new EvalError("string-set! requires mutable string, index, and char")

  def stringToListOp(args: List[Value]): Value = args match
    case Value.Str(s) :: Nil        => Value.SList(s.toList.map(Value.Char.apply))
    case Value.MutStr(chars) :: Nil => Value.SList(chars.toList.map(Value.Char.apply))
    case _ :: Nil                   => throw new EvalError("string->list: not a string")
    case _                          => throw new EvalError("string->list requires 1 argument")

  def listToStringOp(args: List[Value]): Value = args match
    case lst :: Nil =>
      val elems = NumCharOps
        .toScalaList(lst)
        .getOrElse(throw new EvalError("list->string requires 1 list argument"))
      val chars = elems.map {
        case Value.Char(c) => c
        case other         => throw new EvalError(s"list->string: not a character: ${other.display}")
      }
      Value.Str(new String(chars.toArray))
    case _ => throw new EvalError("list->string requires 1 list argument")

  def charToIntegerOp(args: List[Value]): Value = args match
    case Value.Char(c) :: Nil => Value.Integer(c.toLong)
    case _ :: Nil             => throw new EvalError("char->integer: not a character")
    case _                    => throw new EvalError("char->integer requires 1 argument")

  def integerToCharOp(args: List[Value]): Value = args match
    case Value.Integer(n) :: Nil => Value.Char(n.toChar)
    case _ :: Nil                => throw new EvalError("integer->char: not an integer")
    case _                       => throw new EvalError("integer->char requires 1 argument")
