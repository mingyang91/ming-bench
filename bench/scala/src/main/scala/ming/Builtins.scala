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
      case "char?"          => (typeCheck(name, args), "")
      case _                => throw new EvalError(s"unknown procedure: $name")

  private def typeCheck(name: String, args: List[SchemeValue]): SchemeValue =
    val result = (name, args) match
      case ("number?", (_: IntVal) :: Nil)    => true
      case ("boolean?", (_: BoolVal) :: Nil)  => true
      case ("string?", (_: StringVal) :: Nil) => true
      case ("symbol?", (_: SymbolVal) :: Nil) => true
      case ("char?", (_: CharVal) :: Nil)     => true
      case (_, _ :: Nil)                      => false
      case _                                  => throw new EvalError(s"$name: expects 1 argument")
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
    case StringVal(s) => s
    case other        => throw new EvalError(s"expected string, got: ${other.display}")

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
        val str = asString(s)
        val i   = asInt(idx).toInt
        if i < 0 || i >= str.length then throw new EvalError("string-ref: index out of range")
        CharVal(str.charAt(i))
      case _ => throw new EvalError("string-ref: expects 2 arguments")
