package ming

import SchemeValue.*

/** Builtin procedure implementations for the Scheme interpreter. */
object Builtins:

  def arith(
    args: List[SchemeValue],
    op: (Long, Long) => Long,
    identity: Long
  ): SchemeValue =
    IntVal(args.foldLeft(identity) {
      case (acc, IntVal(n, _)) => op(acc, n)
      case _                   => throw new EvalError("arithmetic: expected number")
    })

  def subtractOp(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil                 => throw new EvalError("-: requires at least 1 argument")
      case IntVal(n, _) :: Nil => IntVal(-n)
      case IntVal(first, _) :: rest =>
        IntVal(rest.foldLeft(first) {
          case (acc, IntVal(n, _)) => acc - n
          case _                   => throw new EvalError("-: expected number")
        })
      case _ => throw new EvalError("-: expected number")

  def divideOp(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil => throw new EvalError("/: requires at least 1 argument")
      case IntVal(first, _) :: rest =>
        IntVal(rest.foldLeft(first) {
          case (acc, IntVal(n, _)) =>
            if n == 0 then throw new EvalError("division by zero")
            acc / n
          case _ => throw new EvalError("/: expected number")
        })
      case _ => throw new EvalError("/: expected number")

  def compare(
    args: List[SchemeValue],
    cmp: (Long, Long) => Boolean
  ): SchemeValue =
    args match
      case IntVal(a, _) :: IntVal(b, _) :: Nil => BoolVal(cmp(a, b))
      case _                                   => throw new EvalError("comparison: expected 2 numbers")

  def notOp(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil => BoolVal(!v.isTruthy)
      case _        => throw new EvalError("not: requires 1 argument")

  def consOp(args: List[SchemeValue]): SchemeValue =
    args match
      case car :: cdr :: Nil =>
        cdr match
          case ListVal(es, _) => ListVal(car :: es)
          case _              => PairVal(car, cdr)
      case _ => throw new EvalError("cons: requires 2 arguments")

  def carOp(args: List[SchemeValue]): SchemeValue =
    args match
      case PairVal(car, _) :: Nil       => car
      case ListVal(head :: _, _) :: Nil => head
      case ListVal(Nil, _) :: Nil       => throw new EvalError("car: empty list")
      case _ :: Nil                     => throw new EvalError("car: not a pair")
      case _                            => throw new EvalError("car: requires 1 argument")

  def cdrOp(args: List[SchemeValue]): SchemeValue =
    args match
      case PairVal(_, cdr) :: Nil       => cdr
      case ListVal(_ :: tail, _) :: Nil => ListVal(tail)
      case ListVal(Nil, _) :: Nil       => throw new EvalError("cdr: empty list")
      case _ :: Nil                     => throw new EvalError("cdr: not a pair")
      case _                            => throw new EvalError("cdr: requires 1 argument")

  def nullCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case ListVal(Nil, _) :: Nil => BoolVal(true)
      case _ :: Nil               => BoolVal(false)
      case _                      => throw new EvalError("null?: requires 1 argument")

  def lengthOp(args: List[SchemeValue]): SchemeValue =
    args match
      case ListVal(es, _) :: Nil => IntVal(es.length.toLong)
      case _                     => throw new EvalError("length: expected list")

  def typeCheck(args: List[SchemeValue], pred: SchemeValue => Boolean): SchemeValue =
    args match
      case v :: Nil => BoolVal(pred(v))
      case _        => throw new EvalError("type predicate: requires 1 argument")

  def appendOp(args: List[SchemeValue]): SchemeValue =
    args.foldRight(ListVal(Nil): SchemeValue) {
      case (ListVal(es, _), ListVal(acc, _)) => ListVal(es ++ acc)
      case (ListVal(es, _), acc)             => es.foldRight(acc)((e, a) => consOp(List(e, a)))
      case (other, _)                        => throw new EvalError("append: expected list")
    }

  def pairCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case PairVal(_, _) :: Nil      => BoolVal(true)
      case ListVal(_ :: _, _) :: Nil => BoolVal(true)
      case _ :: Nil                  => BoolVal(false)
      case _                         => throw new EvalError("pair?: requires 1 argument")

  // --- Output operations ---

  def displayOp(args: List[SchemeValue], output: StringBuilder): SchemeValue =
    args match
      case v :: Nil =>
        output.append(v.displayOutput)
        Void
      case _ => throw new EvalError("display: requires 1 argument")

  def writeOp(args: List[SchemeValue], output: StringBuilder): SchemeValue =
    args match
      case v :: Nil =>
        output.append(v.display)
        Void
      case _ => throw new EvalError("write: requires 1 argument")

  def newlineOp(args: List[SchemeValue], output: StringBuilder): SchemeValue =
    if args.nonEmpty then throw new EvalError("newline: requires 0 arguments")
    output.append("\n")
    Void

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
