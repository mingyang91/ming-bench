package ming

import SchemeValue.*

/** Core builtin procedure implementations for the Scheme interpreter. */
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

  def typeCheck(
    args: List[SchemeValue],
    pred: SchemeValue => Boolean
  ): SchemeValue =
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

  def newlineOp(
    args: List[SchemeValue],
    output: StringBuilder
  ): SchemeValue =
    if args.nonEmpty then throw new EvalError("newline: requires 0 arguments")
    output.append("\n")
    Void

  def applyOp(args: List[SchemeValue]): SchemeValue =
    if args.length < 2 then throw new EvalError("apply: requires at least 2 arguments")
    val proc    = args.head
    val lastArg = args.last
    val tailList = lastArg match
      case ListVal(es, _) => es
      case _              => throw new EvalError("apply: last argument must be a list")
    val prefixArgs = args.slice(1, args.length - 1)
    Interpreter.applyProc(proc, prefixArgs ++ tailList)

  // --- eq? and equal? ---

  def eqCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case a :: b :: Nil => BoolVal(schemeEq(a, b))
      case _             => throw new EvalError("eq?: requires 2 arguments")

  def eqvCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case a :: b :: Nil => BoolVal(schemeEqv(a, b))
      case _             => throw new EvalError("eqv?: requires 2 arguments")

  /** eqv? — same as eq? for our integer-only interpreter. */
  private[ming] def schemeEqv(a: SchemeValue, b: SchemeValue): Boolean = schemeEq(a, b)

  private def schemeEq(a: SchemeValue, b: SchemeValue): Boolean = (a, b) match
    case (IntVal(x, _), IntVal(y, _))       => x == y
    case (BoolVal(x, _), BoolVal(y, _))     => x == y
    case (SymbolVal(x, _), SymbolVal(y, _)) => x == y
    case (CharVal(x, _), CharVal(y, _))     => x == y
    case (ListVal(Nil, _), ListVal(Nil, _)) => true
    case (Void, Void)                       => true
    case _                                  => a eq b // reference equality

  def equalCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case a :: b :: Nil => BoolVal(schemeEqual(a, b))
      case _             => throw new EvalError("equal?: requires 2 arguments")

  private[ming] def schemeEqual(
    a: SchemeValue,
    b: SchemeValue
  ): Boolean = (a, b) match
    case (IntVal(x, _), IntVal(y, _))       => x == y
    case (BoolVal(x, _), BoolVal(y, _))     => x == y
    case (SymbolVal(x, _), SymbolVal(y, _)) => x == y
    case (StringVal(x, _), StringVal(y, _)) => x == y
    case (CharVal(x, _), CharVal(y, _))     => x == y
    case (ListVal(Nil, _), ListVal(Nil, _)) => true
    case (ListVal(xs, _), ListVal(ys, _)) =>
      xs.length == ys.length && xs.zip(ys).forall((a, b) => schemeEqual(a, b))
    case (PairVal(a1, d1), PairVal(a2, d2)) =>
      schemeEqual(a1, a2) && schemeEqual(d1, d2)
    case (VectorVal(xs, _), VectorVal(ys, _)) =>
      xs.length == ys.length && xs.zip(ys).forall((a, b) => schemeEqual(a, b))
    case (Void, Void) => true
    case _            => false
