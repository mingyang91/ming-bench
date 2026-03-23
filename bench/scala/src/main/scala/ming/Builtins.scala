package ming

import SchemeValue.*

/** Core builtin procedure implementations for the Scheme interpreter. */
object Builtins:

  def addOp(args: List[SchemeValue]): SchemeValue =
    if args.isEmpty then return IntVal(0)
    if Rational.hasInexact(args) then DoubleVal(args.foldLeft(0.0)((acc, v) => acc + Rational.toDouble(v)))
    else
      val (num, den) = args.foldLeft((0L, 1L)) { case ((an, ad), v) =>
        val (bn, bd) = Rational.toRational(v)
        (an * bd + bn * ad, ad * bd)
      }
      Rational.make(num, den)

  def mulOp(args: List[SchemeValue]): SchemeValue =
    if args.isEmpty then return IntVal(1)
    if Rational.hasInexact(args) then DoubleVal(args.foldLeft(1.0)((acc, v) => acc * Rational.toDouble(v)))
    else
      val (num, den) = args.foldLeft((1L, 1L)) { case ((an, ad), v) =>
        val (bn, bd) = Rational.toRational(v)
        (an * bn, ad * bd)
      }
      Rational.make(num, den)

  def subtractOp(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil => throw new EvalError("-: requires at least 1 argument")
      case v :: Nil =>
        if Rational.isInexact(v) then DoubleVal(-Rational.toDouble(v))
        else
          val (n, d) = Rational.toRational(v)
          Rational.make(-n, d)
      case first :: rest =>
        if Rational.hasInexact(args) then
          DoubleVal(rest.foldLeft(Rational.toDouble(first))((acc, v) => acc - Rational.toDouble(v)))
        else
          val (fn, fd) = Rational.toRational(first)
          val (rn, rd) = rest.foldLeft((fn, fd)) { case ((an, ad), v) =>
            val (bn, bd) = Rational.toRational(v)
            (an * bd - bn * ad, ad * bd)
          }
          Rational.make(rn, rd)

  def divideOp(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil => throw new EvalError("/: requires at least 1 argument")
      case first :: rest if rest.isEmpty =>
        if Rational.isInexact(first) then
          val d = Rational.toDouble(first)
          if d == 0.0 then throw new EvalError("division by zero")
          DoubleVal(1.0 / d)
        else
          val (n, d) = Rational.toRational(first)
          if n == 0 then throw new EvalError("division by zero")
          Rational.make(d, n)
      case first :: rest =>
        if Rational.hasInexact(args) then
          DoubleVal(rest.foldLeft(Rational.toDouble(first)) { (acc, v) =>
            val d = Rational.toDouble(v)
            if d == 0.0 then throw new EvalError("division by zero")
            acc / d
          })
        else
          val (fn, fd) = Rational.toRational(first)
          val (rn, rd) = rest.foldLeft((fn, fd)) { case ((an, ad), v) =>
            val (bn, bd) = Rational.toRational(v)
            if bn == 0 then throw new EvalError("division by zero")
            (an * bd, ad * bn)
          }
          Rational.make(rn, rd)

  def compare(
    args: List[SchemeValue],
    cmp: (Double, Double) => Boolean
  ): SchemeValue =
    args match
      case a :: b :: Nil if Rational.isNumeric(a) && Rational.isNumeric(b) =>
        BoolVal(cmp(Rational.toDouble(a), Rational.toDouble(b)))
      case _ => throw new EvalError("comparison: expected 2 numbers")

  def notOp(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil => BoolVal(!v.isTruthy)
      case _        => throw new EvalError("not: requires 1 argument")

  def consOp(args: List[SchemeValue]): SchemeValue =
    args match
      case car :: cdr :: Nil => MutablePairVal(Array(car, cdr))
      case _                 => throw new EvalError("cons: requires 2 arguments")

  def carOp(args: List[SchemeValue]): SchemeValue =
    args match
      case MutablePairVal(cells) :: Nil => cells(0)
      case PairVal(car, _) :: Nil       => car
      case ListVal(head :: _, _) :: Nil => head
      case ListVal(Nil, _) :: Nil       => throw new EvalError("car: empty list")
      case _ :: Nil                     => throw new EvalError("car: not a pair")
      case _                            => throw new EvalError("car: requires 1 argument")

  def cdrOp(args: List[SchemeValue]): SchemeValue =
    args match
      case MutablePairVal(cells) :: Nil => cells(1)
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
      case ListVal(es, _) :: Nil    => IntVal(es.length.toLong)
      case MutablePairVal(_) :: Nil => IntVal(toScalaList(args.head).length.toLong)
      case _                        => throw new EvalError("length: expected list")

  def typeCheck(
    args: List[SchemeValue],
    pred: SchemeValue => Boolean
  ): SchemeValue =
    args match
      case v :: Nil => BoolVal(pred(v))
      case _        => throw new EvalError("type predicate: requires 1 argument")

  def appendOp(args: List[SchemeValue]): SchemeValue =
    if args.isEmpty then return ListVal(Nil)
    args.foldRight(args.last: SchemeValue) { (elem, acc) =>
      if elem eq acc then acc // last element - identity
      else
        val elems = toScalaList(elem)
        elems.foldRight(acc)((e, a) => MutablePairVal(Array(e, a)))
    }

  /** Convert a Scheme list (ListVal or MutablePairVal chain) to a Scala List. */
  private[ming] def toScalaList(v: SchemeValue): List[SchemeValue] =
    val buf     = scala.collection.mutable.ListBuffer[SchemeValue]()
    var current = v
    var done    = false
    while !done do
      current match
        case MutablePairVal(cells) =>
          buf += cells(0)
          current = cells(1)
        case ListVal(es, _) =>
          buf ++= es
          done = true
        case PairVal(car, cdr) =>
          buf += car
          current = cdr
        case _ => done = true
    buf.toList

  def setCarOp(args: List[SchemeValue]): SchemeValue =
    args match
      case MutablePairVal(cells) :: value :: Nil =>
        cells(0) = value
        Void
      case _ :: _ :: Nil => throw new EvalError("set-car!: not a mutable pair")
      case _             => throw new EvalError("set-car!: requires 2 arguments")

  def setCdrOp(args: List[SchemeValue]): SchemeValue =
    args match
      case MutablePairVal(cells) :: value :: Nil =>
        cells(1) = value
        Void
      case _ :: _ :: Nil => throw new EvalError("set-cdr!: not a mutable pair")
      case _             => throw new EvalError("set-cdr!: requires 2 arguments")

  def pairCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case MutablePairVal(_) :: Nil  => BoolVal(true)
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
    case (IntVal(x, _), IntVal(y, _))                     => x == y
    case (RationalVal(xn, xd, _), RationalVal(yn, yd, _)) => xn == yn && xd == yd
    case (DoubleVal(x, _), DoubleVal(y, _))               => x == y
    case (BoolVal(x, _), BoolVal(y, _))                   => x == y
    case (SymbolVal(x, _), SymbolVal(y, _))               => x == y
    case (CharVal(x, _), CharVal(y, _))                   => x == y
    case (ListVal(Nil, _), ListVal(Nil, _))               => true
    case (Void, Void)                                     => true
    case _                                                => a eq b // reference equality

  def equalCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case a :: b :: Nil => BoolVal(schemeEqual(a, b))
      case _             => throw new EvalError("equal?: requires 2 arguments")

  private[ming] def schemeEqual(
    a: SchemeValue,
    b: SchemeValue
  ): Boolean = schemeEqualSafe(a, b, java.util.IdentityHashMap[AnyRef, java.util.IdentityHashMap[AnyRef, Boolean]]())

  private def schemeEqualSafe(
    a: SchemeValue,
    b: SchemeValue,
    seen: java.util.IdentityHashMap[AnyRef, java.util.IdentityHashMap[AnyRef, Boolean]]
  ): Boolean = (a, b) match
    case (IntVal(x, _), IntVal(y, _))                     => x == y
    case (RationalVal(xn, xd, _), RationalVal(yn, yd, _)) => xn == yn && xd == yd
    case (DoubleVal(x, _), DoubleVal(y, _))               => x == y
    case (BoolVal(x, _), BoolVal(y, _))                   => x == y
    case (SymbolVal(x, _), SymbolVal(y, _))               => x == y
    case (StringVal(x, _), StringVal(y, _))               => x == y
    case (CharVal(x, _), CharVal(y, _))                   => x == y
    case (ListVal(Nil, _), ListVal(Nil, _))               => true
    case _ if isListLike(a) && isListLike(b) =>
      val as    = toScalaList(a)
      val bs    = toScalaList(b)
      val aNull = isNullTerminated(a)
      val bNull = isNullTerminated(b)
      if aNull && bNull then as.length == bs.length && as.zip(bs).forall((x, y) => schemeEqualSafe(x, y, seen))
      else if !aNull && !bNull then
        as.length == bs.length &&
        as.zip(bs).forall((x, y) => schemeEqualSafe(x, y, seen)) &&
        schemeEqualSafe(lastCdr(a), lastCdr(b), seen)
      else false
    case (VectorVal(xs, _), VectorVal(ys, _)) =>
      xs.length == ys.length && xs.zip(ys).forall((a, b) => schemeEqualSafe(a, b, seen))
    case (Void, Void) => true
    case _            => a eq b

  private def isListLike(v: SchemeValue): Boolean = v match
    case MutablePairVal(_)  => true
    case PairVal(_, _)      => true
    case ListVal(_ :: _, _) => true
    case _                  => false

  private def isNullTerminated(v: SchemeValue): Boolean =
    var current = v
    while true do
      current match
        case MutablePairVal(cells) => current = cells(1)
        case PairVal(_, cdr)       => current = cdr
        case ListVal(es, _)        => return true
        case _                     => return false
    false

  private def lastCdr(v: SchemeValue): SchemeValue =
    var current = v
    while true do
      current match
        case MutablePairVal(cells) => current = cells(1)
        case PairVal(_, cdr)       => current = cdr
        case _                     => return current
    current
