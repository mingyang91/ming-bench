package ming

import SchemeValue.*

/** Equality operations (eq?, eqv?, equal?) extracted from Builtins. */
object BuiltinsEquality:

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

  private[ming] def schemeEq(a: SchemeValue, b: SchemeValue): Boolean = (a, b) match
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
      val as    = Builtins.toScalaList(a)
      val bs    = Builtins.toScalaList(b)
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
