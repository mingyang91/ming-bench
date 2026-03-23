package ming

import Value.*

/** Value equality checks: eq?, eqv?, equal?. */
object Equality:

  def eqCheck(a: Value, b: Value): Boolean =
    (a, b) match
      case (IntVal(x), IntVal(y))                     => x == y
      case (RationalVal(n1, d1), RationalVal(n2, d2)) => n1 == n2 && d1 == d2
      case (FloatVal(x), FloatVal(y))                 => x == y
      case (BoolVal(x), BoolVal(y))                   => x == y
      case (SymbolVal(x), SymbolVal(y))               => x == y
      case (CharVal(x), CharVal(y))                   => x == y
      case (NilVal, NilVal)                           => true
      case (VoidVal, VoidVal)                         => true
      case _                                          => a eq b

  /** eqv? — same as eq? for our value representation. */
  def eqvCheck(a: Value, b: Value): Boolean = eqCheck(a, b)

  def equalCheck(a: Value, b: Value): Boolean =
    (a, b) match
      case (PairVal(a1, a2), PairVal(b1, b2)) => equalCheck(a1, b1) && equalCheck(a2, b2)
      case (StrVal(x), StrVal(y))             => java.util.Arrays.equals(x, y)
      case (VectorVal(x), VectorVal(y)) =>
        x.length == y.length && x.indices.forall(i => equalCheck(x(i), y(i)))
      case _ => eqCheck(a, b)
