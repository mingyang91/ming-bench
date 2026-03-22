package ming

import SchemeValue.*

object BuiltinsEquality:

  def eqOp(args: List[SchemeValue]): SchemeValue =
    args match
      case a :: b :: Nil => BoolVal(schemeEq(a, b))
      case _             => throw new EvalError("eq?: expects 2 arguments")

  def eqvOp(args: List[SchemeValue]): SchemeValue =
    args match
      case a :: b :: Nil => BoolVal(schemeEqv(a, b))
      case _             => throw new EvalError("eqv?: expects 2 arguments")

  def equalOp(args: List[SchemeValue]): SchemeValue =
    args match
      case a :: b :: Nil => BoolVal(schemeEqual(a, b))
      case _             => throw new EvalError("equal?: expects 2 arguments")

  private def schemeEq(a: SchemeValue, b: SchemeValue): Boolean =
    (a, b) match
      case (IntVal(x), IntVal(y))             => x == y
      case (BoolVal(x), BoolVal(y))           => x == y
      case (CharVal(x), CharVal(y))           => x == y
      case (SymbolVal(x, _), SymbolVal(y, _)) => x == y
      case (StringVal(x), StringVal(y))       => x eq y
      case (ListVal(Nil, _), ListVal(Nil, _)) => true
      case (Void, Void)                       => true
      case _                                  => a eq b

  private def schemeEqv(a: SchemeValue, b: SchemeValue): Boolean =
    (a, b) match
      case (IntVal(x), IntVal(y))             => x == y
      case (BoolVal(x), BoolVal(y))           => x == y
      case (CharVal(x), CharVal(y))           => x == y
      case (SymbolVal(x, _), SymbolVal(y, _)) => x == y
      case (StringVal(x), StringVal(y))       => x == y
      case (ListVal(Nil, _), ListVal(Nil, _)) => true
      case (Void, Void)                       => true
      case _                                  => a eq b

  def schemeEqual(a: SchemeValue, b: SchemeValue): Boolean =
    (a, b) match
      case (IntVal(x), IntVal(y))                     => x == y
      case (BoolVal(x), BoolVal(y))                   => x == y
      case (CharVal(x), CharVal(y))                   => x == y
      case (SymbolVal(x, _), SymbolVal(y, _))         => x == y
      case (StringVal(x), StringVal(y))               => x == y
      case (MutableStringVal(x), MutableStringVal(y)) => java.util.Arrays.equals(x, y)
      case (StringVal(x), MutableStringVal(y))        => x == String(y)
      case (MutableStringVal(x), StringVal(y))        => String(x) == y
      case (VectorVal(a1), VectorVal(b1)) =>
        a1.length == b1.length && a1.zip(b1).forall((x, y) => schemeEqual(x, y))
      case (ListVal(Nil, _), ListVal(Nil, _))         => true
      case (Void, Void)                               => true
      case (AnyPair(a1, d1), AnyPair(a2, d2)) =>
        schemeEqual(a1, a2) && schemeEqual(d1, d2)
      case (ListVal(es1, _), ListVal(es2, _)) =>
        es1.length == es2.length && es1.zip(es2).forall((x, y) => schemeEqual(x, y))
      case (AnyPair(_, _), ListVal(_, _)) =>
        equalAsPairs(a, b)
      case (ListVal(_, _), AnyPair(_, _)) =>
        equalAsPairs(a, b)
      case _ => false

  private def equalAsPairs(a: SchemeValue, b: SchemeValue): Boolean =
    (a, b) match
      case (AnyPair(a1, d1), AnyPair(a2, d2)) => schemeEqual(a1, a2) && schemeEqual(d1, d2)
      case (ListVal(Nil, _), ListVal(Nil, _)) => true
      case (ListVal(h :: t, _), _) =>
        equalAsPairs(Builtins.makePair(h, Builtins.listToPairs(t)), b)
      case (_, ListVal(h :: t, _)) =>
        equalAsPairs(a, Builtins.makePair(h, Builtins.listToPairs(t)))
      case _ => schemeEqual(a, b)
