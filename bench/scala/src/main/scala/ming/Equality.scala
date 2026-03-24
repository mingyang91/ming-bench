package ming

object Equality:

  def eqvCheck(a: SchemeVal, b: SchemeVal): Boolean = (a, b) match
    case (SchemeVal.IntVal(x), SchemeVal.IntVal(y))           => x == y
    case (SchemeVal.FloatVal(x), SchemeVal.FloatVal(y))       => x == y
    case (SchemeVal.RatVal(n1, d1), SchemeVal.RatVal(n2, d2)) => n1 == n2 && d1 == d2
    case (SchemeVal.BoolVal(x), SchemeVal.BoolVal(y))         => x == y
    case (SchemeVal.CharVal(x), SchemeVal.CharVal(y))         => x == y
    case (SchemeVal.SymVal(x), SchemeVal.SymVal(y))           => x == y
    case (SchemeVal.ListVal(Nil), SchemeVal.ListVal(Nil))     => true
    case _                                                    => a eq b

  def equalCheck(a: SchemeVal, b: SchemeVal): Boolean = (a, b) match
    case (SchemeVal.IntVal(x), SchemeVal.IntVal(y))           => x == y
    case (SchemeVal.FloatVal(x), SchemeVal.FloatVal(y))       => x == y
    case (SchemeVal.RatVal(n1, d1), SchemeVal.RatVal(n2, d2)) => n1 == n2 && d1 == d2
    case (SchemeVal.BoolVal(x), SchemeVal.BoolVal(y))         => x == y
    case (SchemeVal.CharVal(x), SchemeVal.CharVal(y))         => x == y
    case (SchemeVal.SymVal(x), SchemeVal.SymVal(y))           => x == y
    case (SchemeVal.StrVal(x), SchemeVal.StrVal(y))           => java.util.Arrays.equals(x, y)
    case (SchemeVal.ListVal(xs), SchemeVal.ListVal(ys)) =>
      xs.length == ys.length && xs.zip(ys).forall((a, b) => equalCheck(a, b))
    case (SchemeVal.PairVal(a1, d1), SchemeVal.PairVal(a2, d2)) =>
      equalCheck(a1, a2) && equalCheck(d1, d2)
    case (SchemeVal.VectorVal(xs), SchemeVal.VectorVal(ys)) =>
      xs.length == ys.length && xs.zip(ys).forall((a, b) => equalCheck(a, b))
    case _ => false
