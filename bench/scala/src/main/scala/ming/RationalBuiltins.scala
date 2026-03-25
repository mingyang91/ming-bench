package ming

private[ming] object RationalBuiltins:

  def applyRationalBuiltin(name: String, args: List[Expr]): Expr = name match
    case "exact?" => Builtins.unary(name, args)(e => Expr.Bool(NumericUtils.isExact(e)))
    case "inexact?" =>
      Builtins.unary(name, args)(e => Expr.Bool(!NumericUtils.isExact(e) && NumericUtils.isNumber(e)))
    case "integer?" => Builtins.unary(name, args)(e => Expr.Bool(NumericUtils.isInteger(e)))
    case "rational?" =>
      Builtins.unary(name, args) {
        case Expr.Num(_) | Expr.Rational(_, _) => Expr.Bool(true)
        case _                                 => Expr.Bool(false)
      }
    case "exact->inexact" =>
      Builtins.unary(name, args)(e => Expr.Real(NumericUtils.toDouble(e)))
    case "inexact->exact" =>
      Builtins.unary(name, args) {
        case Expr.Real(v) =>
          if v == v.floor && !v.isInfinite then Expr.Num(v.toLong)
          else doubleToExact(v)
        case Expr.Num(n)         => Expr.Num(n)
        case Expr.Rational(n, d) => Expr.Rational(n, d)
        case other =>
          throw EvalError(s"inexact->exact: not a number: ${Builtins.display(other)}")
      }
    case "numerator" =>
      Builtins.unary(name, args) {
        case Expr.Num(n)         => Expr.Num(n)
        case Expr.Rational(n, _) => Expr.Num(n)
        case other =>
          throw EvalError(s"numerator: not a rational: ${Builtins.display(other)}")
      }
    case "denominator" =>
      Builtins.unary(name, args) {
        case Expr.Num(_)         => Expr.Num(1)
        case Expr.Rational(_, d) => Expr.Num(d)
        case other =>
          throw EvalError(s"denominator: not a rational: ${Builtins.display(other)}")
      }
    case _ => throw EvalError(s"unknown rational builtin: $name")

  private def doubleToExact(v: Double): Expr =
    val bits     = java.lang.Double.doubleToLongBits(v)
    val sign     = if bits < 0 then -1L else 1L
    val exponent = ((bits >> 52) & 0x7ffL).toInt - 1023
    val mantissa =
      if exponent == -1023 then (bits & 0xfffffffffffffL) << 1
      else (bits & 0xfffffffffffffL) | 0x10000000000000L
    if exponent >= 52 then Expr.Num(sign * mantissa * (1L << (exponent - 52)))
    else
      val shift = 52 - exponent
      NumericUtils.makeRational(sign * mantissa, 1L << shift)
