package ming

import Value.*

/** Arithmetic and numeric helpers used by Builtins. */
private[ming] object BuiltinsArith:

  def requireInts(args: List[Value]): List[Long] =
    args.map {
      case IntVal(n) => n
      case other     => throw new EvalError(s"expected number, got ${other.display}")
    }

  /** Convert a Value to a double for numeric operations. */
  def toDouble(v: Value): Double = v match
    case IntVal(n)         => n.toDouble
    case RationalVal(n, d) => n.toDouble / d.toDouble
    case FloatVal(d)       => d
    case other             => throw new EvalError(s"expected number, got ${other.display}")

  /** Check if any value in the list is inexact. */
  def hasInexact(args: List[Value]): Boolean =
    args.exists(_.isInstanceOf[FloatVal])

  /** Add two exact values. */
  def addExact(a: Value, b: Value): Value =
    (a, b) match
      case (IntVal(x), IntVal(y))                     => IntVal(x + y)
      case (IntVal(x), RationalVal(n, d))             => Value.makeRational(x * d + n, d)
      case (RationalVal(n, d), IntVal(y))             => Value.makeRational(n + y * d, d)
      case (RationalVal(n1, d1), RationalVal(n2, d2)) => Value.makeRational(n1 * d2 + n2 * d1, d1 * d2)
      case _                                          => throw new EvalError("expected exact number")

  /** Multiply two exact values. */
  def mulExact(a: Value, b: Value): Value =
    (a, b) match
      case (IntVal(x), IntVal(y))                     => IntVal(x * y)
      case (IntVal(x), RationalVal(n, d))             => Value.makeRational(x * n, d)
      case (RationalVal(n, d), IntVal(y))             => Value.makeRational(n * y, d)
      case (RationalVal(n1, d1), RationalVal(n2, d2)) => Value.makeRational(n1 * n2, d1 * d2)
      case _                                          => throw new EvalError("expected exact number")

  /** Subtract two exact values. */
  def subExact(a: Value, b: Value): Value =
    (a, b) match
      case (IntVal(x), IntVal(y))                     => IntVal(x - y)
      case (IntVal(x), RationalVal(n, d))             => Value.makeRational(x * d - n, d)
      case (RationalVal(n, d), IntVal(y))             => Value.makeRational(n - y * d, d)
      case (RationalVal(n1, d1), RationalVal(n2, d2)) => Value.makeRational(n1 * d2 - n2 * d1, d1 * d2)
      case _                                          => throw new EvalError("expected exact number")

  /** Divide two exact values. */
  def divExact(a: Value, b: Value): Value =
    (a, b) match
      case (IntVal(x), IntVal(y))                     => Value.makeRational(x, y)
      case (IntVal(x), RationalVal(n, d))             => Value.makeRational(x * d, n)
      case (RationalVal(n, d), IntVal(y))             => Value.makeRational(n, d * y)
      case (RationalVal(n1, d1), RationalVal(n2, d2)) => Value.makeRational(n1 * d2, d1 * n2)
      case _                                          => throw new EvalError("expected exact number")

  def arithAdd(args: List[Value]): Value =
    if hasInexact(args) then FloatVal(args.map(toDouble).foldLeft(0.0)(_ + _))
    else args.foldLeft(IntVal(0L): Value)(addExact)

  def arithMul(args: List[Value]): Value =
    if hasInexact(args) then FloatVal(args.map(toDouble).foldLeft(1.0)(_ * _))
    else args.foldLeft(IntVal(1L): Value)(mulExact)

  def subtractOp(args: List[Value]): Value =
    if hasInexact(args) then
      val doubles = args.map(toDouble)
      doubles match
        case Nil       => FloatVal(0.0)
        case d :: Nil  => FloatVal(-d)
        case d :: rest => FloatVal(rest.foldLeft(d)(_ - _))
    else
      args match
        case Nil => IntVal(0)
        case v :: Nil =>
          v match
            case IntVal(n)         => IntVal(-n)
            case RationalVal(n, d) => Value.makeRational(-n, d)
            case _                 => throw new EvalError("expected number")
        case first :: rest =>
          rest.foldLeft(first)((acc, v) => subExact(acc, v))

  def divideOp(args: List[Value]): Value =
    if hasInexact(args) then
      val doubles = args.map(toDouble)
      doubles match
        case Nil       => throw new EvalError("/: need at least 1 argument")
        case d :: Nil  => FloatVal(1.0 / d)
        case d :: rest => FloatVal(rest.foldLeft(d)(_ / _))
    else
      args match
        case Nil      => throw new EvalError("/: need at least 1 argument")
        case v :: Nil => divExact(IntVal(1), v)
        case first :: rest =>
          rest.foldLeft(first)((acc, v) => divExact(acc, v))

  def compareOp(args: List[Value], op: (Double, Double) => Boolean): Value =
    val doubles = args.map(toDouble)
    val result  = doubles.zip(doubles.tail).forall((a, b) => op(a, b))
    BoolVal(result)

  /** Convert a double to an exact rational. */
  def doubleToExact(d: Double): Value =
    if d == Math.floor(d) && !d.isInfinite then IntVal(d.toLong)
    else
      val bits     = java.lang.Double.doubleToLongBits(d)
      val sign     = if bits < 0 then -1L else 1L
      val exp      = ((bits >> 52) & 0x7ffL).toInt - 1023
      val mantissa = (bits & 0xfffffffffffffL) | (1L << 52)
      if exp >= 52 then IntVal(sign * (mantissa << (exp - 52)))
      else if exp >= 0 then
        val num = sign * mantissa
        val den = 1L << (52 - exp)
        Value.makeRational(num, den)
      else
        val num = sign * mantissa
        val den = 1L << (52 - exp)
        Value.makeRational(num, den)

  def isNumeric(v: Value): Boolean = v match
    case IntVal(_) | RationalVal(_, _) | FloatVal(_) => true
    case _                                           => false

  private def typePred(args: List[Value], pred: Value => Boolean): Value =
    if args.length != 1 then throw new EvalError("type predicate: expected 1 argument")
    BoolVal(pred(args.head))

  def registerTypePredicates(env: Env): Unit =
    Builtins.define(
      env,
      List(
        ("string?", args => typePred(args, _.isInstanceOf[StrVal])),
        ("number?", args => typePred(args, isNumeric)),
        ("boolean?", args => typePred(args, _.isInstanceOf[BoolVal])),
        ("pair?", args => typePred(args, _.isInstanceOf[PairVal])),
        (
          "procedure?",
          args =>
            typePred(
              args,
              v =>
                v.isInstanceOf[LambdaVal] || v.isInstanceOf[BuiltinVal] || v.isInstanceOf[ContinuationVal] || v
                  .isInstanceOf[CaseLambdaVal]
            )
        ),
        ("symbol?", args => typePred(args, _.isInstanceOf[SymbolVal])),
        ("char?", args => typePred(args, _.isInstanceOf[CharVal])),
        (
          "integer?",
          args =>
            if args.length != 1 then throw new EvalError("integer?: expected 1 argument")
            args.head match
              case IntVal(_) => BoolVal(true)
              case RationalVal(n, d) if n % d == 0 => BoolVal(true)
              case FloatVal(d) if d == Math.floor(d) && !d.isInfinite => BoolVal(true)
              case _ if isNumeric(args.head)                          => BoolVal(false)
              case _                                                  => throw new EvalError("integer?: not a number")
        ),
        (
          "rational?",
          args =>
            if args.length != 1 then throw new EvalError("rational?: expected 1 argument")
            args.head match
              case IntVal(_) | RationalVal(_, _) => BoolVal(true)
              case FloatVal(_)                   => BoolVal(false)
              case _                             => throw new EvalError("rational?: not a number")
        ),
        (
          "exact?",
          args =>
            if args.length != 1 then throw new EvalError("exact?: expected 1 argument")
            args.head match
              case IntVal(_) | RationalVal(_, _) => BoolVal(true)
              case FloatVal(_)                   => BoolVal(false)
              case _                             => throw new EvalError("exact?: not a number")
        ),
        (
          "inexact?",
          args =>
            if args.length != 1 then throw new EvalError("inexact?: expected 1 argument")
            args.head match
              case FloatVal(_)                   => BoolVal(true)
              case IntVal(_) | RationalVal(_, _) => BoolVal(false)
              case _                             => throw new EvalError("inexact?: not a number")
        ),
        (
          "exact->inexact",
          args =>
            if args.length != 1 then throw new EvalError("exact->inexact: expected 1 argument")
            args.head match
              case IntVal(n)         => FloatVal(n.toDouble)
              case RationalVal(n, d) => FloatVal(n.toDouble / d.toDouble)
              case FloatVal(d)       => FloatVal(d)
              case _                 => throw new EvalError("exact->inexact: not a number")
        ),
        (
          "inexact->exact",
          args =>
            if args.length != 1 then throw new EvalError("inexact->exact: expected 1 argument")
            args.head match
              case FloatVal(d)           => doubleToExact(d)
              case IntVal(n)             => IntVal(n)
              case r @ RationalVal(_, _) => r
              case _                     => throw new EvalError("inexact->exact: not a number")
        ),
        (
          "numerator",
          args =>
            if args.length != 1 then throw new EvalError("numerator: expected 1 argument")
            args.head match
              case IntVal(n)         => IntVal(n)
              case RationalVal(n, _) => IntVal(n)
              case _                 => throw new EvalError("numerator: not a rational")
        ),
        (
          "denominator",
          args =>
            if args.length != 1 then throw new EvalError("denominator: expected 1 argument")
            args.head match
              case IntVal(_)         => IntVal(1)
              case RationalVal(_, d) => IntVal(d)
              case _                 => throw new EvalError("denominator: not a rational")
        )
      )
    )
