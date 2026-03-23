package ming

import SchemeValue.*

/** Numeric helper builtins extracted from BuiltinsDefs. */
object BuiltinsDefsNumeric:

  private[ming] def isIntegerVal(v: SchemeValue): Boolean = v match
    case _: IntVal => true
    case _         => false

  private[ming] def isRationalVal(v: SchemeValue): Boolean = v match
    case _: IntVal | _: RationalVal => true
    case _                          => false

  def zeroCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil => BoolVal(n == 0)
      case _                   => throw new EvalError("zero?: expected 1 number")

  def positiveCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil => BoolVal(n > 0)
      case _                   => throw new EvalError("positive?: expected 1 number")

  def negativeCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil => BoolVal(n < 0)
      case _                   => throw new EvalError("negative?: expected 1 number")

  def oddCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil => BoolVal(n % 2 != 0)
      case _                   => throw new EvalError("odd?: expected 1 number")

  def evenCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil => BoolVal(n % 2 == 0)
      case _                   => throw new EvalError("even?: expected 1 number")

  def gcdOp(args: List[SchemeValue]): SchemeValue =
    if args.isEmpty then IntVal(0)
    else
      val result = args
        .map {
          case IntVal(n, _) => math.abs(n)
          case _            => throw new EvalError("gcd: expected integer")
        }
        .reduce { (a, b) =>
          var x = a; var y = b
          while y != 0 do
            val t = y
            y = x % y
            x = t
          x
        }
      IntVal(result)

  def lcmOp(args: List[SchemeValue]): SchemeValue =
    if args.isEmpty then IntVal(1)
    else
      val nums = args.map {
        case IntVal(n, _) => math.abs(n)
        case _            => throw new EvalError("lcm: expected integer")
      }
      val result = nums.reduce { (a, b) =>
        if a == 0 || b == 0 then 0L
        else
          var x = a; var y = b
          while y != 0 do
            val t = y
            y = x % y
            x = t
          a / x * b
      }
      IntVal(result)

  def truncateOp(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil    => IntVal(n)
      case DoubleVal(d, _) :: Nil => IntVal(d.toLong)
      case _                      => throw new EvalError("truncate: expected 1 number")

  def roundOp(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil    => IntVal(n)
      case DoubleVal(d, _) :: Nil => IntVal(math.round(d))
      case _                      => throw new EvalError("round: expected 1 number")

  def exactToInexactOp(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil if Rational.isNumeric(v) => DoubleVal(Rational.toDouble(v))
      case _                                 => throw new EvalError("exact->inexact: expected 1 number")

  def inexactToExactOp(args: List[SchemeValue]): SchemeValue =
    args match
      case DoubleVal(d, _) :: Nil          => Rational.doubleToExact(d)
      case v :: Nil if Rational.isExact(v) => v
      case _                               => throw new EvalError("inexact->exact: expected 1 number")

  def numeratorOp(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(n, _) :: Nil         => IntVal(n)
      case RationalVal(n, _, _) :: Nil => IntVal(n)
      case _                           => throw new EvalError("numerator: expected exact number")

  def denominatorOp(args: List[SchemeValue]): SchemeValue =
    args match
      case IntVal(_, _) :: Nil         => IntVal(1)
      case RationalVal(_, d, _) :: Nil => IntVal(d)
      case _                           => throw new EvalError("denominator: expected exact number")
