package ming

import SchemeTypes.{asNum, errAt, makeRational, numericCompare, numericEqual, toDouble, Pos, Value}

private[ming] object ArithmeticBuiltins:

  def applyArithmetic(
    name: String,
    args: List[Value],
    pos: Pos
  ): Value = name match
    case "+" =>
      if args.isEmpty then Value.VNum(0)
      else args.reduceLeft((a, b) => addValues(a, b, pos))
    case "*" =>
      if args.isEmpty then Value.VNum(1)
      else args.reduceLeft((a, b) => mulValues(a, b, pos))
    case "-" =>
      if args.isEmpty then throw errAt(pos, "- requires at least 1 argument")
      else if args.length == 1 then negateValue(args.head, pos)
      else args.reduceLeft((a, b) => subValues(a, b, pos))
    case "/" =>
      if args.isEmpty then throw errAt(pos, "/ requires at least 1 argument")
      else if args.length == 1 then divValues(Value.VNum(1), args.head, pos)
      else args.reduceLeft((a, b) => divValues(a, b, pos))
    case "<" =>
      Value.VBool(args.zip(args.tail).forall((a, b) => numericCompare(a, b, pos) < 0))
    case ">" =>
      Value.VBool(args.zip(args.tail).forall((a, b) => numericCompare(a, b, pos) > 0))
    case "=" =>
      Value.VBool(args.zip(args.tail).forall((a, b) => numericEqual(a, b, pos)))
    case "<=" =>
      Value.VBool(args.zip(args.tail).forall((a, b) => numericCompare(a, b, pos) <= 0))
    case ">=" =>
      Value.VBool(args.zip(args.tail).forall((a, b) => numericCompare(a, b, pos) >= 0))
    case _ => throw errAt(pos, s"unknown arithmetic op: $name")

  def applyNumericUtils(
    name: String,
    args: List[Value],
    pos: Pos
  ): Value = name match
    case "abs" =>
      if args.length != 1 then throw errAt(pos, "abs requires 1 argument")
      Value.VNum(math.abs(asNum(args.head, pos)))
    case "modulo" =>
      if args.length != 2 then throw errAt(pos, "modulo requires 2 arguments")
      val a = asNum(args(0), pos)
      val b = asNum(args(1), pos)
      Value.VNum(java.lang.Math.floorMod(a, b))
    case "remainder" =>
      if args.length != 2 then throw errAt(pos, "remainder requires 2 arguments")
      Value.VNum(asNum(args(0), pos) % asNum(args(1), pos))
    case "quotient" =>
      if args.length != 2 then throw errAt(pos, "quotient requires 2 arguments")
      val a = asNum(args(0), pos)
      val b = asNum(args(1), pos)
      Value.VNum((a.toDouble / b.toDouble).toLong)
    case "min" =>
      if args.isEmpty then throw errAt(pos, "min requires at least 1 argument")
      Value.VNum(args.map(v => asNum(v, pos)).min)
    case "max" =>
      if args.isEmpty then throw errAt(pos, "max requires at least 1 argument")
      Value.VNum(args.map(v => asNum(v, pos)).max)
    case "expt" =>
      if args.length != 2 then throw errAt(pos, "expt requires 2 arguments")
      val base = asNum(args(0), pos)
      val exp  = asNum(args(1), pos)
      Value.VNum(math.pow(base.toDouble, exp.toDouble).toLong)
    case _ => throw errAt(pos, s"unknown numeric op: $name")

  def applyNumericPredicates(
    name: String,
    args: List[Value],
    pos: Pos
  ): Value =
    if args.length != 1 then throw errAt(pos, s"$name requires 1 argument")
    val arg = args.head
    val result = name match
      case "zero?"     => numericEqual(arg, Value.VNum(0), pos)
      case "positive?" => numericCompare(arg, Value.VNum(0), pos) > 0
      case "negative?" => numericCompare(arg, Value.VNum(0), pos) < 0
      case "odd?"      => asNum(arg, pos) % 2 != 0
      case "even?"     => asNum(arg, pos) % 2 == 0
      case _           => throw errAt(pos, s"unknown predicate: $name")
    Value.VBool(result)

  def applyExactOps(
    name: String,
    args: List[Value],
    pos: Pos
  ): Value =
    if args.length != 1 then throw errAt(pos, s"$name requires 1 argument")
    val arg = args.head
    name match
      case "exact?" =>
        arg match
          case _: Value.VNum | _: Value.VRational => Value.VBool(true)
          case _: Value.VFloat                    => Value.VBool(false)
          case _                                  => throw errAt(pos, "expected number")
      case "inexact?" =>
        arg match
          case _: Value.VFloat                    => Value.VBool(true)
          case _: Value.VNum | _: Value.VRational => Value.VBool(false)
          case _                                  => throw errAt(pos, "expected number")
      case "exact->inexact" =>
        arg match
          case Value.VNum(n)         => Value.VFloat(n.toDouble)
          case Value.VRational(n, d) => Value.VFloat(n.toDouble / d.toDouble)
          case Value.VFloat(d)       => Value.VFloat(d)
          case _                     => throw errAt(pos, "expected number")
      case "inexact->exact" =>
        arg match
          case Value.VFloat(d)                          => doubleToRational(d)
          case v @ (_: Value.VNum | _: Value.VRational) => v
          case _                                        => throw errAt(pos, "expected number")
      case "numerator" =>
        arg match
          case Value.VNum(n)         => Value.VNum(n)
          case Value.VRational(n, _) => Value.VNum(n)
          case _                     => throw errAt(pos, "expected rational")
      case "denominator" =>
        arg match
          case Value.VNum(_)         => Value.VNum(1)
          case Value.VRational(_, d) => Value.VNum(d)
          case _                     => throw errAt(pos, "expected rational")
      case _ => throw errAt(pos, s"unknown exact op: $name")

  private def doubleToRational(d: Double): Value =
    if d == d.toLong.toDouble && !d.isInfinite then Value.VNum(d.toLong)
    else
      var num = d
      var den = 1L
      while num != math.floor(num) && den < 1000000000L do
        num *= 2
        den *= 2
      makeRational(math.round(num), den)

  private def addValues(a: Value, b: Value, pos: Pos): Value = (a, b) match
    case (Value.VNum(x), Value.VNum(y)) => Value.VNum(x + y)
    case (Value.VRational(n1, d1), Value.VRational(n2, d2)) =>
      makeRational(n1 * d2 + n2 * d1, d1 * d2)
    case (Value.VNum(x), Value.VRational(n, d)) =>
      makeRational(x * d + n, d)
    case (Value.VRational(n, d), Value.VNum(y)) =>
      makeRational(n + y * d, d)
    case _ => Value.VFloat(toDouble(a, pos) + toDouble(b, pos))

  private def subValues(a: Value, b: Value, pos: Pos): Value = (a, b) match
    case (Value.VNum(x), Value.VNum(y)) => Value.VNum(x - y)
    case (Value.VRational(n1, d1), Value.VRational(n2, d2)) =>
      makeRational(n1 * d2 - n2 * d1, d1 * d2)
    case (Value.VNum(x), Value.VRational(n, d)) =>
      makeRational(x * d - n, d)
    case (Value.VRational(n, d), Value.VNum(y)) =>
      makeRational(n - y * d, d)
    case _ => Value.VFloat(toDouble(a, pos) - toDouble(b, pos))

  private def mulValues(a: Value, b: Value, pos: Pos): Value = (a, b) match
    case (Value.VNum(x), Value.VNum(y)) => Value.VNum(x * y)
    case (Value.VRational(n1, d1), Value.VRational(n2, d2)) =>
      makeRational(n1 * n2, d1 * d2)
    case (Value.VNum(x), Value.VRational(n, d)) =>
      makeRational(x * n, d)
    case (Value.VRational(n, d), Value.VNum(y)) =>
      makeRational(n * y, d)
    case _ => Value.VFloat(toDouble(a, pos) * toDouble(b, pos))

  private def divValues(a: Value, b: Value, pos: Pos): Value = (a, b) match
    case (Value.VNum(x), Value.VNum(y)) =>
      if y == 0 then throw errAt(pos, "division by zero")
      makeRational(x, y)
    case (Value.VRational(n1, d1), Value.VRational(n2, d2)) =>
      if n2 == 0 then throw errAt(pos, "division by zero")
      makeRational(n1 * d2, d1 * n2)
    case (Value.VNum(x), Value.VRational(n, d)) =>
      if n == 0 then throw errAt(pos, "division by zero")
      makeRational(x * d, n)
    case (Value.VRational(n, d), Value.VNum(y)) =>
      if y == 0 then throw errAt(pos, "division by zero")
      makeRational(n, d * y)
    case _ =>
      val dv = toDouble(b, pos)
      if dv == 0.0 then throw errAt(pos, "division by zero")
      Value.VFloat(toDouble(a, pos) / dv)

  private def negateValue(v: Value, pos: Pos): Value = v match
    case Value.VNum(n)         => Value.VNum(-n)
    case Value.VFloat(d)       => Value.VFloat(-d)
    case Value.VRational(n, d) => Value.VRational(-n, d)
    case _                     => throw errAt(pos, "expected number")
