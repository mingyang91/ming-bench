package ming

/** Numeric arithmetic, comparison, and predicate operations. */
object NumericOps:

  private def toDouble(v: SchemeVal): Double = v match
    case SchemeVal.SInt(n)         => n.toDouble
    case SchemeVal.SFloat(d)       => d
    case SchemeVal.SRational(n, d) => n.toDouble / d.toDouble
    case other                     => throw new EvalError(s"expected number, got ${other.display}")

  private def isNumeric(v: SchemeVal): Boolean = v match
    case SchemeVal.SInt(_) | SchemeVal.SFloat(_) | SchemeVal.SRational(_, _) => true
    case _                                                                   => false

  private def asInt(v: SchemeVal): Long = v match
    case SchemeVal.SInt(n) => n
    case other             => throw new EvalError(s"expected number, got ${other.display}")

  private def requireOne(name: String, args: List[SchemeVal]): SchemeVal =
    if args.length != 1 then throw new EvalError(s"$name: expected 1 argument")
    args.head

  private def requireTwo(name: String, args: List[SchemeVal]): (SchemeVal, SchemeVal) =
    if args.length != 2 then throw new EvalError(s"$name: expected 2 arguments")
    (args(0), args(1))

  private def toRational(v: SchemeVal): (Long, Long) = v match
    case SchemeVal.SInt(n)         => (n, 1L)
    case SchemeVal.SRational(n, d) => (n, d)
    case other                     => throw new EvalError(s"expected exact number, got ${other.display}")

  private def hasInexact(args: List[SchemeVal]): Boolean =
    args.exists(_.isInstanceOf[SchemeVal.SFloat])

  private def ratAdd(a: (Long, Long), b: (Long, Long)): SchemeVal =
    SchemeVal.makeRational(a._1 * b._2 + b._1 * a._2, a._2 * b._2)

  private def ratSub(a: (Long, Long), b: (Long, Long)): SchemeVal =
    SchemeVal.makeRational(a._1 * b._2 - b._1 * a._2, a._2 * b._2)

  private def ratMul(a: (Long, Long), b: (Long, Long)): SchemeVal =
    SchemeVal.makeRational(a._1 * b._1, a._2 * b._2)

  private def ratDiv(a: (Long, Long), b: (Long, Long)): SchemeVal =
    if b._1 == 0 then throw new EvalError("division by zero")
    SchemeVal.makeRational(a._1 * b._2, a._2 * b._1)

  def applyArithmetic(name: String, args: List[SchemeVal]): SchemeVal =
    name match
      case "+" =>
        if hasInexact(args) then SchemeVal.SFloat(args.map(toDouble).sum)
        else
          args.foldLeft(SchemeVal.SInt(0L): SchemeVal) { (acc, v) =>
            val a = toRational(acc); val b = toRational(v)
            ratAdd(a, b)
          }
      case "-" =>
        if args.isEmpty then throw new EvalError("-: expected at least 1 argument")
        if hasInexact(args) then
          val nums = args.map(toDouble)
          if nums.length == 1 then SchemeVal.SFloat(-nums.head)
          else SchemeVal.SFloat(nums.head - nums.tail.sum)
        else if args.length == 1 then
          val (n, d) = toRational(args.head)
          SchemeVal.makeRational(-n, d)
        else
          args.tail.foldLeft(args.head: SchemeVal) { (acc, v) =>
            ratSub(toRational(acc), toRational(v))
          }
      case "*" =>
        if hasInexact(args) then SchemeVal.SFloat(args.map(toDouble).product)
        else
          args.foldLeft(SchemeVal.SInt(1L): SchemeVal) { (acc, v) =>
            val a = toRational(acc); val b = toRational(v)
            ratMul(a, b)
          }
      case "/" =>
        if args.length < 2 then throw new EvalError("/: expected at least 2 arguments")
        if hasInexact(args) then
          val nums = args.map(toDouble)
          if nums.tail.contains(0.0) then throw new EvalError("division by zero")
          SchemeVal.SFloat(nums.tail.foldLeft(nums.head)(_ / _))
        else
          args.tail.foldLeft(args.head: SchemeVal) { (acc, v) =>
            ratDiv(toRational(acc), toRational(v))
          }
      case "abs" =>
        val arg = requireOne("abs", args)
        arg match
          case SchemeVal.SFloat(d)       => SchemeVal.SFloat(d.abs)
          case SchemeVal.SRational(n, d) => SchemeVal.makeRational(n.abs, d)
          case _                         => SchemeVal.SInt(Math.abs(asInt(arg)))
      case "modulo" =>
        val (a, b) = requireTwo("modulo", args)
        val x      = asInt(a); val y = asInt(b)
        SchemeVal.SInt(Math.floorMod(x, y))
      case "remainder" =>
        val (a, b) = requireTwo("remainder", args)
        val x      = asInt(a); val y = asInt(b)
        SchemeVal.SInt(x % y)
      case "quotient" =>
        val (a, b) = requireTwo("quotient", args)
        val x      = asInt(a); val y = asInt(b)
        SchemeVal.SInt(x / y)
      case "min" =>
        if args.isEmpty then throw new EvalError("min: expected at least 1 argument")
        if hasInexact(args) then SchemeVal.SFloat(args.map(toDouble).min)
        else SchemeVal.SInt(args.map(asInt).min)
      case "max" =>
        if args.isEmpty then throw new EvalError("max: expected at least 1 argument")
        if hasInexact(args) then SchemeVal.SFloat(args.map(toDouble).max)
        else SchemeVal.SInt(args.map(asInt).max)
      case "expt" =>
        val (a, b) = requireTwo("expt", args)
        val base   = asInt(a); val exp = asInt(b)
        SchemeVal.SInt(Math.pow(base.toDouble, exp.toDouble).toLong)
      case "gcd" =>
        if args.isEmpty then SchemeVal.SInt(0)
        else
          val ints                           = args.map(asInt)
          def gcdTwo(a: Long, b: Long): Long = if b == 0 then a.abs else gcdTwo(b, a % b)
          SchemeVal.SInt(ints.reduce((a, b) => gcdTwo(a, b)))
      case "lcm" =>
        if args.isEmpty then SchemeVal.SInt(1)
        else
          val ints                           = args.map(asInt)
          def gcdTwo(a: Long, b: Long): Long = if b == 0 then a.abs else gcdTwo(b, a % b)
          def lcmTwo(a: Long, b: Long): Long = if a == 0 && b == 0 then 0 else (a / gcdTwo(a, b) * b).abs
          SchemeVal.SInt(ints.reduce(lcmTwo))
      case "truncate" =>
        val v = requireOne("truncate", args)
        v match
          case SchemeVal.SInt(_)         => v
          case SchemeVal.SFloat(d)       => SchemeVal.SInt(d.toLong)
          case SchemeVal.SRational(n, d) => SchemeVal.SInt(n / d)
          case _                         => throw new EvalError(s"truncate: expected number, got ${v.display}")
      case "round" =>
        val v = requireOne("round", args)
        v match
          case SchemeVal.SInt(_)         => v
          case SchemeVal.SFloat(d)       => SchemeVal.SInt(Math.round(d))
          case SchemeVal.SRational(n, d) => SchemeVal.SInt(Math.round(n.toDouble / d.toDouble))
          case _                         => throw new EvalError(s"round: expected number, got ${v.display}")
      case _ => throw new EvalError(s"unknown arithmetic op: $name")

  def applyComparison(name: String, args: List[SchemeVal]): SchemeVal =
    if args.length < 2 then throw new EvalError(s"$name: expected at least 2 arguments")
    val nums  = args.map(toDouble)
    val pairs = nums.zip(nums.tail)
    SchemeVal.SBool(name match
      case "="  => nums.forall(_ == nums.head)
      case "<"  => pairs.forall((a, b) => a < b)
      case ">"  => pairs.forall((a, b) => a > b)
      case "<=" => pairs.forall((a, b) => a <= b)
      case ">=" => pairs.forall((a, b) => a >= b)
      case _    => throw new EvalError(s"unknown comparison: $name"))

  def applyTypePredicate(name: String, args: List[SchemeVal]): SchemeVal =
    val arg = requireOne(name, args)
    SchemeVal.SBool((name, arg) match
      case ("string?", SchemeVal.SString(_, _))    => true
      case ("number?", _) if isNumeric(arg)        => true
      case ("boolean?", SchemeVal.SBool(_))        => true
      case ("symbol?", SchemeVal.SSymbol(_))       => true
      case ("char?", SchemeVal.SChar(_))           => true
      case ("integer?", SchemeVal.SInt(_))         => true
      case ("integer?", SchemeVal.SRational(_, _)) => false
      case ("integer?", SchemeVal.SFloat(d))       => d == d.toLong.toDouble && !d.isInfinite
      case ("integer?", _)                         => false
      case ("rational?", _) if isNumeric(arg)      => true
      case ("rational?", _)                        => false
      case ("string?" | "number?" | "boolean?" | "symbol?" | "char?", _) =>
        false
      case _ => throw new EvalError(s"unknown predicate: $name"))

  def applyNumericPredicate(name: String, args: List[SchemeVal]): SchemeVal =
    val arg = requireOne(name, args)
    val d   = toDouble(arg)
    SchemeVal.SBool(name match
      case "zero?"     => d == 0.0
      case "positive?" => d > 0.0
      case "negative?" => d < 0.0
      case "odd?"      => asInt(arg) % 2 != 0
      case "even?"     => asInt(arg) % 2 == 0
      case _           => throw new EvalError(s"unknown predicate: $name"))

  def applyCharOp(name: String, args: List[SchemeVal]): SchemeVal =
    def asChar(v: SchemeVal): Char = v match
      case SchemeVal.SChar(c) => c
      case other              => throw new EvalError(s"expected char, got ${other.display}")

    name match
      case "char-alphabetic?" =>
        SchemeVal.SBool(asChar(requireOne(name, args)).isLetter)
      case "char-numeric?" =>
        SchemeVal.SBool(asChar(requireOne(name, args)).isDigit)
      case "char-upcase" =>
        SchemeVal.SChar(asChar(requireOne(name, args)).toUpper)
      case "char-downcase" =>
        SchemeVal.SChar(asChar(requireOne(name, args)).toLower)
      case "char=?" =>
        val (a, b) = requireTwo(name, args)
        SchemeVal.SBool(asChar(a) == asChar(b))
      case "char<?" =>
        val (a, b) = requireTwo(name, args)
        SchemeVal.SBool(asChar(a) < asChar(b))
      case _ => throw new EvalError(s"unknown char op: $name")

  def applyExactness(name: String, args: List[SchemeVal]): SchemeVal =
    val arg = requireOne(name, args)
    name match
      case "exact?" =>
        SchemeVal.SBool(arg match
          case SchemeVal.SInt(_) | SchemeVal.SRational(_, _) => true
          case SchemeVal.SFloat(_)                           => false
          case _ => throw new EvalError(s"exact?: expected number, got ${arg.display}"))
      case "inexact?" =>
        SchemeVal.SBool(arg match
          case SchemeVal.SFloat(_)                           => true
          case SchemeVal.SInt(_) | SchemeVal.SRational(_, _) => false
          case _ => throw new EvalError(s"inexact?: expected number, got ${arg.display}"))
      case "exact->inexact" =>
        SchemeVal.SFloat(toDouble(arg))
      case "inexact->exact" =>
        arg match
          case SchemeVal.SInt(_) | SchemeVal.SRational(_, _) => arg
          case SchemeVal.SFloat(d) =>
            val bd    = BigDecimal(d)
            val scale = bd.scale
            if scale <= 0 then SchemeVal.SInt(d.toLong)
            else
              val pow = BigDecimal(10).pow(scale)
              val num = (bd * pow).toLongExact
              val den = pow.toLongExact
              SchemeVal.makeRational(num, den)
          case _ => throw new EvalError(s"inexact->exact: expected number, got ${arg.display}")
      case "numerator" =>
        arg match
          case SchemeVal.SInt(n)         => SchemeVal.SInt(n)
          case SchemeVal.SRational(n, _) => SchemeVal.SInt(n)
          case _                         => throw new EvalError(s"numerator: expected rational, got ${arg.display}")
      case "denominator" =>
        arg match
          case SchemeVal.SInt(_)         => SchemeVal.SInt(1)
          case SchemeVal.SRational(_, d) => SchemeVal.SInt(d)
          case _                         => throw new EvalError(s"denominator: expected rational, got ${arg.display}")
      case _ => throw new EvalError(s"unknown exactness op: $name")
