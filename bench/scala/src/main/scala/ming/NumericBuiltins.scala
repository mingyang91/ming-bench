package ming

import Evaluator.Val
import Evaluator.Val.*

/** Numeric tower helpers and arithmetic/comparison builtins. */
object NumericBuiltins:

  // --- Numeric tower helpers ---
  sealed private trait SNum
  private case class SInt(n: Long)              extends SNum
  private case class SRat(num: Long, den: Long) extends SNum
  private case class SDbl(d: Double)            extends SNum

  private def toSNum(v: Val): SNum = v match
    case Num(n)         => SInt(n)
    case Rational(n, d) => SRat(n, d)
    case Inexact(d)     => SDbl(d)
    case _              => throw new EvalError(s"not a number: ${Display.write(v)}")

  private def fromSNum(s: SNum): Val = s match
    case SInt(n)    => Num(n)
    case SRat(n, d) => Evaluator.mkRational(n, d)
    case SDbl(d)    => Inexact(d)

  private def toDouble(s: SNum): Double = s match
    case SInt(n)    => n.toDouble
    case SRat(n, d) => n.toDouble / d.toDouble
    case SDbl(d)    => d

  private def toRat(s: SNum): (Long, Long) = s match
    case SInt(n)    => (n, 1L)
    case SRat(n, d) => (n, d)
    case SDbl(_)    => throw new EvalError("unexpected inexact in exact arithmetic")

  private def addSNum(a: SNum, b: SNum): SNum = (a, b) match
    case (SDbl(x), _) => SDbl(x + toDouble(b))
    case (_, SDbl(y)) => SDbl(toDouble(a) + y)
    case _ =>
      val (an, ad) = toRat(a); val (bn, bd) = toRat(b)
      SRat(an * bd + bn * ad, ad * bd)

  private def subSNum(a: SNum, b: SNum): SNum = (a, b) match
    case (SDbl(x), _) => SDbl(x - toDouble(b))
    case (_, SDbl(y)) => SDbl(toDouble(a) - y)
    case _ =>
      val (an, ad) = toRat(a); val (bn, bd) = toRat(b)
      SRat(an * bd - bn * ad, ad * bd)

  private def mulSNum(a: SNum, b: SNum): SNum = (a, b) match
    case (SDbl(x), _) => SDbl(x * toDouble(b))
    case (_, SDbl(y)) => SDbl(toDouble(a) * y)
    case _ =>
      val (an, ad) = toRat(a); val (bn, bd) = toRat(b)
      SRat(an * bn, ad * bd)

  private def divSNum(a: SNum, b: SNum): SNum = (a, b) match
    case (SDbl(x), _) => SDbl(x / toDouble(b))
    case (_, SDbl(y)) => SDbl(toDouble(a) / y)
    case _ =>
      val (an, ad) = toRat(a); val (bn, bd) = toRat(b)
      if bn == 0 then throw new EvalError("division by zero")
      SRat(an * bd, ad * bn)

  private def compareSNum(a: SNum, b: SNum): Int =
    toDouble(a).compare(toDouble(b))

  private def negateSNum(a: SNum): SNum = a match
    case SInt(n)    => SInt(-n)
    case SRat(n, d) => SRat(-n, d)
    case SDbl(d)    => SDbl(-d)

  val all: List[(String, Val)] = List(
    "+" -> Builtin { args =>
      val nums = args.map(toSNum)
      fromSNum(nums.foldLeft(SInt(0L): SNum)(addSNum))
    },
    "-" -> Builtin { args =>
      if args.isEmpty then throw new EvalError("- requires at least 1 argument")
      val nums = args.map(toSNum)
      if nums.length == 1 then fromSNum(negateSNum(nums.head))
      else fromSNum(nums.reduce(subSNum))
    },
    "*" -> Builtin { args =>
      val nums = args.map(toSNum)
      fromSNum(nums.foldLeft(SInt(1L): SNum)(mulSNum))
    },
    "/" -> Builtin { args =>
      if args.length < 2 then throw new EvalError("/ requires at least 2 arguments")
      val nums = args.map(toSNum)
      fromSNum(nums.reduce(divSNum))
    },
    "<" -> Builtin { args =>
      val nums = args.map(toSNum)
      Bool(nums.sliding(2).forall { case Seq(a, b) => compareSNum(a, b) < 0; case _ => true })
    },
    ">" -> Builtin { args =>
      val nums = args.map(toSNum)
      Bool(nums.sliding(2).forall { case Seq(a, b) => compareSNum(a, b) > 0; case _ => true })
    },
    "=" -> Builtin { args =>
      val nums = args.map(toSNum)
      Bool(nums.sliding(2).forall { case Seq(a, b) => compareSNum(a, b) == 0; case _ => true })
    },
    "<=" -> Builtin { args =>
      val nums = args.map(toSNum)
      Bool(nums.sliding(2).forall { case Seq(a, b) => compareSNum(a, b) <= 0; case _ => true })
    },
    ">=" -> Builtin { args =>
      val nums = args.map(toSNum)
      Bool(nums.sliding(2).forall { case Seq(a, b) => compareSNum(a, b) >= 0; case _ => true })
    },
    "abs" -> Builtin {
      case List(Num(n)) => Num(math.abs(n))
      case _            => throw new EvalError("abs requires 1 numeric argument")
    },
    "modulo" -> Builtin {
      case List(Num(a), Num(b)) =>
        if b == 0 then throw new EvalError("modulo: division by zero")
        val r = a % b
        Num(if r != 0 && ((r > 0) != (b > 0)) then r + b else r)
      case _ => throw new EvalError("modulo requires 2 numeric arguments")
    },
    "remainder" -> Builtin {
      case List(Num(a), Num(b)) =>
        if b == 0 then throw new EvalError("remainder: division by zero")
        Num(a % b)
      case _ => throw new EvalError("remainder requires 2 numeric arguments")
    },
    "quotient" -> Builtin {
      case List(Num(a), Num(b)) =>
        if b == 0 then throw new EvalError("quotient: division by zero")
        Num(a / b)
      case _ => throw new EvalError("quotient requires 2 numeric arguments")
    },
    "min" -> Builtin { args =>
      if args.isEmpty then throw new EvalError("min requires at least 1 argument")
      val nums = args.map(toSNum)
      fromSNum(nums.reduce((a, b) => if compareSNum(a, b) <= 0 then a else b))
    },
    "max" -> Builtin { args =>
      if args.isEmpty then throw new EvalError("max requires at least 1 argument")
      val nums = args.map(toSNum)
      fromSNum(nums.reduce((a, b) => if compareSNum(a, b) >= 0 then a else b))
    },
    "expt" -> Builtin {
      case List(Num(base), Num(exp)) =>
        if exp < 0 then throw new EvalError("expt: negative exponent")
        Num(math.pow(base.toDouble, exp.toDouble).toLong)
      case _ => throw new EvalError("expt requires 2 numeric arguments")
    },
    "zero?" -> Builtin {
      case List(Num(n))     => Bool(n == 0)
      case List(Inexact(d)) => Bool(d == 0.0)
      case _                => throw new EvalError("zero? requires 1 numeric argument")
    },
    "positive?" -> Builtin {
      case List(Num(n))         => Bool(n > 0)
      case List(Rational(n, _)) => Bool(n > 0)
      case List(Inexact(d))     => Bool(d > 0.0)
      case _                    => throw new EvalError("positive? requires 1 numeric argument")
    },
    "negative?" -> Builtin {
      case List(Num(n))         => Bool(n < 0)
      case List(Rational(n, _)) => Bool(n < 0)
      case List(Inexact(d))     => Bool(d < 0.0)
      case _                    => throw new EvalError("negative? requires 1 numeric argument")
    },
    "odd?" -> Builtin {
      case List(Num(n)) => Bool(n % 2 != 0)
      case _            => throw new EvalError("odd? requires 1 numeric argument")
    },
    "even?" -> Builtin {
      case List(Num(n)) => Bool(n % 2 == 0)
      case _            => throw new EvalError("even? requires 1 numeric argument")
    },
    "exact->inexact" -> Builtin {
      case List(Num(n))         => Inexact(n.toDouble)
      case List(Rational(n, d)) => Inexact(n.toDouble / d.toDouble)
      case List(Inexact(d))     => Inexact(d)
      case _                    => throw new EvalError("exact->inexact requires 1 numeric argument")
    },
    "inexact->exact" -> Builtin {
      case List(Inexact(d)) =>
        if d == d.floor && !d.isInfinite then Num(d.toLong)
        else
          val str    = d.toString
          val dotIdx = str.indexOf('.')
          if dotIdx >= 0 then
            val decimals = str.length - dotIdx - 1
            val scale    = math.pow(10, decimals).toLong
            val num      = (d * scale).round
            Evaluator.mkRational(num, scale)
          else Num(d.toLong)
      case List(Num(n))         => Num(n)
      case List(Rational(n, d)) => Rational(n, d)
      case _                    => throw new EvalError("inexact->exact requires 1 numeric argument")
    },
    "numerator" -> Builtin {
      case List(Num(n))         => Num(n)
      case List(Rational(n, _)) => Num(n)
      case _                    => throw new EvalError("numerator requires 1 exact argument")
    },
    "denominator" -> Builtin {
      case List(Num(_))         => Num(1)
      case List(Rational(_, d)) => Num(d)
      case _                    => throw new EvalError("denominator requires 1 exact argument")
    },
    "number?" -> Builtin {
      case List(Num(_) | Rational(_, _) | Inexact(_)) => Bool(true)
      case List(_)                                    => Bool(false)
      case _                                          => throw new EvalError("number? requires 1 argument")
    },
    "integer?" -> Builtin {
      case List(Num(_))         => Bool(true)
      case List(Rational(_, _)) => Bool(false)
      case List(Inexact(d))     => Bool(d == d.floor && !d.isInfinite)
      case List(_)              => Bool(false)
      case _                    => throw new EvalError("integer? requires 1 argument")
    },
    "rational?" -> Builtin {
      case List(Num(_) | Rational(_, _)) => Bool(true)
      case List(_)                       => Bool(false)
      case _                             => throw new EvalError("rational? requires 1 argument")
    },
    "exact?" -> Builtin {
      case List(Num(_) | Rational(_, _)) => Bool(true)
      case List(Inexact(_))              => Bool(false)
      case List(_)                       => Bool(false)
      case _                             => throw new EvalError("exact? requires 1 argument")
    },
    "inexact?" -> Builtin {
      case List(Inexact(_))              => Bool(true)
      case List(Num(_) | Rational(_, _)) => Bool(false)
      case List(_)                       => Bool(false)
      case _                             => throw new EvalError("inexact? requires 1 argument")
    }
  )
