package ming

import Evaluator.Val
import Evaluator.Val.*

/** Built-in procedures for the default environment. */
object Builtins:

  private[ming] def requireNum(v: Val): Long = v match
    case Num(n) => n
    case _      => throw new EvalError(s"not a number: ${Display.write(v)}")

  private[ming] def numericArgs(args: List[Val]): List[Long] = args.map(requireNum)

  private[ming] def schemeEqual(a: Val, b: Val): Boolean = (a, b) match
    case (Str(c1), Str(c2))           => java.util.Arrays.equals(c1, c2)
    case (Pair(a1, d1), Pair(a2, d2)) => schemeEqual(a1, a2) && schemeEqual(d1, d2)
    case _                            => a == b

  lazy val all: List[(String, Val)] =
    core ++ ListBuiltins.all ++ StringBuiltins.all

  private val core: List[(String, Val)] = List(
    "+" -> Builtin { args =>
      Num(numericArgs(args).sum)
    },
    "-" -> Builtin { args =>
      val nums = numericArgs(args)
      if nums.length == 1 then Num(-nums.head)
      else Num(nums.reduce(_ - _))
    },
    "*" -> Builtin { args =>
      Num(numericArgs(args).product)
    },
    "/" -> Builtin { args =>
      val nums = numericArgs(args)
      if nums.length < 2 then throw new EvalError("/ requires at least 2 arguments")
      if nums.tail.contains(0L) then throw new EvalError("division by zero")
      Num(nums.reduce(_ / _))
    },
    "<" -> Builtin { args =>
      Bool(numericArgs(args).sliding(2).forall { case Seq(a, b) => a < b; case _ => true })
    },
    ">" -> Builtin { args =>
      Bool(numericArgs(args).sliding(2).forall { case Seq(a, b) => a > b; case _ => true })
    },
    "=" -> Builtin { args =>
      Bool(numericArgs(args).sliding(2).forall { case Seq(a, b) => a == b; case _ => true })
    },
    "<=" -> Builtin { args =>
      Bool(
        numericArgs(args).sliding(2).forall { case Seq(a, b) => a <= b; case _ => true }
      )
    },
    ">=" -> Builtin { args =>
      Bool(
        numericArgs(args).sliding(2).forall { case Seq(a, b) => a >= b; case _ => true }
      )
    },
    "not" -> Builtin {
      case List(Bool(false)) => Bool(true)
      case List(_)           => Bool(false)
      case _                 => throw new EvalError("not requires 1 argument")
    },
    "cons" -> Builtin {
      case List(a, b) => Pair(a, b)
      case args       => throw new EvalError(s"cons requires 2 arguments, got ${args.length}")
    },
    "car" -> Builtin {
      case List(Pair(a, _)) => a
      case List(other)      => throw new EvalError(s"car: not a pair: ${Display.write(other)}")
      case _                => throw new EvalError("car requires 1 argument")
    },
    "cdr" -> Builtin {
      case List(Pair(_, d)) => d
      case List(other)      => throw new EvalError(s"cdr: not a pair: ${Display.write(other)}")
      case _                => throw new EvalError("cdr requires 1 argument")
    },
    "list" -> Builtin { args =>
      args.foldRight(Nil: Val)((a, acc) => Pair(a, acc))
    },
    "null?" -> Builtin {
      case List(Nil) => Bool(true)
      case List(_)   => Bool(false)
      case _         => throw new EvalError("null? requires 1 argument")
    },
    "pair?" -> Builtin {
      case List(Pair(_, _)) => Bool(true)
      case List(_)          => Bool(false)
      case _                => throw new EvalError("pair? requires 1 argument")
    },
    "number?" -> Builtin {
      case List(Num(_)) => Bool(true)
      case List(_)      => Bool(false)
      case _            => throw new EvalError("number? requires 1 argument")
    },
    "boolean?" -> Builtin {
      case List(Bool(_)) => Bool(true)
      case List(_)       => Bool(false)
      case _             => throw new EvalError("boolean? requires 1 argument")
    },
    "string?" -> Builtin {
      case List(Str(_)) => Bool(true)
      case List(_)      => Bool(false)
      case _            => throw new EvalError("string? requires 1 argument")
    },
    "symbol?" -> Builtin {
      case List(Symbol(_)) => Bool(true)
      case List(_)         => Bool(false)
      case _               => throw new EvalError("symbol? requires 1 argument")
    },
    "equal?" -> Builtin {
      case List(a, b) => Bool(schemeEqual(a, b))
      case _          => throw new EvalError("equal? requires 2 arguments")
    },
    "eq?" -> Builtin {
      case List(a, b) => Bool(a == b)
      case _          => throw new EvalError("eq? requires 2 arguments")
    },
    "eqv?" -> Builtin {
      case List(a, b) => Bool(a == b)
      case _          => throw new EvalError("eqv? requires 2 arguments")
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
      val nums = numericArgs(args)
      if nums.isEmpty then throw new EvalError("min requires at least 1 argument")
      Num(nums.min)
    },
    "max" -> Builtin { args =>
      val nums = numericArgs(args)
      if nums.isEmpty then throw new EvalError("max requires at least 1 argument")
      Num(nums.max)
    },
    "expt" -> Builtin {
      case List(Num(base), Num(exp)) =>
        if exp < 0 then throw new EvalError("expt: negative exponent")
        Num(math.pow(base.toDouble, exp.toDouble).toLong)
      case _ => throw new EvalError("expt requires 2 numeric arguments")
    },
    "zero?" -> Builtin {
      case List(Num(n)) => Bool(n == 0)
      case _            => throw new EvalError("zero? requires 1 numeric argument")
    },
    "positive?" -> Builtin {
      case List(Num(n)) => Bool(n > 0)
      case _            => throw new EvalError("positive? requires 1 numeric argument")
    },
    "negative?" -> Builtin {
      case List(Num(n)) => Bool(n < 0)
      case _            => throw new EvalError("negative? requires 1 numeric argument")
    },
    "odd?" -> Builtin {
      case List(Num(n)) => Bool(n % 2 != 0)
      case _            => throw new EvalError("odd? requires 1 numeric argument")
    },
    "even?" -> Builtin {
      case List(Num(n)) => Bool(n % 2 == 0)
      case _            => throw new EvalError("even? requires 1 numeric argument")
    }
  )
