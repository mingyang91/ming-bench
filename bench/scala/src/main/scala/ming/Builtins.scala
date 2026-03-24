package ming

import Evaluator.Val
import Evaluator.Val.*

/** Built-in procedures for the default environment. */
object Builtins:

  private def requireNum(v: Val): Long = v match
    case Num(n) => n
    case _      => throw new EvalError(s"not a number: ${Evaluator.display(v)}")

  private def numericArgs(args: List[Val]): List[Long] = args.map(requireNum)

  val all: List[(String, Val)] = List(
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
      Bool(numericArgs(args).sliding(2).forall { case Seq(a, b) => a <= b; case _ => true })
    },
    ">=" -> Builtin { args =>
      Bool(numericArgs(args).sliding(2).forall { case Seq(a, b) => a >= b; case _ => true })
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
      case List(other)      => throw new EvalError(s"car: not a pair: ${Evaluator.display(other)}")
      case _                => throw new EvalError("car requires 1 argument")
    },
    "cdr" -> Builtin {
      case List(Pair(_, d)) => d
      case List(other)      => throw new EvalError(s"cdr: not a pair: ${Evaluator.display(other)}")
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
    "length" -> Builtin {
      case List(lst) =>
        @scala.annotation.tailrec
        def len(v: Val, n: Long): Long = v match
          case Nil          => n
          case Pair(_, cdr) => len(cdr, n + 1)
          case _            => throw new EvalError("length: not a proper list")
        Num(len(lst, 0))
      case _ => throw new EvalError("length requires 1 argument")
    },
    "append" -> Builtin { args =>
      def appendTwo(a: Val, b: Val): Val = a match
        case Nil            => b
        case Pair(car, cdr) => Pair(car, appendTwo(cdr, b))
        case _              => throw new EvalError("append: not a proper list")
      args.foldRight(Nil: Val)((a, acc) => appendTwo(a, acc))
    },
    "reverse" -> Builtin {
      case List(lst) =>
        @scala.annotation.tailrec
        def rev(v: Val, acc: Val): Val = v match
          case Nil            => acc
          case Pair(car, cdr) => rev(cdr, Pair(car, acc))
          case _              => throw new EvalError("reverse: not a proper list")
        rev(lst, Nil)
      case _ => throw new EvalError("reverse requires 1 argument")
    },
    "map" -> Builtin {
      case List(func, lst) =>
        def mapLoop(v: Val): Val = v match
          case Nil => Nil
          case Pair(car, cdr) =>
            val mapped = Evaluator.applyFunc(func, List(car))
            Pair(mapped, mapLoop(cdr))
          case _ => throw new EvalError("map: not a proper list")
        mapLoop(lst)
      case _ => throw new EvalError("map requires 2 arguments")
    },
    "filter" -> Builtin {
      case List(func, lst) =>
        def filterLoop(v: Val): Val = v match
          case Nil => Nil
          case Pair(car, cdr) =>
            val result = Evaluator.applyFunc(func, List(car))
            if result != Bool(false) then Pair(car, filterLoop(cdr))
            else filterLoop(cdr)
          case _ => throw new EvalError("filter: not a proper list")
        filterLoop(lst)
      case _ => throw new EvalError("filter requires 2 arguments")
    },
    "equal?" -> Builtin {
      case List(a, b) => Bool(a == b)
      case _          => throw new EvalError("equal? requires 2 arguments")
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
    }
  )
