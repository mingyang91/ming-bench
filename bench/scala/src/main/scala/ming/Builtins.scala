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
    }
  )
