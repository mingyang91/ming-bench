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
    },
    // --- L05: I/O ---
    "display" -> Builtin {
      case List(v) =>
        Evaluator.outputBuffer.append(Evaluator.displayVal(v))
        Void
      case _ => throw new EvalError("display requires 1 argument")
    },
    "write" -> Builtin {
      case List(v) =>
        Evaluator.outputBuffer.append(Evaluator.display(v))
        Void
      case _ => throw new EvalError("write requires 1 argument")
    },
    "newline" -> Builtin {
      case scala.List() =>
        Evaluator.outputBuffer.append("\n")
        Void
      case _ => throw new EvalError("newline requires 0 arguments")
    },
    // --- L05: String operations ---
    "string-append" -> Builtin { args =>
      val strs = args.map {
        case Str(s) => s
        case v      => throw new EvalError(s"string-append: not a string: ${Evaluator.display(v)}")
      }
      Str(strs.mkString)
    },
    "string-length" -> Builtin {
      case List(Str(s)) => Num(s.length.toLong)
      case _            => throw new EvalError("string-length requires 1 string argument")
    },
    "substring" -> Builtin {
      case List(Str(s), Num(start), Num(end)) =>
        Str(s.substring(start.toInt, end.toInt))
      case _ => throw new EvalError("substring requires a string and two integers")
    },
    "string->number" -> Builtin {
      case List(Str(s)) =>
        try Num(s.toLong)
        catch case _: NumberFormatException => Bool(false)
      case _ => throw new EvalError("string->number requires 1 string argument")
    },
    "number->string" -> Builtin {
      case List(Num(n)) => Str(n.toString)
      case _            => throw new EvalError("number->string requires 1 numeric argument")
    },
    "symbol->string" -> Builtin {
      case List(Symbol(name)) => Str(name)
      case _                  => throw new EvalError("symbol->string requires 1 symbol argument")
    },
    "string->symbol" -> Builtin {
      case List(Str(s)) => Symbol(s)
      case _            => throw new EvalError("string->symbol requires 1 string argument")
    },
    "string-ref" -> Builtin {
      case List(Str(s), Num(i)) => SchemeChar(s.charAt(i.toInt))
      case _                    => throw new EvalError("string-ref requires a string and an integer")
    },
    "char?" -> Builtin {
      case List(SchemeChar(_)) => Bool(true)
      case List(_)             => Bool(false)
      case _                   => throw new EvalError("char? requires 1 argument")
    }
  )
