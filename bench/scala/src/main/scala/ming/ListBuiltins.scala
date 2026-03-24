package ming

import Evaluator.Val
import Evaluator.Val.*

/** List operations: length, append, reverse, map, filter, apply, list?, list-ref, list-tail, assoc. */
object ListBuiltins:

  val all: List[(String, Val)] = List(
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
    "map" -> Builtin { args =>
      if args.length < 2 then throw new EvalError("map requires at least 2 arguments")
      val func  = args.head
      val lists = args.tail
      if lists.length == 1 then mapSingle(func, lists.head)
      else mapMulti(func, lists)
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
    "apply" -> Builtin { args =>
      if args.length < 2 then throw new EvalError("apply requires at least 2 arguments")
      val func    = args.head
      val prefix  = args.slice(1, args.length - 1)
      val allArgs = prefix ++ toArgList(args.last)
      Evaluator.applyFunc(func, allArgs)
    },
    "list?" -> Builtin {
      case List(v) =>
        @scala.annotation.tailrec
        def isProperList(x: Val): Boolean = x match
          case Nil          => true
          case Pair(_, cdr) => isProperList(cdr)
          case _            => false
        Bool(isProperList(v))
      case _ => throw new EvalError("list? requires 1 argument")
    },
    "list-ref" -> Builtin {
      case List(lst, Num(idx)) =>
        @scala.annotation.tailrec
        def ref(v: Val, i: Long): Val = v match
          case Pair(car, cdr) => if i == 0 then car else ref(cdr, i - 1)
          case _              => throw new EvalError("list-ref: index out of range")
        ref(lst, idx)
      case _ => throw new EvalError("list-ref requires a list and an integer")
    },
    "list-tail" -> Builtin {
      case List(lst, Num(idx)) =>
        @scala.annotation.tailrec
        def tail(v: Val, i: Long): Val =
          if i == 0 then v
          else
            v match
              case Pair(_, cdr) => tail(cdr, i - 1)
              case _            => throw new EvalError("list-tail: index out of range")
        tail(lst, idx)
      case _ => throw new EvalError("list-tail requires a list and an integer")
    },
    "assoc" -> Builtin {
      case List(key, lst) =>
        @scala.annotation.tailrec
        def search(v: Val): Val = v match
          case Nil => Bool(false)
          case Pair(entry @ Pair(k, _), rest) =>
            if Builtins.schemeEqual(k, key) then entry else search(rest)
          case _ => throw new EvalError("assoc: not a proper association list")
        search(lst)
      case _ => throw new EvalError("assoc requires 2 arguments")
    }
  )

  private def mapSingle(func: Val, lst: Val): Val = lst match
    case Nil => Nil
    case Pair(car, cdr) =>
      val mapped = Evaluator.applyFunc(func, List(car))
      Pair(mapped, mapSingle(func, cdr))
    case _ => throw new EvalError("map: not a proper list")

  private def mapMulti(func: Val, lsts: List[Val]): Val =
    if lsts.exists(_ == Nil) then Nil
    else
      val cars = lsts.map {
        case Pair(car, _) => car
        case Nil          => throw new EvalError("map: lists of different lengths")
        case _            => throw new EvalError("map: not a proper list")
      }
      val cdrs = lsts.map {
        case Pair(_, cdr) => cdr
        case other        => other
      }
      Pair(Evaluator.applyFunc(func, cars), mapMulti(func, cdrs))

  private def toArgList(v: Val): List[Val] = v match
    case Nil            => List.empty
    case Pair(car, cdr) => car :: toArgList(cdr)
    case _              => throw new EvalError("apply: last argument must be a list")
