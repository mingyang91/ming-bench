package ming

import Evaluator.Val
import Evaluator.Val.*

/** List operations: length, append, reverse, map, filter, apply, list?, list-ref, list-tail, assoc. */
object ListBuiltins:

  val all: List[(String, Val)] = List(
    "length" -> Builtin {
      case List(lst) =>
        var n   = 0L
        var cur = lst
        while cur != Nil do
          cur match
            case p: Pair => n += 1; cur = p.cdr
            case _       => throw new EvalError("length: not a proper list")
        Num(n)
      case _ => throw new EvalError("length requires 1 argument")
    },
    "append" -> Builtin { args =>
      def appendTwo(a: Val, b: Val): Val = a match
        case Nil            => b
        case Pair(car, cdr) => Pair(car, appendTwo(cdr, b))
        case _              => throw new EvalError("append: not a proper list")
      if args.isEmpty then Nil
      else args.init.foldRight(args.last)((a, acc) => appendTwo(a, acc))
    },
    "reverse" -> Builtin {
      case List(lst) =>
        var acc: Val = Nil
        var cur      = lst
        while cur != Nil do
          cur match
            case p: Pair => acc = Pair(p.car, acc); cur = p.cdr
            case _       => throw new EvalError("reverse: not a proper list")
        acc
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
            val result = Interpreter.applyFunc(func, List(car))
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
      Interpreter.applyFunc(func, allArgs)
    },
    "list?" -> Builtin {
      case List(v) =>
        // Floyd's cycle detection (tortoise-and-hare)
        def isList: Boolean =
          var slow: Val = v
          var fast: Val = v
          while true do
            // Move fast by 2
            fast match
              case f1: Pair =>
                f1.cdr match
                  case f2: Pair => fast = f2.cdr
                  case Nil      => return true
                  case _        => return false // improper
              case Nil => return true
              case _   => return false // not a list
            // Move slow by 1
            slow match
              case s: Pair => slow = s.cdr
              case _       => return true // shouldn't happen
            // Check cycle
            (slow, fast) match
              case (s: Pair, f: Pair) if s eq f => return false
              case _                            => ()
          false // unreachable
        Bool(isList)
      case _ => throw new EvalError("list? requires 1 argument")
    },
    "list-ref" -> Builtin {
      case List(lst, Num(idx)) =>
        var cur = lst
        var i   = idx
        while i > 0 do
          cur match
            case p: Pair => cur = p.cdr; i -= 1
            case _       => throw new EvalError("list-ref: index out of range")
        cur match
          case p: Pair => p.car
          case _       => throw new EvalError("list-ref: index out of range")
      case _ => throw new EvalError("list-ref requires a list and an integer")
    },
    "list-tail" -> Builtin {
      case List(lst, Num(idx)) =>
        var cur = lst
        var i   = idx
        while i > 0 do
          cur match
            case p: Pair => cur = p.cdr; i -= 1
            case _       => throw new EvalError("list-tail: index out of range")
        cur
      case _ => throw new EvalError("list-tail requires a list and an integer")
    },
    "assoc" -> Builtin {
      case List(key, lst) =>
        def search(v: Val): Val = v match
          case Nil => Bool(false)
          case p: Pair =>
            p.car match
              case entry @ Pair(k, _) =>
                if Builtins.schemeEqual(k, key) then entry else search(p.cdr)
              case _ => throw new EvalError("assoc: not a proper association list")
          case _ => throw new EvalError("assoc: not a proper list")
        search(lst)
      case _ => throw new EvalError("assoc requires 2 arguments")
    }
  )

  private def mapSingle(func: Val, lst: Val): Val = lst match
    case Nil => Nil
    case Pair(car, cdr) =>
      val mapped = Interpreter.applyFunc(func, List(car))
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
      Pair(Interpreter.applyFunc(func, cars), mapMulti(func, cdrs))

  private def toArgList(v: Val): List[Val] = v match
    case Nil            => List.empty
    case Pair(car, cdr) => car :: toArgList(cdr)
    case _              => throw new EvalError("apply: last argument must be a list")
