package ming

import Evaluator.Val
import Evaluator.Val.*

/** Built-in procedures for the default environment. */
object Builtins:

  private[ming] def requireNum(v: Val): Long = v match
    case Num(n) => n
    case _      => throw new EvalError(s"not a number: ${Display.write(v)}")

  private[ming] def numericArgs(args: List[Val]): List[Long] = args.map(requireNum)

  private[ming] def schemeEqual(a: Val, b: Val): Boolean =
    schemeEqualImpl(a, b, 0)

  private def schemeEqualImpl(a: Val, b: Val, depth: Int): Boolean =
    if depth > 100000 then true // safety: avoid infinite recursion on cycles
    else
      (a, b) match
        case (p1: Pair, p2: Pair) =>
          if p1 eq p2 then true
          else schemeEqualImpl(p1.car, p2.car, depth + 1) && schemeEqualImpl(p1.cdr, p2.cdr, depth + 1)
        case (Str(c1), Str(c2)) => java.util.Arrays.equals(c1, c2)
        case (Vector(e1), Vector(e2)) =>
          e1.length == e2.length && e1.indices.forall(i => schemeEqualImpl(e1(i), e2(i), depth + 1))
        case _ => a == b

  private def pairCell(v: Val): Pair = v match
    case p: Pair => p
    case _       => throw new EvalError(s"not a pair: ${Display.write(v)}")

  /** Generic cXXXr: apply a/d operations right-to-left. e.g. "ad" for cadr = car of cdr */
  private def cxr(ops: String, v: Val): Val =
    var cur = v
    for c <- ops.reverse do
      cur = c match
        case 'a' => pairCell(cur).car
        case 'd' => pairCell(cur).cdr
        case _   => throw new EvalError(s"invalid cxr op: $c")
    cur

  /** Generate all cXXr, cXXXr, cXXXXr builtins. */
  private val cxrBuiltins: List[(String, Val)] =
    val ads = List("a", "d")
    val combos = for a <- ads; b <- ads
    yield s"$a$b"
    val combos3 = for a <- ads; b <- ads; c <- ads
    yield s"$a$b$c"
    val combos4 = for a <- ads; b <- ads; c <- ads; d <- ads
    yield s"$a$b$c$d"
    (combos ++ combos3 ++ combos4).map { ops =>
      val name = s"c${ops}r"
      name -> Builtin {
        case List(v) => cxr(ops, v)
        case _       => throw new EvalError(s"$name requires 1 argument")
      }
    }

  lazy val all: List[(String, Val)] =
    core ++ NumericBuiltins.all ++ ListBuiltins.all ++ StringBuiltins.all ++ VectorBuiltins.all ++ cxrBuiltins

  private val core: List[(String, Val)] = List(
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
      case List(p: Pair) => p.car
      case List(other)   => throw new EvalError(s"car: not a pair: ${Display.write(other)}")
      case _             => throw new EvalError("car requires 1 argument")
    },
    "cdr" -> Builtin {
      case List(p: Pair) => p.cdr
      case List(other)   => throw new EvalError(s"cdr: not a pair: ${Display.write(other)}")
      case _             => throw new EvalError("cdr requires 1 argument")
    },
    "set-car!" -> Builtin {
      case List(p: Pair, v) => p.car = v; Void
      case List(other, _)   => throw new EvalError(s"set-car!: not a pair: ${Display.write(other)}")
      case _                => throw new EvalError("set-car! requires 2 arguments")
    },
    "set-cdr!" -> Builtin {
      case List(p: Pair, v) => p.cdr = v; Void
      case List(other, _)   => throw new EvalError(s"set-cdr!: not a pair: ${Display.write(other)}")
      case _                => throw new EvalError("set-cdr! requires 2 arguments")
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
      case List(_: Pair) => Bool(true)
      case List(_)       => Bool(false)
      case _             => throw new EvalError("pair? requires 1 argument")
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
      case List(a, b) =>
        val result = (a, b) match
          case (p1: Pair, p2: Pair) => p1 eq p2
          case _                    => a == b
        Bool(result)
      case _ => throw new EvalError("eq? requires 2 arguments")
    },
    "eqv?" -> Builtin {
      case List(a, b) =>
        val result = (a, b) match
          case (p1: Pair, p2: Pair) => p1 eq p2
          case _                    => a == b
        Bool(result)
      case _ => throw new EvalError("eqv? requires 2 arguments")
    },
    "syntax->datum" -> Builtin {
      case List(v) => v // In our simplified representation, syntax objects are plain values
      case _       => throw new EvalError("syntax->datum requires 1 argument")
    },
    "datum->syntax" -> Builtin {
      case List(_, datum) => datum // context is ignored in our simplified implementation
      case _              => throw new EvalError("datum->syntax requires 2 arguments")
    },
    "procedure?" -> Builtin {
      case List(Builtin(_))          => Bool(true)
      case List(Closure(_, _, _, _)) => Bool(true)
      case List(_: ContinuationVal)  => Bool(true)
      case List(CallCCVal)           => Bool(true)
      case List(_)                   => Bool(false)
      case _                         => throw new EvalError("procedure? requires 1 argument")
    },
    "for-each" -> Builtin { args =>
      if args.length < 2 then throw new EvalError("for-each requires at least 2 arguments")
      val func  = args.head
      val lists = args.tail
      if lists.length == 1 then
        var cur = lists.head
        while cur != Nil do
          cur match
            case p: Pair =>
              Interpreter.applyFunc(func, List(p.car))
              cur = p.cdr
            case _ => throw new EvalError("for-each: not a proper list")
        Void
      else
        var curs = lists
        while !curs.exists(_ == Nil) do
          val cars = curs.map {
            case p: Pair => p.car
            case _       => throw new EvalError("for-each: not a proper list")
          }
          Interpreter.applyFunc(func, cars)
          curs = curs.map {
            case p: Pair => p.cdr
            case other   => other
          }
        Void
    },
    "memq" -> Builtin {
      case List(key, lst) =>
        def search(v: Val): Val = v match
          case Nil => Bool(false)
          case p: Pair =>
            val isEq = (p.car, key) match
              case (p1: Pair, p2: Pair) => p1 eq p2
              case _                    => p.car == key
            if isEq then p else search(p.cdr)
          case _ => throw new EvalError("memq: not a proper list")
        search(lst)
      case _ => throw new EvalError("memq requires 2 arguments")
    },
    "memv" -> Builtin {
      case List(key, lst) =>
        def search(v: Val): Val = v match
          case Nil => Bool(false)
          case p: Pair =>
            val isEq = (p.car, key) match
              case (p1: Pair, p2: Pair) => p1 eq p2
              case _                    => p.car == key
            if isEq then p else search(p.cdr)
          case _ => throw new EvalError("memv: not a proper list")
        search(lst)
      case _ => throw new EvalError("memv requires 2 arguments")
    },
    "assq" -> Builtin {
      case List(key, lst) =>
        def search(v: Val): Val = v match
          case Nil => Bool(false)
          case p: Pair =>
            p.car match
              case entry: Pair =>
                val isEq = (entry.car, key) match
                  case (p1: Pair, p2: Pair) => p1 eq p2
                  case _                    => entry.car == key
                if isEq then entry else search(p.cdr)
              case _ => throw new EvalError("assq: not a proper association list")
          case _ => throw new EvalError("assq: not a proper list")
        search(lst)
      case _ => throw new EvalError("assq requires 2 arguments")
    },
    "member" -> Builtin {
      case List(key, lst) =>
        def search(v: Val): Val = v match
          case Nil => Bool(false)
          case p: Pair =>
            if schemeEqual(p.car, key) then p else search(p.cdr)
          case _ => throw new EvalError("member: not a proper list")
        search(lst)
      case _ => throw new EvalError("member requires 2 arguments")
    },
    "assv" -> Builtin {
      case List(key, lst) =>
        def search(v: Val): Val = v match
          case Nil => Bool(false)
          case p: Pair =>
            p.car match
              case entry: Pair =>
                val isEq = (entry.car, key) match
                  case (p1: Pair, p2: Pair) => p1 eq p2
                  case _                    => entry.car == key
                if isEq then entry else search(p.cdr)
              case _ => throw new EvalError("assv: not a proper association list")
          case _ => throw new EvalError("assv: not a proper list")
        search(lst)
      case _ => throw new EvalError("assv requires 2 arguments")
    },
    "raise" -> Builtin {
      case List(v) => throw new Evaluator.SchemeRaise(v)
      case _       => throw new EvalError("raise requires 1 argument")
    },
    "values" -> Builtin { args =>
      args match
        case List(single) => single
        case _            => Val.MultipleValues(args)
    }
  )
