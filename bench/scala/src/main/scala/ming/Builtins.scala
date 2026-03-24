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
    case (Vector(e1), Vector(e2))     => e1.length == e2.length && e1.zip(e2).forall((x, y) => schemeEqual(x, y))
    case _                            => a == b

  lazy val all: List[(String, Val)] =
    core ++ NumericBuiltins.all ++ ListBuiltins.all ++ StringBuiltins.all ++ vectorBuiltins

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
    "procedure?" -> Builtin {
      case List(Builtin(_)) => Bool(true)
      case List(_)          => Bool(false)
      case _                => throw new EvalError("procedure? requires 1 argument")
    }
  )

  private val vectorBuiltins: List[(String, Val)] = List(
    "vector" -> Builtin { args =>
      Vector(args.toArray)
    },
    "make-vector" -> Builtin {
      case List(Num(n))       => Vector(Array.fill(n.toInt)(Num(0)))
      case List(Num(n), fill) => Vector(Array.fill(n.toInt)(fill))
      case _                  => throw new EvalError("make-vector requires 1-2 arguments")
    },
    "vector-ref" -> Builtin {
      case List(Vector(elems), Num(idx)) =>
        if idx < 0 || idx >= elems.length then throw new EvalError("vector-ref: index out of range")
        elems(idx.toInt)
      case _ => throw new EvalError("vector-ref requires a vector and an integer")
    },
    "vector-set!" -> Builtin {
      case List(Vector(elems), Num(idx), value) =>
        if idx < 0 || idx >= elems.length then throw new EvalError("vector-set!: index out of range")
        elems(idx.toInt) = value
        Void
      case _ => throw new EvalError("vector-set! requires a vector, an integer, and a value")
    },
    "vector-length" -> Builtin {
      case List(Vector(elems)) => Num(elems.length.toLong)
      case _                   => throw new EvalError("vector-length requires a vector")
    },
    "vector?" -> Builtin {
      case List(Vector(_)) => Bool(true)
      case List(_)         => Bool(false)
      case _               => throw new EvalError("vector? requires 1 argument")
    },
    "vector->list" -> Builtin {
      case List(Vector(elems)) =>
        elems.foldRight(Nil: Val)((a, acc) => Pair(a, acc))
      case _ => throw new EvalError("vector->list requires a vector")
    },
    "list->vector" -> Builtin {
      case List(lst) =>
        val elems = scala.collection.mutable.ArrayBuffer[Val]()
        var cur   = lst
        while cur != Nil do
          cur match
            case Pair(car, cdr) => elems += car; cur = cdr
            case _              => throw new EvalError("list->vector: not a proper list")
        Vector(elems.toArray)
      case _ => throw new EvalError("list->vector requires a list")
    }
  )
