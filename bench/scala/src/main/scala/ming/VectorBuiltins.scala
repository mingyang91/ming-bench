package ming

import Evaluator.Val
import Evaluator.Val.*

object VectorBuiltins:

  val all: List[(String, Val)] = List(
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
            case p: Pair => elems += p.car; cur = p.cdr
            case _       => throw new EvalError("list->vector: not a proper list")
        Vector(elems.toArray)
      case _ => throw new EvalError("list->vector requires a list")
    },
    "vector-fill!" -> Builtin {
      case List(Vector(elems), fill) =>
        java.util.Arrays.fill(elems.asInstanceOf[Array[AnyRef]], fill)
        Void
      case _ => throw new EvalError("vector-fill! requires a vector and a value")
    },
    "vector-copy" -> Builtin {
      case List(Vector(elems)) => Vector(elems.clone())
      case _                   => throw new EvalError("vector-copy requires a vector")
    }
  )
