package ming

import Value.*

/** Built-in vector procedures. */
object BuiltinsVector:

  private def define(env: Env, entries: List[(String, List[Value] => Value)]): Unit =
    Builtins.define(env, entries)

  def register(env: Env): Unit =
    define(
      env,
      List(
        (
          "vector",
          args => VectorVal(args.toArray)
        ),
        (
          "make-vector",
          args =>
            args match
              case IntVal(n) :: Nil         => VectorVal(Array.fill(n.toInt)(IntVal(0)))
              case IntVal(n) :: fill :: Nil => VectorVal(Array.fill(n.toInt)(fill))
              case _                        => throw new EvalError("make-vector: expected (size) or (size, fill)")
        ),
        (
          "vector-ref",
          args =>
            args match
              case VectorVal(elems) :: IntVal(idx) :: Nil =>
                val i = idx.toInt
                if i < 0 || i >= elems.length then throw new EvalError(s"vector-ref: index $i out of range")
                elems(i)
              case _ => throw new EvalError("vector-ref: expected (vector, index)")
        ),
        (
          "vector-set!",
          args =>
            args match
              case VectorVal(elems) :: IntVal(idx) :: value :: Nil =>
                val i = idx.toInt
                if i < 0 || i >= elems.length then throw new EvalError(s"vector-set!: index $i out of range")
                elems(i) = value
                VoidVal
              case _ => throw new EvalError("vector-set!: expected (vector, index, value)")
        ),
        (
          "vector-length",
          args =>
            if args.length != 1 then throw new EvalError("vector-length: expected 1 argument")
            args.head match
              case VectorVal(elems) => IntVal(elems.length.toLong)
              case _                => throw new EvalError("vector-length: not a vector")
        ),
        (
          "vector?",
          args =>
            if args.length != 1 then throw new EvalError("vector?: expected 1 argument")
            BoolVal(args.head.isInstanceOf[VectorVal])
        ),
        (
          "vector->list",
          args =>
            if args.length != 1 then throw new EvalError("vector->list: expected 1 argument")
            args.head match
              case VectorVal(elems) =>
                elems.foldRight(NilVal: Value)((v, acc) => PairVal(v, acc))
              case _ => throw new EvalError("vector->list: not a vector")
        ),
        (
          "list->vector",
          args =>
            if args.length != 1 then throw new EvalError("list->vector: expected 1 argument")
            val items = EvalHelpers.valueToList(args.head)
            VectorVal(items.toArray)
        )
      )
    )
