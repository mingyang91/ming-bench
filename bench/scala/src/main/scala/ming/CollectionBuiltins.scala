package ming

/** Map, equality, and higher-order list builtins. */
object CollectionBuiltins:

  def register(env: Env): Unit =
    registerMapBuiltin(env)
    registerEquality(env)
    registerVectors(env)

  private def registerMapBuiltin(env: Env): Unit =
    env.define(
      "map",
      SchemeVal.BuiltinProc(
        "map",
        args =>
          if args.size < 2 then throw new EvalError("map: expected at least 2 arguments")
          val proc = args.head
          val lists = args.tail.map {
            case SchemeVal.SList(elems) => elems
            case other =>
              throw new EvalError(s"map: expected list, got ${SchemeVal.display(other)}")
          }
          val len = lists.head.size
          val result = (0 until len).map { i =>
            val mapArgs = lists.map(_(i))
            Apply(proc, mapArgs)
          }.toList
          SchemeVal.SList(result)
      )
    )

  private def registerEquality(env: Env): Unit =
    env.define(
      "equal?",
      SchemeVal.BuiltinProc(
        "equal?",
        args =>
          if args.size != 2 then throw new EvalError("equal?: expected 2 arguments")
          SchemeVal.BoolVal(SchemeVal.schemeEqual(args(0), args(1)))
      )
    )
    env.define(
      "eq?",
      SchemeVal.BuiltinProc(
        "eq?",
        args =>
          if args.size != 2 then throw new EvalError("eq?: expected 2 arguments")
          (args(0), args(1)) match
            case (SchemeVal.Symbol(a), SchemeVal.Symbol(b))   => SchemeVal.BoolVal(a == b)
            case (SchemeVal.IntVal(a), SchemeVal.IntVal(b))   => SchemeVal.BoolVal(a == b)
            case (SchemeVal.BoolVal(a), SchemeVal.BoolVal(b)) => SchemeVal.BoolVal(a == b)
            case (SchemeVal.CharVal(a), SchemeVal.CharVal(b)) => SchemeVal.BoolVal(a == b)
            case (a, b)                                       => SchemeVal.BoolVal(a eq b)
      )
    )
    env.define(
      "eqv?",
      SchemeVal.BuiltinProc(
        "eqv?",
        args =>
          if args.size != 2 then throw new EvalError("eqv?: expected 2 arguments")
          (args(0), args(1)) match
            case (SchemeVal.Symbol(a), SchemeVal.Symbol(b)) => SchemeVal.BoolVal(a == b)
            case (SchemeVal.IntVal(a), SchemeVal.IntVal(b)) => SchemeVal.BoolVal(a == b)
            case (SchemeVal.RationalVal(n1, d1), SchemeVal.RationalVal(n2, d2)) =>
              SchemeVal.BoolVal(n1 == n2 && d1 == d2)
            case (SchemeVal.FloatVal(a), SchemeVal.FloatVal(b))   => SchemeVal.BoolVal(a == b)
            case (SchemeVal.BoolVal(a), SchemeVal.BoolVal(b))     => SchemeVal.BoolVal(a == b)
            case (SchemeVal.CharVal(a), SchemeVal.CharVal(b))     => SchemeVal.BoolVal(a == b)
            case (SchemeVal.StringVal(a), SchemeVal.StringVal(b)) => SchemeVal.BoolVal(a eq b)
            case (SchemeVal.SList(Nil), SchemeVal.SList(Nil))     => SchemeVal.BoolVal(true)
            case (a, b)                                           => SchemeVal.BoolVal(a eq b)
      )
    )

  private def registerVectors(env: Env): Unit =
    env.define("vector", SchemeVal.BuiltinProc("vector", args => SchemeVal.VectorVal(args.toArray)))
    env.define(
      "make-vector",
      SchemeVal.BuiltinProc(
        "make-vector",
        args =>
          if args.isEmpty || args.size > 2 then throw new EvalError("make-vector: expected 1-2 arguments")
          val k = args(0) match
            case SchemeVal.IntVal(n) => n.toInt
            case _                   => throw new EvalError("make-vector: expected integer")
          val fill = if args.size == 2 then args(1) else SchemeVal.IntVal(0)
          SchemeVal.VectorVal(Array.fill(k)(fill))
      )
    )
    env.define(
      "vector-ref",
      SchemeVal.BuiltinProc(
        "vector-ref",
        args =>
          if args.size != 2 then throw new EvalError("vector-ref: expected 2 arguments")
          (args(0), args(1)) match
            case (SchemeVal.VectorVal(elems), SchemeVal.IntVal(i)) =>
              if i < 0 || i >= elems.length then throw new EvalError("vector-ref: index out of bounds")
              elems(i.toInt)
            case _ => throw new EvalError("vector-ref: expected vector and integer")
      )
    )
    env.define(
      "vector-set!",
      SchemeVal.BuiltinProc(
        "vector-set!",
        args =>
          if args.size != 3 then throw new EvalError("vector-set!: expected 3 arguments")
          (args(0), args(1)) match
            case (SchemeVal.VectorVal(elems), SchemeVal.IntVal(i)) =>
              if i < 0 || i >= elems.length then throw new EvalError("vector-set!: index out of bounds")
              elems(i.toInt) = args(2)
              SchemeVal.Void
            case _ => throw new EvalError("vector-set!: expected vector and integer")
      )
    )
    env.define(
      "vector-length",
      SchemeVal.BuiltinProc(
        "vector-length",
        args =>
          if args.size != 1 then throw new EvalError("vector-length: expected 1 argument")
          args.head match
            case SchemeVal.VectorVal(elems) => SchemeVal.IntVal(elems.length.toLong)
            case _                          => throw new EvalError("vector-length: expected vector")
      )
    )
    env.define(
      "vector?",
      SchemeVal.BuiltinProc(
        "vector?",
        args =>
          if args.size != 1 then throw new EvalError("vector?: expected 1 argument")
          args.head match
            case SchemeVal.VectorVal(_) => SchemeVal.BoolVal(true)
            case _                      => SchemeVal.BoolVal(false)
      )
    )
    env.define(
      "vector->list",
      SchemeVal.BuiltinProc(
        "vector->list",
        args =>
          if args.size != 1 then throw new EvalError("vector->list: expected 1 argument")
          args.head match
            case SchemeVal.VectorVal(elems) => SchemeVal.SList(elems.toList)
            case _                          => throw new EvalError("vector->list: expected vector")
      )
    )
    env.define(
      "list->vector",
      SchemeVal.BuiltinProc(
        "list->vector",
        args =>
          if args.size != 1 then throw new EvalError("list->vector: expected 1 argument")
          args.head match
            case SchemeVal.SList(elems) => SchemeVal.VectorVal(elems.toArray)
            case _                      => throw new EvalError("list->vector: expected list")
      )
    )
