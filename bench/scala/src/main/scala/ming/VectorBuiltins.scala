package ming

/** Vector-related built-in procedures extracted from ListBuiltins. */
private[ming] object VectorBuiltins:

  import Builtins.typeCheck

  def install(env: Env): Unit =
    env.set(
      "vector",
      SchemeBuiltin("vector", args => new SchemeVector(args.toArray))
    )
    env.set(
      "make-vector",
      SchemeBuiltin(
        "make-vector",
        args =>
          if args.size < 1 || args.size > 2 then throw new EvalError("make-vector: expected 1-2 arguments")
          val len  = Builtins.asLong(args(0), "make-vector").toInt
          val fill = if args.size == 2 then args(1) else SchemeInt(0)
          new SchemeVector(Array.fill(len)(fill))
      )
    )
    env.set(
      "vector-ref",
      SchemeBuiltin(
        "vector-ref",
        args =>
          if args.size != 2 then throw new EvalError("vector-ref: expected 2 arguments")
          args(0) match
            case v: SchemeVector =>
              val i = Builtins.asLong(args(1), "vector-ref").toInt
              if i < 0 || i >= v.elems.length then throw new EvalError("vector-ref: index out of bounds")
              v.elems(i)
            case _ => throw new EvalError("vector-ref: expected vector")
      )
    )
    env.set(
      "vector-set!",
      SchemeBuiltin(
        "vector-set!",
        args =>
          if args.size != 3 then throw new EvalError("vector-set!: expected 3 arguments")
          args(0) match
            case v: SchemeVector =>
              val i = Builtins.asLong(args(1), "vector-set!").toInt
              if i < 0 || i >= v.elems.length then throw new EvalError("vector-set!: index out of bounds")
              v.elems(i) = args(2)
              SchemeVoid
            case _ => throw new EvalError("vector-set!: expected vector")
      )
    )
    env.set(
      "vector-length",
      SchemeBuiltin(
        "vector-length",
        args =>
          if args.size != 1 then throw new EvalError("vector-length: expected 1 argument")
          args.head match
            case v: SchemeVector => SchemeInt(v.elems.length.toLong)
            case _               => throw new EvalError("vector-length: expected vector")
      )
    )
    env.set(
      "vector?",
      typeCheck("vector?") { case _: SchemeVector => true; case _ => false }
    )
    env.set(
      "vector->list",
      SchemeBuiltin(
        "vector->list",
        args =>
          if args.size != 1 then throw new EvalError("vector->list: expected 1 argument")
          args.head match
            case v: SchemeVector => SchemeList(v.elems.toList)
            case _               => throw new EvalError("vector->list: expected vector")
      )
    )
    env.set(
      "list->vector",
      SchemeBuiltin(
        "list->vector",
        args =>
          if args.size != 1 then throw new EvalError("list->vector: expected 1 argument")
          args.head match
            case SchemeList(elems) => new SchemeVector(elems.toArray)
            case _                 => throw new EvalError("list->vector: expected list")
      )
    )
