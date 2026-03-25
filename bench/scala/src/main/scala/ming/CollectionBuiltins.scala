package ming

/** Map, equality, and higher-order list builtins. */
object CollectionBuiltins:

  def register(env: Env): Unit =
    registerMapBuiltin(env)
    registerEquality(env)

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
