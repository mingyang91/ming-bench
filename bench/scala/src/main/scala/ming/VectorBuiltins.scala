package ming

object VectorBuiltins:

  def vectorBuiltins: List[(String, SchemeVal)] = List(
    "vector" -> SchemeVal.BuiltinProc("vector", args => SchemeVal.VectorVal(args.toArray)),
    "make-vector" -> SchemeVal.BuiltinProc(
      "make-vector",
      {
        case List(SchemeVal.IntVal(n)) =>
          SchemeVal.VectorVal(Array.fill(n.toInt)(SchemeVal.IntVal(0)))
        case List(SchemeVal.IntVal(n), fill) =>
          SchemeVal.VectorVal(Array.fill(n.toInt)(fill))
        case _ => throw new EvalError("make-vector: invalid arguments")
      }
    ),
    "vector-ref" -> SchemeVal.BuiltinProc(
      "vector-ref",
      {
        case List(SchemeVal.VectorVal(elems), SchemeVal.IntVal(i)) =>
          elems(i.toInt)
        case List(v, SchemeVal.IntVal(i)) =>
          throw new EvalError(s"vector-ref: expected vector, got ${v.display}")
        case List(SchemeVal.VectorVal(_), idx) =>
          throw new EvalError(s"vector-ref: expected integer index, got ${idx.display}")
        case args => throw new EvalError(s"vector-ref: invalid arguments (${args.length} args)")
      }
    ),
    "vector-set!" -> SchemeVal.BuiltinProc(
      "vector-set!",
      {
        case List(SchemeVal.VectorVal(elems), SchemeVal.IntVal(i), v) =>
          elems(i.toInt) = v
          SchemeVal.Void
        case _ => throw new EvalError("vector-set!: invalid arguments")
      }
    ),
    "vector-length" -> SchemeVal.BuiltinProc(
      "vector-length",
      {
        case List(SchemeVal.VectorVal(elems)) => SchemeVal.IntVal(elems.length.toLong)
        case _                                => throw new EvalError("vector-length: invalid arguments")
      }
    ),
    Builtins.typePredicate("vector?", _.isInstanceOf[SchemeVal.VectorVal]),
    "vector->list" -> SchemeVal.BuiltinProc(
      "vector->list",
      {
        case List(SchemeVal.VectorVal(elems)) => SchemeVal.schemeList(elems.toList)
        case _                                => throw new EvalError("vector->list: invalid arguments")
      }
    ),
    "list->vector" -> SchemeVal.BuiltinProc(
      "list->vector",
      {
        case List(v @ (SchemeVal.PairVal(_) | SchemeVal.ListVal(_))) =>
          SchemeVal.VectorVal(SchemeVal.toScalaList(v).toArray)
        case _ => throw new EvalError("list->vector: invalid arguments")
      }
    ),
    "vector-fill!" -> SchemeVal.BuiltinProc(
      "vector-fill!",
      {
        case List(SchemeVal.VectorVal(elems), fill) =>
          for i <- elems.indices do elems(i) = fill
          SchemeVal.Void
        case _ => throw new EvalError("vector-fill!: invalid arguments")
      }
    )
  )

  def equalityBuiltins: List[(String, SchemeVal)] = List(
    "equal?" -> SchemeVal.BuiltinProc(
      "equal?",
      {
        case List(a, b) => SchemeVal.BoolVal(Equality.equalCheck(a, b))
        case args       => throw new EvalError(s"equal?: expected 2 arguments, got ${args.length}")
      }
    ),
    "eqv?" -> SchemeVal.BuiltinProc(
      "eqv?",
      {
        case List(a, b) => SchemeVal.BoolVal(Equality.eqvCheck(a, b))
        case args       => throw new EvalError(s"eqv?: expected 2 arguments, got ${args.length}")
      }
    ),
    "eq?" -> SchemeVal.BuiltinProc(
      "eq?",
      {
        case List(a, b) => SchemeVal.BoolVal(SchemeVal.schemeEq(a, b))
        case args => throw new EvalError(s"eq?: expected 2 arguments, got ${args.length}")
      }
    )
  )
