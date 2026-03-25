package ming

/** Vector built-in operations. */
object VectorOps:

  private def requireTwo(name: String, args: List[SchemeVal]): (SchemeVal, SchemeVal) =
    if args.length != 2 then throw new EvalError(s"$name: expected 2 arguments")
    (args(0), args(1))

  def apply(name: String, args: List[SchemeVal]): SchemeVal =
    name match
      case "vector" =>
        SchemeVal.SVector(args.toArray)
      case "make-vector" =>
        args match
          case SchemeVal.SInt(n) :: Nil =>
            SchemeVal.SVector(Array.fill(n.toInt)(SchemeVal.SInt(0)))
          case SchemeVal.SInt(n) :: fill :: Nil =>
            SchemeVal.SVector(Array.fill(n.toInt)(fill))
          case _ => throw new EvalError("make-vector: expected (size) or (size fill)")
      case "vector-ref" =>
        val (v, idx) = requireTwo("vector-ref", args)
        (v, idx) match
          case (SchemeVal.SVector(elems), SchemeVal.SInt(n)) =>
            val i = n.toInt
            if i < 0 || i >= elems.length then throw new EvalError("vector-ref: index out of range")
            elems(i)
          case (SchemeVal.SVector(_), _) => throw new EvalError("vector-ref: expected integer index")
          case _                         => throw new EvalError("vector-ref: expected vector")
      case "vector-set!" =>
        if args.length != 3 then throw new EvalError("vector-set!: expected 3 arguments")
        (args(0), args(1)) match
          case (SchemeVal.SVector(elems), SchemeVal.SInt(n)) =>
            val i = n.toInt
            if i < 0 || i >= elems.length then throw new EvalError("vector-set!: index out of range")
            elems(i) = args(2)
            SchemeVal.SVoid
          case (SchemeVal.SVector(_), _) => throw new EvalError("vector-set!: expected integer index")
          case _                         => throw new EvalError("vector-set!: expected vector")
      case "vector-length" =>
        if args.length != 1 then throw new EvalError("vector-length: expected 1 argument")
        args.head match
          case SchemeVal.SVector(elems) => SchemeVal.SInt(elems.length.toLong)
          case _                        => throw new EvalError("vector-length: expected vector")
      case "vector?" =>
        if args.length != 1 then throw new EvalError("vector?: expected 1 argument")
        val isVec = args.head match
          case _: SchemeVal.SVector => true
          case _                    => false
        SchemeVal.SBool(isVec)
      case "vector->list" =>
        if args.length != 1 then throw new EvalError("vector->list: expected 1 argument")
        args.head match
          case SchemeVal.SVector(elems) =>
            if elems.isEmpty then SchemeVal.SList(Nil)
            else SchemeVal.buildList(elems.toList)
          case _ => throw new EvalError("vector->list: expected vector")
      case "list->vector" =>
        if args.length != 1 then throw new EvalError("list->vector: expected 1 argument")
        val elems = SchemeVal
          .toScalaList(args.head)
          .getOrElse(
            throw new EvalError("list->vector: expected list")
          )
        SchemeVal.SVector(elems.toArray)
      case _ => throw new EvalError(s"unknown vector op: $name")
