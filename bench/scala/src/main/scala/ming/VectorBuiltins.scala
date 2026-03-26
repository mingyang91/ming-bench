package ming

import SchemeTypes.{errAt, pairToScalaList, schemeList, Pos, Value}

private[ming] object VectorBuiltins:

  def applyVectorOps(
    name: String,
    args: List[Value],
    pos: Pos
  ): Value = name match
    case "vector" => Value.VVector(args.toArray)
    case "make-vector" =>
      args match
        case Value.VNum(n) :: Nil         => Value.VVector(Array.fill(n.toInt)(Value.VNum(0)))
        case Value.VNum(n) :: fill :: Nil => Value.VVector(Array.fill(n.toInt)(fill))
        case _                            => throw errAt(pos, "make-vector: invalid arguments")
    case "vector-ref" =>
      if args.length != 2 then throw errAt(pos, "vector-ref requires 2 arguments")
      (args(0), args(1)) match
        case (Value.VVector(elems), Value.VNum(i)) => elems(i.toInt)
        case _                                     => throw errAt(pos, "vector-ref: invalid arguments")
    case "vector-set!" =>
      if args.length != 3 then throw errAt(pos, "vector-set! requires 3 arguments")
      (args(0), args(1)) match
        case (Value.VVector(elems), Value.VNum(i)) =>
          elems(i.toInt) = args(2)
          Value.VVoid
        case _ => throw errAt(pos, "vector-set!: invalid arguments")
    case "vector-length" =>
      if args.length != 1 then throw errAt(pos, "vector-length requires 1 argument")
      args.head match
        case Value.VVector(elems) => Value.VNum(elems.length.toLong)
        case _                    => throw errAt(pos, "vector-length: not a vector")
    case "vector?" =>
      if args.length != 1 then throw errAt(pos, "vector? requires 1 argument")
      Value.VBool(args.head match
        case _: Value.VVector => true
        case _                => false)
    case "vector->list" =>
      if args.length != 1 then throw errAt(pos, "vector->list requires 1 argument")
      args.head match
        case Value.VVector(elems) => schemeList(elems.toList)
        case _                    => throw errAt(pos, "vector->list: not a vector")
    case "list->vector" =>
      if args.length != 1 then throw errAt(pos, "list->vector requires 1 argument")
      args.head match
        case Value.VList(elems) => Value.VVector(elems.toArray)
        case Value.VPair(_) =>
          val elems = pairToScalaList(args.head, pos)
          Value.VVector(elems.toArray)
        case _ => throw errAt(pos, "list->vector: not a list")
    case _ => throw errAt(pos, s"unknown vector op: $name")
