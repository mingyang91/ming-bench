package ming

/** List built-in operations. */
object ListOps:

  private def asInt(v: SchemeVal): Long = v match
    case SchemeVal.SInt(n) => n
    case other             => throw new EvalError(s"expected number, got ${other.display}")

  private def requireOne(name: String, args: List[SchemeVal]): SchemeVal =
    if args.length != 1 then throw new EvalError(s"$name: expected 1 argument")
    args.head

  private def requireTwo(
    name: String,
    args: List[SchemeVal]
  ): (SchemeVal, SchemeVal) =
    if args.length != 2 then throw new EvalError(s"$name: expected 2 arguments")
    (args(0), args(1))

  def apply(name: String, args: List[SchemeVal]): SchemeVal =
    name match
      case "cons" =>
        if args.length != 2 then throw new EvalError("cons: expected 2 arguments")
        args(1) match
          case SchemeVal.SList(elems) => SchemeVal.SList(args(0) :: elems)
          case other                  => SchemeVal.SPair(args(0), other)
      case "car" =>
        requireOne("car", args) match
          case SchemeVal.SList(head :: _) => head
          case SchemeVal.SPair(a, _)      => a
          case _                          => throw new EvalError("car: expected pair")
      case "cdr" =>
        requireOne("cdr", args) match
          case SchemeVal.SList(_ :: tail) => SchemeVal.SList(tail)
          case SchemeVal.SPair(_, d)      => d
          case _                          => throw new EvalError("cdr: expected pair")
      case "null?" =>
        SchemeVal.SBool(requireOne("null?", args) match
          case SchemeVal.SList(Nil) => true
          case _                    => false)
      case "list" => SchemeVal.SList(args)
      case "length" =>
        requireOne("length", args) match
          case SchemeVal.SList(elems) => SchemeVal.SInt(elems.length.toLong)
          case _                      => throw new EvalError("length: expected list")
      case "append" =>
        val lists = args.map {
          case SchemeVal.SList(elems) => elems
          case other =>
            throw new EvalError(
              s"append: expected list, got ${other.display}"
            )
        }
        SchemeVal.SList(lists.flatten)
      case "pair?" =>
        SchemeVal.SBool(requireOne("pair?", args) match
          case SchemeVal.SList(elems) => elems.nonEmpty
          case SchemeVal.SPair(_, _)  => true
          case _                      => false)
      case "list-ref" =>
        val (lst, idx) = requireTwo("list-ref", args)
        lst match
          case SchemeVal.SList(elems) =>
            val i = asInt(idx).toInt
            if i < 0 || i >= elems.length then throw new EvalError("list-ref: index out of range")
            elems(i)
          case _ => throw new EvalError("list-ref: expected list")
      case "list-tail" =>
        val (lst, idx) = requireTwo("list-tail", args)
        lst match
          case SchemeVal.SList(elems) =>
            val i = asInt(idx).toInt
            if i < 0 || i > elems.length then throw new EvalError("list-tail: index out of range")
            SchemeVal.SList(elems.drop(i))
          case _ => throw new EvalError("list-tail: expected list")
      case "list?" =>
        SchemeVal.SBool(requireOne("list?", args) match
          case SchemeVal.SList(_) => true
          case _                  => false)
      case "assoc" =>
        val (key, lst) = requireTwo("assoc", args)
        lst match
          case SchemeVal.SList(elems) =>
            elems
              .collectFirst {
                case e @ SchemeVal.SList(k :: _) if Builtins.schemeEqual(k, key) => e
              }
              .getOrElse(SchemeVal.SBool(false))
          case _ => throw new EvalError("assoc: expected list")
      case _ => throw new EvalError(s"unknown list op: $name")
