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
        SchemeVal.SPair(new MutableCell(args(0), args(1)))
      case "car" =>
        SchemeVal.pairCar(requireOne("car", args))
      case "cdr" =>
        SchemeVal.pairCdr(requireOne("cdr", args))
      case "null?" =>
        SchemeVal.SBool(requireOne("null?", args) match
          case SchemeVal.SList(Nil) => true
          case _                    => false)
      case "list" =>
        if args.isEmpty then SchemeVal.SList(Nil)
        else SchemeVal.buildList(args)
      case "length" =>
        val elems = SchemeVal
          .toScalaList(requireOne("length", args))
          .getOrElse(throw new EvalError("length: expected list"))
        SchemeVal.SInt(elems.length.toLong)
      case "append"                => applyAppend(args)
      case "pair?"                 => SchemeVal.SBool(SchemeVal.isPairLike(requireOne("pair?", args)))
      case "list-ref"              => applyListRef(args)
      case "list-tail"             => applyListTail(args)
      case "list?"                 => SchemeVal.SBool(SchemeVal.isList(requireOne("list?", args)))
      case "assoc"                 => applyAssoc(args)
      case "set-car!" | "set-cdr!" => applySetPair(name, args)
      case "caar"                  => SchemeVal.pairCar(SchemeVal.pairCar(requireOne("caar", args)))
      case "cadr"                  => SchemeVal.pairCar(SchemeVal.pairCdr(requireOne("cadr", args)))
      case "cdar"                  => SchemeVal.pairCdr(SchemeVal.pairCar(requireOne("cdar", args)))
      case "cddr"                  => SchemeVal.pairCdr(SchemeVal.pairCdr(requireOne("cddr", args)))
      case "caddr"                 => SchemeVal.pairCar(SchemeVal.pairCdr(SchemeVal.pairCdr(requireOne("caddr", args))))
      case "member"                => applyMember(args)
      case "assv"                  => applyAssv(args)
      case "assq"                  => applyAssq(args)
      case "memq"                  => applyMemq(args)
      case "memv"                  => applyMemv(args)
      case "reverse"               => applyReverse(args)
      case _                       => throw new EvalError(s"unknown list op: $name")

  private def applyAppend(args: List[SchemeVal]): SchemeVal =
    if args.isEmpty then SchemeVal.SList(Nil)
    else if args.length == 1 then args.head
    else
      // All but the last must be proper lists; the last can be any value
      val init = args.init
      val last = args.last
      val allElems = init.flatMap { v =>
        SchemeVal
          .toScalaList(v)
          .getOrElse(throw new EvalError(s"append: expected list, got ${v.display}"))
      }
      if allElems.isEmpty then last
      else
        // Build a chain ending with last
        allElems.foldRight(last) { (elem, acc) =>
          SchemeVal.SPair(new MutableCell(elem, acc))
        }

  private def applyListRef(args: List[SchemeVal]): SchemeVal =
    val (lst, idx) = requireTwo("list-ref", args)
    val elems = SchemeVal
      .toScalaList(lst)
      .getOrElse(throw new EvalError("list-ref: expected list"))
    val i = asInt(idx).toInt
    if i < 0 || i >= elems.length then throw new EvalError("list-ref: index out of range")
    elems(i)

  private def applyListTail(args: List[SchemeVal]): SchemeVal =
    val (lst, idx)     = requireTwo("list-tail", args)
    val i              = asInt(idx).toInt
    var cur: SchemeVal = lst
    var steps          = 0
    while steps < i do
      cur match
        case SchemeVal.SPair(c)      => cur = c.cdr
        case SchemeVal.SList(_ :: t) => cur = SchemeVal.SList(t)
        case _                       => throw new EvalError("list-tail: index out of range")
      steps += 1
    cur

  private def applyAssoc(args: List[SchemeVal]): SchemeVal =
    val (key, lst) = requireTwo("assoc", args)
    val elems = SchemeVal
      .toScalaList(lst)
      .getOrElse(throw new EvalError("assoc: expected list"))
    elems
      .collectFirst {
        case e if SchemeVal.isPairLike(e) && Builtins.schemeEqual(SchemeVal.pairCar(e), key) => e
      }
      .getOrElse(SchemeVal.SBool(false))

  private def applySetPair(name: String, args: List[SchemeVal]): SchemeVal =
    if args.length != 2 then throw new EvalError(s"$name: expected 2 arguments")
    args(0) match
      case SchemeVal.SPair(cell) =>
        if name == "set-car!" then cell.car = args(1) else cell.cdr = args(1)
        SchemeVal.SVoid
      case _ => throw new EvalError(s"$name: expected mutable pair, got ${args(0).display}")

  private def applyMember(args: List[SchemeVal]): SchemeVal =
    val (key, lst) = requireTwo("member", args)
    val elems = SchemeVal
      .toScalaList(lst)
      .getOrElse(throw new EvalError("member: expected list"))
    import scala.util.boundary, boundary.break
    boundary:
      var remaining = lst
      for e <- elems do
        if Builtins.schemeEqual(e, key) then break(remaining)
        remaining = remaining match
          case SchemeVal.SPair(c)      => c.cdr
          case SchemeVal.SList(_ :: t) => SchemeVal.SList(t)
          case _                       => SchemeVal.SList(Nil)
      SchemeVal.SBool(false)

  private def applyAssv(args: List[SchemeVal]): SchemeVal =
    val (key, lst) = requireTwo("assv", args)
    val elems = SchemeVal
      .toScalaList(lst)
      .getOrElse(throw new EvalError("assv: expected list"))
    elems
      .collectFirst {
        case e if SchemeVal.isPairLike(e) && Builtins.schemeEqv(SchemeVal.pairCar(e), key) => e
      }
      .getOrElse(SchemeVal.SBool(false))

  private def applyAssq(args: List[SchemeVal]): SchemeVal =
    val (key, lst) = requireTwo("assq", args)
    val elems = SchemeVal
      .toScalaList(lst)
      .getOrElse(throw new EvalError("assq: expected list"))
    elems
      .collectFirst {
        case e if SchemeVal.isPairLike(e) && Builtins.schemeEq(SchemeVal.pairCar(e), key) => e
      }
      .getOrElse(SchemeVal.SBool(false))

  private def applyMemq(args: List[SchemeVal]): SchemeVal =
    val (key, lst) = requireTwo("memq", args)
    val elems = SchemeVal
      .toScalaList(lst)
      .getOrElse(throw new EvalError("memq: expected list"))
    import scala.util.boundary, boundary.break
    boundary:
      var remaining = lst
      for e <- elems do
        if Builtins.schemeEq(e, key) then break(remaining)
        remaining = remaining match
          case SchemeVal.SPair(c)      => c.cdr
          case SchemeVal.SList(_ :: t) => SchemeVal.SList(t)
          case _                       => SchemeVal.SList(Nil)
      SchemeVal.SBool(false)

  private def applyMemv(args: List[SchemeVal]): SchemeVal =
    val (key, lst) = requireTwo("memv", args)
    val elems = SchemeVal
      .toScalaList(lst)
      .getOrElse(throw new EvalError("memv: expected list"))
    import scala.util.boundary, boundary.break
    boundary:
      var remaining = lst
      for e <- elems do
        if Builtins.schemeEqv(e, key) then break(remaining)
        remaining = remaining match
          case SchemeVal.SPair(c)      => c.cdr
          case SchemeVal.SList(_ :: t) => SchemeVal.SList(t)
          case _                       => SchemeVal.SList(Nil)
      SchemeVal.SBool(false)

  private def applyReverse(args: List[SchemeVal]): SchemeVal =
    val elems = SchemeVal
      .toScalaList(requireOne("reverse", args))
      .getOrElse(throw new EvalError("reverse: expected list"))
    if elems.isEmpty then SchemeVal.SList(Nil)
    else SchemeVal.buildList(elems.reverse)
