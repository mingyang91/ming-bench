package ming

private[ming] object ListSearchBuiltins:

  def applyListSearch(name: String, args: List[Expr]): Expr = name match
    case "member" => applyMember(args)
    case "memv"   => applyMemv(args)
    case "memq"   => applyMemv(args)
    case "assv"   => applyAssv(args)
    case "assq"   => applyAssq(args)
    case _        => throw EvalError(s"unknown procedure: $name")

  private def applyMember(args: List[Expr]): Expr =
    if args.length != 2 then throw EvalError("member: need exactly 2 arguments")
    val key = args(0)
    var cur = args(1)
    while true do
      cur match
        case Expr.Lst(Nil) => return Expr.Bool(false)
        case Expr.Pair(cell) =>
          if EqualityOps.schemeEqual(cell.car, key) then return cur
          cur = cell.cdr
        case Expr.Lst(elems) =>
          val idx = elems.indexWhere(e => EqualityOps.schemeEqual(e, key))
          if idx < 0 then return Expr.Bool(false)
          else return PairOps.makeList(elems.drop(idx))
        case _ => throw EvalError("member: not a proper list")
    Expr.Bool(false) // unreachable

  private def applyMemv(args: List[Expr]): Expr =
    if args.length != 2 then throw EvalError("memv: need exactly 2 arguments")
    val key = args(0)
    var cur = args(1)
    while true do
      cur match
        case Expr.Lst(Nil) => return Expr.Bool(false)
        case Expr.Pair(cell) =>
          if EqualityOps.eqv(cell.car, key) then return cur
          cur = cell.cdr
        case Expr.Lst(elems) =>
          val idx = elems.indexWhere(e => EqualityOps.eqv(e, key))
          if idx < 0 then return Expr.Bool(false)
          else return PairOps.makeList(elems.drop(idx))
        case _ => throw EvalError("memv: not a proper list")
    Expr.Bool(false) // unreachable

  private def applyAssv(args: List[Expr]): Expr =
    if args.length != 2 then throw EvalError("assv: need exactly 2 arguments")
    val key   = args(0)
    val elems = PairOps.toScalaList(args(1))
    elems
      .find { elem =>
        EqualityOps.eqv(PairOps.carOf(elem), key)
      }
      .getOrElse(Expr.Bool(false))

  private def applyAssq(args: List[Expr]): Expr =
    if args.length != 2 then throw EvalError("assq: need exactly 2 arguments")
    val key   = args(0)
    val elems = PairOps.toScalaList(args(1))
    elems
      .find { elem =>
        val k = PairOps.carOf(elem)
        (k eq key) || EqualityOps.eqv(k, key)
      }
      .getOrElse(Expr.Bool(false))
