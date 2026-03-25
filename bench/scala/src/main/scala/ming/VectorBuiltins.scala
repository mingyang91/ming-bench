package ming

private[ming] object VectorBuiltins:

  def applyVectorBuiltin(name: String, args: List[Expr]): Expr = name match
    case "vector"      => Expr.Vec(args.toArray)
    case "make-vector" => applyMakeVector(args)
    case "vector-ref"  => applyVectorRef(args)
    case "vector-set!" => applyVectorSet(args)
    case "vector-length" =>
      Builtins.unary(name, args) {
        case Expr.Vec(elems) => Expr.Num(elems.length.toLong)
        case _               => throw EvalError("vector-length: not a vector")
      }
    case "vector?" => Builtins.unary(name, args)(e => Expr.Bool(e.isInstanceOf[Expr.Vec]))
    case "vector->list" =>
      Builtins.unary(name, args) {
        case Expr.Vec(elems) => Expr.Lst(elems.toList)
        case _               => throw EvalError("vector->list: not a vector")
      }
    case "list->vector" =>
      Builtins.unary(name, args) {
        case Expr.Lst(elems) => Expr.Vec(elems.toArray)
        case _               => throw EvalError("list->vector: not a list")
      }
    case _ => throw EvalError(s"unknown vector procedure: $name")

  private def applyMakeVector(args: List[Expr]): Expr =
    if args.length < 1 || args.length > 2 then throw EvalError("make-vector: need 1 or 2 arguments")
    val len  = Builtins.asNum(args.head).toInt
    val fill = if args.length == 2 then args(1) else Expr.Num(0)
    Expr.Vec(Array.fill(len)(fill))

  private def applyVectorRef(args: List[Expr]): Expr =
    if args.length != 2 then throw EvalError("vector-ref: need exactly 2 arguments")
    args(0) match
      case Expr.Vec(elems) =>
        val idx = Builtins.asNum(args(1)).toInt
        if idx < 0 || idx >= elems.length then throw EvalError("vector-ref: index out of bounds")
        elems(idx)
      case _ => throw EvalError("vector-ref: not a vector")

  private def applyVectorSet(args: List[Expr]): Expr =
    if args.length != 3 then throw EvalError("vector-set!: need exactly 3 arguments")
    args(0) match
      case Expr.Vec(elems) =>
        val idx = Builtins.asNum(args(1)).toInt
        if idx < 0 || idx >= elems.length then throw EvalError("vector-set!: index out of bounds")
        elems(idx) = args(2)
        Expr.Bool(false)
      case _ => throw EvalError("vector-set!: not a vector")
