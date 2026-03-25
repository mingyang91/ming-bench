package ming

private[ming] object ParamUtils:

  def extractParamsWithRest(context: String, params: List[Expr]): (List[String], Option[String]) =
    val dotIdx = params.indexWhere(_ == Expr.Sym("."))
    if dotIdx >= 0 then
      if dotIdx != params.length - 2 then throw EvalError(s"$context: invalid dot syntax")
      val fixed = params.take(dotIdx).map {
        case Expr.Sym(p) => p
        case other       => throw EvalError(s"$context: invalid parameter: ${Display.display(other)}")
      }
      val rest = params.last match
        case Expr.Sym(p) => p
        case other       => throw EvalError(s"$context: invalid rest parameter: ${Display.display(other)}")
      (fixed, Some(rest))
    else
      (
        params.map {
          case Expr.Sym(p) => p
          case other       => throw EvalError(s"$context: invalid parameter: ${Display.display(other)}")
        },
        None
      )

  /** Extract function name, parameter names, and optional rest parameter from a form head that may be either Lst or
    * Pair (dotted pair from parser). Handles: Lst(Sym(name) :: params) — standard list notation Pair(Sym(name), ...) —
    * dotted pair notation from parser
    */
  def extractDefineHead(context: String, head: Expr): (String, List[String], Option[String]) =
    head match
      case Expr.Lst(Expr.Sym(name) :: params) =>
        val (pn, rp) = extractParamsWithRest(context, params)
        (name, pn, rp)
      case Expr.Pair(mp) =>
        mp.car match
          case Expr.Sym(name) =>
            val (pn, rp) = extractFromPairTail(context, mp.cdr)
            (name, pn, rp)
          case _ => throw EvalError(s"$context: invalid syntax")
      case _ => throw EvalError(s"$context: invalid syntax")

  /** Extract parameter names and optional rest from a parameter specification that may be either Lst or Pair.
    */
  def extractParamSpec(context: String, spec: Expr): (List[String], Option[String]) =
    spec match
      case Expr.Lst(params) => extractParamsWithRest(context, params)
      case Expr.Pair(_)     => extractFromPairTail(context, spec)
      case Expr.Sym(rest)   => (Nil, Some(rest))
      case _                => throw EvalError(s"$context: invalid parameter list")

  private def extractFromPairTail(context: String, expr: Expr): (List[String], Option[String]) =
    val fixed = List.newBuilder[String]
    var cur   = expr
    while true do
      cur match
        case Expr.Pair(mp) =>
          mp.car match
            case Expr.Sym(p) => fixed += p
            case other       => throw EvalError(s"$context: invalid parameter: ${Display.display(other)}")
          cur = mp.cdr
        case Expr.Lst(Nil) =>
          return (fixed.result(), None)
        case Expr.Sym(rest) =>
          return (fixed.result(), Some(rest))
        case _ =>
          throw EvalError(s"$context: invalid parameter list")
    throw EvalError(s"$context: unreachable") // unreachable
