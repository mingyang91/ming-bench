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
