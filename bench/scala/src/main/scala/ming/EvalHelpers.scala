package ming

object EvalHelpers:

  /** Parse a parameter list, handling dot notation for rest params */
  def parseParams(params: List[Expr]): (List[String], Option[String]) =
    val dotIdx = params.indexWhere {
      case Expr.Symbol(".") => true
      case _                => false
    }
    if dotIdx >= 0 then
      if dotIdx != params.length - 2 then throw new EvalError("lambda: invalid dot notation in parameters")
      val fixed = params.take(dotIdx).map {
        case Expr.Symbol(n) => n
        case _              => throw new EvalError("lambda: expected parameter name")
      }
      val rest = params(dotIdx + 1) match
        case Expr.Symbol(n) => n
        case _              => throw new EvalError("lambda: expected parameter name after dot")
      (fixed, Some(rest))
    else
      val names = params.map {
        case Expr.Symbol(n) => n
        case _              => throw new EvalError("lambda: expected parameter name")
      }
      (names, None)

  def quoteToVal(expr: Expr): SchemeVal = expr match
    case Expr.IntLit(n)    => SchemeVal.IntVal(n)
    case Expr.FloatLit(d)  => SchemeVal.FloatVal(d)
    case Expr.RatLit(n, d) => SchemeNum.makeRational(n, d)
    case Expr.BoolLit(b)   => SchemeVal.BoolVal(b)
    case Expr.StrLit(s)    => SchemeVal.StrVal(s.toCharArray)
    case Expr.CharLit(c)   => SchemeVal.CharVal(c)
    case Expr.Symbol(n)    => SchemeVal.SymVal(n)
    case Expr.SList(es)    => SchemeVal.schemeList(es.map(quoteToVal))
