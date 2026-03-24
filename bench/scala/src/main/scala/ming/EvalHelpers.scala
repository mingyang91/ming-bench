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

  /** Convert a SchemeVal to an Expr (for datum->syntax) */
  def valToExpr(v: SchemeVal): Expr = v match
    case SchemeVal.IntVal(n)      => Expr.IntLit(n)
    case SchemeVal.FloatVal(d)    => Expr.FloatLit(d)
    case SchemeVal.RatVal(n, d)   => Expr.RatLit(n, d)
    case SchemeVal.BoolVal(b)     => Expr.BoolLit(b)
    case SchemeVal.StrVal(s)      => Expr.StrLit(new String(s))
    case SchemeVal.SymVal(n)      => Expr.Symbol(n)
    case SchemeVal.CharVal(c)     => Expr.CharLit(c)
    case SchemeVal.ListVal(Nil)   => Expr.SList(Nil)
    case SchemeVal.ListVal(elems) => Expr.SList(elems.map(valToExpr))
    case SchemeVal.SyntaxObj(e)   => e
    case _                        => throw new EvalError(s"cannot convert to syntax: ${v.display}")

  /** Convert an Expr to a SchemeVal (for syntax->datum) */
  def exprToVal(e: Expr): SchemeVal = e match
    case Expr.IntLit(n)    => SchemeVal.IntVal(n)
    case Expr.FloatLit(d)  => SchemeVal.FloatVal(d)
    case Expr.RatLit(n, d) => SchemeNum.makeRational(n, d)
    case Expr.BoolLit(b)   => SchemeVal.BoolVal(b)
    case Expr.StrLit(s)    => SchemeVal.StrVal(s.toCharArray)
    case Expr.Symbol(n)    => SchemeVal.SymVal(n)
    case Expr.CharLit(c)   => SchemeVal.CharVal(c)
    case Expr.SList(Nil)   => SchemeVal.ListVal(Nil)
    case Expr.SList(elems) => SchemeVal.ListVal(elems.map(exprToVal))

  def quoteToVal(expr: Expr): SchemeVal = expr match
    case Expr.IntLit(n)    => SchemeVal.IntVal(n)
    case Expr.FloatLit(d)  => SchemeVal.FloatVal(d)
    case Expr.RatLit(n, d) => SchemeNum.makeRational(n, d)
    case Expr.BoolLit(b)   => SchemeVal.BoolVal(b)
    case Expr.StrLit(s)    => SchemeVal.StrVal(s.toCharArray)
    case Expr.CharLit(c)   => SchemeVal.CharVal(c)
    case Expr.Symbol(n)    => SchemeVal.SymVal(n)
    case Expr.SList(es) =>
      val dotIdx = es.indexWhere { case Expr.Symbol(".") => true; case _ => false }
      if dotIdx >= 0 && dotIdx == es.length - 2 then
        val heads = es.take(dotIdx).map(quoteToVal)
        val tail  = quoteToVal(es.last)
        heads.foldRight(tail)((h, t) => SchemeVal.PairVal(new MutablePair(h, t)))
      else SchemeVal.schemeList(es.map(quoteToVal))

  def evalQuasiquote(expr: Expr, env: Env): SchemeVal = expr match
    case Expr.SList(List(Expr.Symbol("unquote"), inner)) =>
      Evaluator.eval(inner, env)
    case Expr.SList(es) =>
      val dotIdx = es.indexWhere { case Expr.Symbol(".") => true; case _ => false }
      if dotIdx >= 0 && dotIdx == es.length - 2 then
        val heads = es.take(dotIdx).flatMap(expandQQElement(_, env))
        val tail  = evalQuasiquote(es.last, env)
        heads.foldRight(tail)((h, t) => SchemeVal.PairVal(new MutablePair(h, t)))
      else
        val expanded = es.flatMap(expandQQElement(_, env))
        SchemeVal.schemeList(expanded)
    case _ => quoteToVal(expr)

  private def expandQQElement(expr: Expr, env: Env): List[SchemeVal] = expr match
    case Expr.SList(List(Expr.Symbol("unquote"), inner)) =>
      List(Evaluator.eval(inner, env))
    case Expr.SList(List(Expr.Symbol("unquote-splicing"), inner)) =>
      val v = Evaluator.eval(inner, env)
      v match
        case SchemeVal.ListVal(Nil)   => Nil
        case SchemeVal.ListVal(elems) => elems
        case SchemeVal.PairVal(_)     => SchemeVal.toScalaList(v)
        case _                        => throw new EvalError(s"unquote-splicing: expected list, got ${v.display}")
    case _ => List(evalQuasiquote(expr, env))
