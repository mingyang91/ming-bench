package ming

private[ming] object SchemeInterpreterSyntax:

  import SchemeInterpreter.Expr
  import SchemeInterpreter.Value

  def readParams(formals: Expr): LambdaParams =
    formals match
      case Expr.Symbol(name, _) if name != "." =>
        LambdaParams(required = Nil, rest = Some(name))
      case Expr.Symbol(_, _) =>
        throw EvalError.at(formals.pos, s"invalid parameter list: ${renderExpr(formals)}")
      case Expr.ListExpr(params, _) =>
        readParamList(params)
      case other =>
        throw EvalError.at(other.pos, s"invalid parameter list: ${renderExpr(other)}")

  def readParamList(params: List[Expr]): LambdaParams =
    val dotIndices = params.zipWithIndex.collect { case (Expr.Symbol(".", _), index) =>
      index
    }

    dotIndices match
      case Nil =>
        LambdaParams.fixed(params.map(readParamName))
      case index :: Nil if index == params.length - 2 =>
        val required = params.take(index).map(readParamName)
        params(index + 1) match
          case Expr.Symbol(name, _) if name != "." =>
            LambdaParams(required, Some(name))
          case other =>
            throw EvalError.at(other.pos, s"invalid parameter: ${renderExpr(other)}")
      case index :: _ =>
        throw EvalError.at(params(index).pos, "invalid dotted parameter list")

  def readBindings(bindings: List[Expr]): List[(String, Expr)] =
    bindings.map {
      case Expr.ListExpr(List(Expr.Symbol(name, _), valueExpr), _) => (name, valueExpr)
      case other => throw EvalError.at(other.pos, s"invalid binding: ${renderExpr(other)}")
    }

  def quote(expr: Expr): Value =
    expr match
      case Expr.Number(value, _)    => Value.Number(value)
      case Expr.Bool(value, _)      => Value.Bool(value)
      case Expr.StringLit(value, _) => Value.StringLit(value)
      case Expr.Character(value, _) => Value.Character(value)
      case Expr.Symbol(name, _)     => Value.Symbol(name)
      case Expr.ListExpr(items, _)  => Value.list(items.map(quote))

  def isTruthy(value: Value): Boolean =
    value match
      case Value.Bool(false) => false
      case _                 => true

  private def readParamName(param: Expr): String =
    param match
      case Expr.Symbol(name, _) if name != "." => name
      case other =>
        throw EvalError.at(other.pos, s"invalid parameter: ${renderExpr(other)}")

  private def renderExpr(expr: Expr): String =
    SchemeRendering.renderExpr(expr)
