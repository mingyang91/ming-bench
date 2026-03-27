package ming

private[ming] object ExpressionEvaluator:

  def eval(expr: Expr, env: Env, context: EvalContext): Value =
    expr match
      case Expr.IntLit(value, _)                       => Value.IntVal(value)
      case Expr.RationalLit(numerator, denominator, _) => Value.RationalVal(numerator, denominator)
      case Expr.InexactLit(value, _)                   => Value.InexactVal(value)
      case Expr.BoolLit(value, _)                      => Value.BoolVal(value)
      case Expr.StringLit(value, _)                    => Value.StringVal(MutableString.immutable(value))
      case Expr.CharLit(value, _)                      => Value.CharVal(value)
      case Expr.Symbol(name, pos) =>
        env.lookup(name).getOrElse(throw EvalError.at(pos, s"unbound variable: $name"))
      case Expr.ListExpr(items, pos) =>
        SpecialFormEvaluator.evalList(items, env, pos, context)

  def evalSequence(exprs: List[Expr], env: Env, context: EvalContext): Value =
    exprs.foldLeft[Value](Value.Void) { (_, expr) =>
      eval(expr, env, context)
    }

  @annotation.tailrec
  def evalAnd(args: List[Expr], env: Env, lastValue: Value, context: EvalContext): Value =
    args match
      case Nil => lastValue
      case head :: tail =>
        val value = eval(head, env, context)
        if ValueSemantics.isTruthy(value) then evalAnd(tail, env, value, context)
        else value

  @annotation.tailrec
  def evalOr(args: List[Expr], env: Env, context: EvalContext): Value =
    args match
      case Nil => Value.BoolVal(false)
      case head :: tail =>
        val value = eval(head, env, context)
        if ValueSemantics.isTruthy(value) then value
        else evalOr(tail, env, context)
