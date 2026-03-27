package ming

private[ming] object ExpressionEvaluator:

  def eval(expr: Expr, env: Env, context: EvalContext): Value =
    run(EvalStep.EvalExpr(expr, env), context)

  def evalSequence(exprs: List[Expr], env: Env, context: EvalContext): Value =
    run(EvalStep.EvalSequence(exprs, env), context)

  private[ming] def run(step: EvalStep, context: EvalContext): Value =
    var current = step
    while true do
      current match
        case EvalStep.Done(value) =>
          return value
        case EvalStep.EvalExpr(expr, env) =>
          current = evalExprStep(expr, env, context)
        case EvalStep.EvalSequence(exprs, env) =>
          current = evalSequenceStep(exprs, env, context)
        case EvalStep.InvokeProcedure(procedure, evaluatedArgs, pos) =>
          current = ProcedureInvoker.prepareProcedureCall(procedure, evaluatedArgs, pos, context)

    throw IllegalStateException("unreachable")

  private def evalExprStep(expr: Expr, env: Env, context: EvalContext): EvalStep =
    expr match
      case Expr.IntLit(value, _)                       => EvalStep.Done(Value.IntVal(value))
      case Expr.RationalLit(numerator, denominator, _) => EvalStep.Done(Value.RationalVal(numerator, denominator))
      case Expr.InexactLit(value, _)                   => EvalStep.Done(Value.InexactVal(value))
      case Expr.BoolLit(value, _)                      => EvalStep.Done(Value.BoolVal(value))
      case Expr.StringLit(value, _)                    => EvalStep.Done(Value.StringVal(MutableString.immutable(value)))
      case Expr.CharLit(value, _)                      => EvalStep.Done(Value.CharVal(value))
      case Expr.Symbol(name, pos) =>
        EvalStep.Done(env.lookup(name).getOrElse(throw EvalError.at(pos, s"unbound variable: $name")))
      case Expr.ListExpr(items, pos) =>
        SpecialFormEvaluator.evalListStep(items, env, pos, context)

  private def evalSequenceStep(exprs: List[Expr], env: Env, context: EvalContext): EvalStep =
    exprs match
      case Nil =>
        EvalStep.Done(Value.Void)
      case _ =>
        var remaining = exprs
        while remaining.tail.nonEmpty do
          eval(remaining.head, env, context)
          remaining = remaining.tail
        EvalStep.EvalExpr(remaining.head, env)
