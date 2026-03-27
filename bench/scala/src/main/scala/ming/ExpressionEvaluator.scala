package ming

private[ming] object ExpressionEvaluator:

  def eval(expr: Expr, env: Env, context: EvalContext): Value =
    new Machine(context).runExpr(expr, env)

  def evalSequence(exprs: List[Expr], env: Env, context: EvalContext): Value =
    new Machine(context).runSequence(exprs, env)

  private[ming] def run(step: EvalStep, context: EvalContext): Value =
    val machine = new Machine(context)
    step match
      case EvalStep.Done(value) =>
        value
      case EvalStep.EvalExpr(expr, env) =>
        machine.runExpr(expr, env)
      case EvalStep.EvalSequence(exprs, env) =>
        machine.runSequence(exprs, env)
      case EvalStep.InvokeProcedure(procedure, evaluatedArgs, pos) =>
        machine.runInvoke(procedure, evaluatedArgs, pos)
