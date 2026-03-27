package ming

private[ming] enum EvalStep:
  case Done(value: Value)
  case EvalExpr(expr: Expr, env: Env)
  case EvalSequence(exprs: List[Expr], env: Env)
  case InvokeProcedure(procedure: Value, evaluatedArgs: List[Value], pos: SourcePos)
