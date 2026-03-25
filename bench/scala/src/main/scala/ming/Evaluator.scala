package ming

object Evaluator:

  import Builtins.isFalsy

  private[ming] def bindLambdaParams(
    params: List[String],
    restParam: Option[String],
    args: List[Expr],
    closure: Env
  ): Env =
    val localEnv = closure.child()
    params.zip(args).foreach((p, a) => localEnv.define(p, a))
    restParam match
      case None =>
        if params.length != args.length then
          throw EvalError(
            s"lambda: expected ${params.length} arguments, got ${args.length}"
          )
      case Some(rest) =>
        if args.length < params.length then
          throw EvalError(
            s"lambda: expected at least ${params.length} arguments, got ${args.length}"
          )
        localEnv.define(rest, PairOps.makeList(args.drop(params.length)))
    localEnv

  private def evalBodyTail(
    exprs: List[Expr],
    env: Env,
    state: TrampolineState
  ): Unit =
    val len = exprs.length
    var i   = 0
    while i < len - 1 do
      eval(exprs(i), env)
      i += 1
    state.curExpr = exprs(len - 1)
    state.curEnv = env

  private class TrampolineState:
    var curExpr: Expr = null.asInstanceOf[Expr]
    var curEnv: Env   = null.asInstanceOf[Env]

  private[ming] def eval(expr: Expr, env: Env): Expr = CekMachine.eval(expr, env)

  private def evalIfTail(
    args: List[Expr],
    env: Env,
    state: TrampolineState
  ): Expr =
    if args.length < 2 || args.length > 3 then throw EvalError("if: need 2 or 3 arguments")
    val cond = eval(args.head, env)
    if !isFalsy(cond) then
      state.curExpr = args(1); state.curEnv = env; null
    else if args.length == 3 then
      state.curExpr = args(2); state.curEnv = env; null
    else Expr.Bool(false)

  private def evalApplicationTail(
    op: Expr,
    args: List[Expr],
    env: Env,
    state: TrampolineState
  ): Expr =
    val func          = eval(op, env)
    val evaluatedArgs = args.map(a => eval(a, env))
    func match
      case Expr.Lambda(params, restParam, body, closure) =>
        val localEnv =
          bindLambdaParams(params, restParam, evaluatedArgs, closure)
        evalBodyTail(body, localEnv, state); null
      case Expr.CaseLambda(clauses, closure) =>
        Applier.matchCaseLambda(clauses, evaluatedArgs) match
          case Some((params, restParam, body)) =>
            val localEnv =
              bindLambdaParams(params, restParam, evaluatedArgs, closure)
            evalBodyTail(body, localEnv, state); null
          case None =>
            throw EvalError(
              s"case-lambda: no matching clause for ${evaluatedArgs.length} arguments"
            )
      case _ => Applier.applyProc(func, evaluatedArgs)

  private def parseBindingPairs(form: String, bindings: List[Expr]): List[(String, Expr)] =
    bindings.map {
      case Expr.Lst(List(Expr.Sym(name), valueExpr)) => (name, valueExpr)
      case _                                         => throw EvalError(s"$form: invalid binding")
    }

  private def evalLetTail(args: List[Expr], env: Env, state: TrampolineState): Unit = args match
    case Expr.Sym(name) :: Expr.Lst(bindings) :: body if body.nonEmpty =>
      val letEnv   = env.child()
      val parsed   = parseBindingPairs("let", bindings)
      val pNames   = parsed.map(_._1)
      val initVals = parsed.map((_, v) => eval(v, env))
      letEnv.define(name, Expr.Lambda(pNames, None, body, letEnv))
      val localEnv = bindLambdaParams(pNames, None, initVals, letEnv)
      evalBodyTail(body, localEnv, state)
    case Expr.Lst(bindings) :: body if body.nonEmpty =>
      val letEnv = env.child()
      parseBindingPairs("let", bindings).foreach((name, valueExpr) => letEnv.define(name, eval(valueExpr, env)))
      evalBodyTail(body, letEnv, state)
    case _ => throw EvalError("let: invalid syntax")

  private def evalLetStarTail(args: List[Expr], env: Env, state: TrampolineState): Unit = args match
    case Expr.Lst(bindings) :: body if body.nonEmpty =>
      val letEnv = env.child()
      parseBindingPairs("let*", bindings).foreach((name, valueExpr) => letEnv.define(name, eval(valueExpr, letEnv)))
      evalBodyTail(body, letEnv, state)
    case _ => throw EvalError("let*: invalid syntax")

  private def evalLetrecTail(args: List[Expr], env: Env, state: TrampolineState): Unit = args match
    case Expr.Lst(bindings) :: body if body.nonEmpty =>
      val letEnv = env.child()
      val parsed = parseBindingPairs("letrec", bindings)
      parsed.foreach((name, _) => letEnv.define(name, Expr.Bool(false)))
      parsed.foreach((name, valueExpr) => letEnv.define(name, eval(valueExpr, letEnv)))
      evalBodyTail(body, letEnv, state)
    case _ => throw EvalError("letrec: invalid syntax")

  private def evalLetrecStarTail(args: List[Expr], env: Env, state: TrampolineState): Unit = args match
    case Expr.Lst(bindings) :: body if body.nonEmpty =>
      val letEnv = env.child()
      parseBindingPairs("letrec*", bindings).foreach((n, v) => letEnv.define(n, eval(v, letEnv)))
      evalBodyTail(body, letEnv, state)
    case _ => throw EvalError("letrec*: invalid syntax")

  private def evalAndTail(
    args: List[Expr],
    env: Env,
    state: TrampolineState
  ): Expr =
    if args.isEmpty then return Expr.Bool(true)
    var i       = 0
    val lastIdx = args.length - 1
    while i < lastIdx do
      val v = eval(args(i), env)
      if isFalsy(v) then return v
      i += 1
    state.curExpr = args(lastIdx); state.curEnv = env
    null

  private def evalOrTail(
    args: List[Expr],
    env: Env,
    state: TrampolineState
  ): Expr =
    if args.isEmpty then return Expr.Bool(false)
    var i       = 0
    val lastIdx = args.length - 1
    while i < lastIdx do
      val v = eval(args(i), env)
      if !isFalsy(v) then return v
      i += 1
    state.curExpr = args(lastIdx); state.curEnv = env
    null

  private def evalCondTail(
    clauses: List[Expr],
    env: Env,
    state: TrampolineState
  ): Expr = clauses match
    case Nil => Expr.Bool(false)
    case Expr.Lst(Expr.Sym("else") :: body) :: _ =>
      evalBodyTail(body, env, state); null
    case Expr.Lst(test :: body) :: rest =>
      val v = eval(test, env)
      if !isFalsy(v) then
        if body.isEmpty then v
        else
          evalBodyTail(body, env, state); null
      else evalCondTail(rest, env, state)
    case _ => throw EvalError("cond: invalid clause")

  private def evalCaseClausesTail(
    key: Expr,
    clauses: List[Expr],
    env: Env,
    state: TrampolineState
  ): Expr = clauses match
    case Nil => Expr.Bool(false)
    case Expr.Lst(Expr.Sym("else") :: body) :: _ =>
      evalBodyTail(body, env, state); null
    case Expr.Lst(Expr.Lst(datums) :: body) :: rest =>
      if datums.exists(d => EqualityOps.eqv(key, d)) then
        evalBodyTail(body, env, state); null
      else evalCaseClausesTail(key, rest, env, state)
    case _ => throw EvalError("case: invalid clause")

  private[ming] def evalBody(exprs: List[Expr], env: Env): Expr = CekMachine.evalBody(exprs, env)

  def evalStr(input: String): String = SchemeEntry.evalStr(input)

  def evalStrWithOutput(input: String): (String, String) = SchemeEntry.evalStrWithOutput(input)
