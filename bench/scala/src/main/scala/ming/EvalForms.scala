package ming

import scala.collection.mutable

object EvalForms:

  def evalCase(
    keyVal: SchemeVal,
    clauses: List[Expr],
    env: Env
  ): SchemeVal =
    clauses match
      case Nil => SchemeVal.Void
      case Expr.SList(Expr.Symbol("else") :: body) :: _ =>
        Evaluator.evalBody(body, env)
      case Expr.SList(Expr.SList(datums) :: body) :: rest =>
        val matched = datums.exists(d => Equality.eqvCheck(keyVal, EvalHelpers.quoteToVal(d)))
        if matched then Evaluator.evalBody(body, env)
        else evalCase(keyVal, rest, env)
      case _ => throw new EvalError("case: invalid clause")

  def evalDo(
    varClauses: List[Expr],
    testAndResult: List[Expr],
    bodyExprs: List[Expr],
    env: Env
  ): SchemeVal =
    val parsed = varClauses.map {
      case Expr.SList(Expr.Symbol(name) :: init :: step :: Nil) => (name, init, Some(step))
      case Expr.SList(Expr.Symbol(name) :: init :: Nil)         => (name, init, None)
      case _                                                    => throw new EvalError("do: invalid variable clause")
    }
    val test        = testAndResult.head
    val resultExprs = testAndResult.tail

    val doEnv = new Env(mutable.Map.empty, Some(env))
    for (name, init, _) <- parsed do doEnv.define(name, Evaluator.eval(init, env))

    while !Evaluator.isTruthy(Evaluator.eval(test, doEnv)) do
      for e <- bodyExprs do Evaluator.eval(e, doEnv)
      val newVals = parsed.map { (name, _, step) =>
        step match
          case Some(s) => Some(Evaluator.eval(s, doEnv))
          case None    => None
      }
      parsed.zip(newVals).foreach { case ((name, _, _), newVal) =>
        newVal.foreach(v => doEnv.define(name, v))
      }

    if resultExprs.isEmpty then SchemeVal.Void
    else Evaluator.evalBody(resultExprs, doEnv)

  def evalLet(
    bindings: List[Expr],
    body: List[Expr],
    env: Env
  ): SchemeVal =
    val letEnv = new Env(mutable.Map.empty, Some(env))
    for b <- bindings do
      b match
        case Expr.SList(Expr.Symbol(name) :: valExpr :: Nil) =>
          letEnv.define(name, Evaluator.eval(valExpr, env))
        case _ => throw new EvalError("let: invalid binding")
    Evaluator.evalBodyTail(body, letEnv)

  def evalLetStar(
    bindings: List[Expr],
    body: List[Expr],
    env: Env
  ): SchemeVal =
    val letEnv = new Env(mutable.Map.empty, Some(env))
    for b <- bindings do
      b match
        case Expr.SList(Expr.Symbol(name) :: valExpr :: Nil) =>
          letEnv.define(name, Evaluator.eval(valExpr, letEnv))
        case _ => throw new EvalError("let*: invalid binding")
    Evaluator.evalBodyTail(body, letEnv)

  def evalNamedLet(
    name: String,
    bindings: List[Expr],
    body: List[Expr],
    env: Env
  ): SchemeVal =
    val (paramNames, initExprs) = bindings.map {
      case Expr.SList(Expr.Symbol(p) :: v :: Nil) => (p, v)
      case _                                      => throw new EvalError("let: invalid binding")
    }.unzip
    val letEnv = new Env(mutable.Map.empty, Some(env))
    val proc   = SchemeVal.Procedure(paramNames, None, body, letEnv)
    letEnv.define(name, proc)
    val initVals = initExprs.map(e => Evaluator.eval(e, env))
    val callEnv  = new Env(mutable.Map.empty, Some(letEnv))
    paramNames.zip(initVals).foreach((p, v) => callEnv.define(p, v))
    Evaluator.evalBodyTail(body, callEnv)

  def evalCond(clauses: List[Expr], env: Env): SchemeVal =
    clauses match
      case Nil => SchemeVal.Void
      case Expr.SList(Expr.Symbol("else") :: body) :: _ =>
        Evaluator.evalBodyTail(body, env)
      case Expr.SList(test :: Expr.Symbol("=>") :: proc :: Nil) :: rest =>
        val v = Evaluator.eval(test, env)
        if Evaluator.isTruthy(v) then
          val fn = Evaluator.eval(proc, env)
          Evaluator.applyProc(fn, List(v))
        else evalCond(rest, env)
      case Expr.SList(test :: Nil) :: rest =>
        val v = Evaluator.eval(test, env)
        if Evaluator.isTruthy(v) then v
        else evalCond(rest, env)
      case Expr.SList(test :: body) :: rest =>
        if Evaluator.isTruthy(Evaluator.eval(test, env)) then Evaluator.evalBodyTail(body, env)
        else evalCond(rest, env)
      case _ => throw new EvalError("cond: invalid clause")

  def evalAnd(args: List[Expr], env: Env): SchemeVal =
    args match
      case Nil         => SchemeVal.BoolVal(true)
      case head :: Nil => SchemeVal.TailCall(head, env)
      case head :: tail =>
        val v = Evaluator.eval(head, env)
        if !Evaluator.isTruthy(v) then v
        else evalAnd(tail, env)

  def evalOr(args: List[Expr], env: Env): SchemeVal =
    args match
      case Nil         => SchemeVal.BoolVal(false)
      case head :: Nil => SchemeVal.TailCall(head, env)
      case head :: tail =>
        val v = Evaluator.eval(head, env)
        if Evaluator.isTruthy(v) then v
        else evalOr(tail, env)

  def evalLetrec(bindings: List[Expr], body: List[Expr], env: Env): SchemeVal =
    val letEnv = new Env(mutable.Map.empty, Some(env))
    val parsed = bindings.map {
      case Expr.SList(Expr.Symbol(name) :: valExpr :: Nil) => (name, valExpr)
      case _                                               => throw new EvalError("letrec: invalid binding")
    }
    for (name, _) <- parsed do letEnv.define(name, SchemeVal.Void)
    for (name, valExpr) <- parsed do letEnv.define(name, Evaluator.eval(valExpr, letEnv))
    Evaluator.evalBodyTail(body, letEnv)

  def evalLetrecStar(bindings: List[Expr], body: List[Expr], env: Env): SchemeVal =
    val letEnv = new Env(mutable.Map.empty, Some(env))
    for b <- bindings do
      b match
        case Expr.SList(Expr.Symbol(name) :: valExpr :: Nil) =>
          letEnv.define(name, Evaluator.eval(valExpr, letEnv))
        case _ => throw new EvalError("letrec*: invalid binding")
    Evaluator.evalBodyTail(body, letEnv)

  def evalGuard(
    varName: String,
    clauses: List[Expr],
    body: List[Expr],
    env: Env
  ): SchemeVal =
    try Evaluator.evalBody(body, env)
    catch
      case sr: SchemeRaise =>
        val guardEnv = new Env(mutable.Map.empty, Some(env))
        guardEnv.define(varName, sr.value)
        evalGuardClauses(sr.value, clauses, guardEnv)

  private def evalGuardClauses(
    raised: SchemeVal,
    clauses: List[Expr],
    env: Env
  ): SchemeVal =
    clauses match
      case Nil => throw new SchemeRaise(raised)
      case Expr.SList(Expr.Symbol("else") :: body) :: _ =>
        Evaluator.evalBody(body, env)
      case Expr.SList(test :: body) :: rest =>
        if Evaluator.isTruthy(Evaluator.eval(test, env)) then Evaluator.evalBody(body, env)
        else evalGuardClauses(raised, rest, env)
      case _ => throw new EvalError("guard: invalid clause")

  def evalCaseLambda(clauseExprs: List[Expr], env: Env): SchemeVal =
    val clauses = clauseExprs.map {
      case Expr.SList(Expr.SList(params) :: body) =>
        val (paramNames, restParam) = EvalHelpers.parseParams(params)
        (paramNames, restParam, body, env)
      case _ => throw new EvalError("case-lambda: invalid clause")
    }
    SchemeVal.CaseLambda(clauses)
