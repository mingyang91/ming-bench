package ming

import Value.*
import Expr.*
import EvalHelpers.evalError

/** CPS evaluators for do, case, and argument evaluation — mixed into Evaluator via EvalForms. */
private[ming] trait EvalDo:
  protected def eval(expr: Expr, env: Env, k: Value => Bounce): Bounce
  protected def evalBody(exprs: List[Expr], env: Env, k: Value => Bounce): Bounce
  protected def tailBody(exprs: List[Expr], env: Env, k: Value => Bounce): Bounce
  protected def trampoline(thunk: => Bounce): Bounce
  protected def applyProc(proc: Value, values: List[Value], pos: Option[Pos], k: Value => Bounce): Bounce

  protected def evalArgs(args: List[Expr], env: Env, k: List[Value] => Bounce): Bounce =
    args match
      case Nil => k(Nil)
      case head :: tail =>
        eval(head, env, v => trampoline(evalArgs(tail, env, vs => k(v :: vs))))

  protected def evalCaseCps(
    keyVal: Value,
    clauses: List[Expr],
    env: Env,
    pos: Option[Pos],
    k: Value => Bounce
  ): Bounce =
    clauses match
      case Nil => k(VoidVal)
      case SList(Sym("else", _) :: body, _) :: Nil =>
        evalBody(body, env, k)
      case SList(SList(datums, _) :: body, _) :: rest =>
        val matched = datums.exists { d =>
          Equality.eqvCheck(keyVal, EvalHelpers.exprToValue(d))
        }
        if matched then
          if body.isEmpty then k(VoidVal)
          else evalBody(body, env, k)
        else evalCaseCps(keyVal, rest, env, pos, k)
      case _ => evalError("case: bad syntax", pos)

  protected def evalDoCps(
    args: List[Expr],
    env: Env,
    pos: Option[Pos],
    k: Value => Bounce
  ): Bounce =
    args match
      case SList(varSpecs, _) :: SList(test :: resultExprs, _) :: bodyExprs =>
        val parsed = varSpecs.map {
          case SList(Sym(name, _) :: initExpr :: stepExpr :: Nil, _) =>
            (name, initExpr, Some(stepExpr))
          case SList(Sym(name, _) :: initExpr :: Nil, _) =>
            (name, initExpr, None)
          case _ => evalError("do: bad variable spec", pos)
        }
        val names     = parsed.map(_._1)
        val initExprs = parsed.map(_._2)
        val steps     = parsed.map(_._3)
        evalDoInits(names, initExprs, steps, test, resultExprs, bodyExprs, env, pos, k)
      case _ => evalError("do: bad syntax", pos)

  private def evalDoInits(
    names: List[String],
    initExprs: List[Expr],
    steps: List[Option[Expr]],
    test: Expr,
    resultExprs: List[Expr],
    bodyExprs: List[Expr],
    env: Env,
    pos: Option[Pos],
    k: Value => Bounce
  ): Bounce =
    evalExprList(
      initExprs,
      env,
      pos,
      { initVals =>
        val loopEnv = env.extend(names, initVals)
        doLoop(names, steps, test, resultExprs, bodyExprs, loopEnv, pos, k)
      }
    )

  private def doLoop(
    names: List[String],
    steps: List[Option[Expr]],
    test: Expr,
    resultExprs: List[Expr],
    bodyExprs: List[Expr],
    loopEnv: Env,
    pos: Option[Pos],
    k: Value => Bounce
  ): Bounce =
    eval(
      test,
      loopEnv,
      testVal =>
        if testVal.isTruthy then
          if resultExprs.isEmpty then k(VoidVal)
          else evalBody(resultExprs, loopEnv, k)
        else
          val afterBody: () => Bounce = () =>
            evalDoSteps(
              names,
              steps,
              loopEnv,
              pos,
              { newVals =>
                names.zip(newVals).foreach((n, v) => loopEnv.define(n, v))
                trampoline(doLoop(names, steps, test, resultExprs, bodyExprs, loopEnv, pos, k))
              }
            )
          if bodyExprs.isEmpty then afterBody()
          else evalBody(bodyExprs, loopEnv, _ => afterBody())
    )

  private def evalDoSteps(
    names: List[String],
    steps: List[Option[Expr]],
    env: Env,
    pos: Option[Pos],
    k: List[Value] => Bounce
  ): Bounce =
    evalDoStepsHelper(names, steps, env, pos, Nil, k)

  private def evalDoStepsHelper(
    names: List[String],
    steps: List[Option[Expr]],
    env: Env,
    pos: Option[Pos],
    acc: List[Value],
    k: List[Value] => Bounce
  ): Bounce =
    (names, steps) match
      case (Nil, Nil) => k(acc.reverse)
      case (name :: restN, Some(stepExpr) :: restS) =>
        eval(stepExpr, env, v => trampoline(evalDoStepsHelper(restN, restS, env, pos, v :: acc, k)))
      case (name :: restN, None :: restS) =>
        val cur = env.lookup(name, pos)
        trampoline(evalDoStepsHelper(restN, restS, env, pos, cur :: acc, k))
      case _ => evalError("do: internal error", pos)

  private def evalExprList(
    exprs: List[Expr],
    env: Env,
    pos: Option[Pos],
    k: List[Value] => Bounce
  ): Bounce =
    exprs match
      case Nil => k(Nil)
      case head :: tail =>
        eval(head, env, v => trampoline(evalExprList(tail, env, pos, vs => k(v :: vs))))
