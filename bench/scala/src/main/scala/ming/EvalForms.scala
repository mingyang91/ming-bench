package ming

import Value.*
import Expr.*
import EvalHelpers.{evalError, parseParams}

/** CPS form evaluators (define, let, cond, and, or) mixed into Evaluator. */
private[ming] trait EvalForms:
  protected def eval(expr: Expr, env: Env, k: Value => Bounce): Bounce
  protected def evalBody(exprs: List[Expr], env: Env, k: Value => Bounce): Bounce
  protected def tailBody(exprs: List[Expr], env: Env, k: Value => Bounce): Bounce

  protected def evalDefineCps(
    args: List[Expr],
    env: Env,
    pos: Option[Pos],
    k: Value => Bounce
  ): Bounce =
    args match
      case Sym(name, _) :: valueExpr :: Nil =>
        eval(
          valueExpr,
          env,
          { v =>
            env.define(name, v); k(BoolVal(true))
          }
        )
      case SList(Sym(name, _) :: params, _) :: body if body.nonEmpty =>
        val (paramNames, restParam) = parseParams(params, "define", pos)
        env.define(name, LambdaVal(paramNames, restParam, body, env))
        k(BoolVal(true))
      case _ => evalError("define: bad syntax", pos)

  protected def evalLetCps(
    args: List[Expr],
    env: Env,
    pos: Option[Pos],
    k: Value => Bounce
  ): Bounce =
    args match
      case Sym(name, _) :: SList(bindings, _) :: body if body.nonEmpty =>
        evalBindingsCps(
          bindings,
          env,
          pos,
          { pairs =>
            val paramNames = pairs.map(_._1)
            val initVals   = pairs.map(_._2)
            val localEnv   = env.extend(Nil, Nil)
            localEnv.define(name, LambdaVal(paramNames, None, body, localEnv))
            val callEnv = localEnv.extend(paramNames, initVals)
            tailBody(body, callEnv, k)
          }
        )
      case SList(bindings, _) :: body if body.nonEmpty =>
        evalBindingsCps(
          bindings,
          env,
          pos,
          { pairs =>
            val localEnv = env.extend(pairs.map(_._1), pairs.map(_._2))
            tailBody(body, localEnv, k)
          }
        )
      case _ => evalError("let: bad syntax", pos)

  protected def evalBindingsCps(
    bindings: List[Expr],
    env: Env,
    pos: Option[Pos],
    k: List[(String, Value)] => Bounce
  ): Bounce =
    bindings match
      case Nil => k(Nil)
      case SList(Sym(name, _) :: valExpr :: Nil, _) :: rest =>
        eval(valExpr, env, v => evalBindingsCps(rest, env, pos, pairs => k((name, v) :: pairs)))
      case _ => evalError("let: bad binding", pos)

  protected def evalCondCps(
    clauses: List[Expr],
    env: Env,
    pos: Option[Pos],
    k: Value => Bounce
  ): Bounce =
    clauses match
      case Nil => evalError("cond: no matching clause", pos)
      case SList(Sym("else", _) :: body, _) :: Nil =>
        evalBody(body, env, k)
      case SList(test :: body, _) :: rest =>
        eval(
          test,
          env,
          tv =>
            if tv.isTruthy then evalBody(body, env, k)
            else evalCondCps(rest, env, pos, k)
        )
      case _ => evalError("cond: bad syntax", pos)

  protected def evalAndCps(args: List[Expr], env: Env, k: Value => Bounce): Bounce =
    args match
      case Nil         => k(BoolVal(true))
      case last :: Nil => eval(last, env, k)
      case head :: tail =>
        eval(head, env, v => if !v.isTruthy then k(v) else evalAndCps(tail, env, k))

  protected def evalOrCps(args: List[Expr], env: Env, k: Value => Bounce): Bounce =
    args match
      case Nil         => k(BoolVal(false))
      case last :: Nil => eval(last, env, k)
      case head :: tail =>
        eval(head, env, v => if v.isTruthy then k(v) else evalOrCps(tail, env, k))
