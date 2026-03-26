package ming

import SchemeTypes.{errAt, Env, Pos, Value}

/** Let-binding step helpers for the CEK machine. */
object CekLetSteps:

  private[ming] def stepLet(rest: List[Expr], env: Env, pos: Pos, k: Kont): CekState =
    rest match
      case Expr.Symbol(name, _) :: Expr.SList(bindings, _) :: body if body.nonEmpty =>
        stepNamedLet(name, bindings, body, env, pos, k)
      case Expr.SList(bindings, _) :: body if body.nonEmpty =>
        stepSimpleLet(bindings, body, env, pos, k)
      case _ => throw errAt(pos, "invalid let")

  private def stepSimpleLet(
    bindings: List[Expr],
    body: List[Expr],
    env: Env,
    pos: Pos,
    k: Kont
  ): CekState =
    val parsed = parseBindings(bindings, pos, "invalid let binding")
    val letEnv = env.child()
    parsed match
      case Nil =>
        CekSteps.bodyToCek(body, letEnv, k)
      case (name, initExpr) :: rest =>
        CekState.Eval(initExpr, env, Kont.LetBind(name, rest, letEnv, env, body, k))

  private def stepNamedLet(
    name: String,
    bindings: List[Expr],
    body: List[Expr],
    env: Env,
    pos: Pos,
    k: Kont
  ): CekState =
    val parsed     = parseBindings(bindings, pos, "invalid let binding")
    val paramNames = parsed.map(_._1)
    val loopEnv    = env.child()
    val lambda     = Value.VLambda(paramNames, None, body, loopEnv)
    loopEnv.define(name, lambda)
    parsed match
      case Nil =>
        val callEnv = loopEnv.child()
        CekSteps.bodyToCek(body, callEnv, k)
      case (_, initExpr) :: rest =>
        CekState.Eval(
          initExpr,
          env,
          Kont.NamedLetArgs(paramNames, Nil, rest.map(_._2), env, loopEnv, body, k)
        )

  private[ming] def stepLetStar(rest: List[Expr], env: Env, pos: Pos, k: Kont): CekState =
    rest match
      case Expr.SList(bindings, _) :: body if body.nonEmpty =>
        val parsed = parseBindings(bindings, pos, "invalid let* binding")
        val letEnv = env.child()
        parsed match
          case Nil =>
            CekSteps.bodyToCek(body, letEnv, k)
          case (name, initExpr) :: rest =>
            CekState.Eval(initExpr, letEnv, Kont.LetStarBind(name, rest, letEnv, body, k))
      case _ => throw errAt(pos, "invalid let*")

  private def parseBindings(bindings: List[Expr], pos: Pos, msg: String): List[(String, Expr)] =
    bindings.map {
      case Expr.SList(Expr.Symbol(n, _) :: initExpr :: Nil, _) => (n, initExpr)
      case _                                                   => throw errAt(pos, msg)
    }
