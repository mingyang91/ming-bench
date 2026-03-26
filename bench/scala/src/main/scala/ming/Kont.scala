package ming

import SchemeTypes.{Env, Pos, Value}

/** Explicit continuation frames for the CEK machine. call/cc captures these as first-class values.
  */
enum Kont:
  case Halt
  case Seq(remaining: List[Expr], env: Env, next: Kont)
  case Define(name: String, env: Env, next: Kont)
  case Set(name: String, env: Env, pos: Pos, next: Kont)
  case If(thenExpr: Expr, elseExpr: Option[Expr], env: Env, next: Kont)
  case And(remaining: List[Expr], env: Env, next: Kont)
  case Or(remaining: List[Expr], env: Env, next: Kont)
  case AppHead(argExprs: List[Expr], env: Env, pos: Pos, next: Kont)

  case AppArg(
    func: Value,
    done: List[Value],
    remaining: List[Expr],
    env: Env,
    pos: Pos,
    next: Kont
  )

  case LetBind(
    varName: String,
    remaining: List[(String, Expr)],
    letEnv: Env,
    initEnv: Env,
    body: List[Expr],
    next: Kont
  )

  case LetStarBind(
    varName: String,
    remaining: List[(String, Expr)],
    letEnv: Env,
    body: List[Expr],
    next: Kont
  )

  case NamedLetArgs(
    params: List[String],
    doneVals: List[Value],
    remainingExprs: List[Expr],
    initEnv: Env,
    loopEnv: Env,
    body: List[Expr],
    next: Kont
  )

  case CondTest(
    body: List[Expr],
    remainingClauses: List[Expr],
    env: Env,
    next: Kont
  )

/** Exception for continuation invocation from non-CEK code paths (builtins). */
class ContinuationInvoke(val value: Value, val kont: Kont) extends RuntimeException(null, null, true, false)
