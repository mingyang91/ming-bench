package ming

import Value.*
import EvalHelpers.evalError

/** Exception handling: raise, with-exception-handler, guard — mixed into Evaluator. */
private[ming] trait EvalExceptions:
  import Evaluator.{HandlerEntry, K, WindEntry}

  protected def handlerStack: List[HandlerEntry]
  protected def handlerStack_=(hs: List[HandlerEntry]): Unit
  protected def windStack: List[WindEntry]
  protected def applyProc(proc: Value, values: List[Value], pos: Option[Pos], k: K): Bounce
  protected def doWindTransition(target: List[WindEntry], pos: Option[Pos], andThen: () => Bounce): Bounce
  protected def evalBody(exprs: List[Expr], env: Env, k: K): Bounce
  protected def eval(expr: Expr, env: Env, k: K): Bounce
  protected def trampoline(thunk: => Bounce): Bounce

  /** Raise a Scheme exception: unwind to the nearest handler's wind stack, then invoke it. */
  protected def raiseException(value: Value, pos: Option[Pos]): Bounce =
    if handlerStack.isEmpty then throw new EvalError(s"unhandled exception: ${value.display}")
    val HandlerEntry(handler, targetWind) = handlerStack.head
    handlerStack = handlerStack.tail
    doWindTransition(targetWind, pos, () => handler(value))

  /** with-exception-handler: install handler proc, run thunk, pop handler on normal exit. */
  protected def evalWithExceptionHandler(handlerProc: Value, thunkProc: Value, pos: Option[Pos], k: K): Bounce =
    val savedHandlers = handlerStack
    val entry = HandlerEntry(
      handler = exnVal =>
        handlerStack = savedHandlers
        applyProc(
          handlerProc,
          List(exnVal),
          pos,
          _ => throw new EvalError("handler returned from non-continuable exception")
        )
      ,
      windAtInstall = windStack
    )
    handlerStack = entry :: handlerStack
    applyProc(
      thunkProc,
      Nil,
      pos,
      { result =>
        handlerStack = savedHandlers
        k(result)
      }
    )

  /** guard: install exception handler, evaluate body; on exception evaluate cond clauses. */
  protected def evalGuard(
    variable: String,
    clauses: List[Expr],
    body: List[Expr],
    env: Env,
    pos: Option[Pos],
    k: K
  ): Bounce =
    val savedHandlers = handlerStack
    val guardK        = k
    val guardEnv      = env
    val entry = HandlerEntry(
      handler = exnVal =>
        handlerStack = savedHandlers
        val clauseEnv = guardEnv.extend(List(variable), List(exnVal))
        evalGuardClauses(clauses, clauseEnv, exnVal, pos, guardK)
      ,
      windAtInstall = windStack
    )
    handlerStack = entry :: handlerStack
    evalBody(
      body,
      env,
      { result =>
        handlerStack = savedHandlers
        trampoline(k(result))
      }
    )

  /** Evaluate guard cond clauses; re-raise if no clause matches. */
  private def evalGuardClauses(
    clauses: List[Expr],
    env: Env,
    exnVal: Value,
    pos: Option[Pos],
    k: K
  ): Bounce =
    clauses match
      case Nil => raiseException(exnVal, pos)
      case Expr.SList(Expr.Sym("else", _) :: elseBody, _) :: _ =>
        evalBody(elseBody, env, k)
      case Expr.SList(test :: clauseBody, _) :: rest =>
        eval(
          test,
          env,
          testVal =>
            if testVal.isTruthy then
              if clauseBody.isEmpty then k(testVal)
              else evalBody(clauseBody, env, k)
            else evalGuardClauses(rest, env, exnVal, pos, k)
        )
      case _ => evalError("guard: bad clause", pos)
