package ming

import Evaluator.*
import Evaluator.Val.*

/** dynamic-wind, guard, and with-exception-handler evaluation. */
object WindGuard:

  /** Evaluate `(dynamic-wind in body out)` in CPS. */
  def evalDynamicWindK(inExpr: Val, bodyExpr: Val, outExpr: Val, env: Env, k: Cont): Bounce =
    evalK(
      inExpr,
      env,
      inThunk =>
        evalK(
          bodyExpr,
          env,
          bodyThunk =>
            evalK(
              outExpr,
              env,
              outThunk =>
                applyK(
                  inThunk,
                  List.empty,
                  _ =>
                    BMore { () =>
                      windingStack = (inThunk, outThunk) :: windingStack
                      applyK(
                        bodyThunk,
                        List.empty,
                        bodyVal =>
                          BMore { () =>
                            windingStack = windingStack.tail
                            applyK(outThunk, List.empty, _ => k(bodyVal))
                          }
                      )
                    }
                )
            )
        )
    )

  /** Evaluate `(guard (var clause ...) body ...)` in CPS. */
  def evalGuardK(varName: String, clauses: Val, body: Val, env: Env, k: Cont): Bounce =
    val guardK     = k
    val guardWinds = windingStack
    val handler: Val => Bounce = exn =>
      val currentWinds = windingStack
      val commonLen    = commonWindTailLength(currentWinds, guardWinds)
      val toUnwind     = currentWinds.take(currentWinds.length - commonLen).map(_._2)
      val toRewind     = guardWinds.take(guardWinds.length - commonLen).reverse.map(_._1)
      runThunks(
        toUnwind,
        BMore { () =>
          runThunks(
            toRewind,
            BMore { () =>
              windingStack = guardWinds
              val clauseEnv = Env.empty(Some(env))
              clauseEnv.define(varName, exn)
              evalGuardClauses(clauses, clauseEnv, guardK, exn)
            }
          )
        }
      )
    raiseHandlers = handler :: raiseHandlers
    evalSeqK(
      toList(body),
      env,
      result =>
        BMore { () =>
          raiseHandlers = raiseHandlers.tail
          k(result)
        }
    )

  /** Evaluate `(with-exception-handler handler thunk)` in CPS. */
  def evalWithExceptionHandlerK(
    handlerExpr: Val,
    thunkExpr: Val,
    env: Env,
    k: Cont
  ): Bounce =
    evalK(
      handlerExpr,
      env,
      handlerProc =>
        evalK(
          thunkExpr,
          env,
          thunkProc =>
            val handler: Val => Bounce =
              exn => applyK(handlerProc, List(exn), _ => error("exception handler returned"))
            raiseHandlers = handler :: raiseHandlers
            applyK(
              thunkProc,
              List.empty,
              result =>
                BMore { () =>
                  raiseHandlers = raiseHandlers.tail
                  k(result)
                }
            )
        )
    )

  /** Evaluate guard clauses, re-raising if none match. */
  private def evalGuardClauses(clauses: Val, env: Env, k: Cont, exn: Val): Bounce =
    clauses match
      case Nil =>
        throw new SchemeRaise(exn)
      case Pair(clause, rest) =>
        val clauseList = toList(clause)
        if clauseList.isEmpty then error("bad guard clause")
        clauseList.head match
          case Symbol("else") => evalSeqK(clauseList.tail, env, k)
          case test =>
            evalK(
              test,
              env,
              v =>
                if v != Bool(false) then
                  if clauseList.tail.isEmpty then k(v)
                  else evalSeqK(clauseList.tail, env, k)
                else BMore(() => evalGuardClauses(rest, env, k, exn))
            )
      case _ => error("bad guard syntax")

  /** Find the length of the common tail of two winding stacks (by reference identity). */
  private[ming] def commonWindTailLength(a: List[(Val, Val)], b: List[(Val, Val)]): Int =
    var aa = a; var bb = b
    if aa.length > bb.length then for _ <- 0 until (aa.length - bb.length) do aa = aa.tail
    else for _ <- 0 until (bb.length - aa.length) do bb = bb.tail
    while aa ne bb do
      aa = aa.tail; bb = bb.tail
    aa.length

  /** Run a sequence of thunks (zero-arg procedures) in order, then continue. */
  private[ming] def runThunks(thunks: List[Val], andThen: => Bounce): Bounce =
    thunks match
      case scala.Nil     => andThen
      case thunk :: rest => applyK(thunk, List.empty, _ => BMore(() => runThunks(rest, andThen)))
