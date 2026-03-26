package ming

import java.util.concurrent.atomic.AtomicLong

import SchemeEvaluatorState.*
import SchemeModel.*

private[ming] trait SchemeEvaluatorGuardForms extends SchemeEvaluatorSpecialForms:

  protected def applyWithExceptionHandler(
    handler: Value,
    thunk: Value,
    continuation: Continuation,
    pos: Option[SourcePos]
  ): Computation

  protected def captureContinuationValue(continuation: Continuation): Value

  private val nextGuardExitId = new AtomicLong()

  final protected def evalGuard(
    args: List[Expr],
    env: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation =
    args match
      case Expr.ListExpr(Expr.Symbol(exceptionName, _) :: clauses, _) :: body if body.nonEmpty =>
        val exitName   = s"__guard_exit_${nextGuardExitId.incrementAndGet()}"
        val handlerEnv = new Env(Some(env))
        handlerEnv.define(exitName, captureContinuationValue(continuation))

        val handler = Value.Closure(
          None,
          List(exceptionName),
          None,
          List(
            Expr.ListExpr(
              List(
                Expr.Symbol(exitName, pos),
                buildGuardCond(exceptionName, clauses, pos)
              ),
              pos
            )
          ),
          handlerEnv
        )
        val thunk = Value.Closure(None, Nil, None, body, env)
        applyWithExceptionHandler(handler, thunk, continuation, Some(pos))
      case _ =>
        throw new EvalError("invalid guard form")

  private def buildGuardCond(exceptionName: String, clauses: List[Expr], pos: SourcePos): Expr =
    val normalizedClauses =
      if hasGuardElseClause(clauses) then clauses
      else
        clauses :+ Expr.ListExpr(
          List(
            Expr.Symbol("else", pos),
            Expr.ListExpr(
              List(
                Expr.Symbol("raise", pos),
                Expr.Symbol(exceptionName, pos)
              ),
              pos
            )
          ),
          pos
        )

    Expr.ListExpr(Expr.Symbol("cond", pos) :: normalizedClauses, pos)

  private def hasGuardElseClause(clauses: List[Expr]): Boolean =
    clauses.exists {
      case Expr.ListExpr(Expr.Symbol("else", _) :: _, _) => true
      case _                                             => false
    }
