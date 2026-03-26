package ming

import SchemeBuiltinSupport.*
import SchemeEvaluatorState.*
import SchemeEvaluatorSupport.*
import SchemeModel.*
import SchemeRuntime.*

abstract private[ming] class SchemeEvaluatorSpecialForms extends SchemeEvaluatorQuotedForms:

  protected def evalSequence(
    expressions: List[Expr],
    env: Env,
    continuation: Continuation
  ): Computation

  protected def applyProcedure(
    procedure: Value,
    args: List[Value],
    continuation: Continuation,
    pos: Option[SourcePos]
  ): Computation

  protected def withErrorContext[T](pos: SourcePos)(thunk: => T): T

  final protected def contextual(pos: SourcePos)(thunk: => Computation): Computation =
    suspend(withErrorContext(pos)(thunk))

  final protected def contextualCont(pos: SourcePos)(next: Value => Computation): Continuation =
    value => contextual(pos)(next(requireSingleValue(value)))

  final protected def evalExprList(
    expressions: List[Expr],
    env: Env
  )(continuation: List[Value] => Computation): Computation =
    def loop(remaining: List[Expr], reversedValues: List[Value]): Computation =
      remaining match
        case Nil =>
          suspend(continuation(reversedValues.reverse))
        case expr :: tail =>
          eval(
            expr,
            env,
            contextualCont(expr.pos) { value =>
              suspend(loop(tail, value :: reversedValues))
            }
          )

    loop(expressions, Nil)

  protected def evalIf(
    args: List[Expr],
    env: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation =
    args match
      case condition :: whenTrue :: Nil =>
        eval(
          condition,
          env,
          contextualCont(pos) { value =>
            if isTruthy(value) then eval(whenTrue, env, continuation)
            else resume(continuation, Value.VoidValue)
          }
        )
      case condition :: whenTrue :: whenFalse :: Nil =>
        eval(
          condition,
          env,
          contextualCont(pos) { value =>
            if isTruthy(value) then eval(whenTrue, env, continuation)
            else eval(whenFalse, env, continuation)
          }
        )
      case _ =>
        throw new EvalError(s"if expected 2 or 3 arguments, got ${args.length}")

  protected def evalCase(
    args: List[Expr],
    env: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation =
    args match
      case keyExpr :: clauses if clauses.nonEmpty =>
        def loop(key: Value, remaining: List[Expr]): Computation =
          remaining match
            case Nil =>
              resume(continuation, Value.VoidValue)
            case Expr.ListExpr(Nil, _) :: _ =>
              throw new EvalError("case clauses must be non-empty lists")
            case Expr.ListExpr(Expr.Symbol("else", _) :: expressions, _) :: tail =>
              if tail.nonEmpty then throw new EvalError("case else clause must be last")
              evalSequence(expressions, env, continuation)
            case Expr.ListExpr(Expr.ListExpr(datums, _) :: expressions, _) :: tail =>
              if datums.exists(datum => eqvValues(key, quoteExpr(datum))) then
                evalSequence(expressions, env, continuation)
              else contextual(pos)(loop(key, tail))
            case _ =>
              throw new EvalError("case clauses must begin with a datum list or else")

        eval(keyExpr, env, contextualCont(pos)(key => loop(key, clauses)))
      case _ =>
        throw new EvalError("invalid case form")

  protected def evalDefine(
    args: List[Expr],
    env: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation =
    args match
      case Expr.Symbol(name, _) :: valueExpr :: Nil =>
        eval(
          valueExpr,
          env,
          contextualCont(pos) { value =>
            env.define(name, value)
            resume(continuation, Value.VoidValue)
          }
        )
      case Expr.ListExpr(Expr.Symbol(name, _) :: params, _) :: body if body.nonEmpty =>
        env.define(name, buildClosure(params, body, env, Some(name)))
        resume(continuation, Value.VoidValue)
      case _ =>
        throw new EvalError("invalid define form")

  protected def evalDefineSyntax(
    args: List[Expr],
    env: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation

  protected def evalSyntax(
    args: List[Expr],
    env: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation

  protected def evalSyntaxCase(
    args: List[Expr],
    env: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation

  protected def evalWithSyntax(
    args: List[Expr],
    env: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation

  protected def evalLambda(
    args: List[Expr],
    env: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation =
    args match
      case Expr.ListExpr(params, _) :: body if body.nonEmpty =>
        resume(continuation, buildClosure(params, body, env, None))
      case _ =>
        throw new EvalError("invalid lambda form")

  protected def evalCaseLambda(
    args: List[Expr],
    env: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation =
    resume(continuation, buildCaseClosure(args, env))

  protected def evalSet(
    args: List[Expr],
    env: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation =
    args match
      case Expr.Symbol(name, _) :: valueExpr :: Nil =>
        eval(
          valueExpr,
          env,
          contextualCont(pos) { value =>
            env.set(name, value)
            resume(continuation, Value.VoidValue)
          }
        )
      case _ =>
        throw new EvalError("invalid set! form")

  protected def evalCond(
    clauses: List[Expr],
    env: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation =
    def loop(remaining: List[Expr]): Computation =
      remaining match
        case Nil =>
          resume(continuation, Value.VoidValue)
        case Expr.ListExpr(Nil, _) :: _ =>
          throw new EvalError("cond clauses must be non-empty lists")
        case Expr.ListExpr(Expr.Symbol("else", _) :: expressions, _) :: tail =>
          if tail.nonEmpty then throw new EvalError("cond else clause must be last")
          evalSequence(expressions, env, continuation)
        case Expr.ListExpr(List(testExpr, Expr.Symbol("=>", _), procedureExpr), _) :: tail =>
          eval(
            testExpr,
            env,
            contextualCont(pos) { testValue =>
              if isTruthy(testValue) then
                eval(
                  procedureExpr,
                  env,
                  contextualCont(procedureExpr.pos) { procedure =>
                    applyProcedure(procedure, List(testValue), continuation, Some(procedureExpr.pos))
                  }
                )
              else loop(tail)
            }
          )
        case Expr.ListExpr(testExpr :: expressions, _) :: tail =>
          eval(
            testExpr,
            env,
            contextualCont(pos) { testValue =>
              if isTruthy(testValue) then
                if expressions.isEmpty then resume(continuation, testValue)
                else evalSequence(expressions, env, continuation)
              else loop(tail)
            }
          )
        case _ =>
          throw new EvalError("cond clauses must be non-empty lists")

    loop(clauses)

  protected def evalAnd(
    args: List[Expr],
    env: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation =
    def loop(remaining: List[Expr]): Computation =
      remaining match
        case Nil =>
          resume(continuation, Value.BooleanValue(true))
        case last :: Nil =>
          eval(last, env, continuation)
        case head :: tail =>
          eval(
            head,
            env,
            contextualCont(pos) { value =>
              if isTruthy(value) then loop(tail) else resume(continuation, value)
            }
          )

    loop(args)

  protected def evalOr(
    args: List[Expr],
    env: Env,
    continuation: Continuation,
    pos: SourcePos
  ): Computation =
    def loop(remaining: List[Expr]): Computation =
      remaining match
        case Nil =>
          resume(continuation, Value.BooleanValue(false))
        case last :: Nil =>
          eval(last, env, continuation)
        case head :: tail =>
          eval(
            head,
            env,
            contextualCont(pos) { value =>
              if isTruthy(value) then resume(continuation, value) else loop(tail)
            }
          )

    loop(args)
