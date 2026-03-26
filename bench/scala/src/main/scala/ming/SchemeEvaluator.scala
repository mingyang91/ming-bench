package ming

import scala.annotation.tailrec

import SchemeBuiltinSupport.*
import SchemeEvaluatorState.*
import SchemeModel.*
import SchemeRecords.*
import SchemeRuntime.*

private[ming] object SchemeEvaluator
    extends SchemeEvaluatorSpecialForms
    with SchemeEvaluatorMacroForms
    with SchemeEvaluatorGuardForms
    with SchemeEvaluatorBindingForms
    with SchemeEvaluatorProcedureSupport
    with SchemeEvaluatorApplicationSupport:

  private val finalContinuation: Continuation = value => done(requireSingleValue(value))

  def evalSequence(expressions: List[Expr], env: Env): Value =
    withFreshDynamicContext(run(evalSequence(expressions, env, finalContinuation)))

  def applyProcedure(procedure: Value, args: List[Value]): Value =
    withFreshDynamicContext(run(applyProcedure(procedure, args, finalContinuation, None)))

  private[ming] def applyProcedureInCurrentContext(
    procedure: Value,
    args: List[Value],
    pos: Option[SourcePos]
  ): Value =
    run(applyProcedure(procedure, args, finalContinuation, pos))

  override protected def eval(expr: Expr, env: Env, continuation: Continuation): Computation =
    evalExpr(expr, env, continuation)

  override protected def evalSequence(
    expressions: List[Expr],
    env: Env,
    continuation: Continuation
  ): Computation =
    evalSequenceCps(expressions, env, continuation)

  override protected def applyProcedure(
    procedure: Value,
    args: List[Value],
    continuation: Continuation,
    pos: Option[SourcePos]
  ): Computation =
    applyProcedureCps(procedure, args, continuation, pos)

  override protected def withErrorContext[T](pos: SourcePos)(thunk: => T): T =
    try thunk
    catch
      case error: EvalError if error.position.isEmpty =>
        throw error.withPosition(pos)

  private def withFreshDynamicContext(runEvaluation: => Value): Value =
    resetDynamicContext()
    try runEvaluation
    finally resetDynamicContext()

  @tailrec
  private def run(current: Computation): Value =
    current match
      case Computation.Done(value) =>
        value
      case Computation.Suspend(step) =>
        run(step())

  private def evalExpr(expr: Expr, env: Env, continuation: Continuation): Computation =
    suspend {
      withErrorContext(expr.pos) {
        evalExprStep(expr, env, continuation)
      }
    }

  private def evalExprStep(expr: Expr, env: Env, continuation: Continuation): Computation =
    expr match
      case Expr.IntegerLiteral(value, _) =>
        resume(continuation, Value.IntegerValue(value))
      case Expr.RationalLiteral(numerator, denominator, _) =>
        resume(continuation, SchemeNumbers.exactRational(numerator, denominator))
      case Expr.InexactLiteral(value, _) =>
        resume(continuation, Value.InexactValue(value))
      case Expr.BooleanLiteral(value, _) =>
        resume(continuation, Value.BooleanValue(value))
      case Expr.StringLiteral(value, _) =>
        resume(continuation, Value.StringValue(SchemeString.fromLiteral(value)))
      case Expr.CharLiteral(value, _) =>
        resume(continuation, Value.CharValue(value))
      case Expr.Symbol(name, _) =>
        resume(continuation, env.lookup(name))
      case Expr.ListExpr(Nil, _) =>
        throw new EvalError("cannot evaluate an empty list")
      case list @ Expr.ListExpr(operator :: args, _) =>
        evalCompoundExpression(list, operator, args, env, continuation)

  private def evalSequenceCps(
    expressions: List[Expr],
    env: Env,
    continuation: Continuation
  ): Computation =
    expressions match
      case Nil =>
        resume(continuation, Value.VoidValue)
      case last :: Nil =>
        eval(last, env, continuation)
      case head :: tail =>
        eval(
          head,
          env,
          contextualCont(head.pos) { _ =>
            suspend(evalSequenceCps(tail, env, continuation))
          }
        )

  private def applyProcedureCps(
    procedure: Value,
    args: List[Value],
    continuation: Continuation,
    pos: Option[SourcePos]
  ): Computation =
    suspend {
      withOptionalErrorContext(pos) {
        procedure match
          case builtin @ Value.Builtin(name, implementation) =>
            applyBuiltinProcedure(builtin, name, implementation, args, continuation, pos)
          case closure: Value.Closure =>
            applyClosure(
              closure.name.getOrElse("lambda"),
              closure.fixedParams,
              closure.restParam,
              closure.body,
              closure.env,
              args,
              continuation
            )
          case caseClosure: Value.CaseClosure =>
            val clause = caseClosure.clauses.find(clauseMatchesArgCount(_, args.length)).getOrElse {
              throw new EvalError(s"case-lambda expected a matching clause for ${args.length} argument(s)")
            }
            applyClosure(
              "case-lambda",
              clause.fixedParams,
              clause.restParam,
              clause.body,
              clause.env,
              args,
              continuation
            )
          case other =>
            throw new EvalError(s"not a procedure: ${SchemeRuntime.render(other)}")
      }
    }

  private def applyBuiltinProcedure(
    builtin: Value.Builtin,
    name: String,
    implementation: List[Value] => Value,
    args: List[Value],
    continuation: Continuation,
    pos: Option[SourcePos]
  ): Computation =
    capturedContinuation(builtin) match
      case Some(saved) =>
        applyCapturedContinuation(name, saved, args, pos)
      case None =>
        applyBuiltinImplementation(name, implementation, args, continuation, pos)

  private def applyBuiltinImplementation(
    name: String,
    implementation: List[Value] => Value,
    args: List[Value],
    continuation: Continuation,
    pos: Option[SourcePos]
  ): Computation =
    name match
      case "call/cc" | "call-with-current-continuation" =>
        applyCallWithCurrentContinuation(name, args, continuation, pos)
      case "dynamic-wind" =>
        applyDynamicWind(args, continuation, pos)
      case "with-exception-handler" =>
        applyWithExceptionHandlerBuiltin(args, continuation, pos)
      case "raise" =>
        applyRaise(args, pos)
      case "values" =>
        applyValues(args, continuation)
      case "call-with-values" =>
        applyCallWithValues(args, continuation, pos)
      case "apply" =>
        applyBuiltinApply(args, continuation, pos)
      case "map" =>
        applyBuiltinMap(args, continuation, pos)
      case "for-each" =>
        applyBuiltinForEach(args, continuation, pos)
      case _ =>
        resume(continuation, implementation(args))

  private def clauseMatchesArgCount(clause: CaseLambdaClause, argCount: Int): Boolean =
    clause.restParam match
      case Some(_) => argCount >= clause.fixedParams.length
      case None    => argCount == clause.fixedParams.length

  private def applyClosure(
    name: String,
    fixedParams: List[String],
    restParam: Option[String],
    body: List[Expr],
    closureEnv: Env,
    args: List[Value],
    continuation: Continuation
  ): Computation =
    restParam match
      case Some(_) =>
        requireMinArgCount(name, args, fixedParams.length)
      case None =>
        requireArgCount(name, args, fixedParams.length)

    val callEnv = new Env(Some(closureEnv))
    fixedParams.zip(args).foreach { case (paramName, value) =>
      callEnv.define(paramName, value)
    }
    restParam.foreach { restName =>
      callEnv.define(restName, makeList(args.drop(fixedParams.length)))
    }
    evalSequence(body, callEnv, continuation)

  private def withOptionalErrorContext[T](pos: Option[SourcePos])(thunk: => T): T =
    pos match
      case Some(sourcePos) =>
        withErrorContext(sourcePos)(thunk)
      case None =>
        thunk
