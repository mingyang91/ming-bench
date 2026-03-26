package ming

import SchemeBuiltinSupport.*
import SchemeEvaluatorState.*
import SchemeModel.*
import SchemeRecords.*
import SchemeRuntime.*

private[ming] object SchemeEvaluator extends SchemeEvaluatorSpecialForms:

  def evalSequence(expressions: List[Expr], env: Env): Value =
    run(EvalState.SequenceState(expressions, env))

  def applyProcedure(procedure: Value, args: List[Value]): Value =
    run(EvalState.CallState(procedure, args, None))

  override protected def eval(expr: Expr, env: Env): Value =
    run(EvalState.ExprState(expr, env))

  // The evaluator runs a small explicit state machine so tail-position calls
  // bounce through this loop instead of consuming the JVM stack.
  private def run(initialState: EvalState): Value =
    var state = initialState

    while true do
      val step =
        state match
          case EvalState.ExprState(expr, env) =>
            withErrorContext(expr.pos) {
              evalExprStep(expr, env)
            }
          case EvalState.SequenceState(expressions, env) =>
            evalSequenceStep(expressions, env)
          case EvalState.CallState(procedure, args, pos) =>
            withOptionalErrorContext(pos) {
              applyProcedureStep(procedure, args)
            }

      step match
        case StepResult.Final(value) =>
          return value
        case StepResult.Continue(nextState) =>
          state = nextState

    throw new IllegalStateException("unreachable")

  private def evalExprStep(expr: Expr, env: Env): StepResult =
    expr match
      case Expr.IntegerLiteral(value, _) =>
        StepResult.Final(Value.IntegerValue(value))
      case Expr.RationalLiteral(numerator, denominator, _) =>
        StepResult.Final(SchemeNumbers.exactRational(numerator, denominator))
      case Expr.InexactLiteral(value, _) =>
        StepResult.Final(Value.InexactValue(value))
      case Expr.BooleanLiteral(value, _) =>
        StepResult.Final(Value.BooleanValue(value))
      case Expr.StringLiteral(value, _) =>
        StepResult.Final(Value.StringValue(SchemeString.fromLiteral(value)))
      case Expr.CharLiteral(value, _) =>
        StepResult.Final(Value.CharValue(value))
      case Expr.Symbol(name, _) =>
        StepResult.Final(env.lookup(name))
      case Expr.ListExpr(Nil, _) =>
        throw new EvalError("cannot evaluate an empty list")
      case list @ Expr.ListExpr(operator :: args, _) =>
        evalCompoundExpression(list, operator, args, env)

  private def evalSequenceStep(expressions: List[Expr], env: Env): StepResult =
    var remaining = expressions

    while remaining match
        case _ :: _ :: _ => true
        case _           => false
    do
      eval(remaining.head, env)
      remaining = remaining.tail

    remaining match
      case Nil =>
        StepResult.Final(Value.VoidValue)
      case last :: Nil =>
        StepResult.Continue(EvalState.ExprState(last, env))
      case _ =>
        throw new IllegalStateException("unexpected sequence shape")

  private def applyProcedureStep(procedure: Value, args: List[Value]): StepResult =
    procedure match
      case Value.Builtin(_, implementation) =>
        StepResult.Final(implementation(args))
      case closure: Value.Closure =>
        StepResult.Continue(buildCallState(closure.name.getOrElse("lambda"), closure, args))
      case caseClosure: Value.CaseClosure =>
        val clause = caseClosure.clauses.find(clauseMatchesArgCount(_, args.length)).getOrElse {
          throw new EvalError(s"case-lambda expected a matching clause for ${args.length} argument(s)")
        }
        StepResult.Continue(buildCallState("case-lambda", clause, args))
      case other =>
        throw new EvalError(s"not a procedure: ${SchemeRuntime.render(other)}")

  private def evalCompoundExpression(
    list: Expr.ListExpr,
    operator: Expr,
    args: List[Expr],
    env: Env
  ): StepResult =
    operator match
      case Expr.Symbol(name, _) =>
        env.lookupSyntax(name) match
          case Some(transformer) =>
            val expanded = transformer.expand(list, env)
            StepResult.Continue(EvalState.ExprState(expanded.expr, expanded.env))
          case None =>
            evalApplication(list.pos, operator, args, env)
      case _ =>
        evalApplication(list.pos, operator, args, env)

  private def evalApplication(
    callPos: SourcePos,
    operator: Expr,
    args: List[Expr],
    env: Env
  ): StepResult =
    operator match
      case Expr.Symbol("quote", _) =>
        evalQuote(args)
      case Expr.Symbol("if", _) =>
        evalIf(args, env)
      case Expr.Symbol("case", _) =>
        evalCase(args, env)
      case Expr.Symbol("define", _) =>
        evalDefine(args, env)
      case Expr.Symbol("define-syntax", _) =>
        evalDefineSyntax(args, env)
      case Expr.Symbol("define-record-type", _) =>
        StepResult.Final(defineRecordType(args, env))
      case Expr.Symbol("lambda", _) =>
        evalLambda(args, env)
      case Expr.Symbol("case-lambda", _) =>
        evalCaseLambda(args, env)
      case Expr.Symbol("set!", _) =>
        evalSet(args, env)
      case Expr.Symbol("and", _) =>
        evalAnd(args, env)
      case Expr.Symbol("or", _) =>
        evalOr(args, env)
      case Expr.Symbol("begin", _) =>
        StepResult.Continue(EvalState.SequenceState(args, env))
      case Expr.Symbol("cond", _) =>
        evalCond(args, env)
      case Expr.Symbol("let", _) =>
        evalLet(args, env, callPos)
      case Expr.Symbol("letrec", _) =>
        evalLetRec(args, env, sequential = false)
      case Expr.Symbol("letrec*", _) =>
        evalLetRec(args, env, sequential = true)
      case Expr.Symbol("do", _) =>
        evalDo(args, env)
      case _ =>
        val procedure     = eval(operator, env)
        val evaluatedArgs = args.map(arg => eval(arg, env))
        StepResult.Continue(EvalState.CallState(procedure, evaluatedArgs, Some(callPos)))

  private def clauseMatchesArgCount(clause: CaseLambdaClause, argCount: Int): Boolean =
    clause.restParam match
      case Some(_) => argCount >= clause.fixedParams.length
      case None    => argCount == clause.fixedParams.length

  private def buildCallState(
    name: String,
    closure: Value.Closure,
    args: List[Value]
  ): EvalState =
    buildCallState(name, closure.fixedParams, closure.restParam, closure.body, closure.env, args)

  private def buildCallState(
    name: String,
    clause: CaseLambdaClause,
    args: List[Value]
  ): EvalState =
    buildCallState(name, clause.fixedParams, clause.restParam, clause.body, clause.env, args)

  private def buildCallState(
    name: String,
    fixedParams: List[String],
    restParam: Option[String],
    body: List[Expr],
    closureEnv: Env,
    args: List[Value]
  ): EvalState =
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
    EvalState.SequenceState(body, callEnv)

  private def withOptionalErrorContext[T](pos: Option[SourcePos])(thunk: => T): T =
    pos match
      case Some(sourcePos) =>
        withErrorContext(sourcePos)(thunk)
      case None =>
        thunk

  private def withErrorContext[T](pos: SourcePos)(thunk: => T): T =
    try thunk
    catch
      case error: EvalError if error.position.isEmpty =>
        throw error.withPosition(pos)
