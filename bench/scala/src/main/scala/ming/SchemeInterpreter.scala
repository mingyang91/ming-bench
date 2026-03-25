package ming

import scala.util.DynamicVariable

private[ming] object SchemeInterpreter extends SchemeInterpreterTypes:

  private val halt: Resume = value => EvalState.Done(value)
  private val runtimeVar   = new DynamicVariable[Option[Runtime]](None)

  def evalProgram(input: String): Value =
    runProgram(input)._1

  def evalProgramWithOutput(input: String): (Value, String) =
    val (result, runtime) = runProgram(input)
    (result, runtime.capturedOutput)

  def render(value: Value): String =
    SchemeRendering.render(value)

  def renderDisplay(value: Value): String =
    SchemeRendering.renderDisplay(value)

  private[ming] def currentRuntime: Runtime =
    runtimeVar.value.getOrElse {
      throw new IllegalStateException("no active runtime")
    }

  private def withRuntime[A](runtime: Runtime)(body: => A): A =
    runtimeVar.withValue(Some(runtime))(body)

  private def withCurrentRuntime[A](body: Runtime => A): A =
    runtimeVar.value match
      case Some(runtime) =>
        body(runtime)
      case None =>
        val runtime = Runtime()
        withRuntime(runtime) {
          body(runtime)
        }

  private def runProgram(input: String): (Value, Runtime) =
    val expressions = SchemeReader.readAll(input)
    if expressions.isEmpty then throw EvalError.at(SourcePos(1, 1), "empty input")
    val runtime    = Runtime()
    val env        = SchemeRuntimeSupport.initialEnv(runtime)
    val macroScope = MacroScope.root()
    (
      withRuntime(runtime) {
        evalSequence(expressions, env, macroScope)
      },
      runtime
    )

  private[ming] def evalSequence(expressions: List[Expr], env: Env, macros: MacroScope): Value =
    withCurrentRuntime { _ =>
      runState(evalSequenceState(expressions, env, macros, halt))
    }

  private def runState(initialState: EvalState): Value =
    var state = initialState

    while true do
      state match
        case EvalState.Done(value) =>
          return value
        case EvalState.EvaluateSequence(expressions, env, macros, cont) =>
          state = nextSequenceState(expressions, env, macros, cont)
        case EvalState.EvaluateExpr(expr, env, macros, cont) =>
          state = evalExprState(expr, env, macros, cont)

    throw new IllegalStateException("unreachable")

  private def nextSequenceState(
    expressions: List[Expr],
    env: Env,
    macros: MacroScope,
    cont: Resume
  ): EvalState =
    expressions match
      case Nil =>
        cont(Value.Void)
      case head :: Nil =>
        EvalState.EvaluateExpr(head, env, macros, cont)
      case head :: tail =>
        val next: Resume = _ => evalSequenceState(tail, env, macros, cont)
        EvalState.EvaluateExpr(head, env, macros, next)

  private def evalExprState(expr: Expr, env: Env, macros: MacroScope, cont: Resume): EvalState =
    expr match
      case Expr.Number(value, _)    => cont(Value.Number(value))
      case Expr.Bool(value, _)      => cont(Value.Bool(value))
      case Expr.StringLit(value, _) => cont(Value.StringLit(value))
      case Expr.Character(value, _) => cont(Value.Character(value))
      case Expr.Symbol(name, pos)   => cont(env.lookup(name, pos))
      case Expr.ListExpr(items, pos) =>
        evalListExpr(items, env, macros, pos, cont)

  private def evalSequenceState(
    expressions: List[Expr],
    env: Env,
    macros: MacroScope,
    cont: Resume
  ): EvalState =
    EvalState.EvaluateSequence(expressions, env, macros, cont)

  private def evalListExpr(
    items: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    cont: Resume
  ): EvalState =
    items match
      case Nil =>
        throw EvalError.at(pos, "cannot evaluate empty list")
      case Expr.Symbol("define-record-type", _) :: args =>
        cont(SchemeRecords.evalDefineRecordType(args, env, pos))
      case Expr.Symbol("define-syntax", _) :: args =>
        SchemeInterpreterSpecialForms.evalDefineSyntaxState(args, env, macros, pos, cont)
      case Expr.Symbol("define", _) :: args =>
        SchemeInterpreterSpecialForms.evalDefineState(args, env, macros, pos, cont, evalExprState)
      case Expr.Symbol("set!", _) :: args =>
        SchemeInterpreterSpecialForms.evalSetState(args, env, macros, pos, cont, evalExprState)
      case Expr.Symbol("begin", _) :: args =>
        evalSequenceState(args, env, macros, cont)
      case Expr.Symbol("if", _) :: args =>
        SchemeInterpreterSpecialForms.evalIfState(args, env, macros, pos, cont, evalExprState)
      case Expr.Symbol("let", _) :: args =>
        SchemeInterpreterBindingForms.evalLetState(
          args,
          env,
          macros,
          pos,
          cont,
          evalExprState,
          evalSequenceState,
          applyProcedureState
        )
      case Expr.Symbol("let*", _) :: args =>
        SchemeInterpreterBindingForms.evalLetStarState(
          args,
          env,
          macros,
          pos,
          cont,
          evalExprState,
          evalSequenceState
        )
      case Expr.Symbol("letrec", _) :: args =>
        SchemeInterpreterBindingForms.evalLetrecState(
          args,
          env,
          macros,
          pos,
          cont,
          evalExprState,
          evalSequenceState
        )
      case Expr.Symbol("letrec*", _) :: args =>
        SchemeInterpreterBindingForms.evalLetrecStarState(
          args,
          env,
          macros,
          pos,
          cont,
          evalExprState,
          evalSequenceState
        )
      case Expr.Symbol("cond", _) :: args =>
        SchemeInterpreterSpecialForms.evalCondState(
          args,
          env,
          macros,
          pos,
          cont,
          evalExprState,
          evalSequenceState
        )
      case Expr.Symbol("case", _) :: args =>
        SchemeInterpreterSpecialForms.evalCaseState(
          args,
          env,
          macros,
          pos,
          cont,
          evalExprState,
          evalSequenceState
        )
      case Expr.Symbol("quote", _) :: args =>
        SchemeInterpreterSpecialForms.evalQuoteState(args, pos, cont)
      case Expr.Symbol("lambda", _) :: args =>
        SchemeInterpreterSpecialForms.evalLambdaState(args, env, macros, pos, cont)
      case Expr.Symbol("case-lambda", _) :: args =>
        cont(SchemeProcedures.evalCaseLambda(args, env, macros, pos))
      case Expr.Symbol("do", _) :: args =>
        evalExprState(SchemeInterpreterDoSupport.expand(args, pos), env, macros, cont)
      case Expr.Symbol("and", _) :: args =>
        SchemeInterpreterSpecialForms.evalAndState(args, env, macros, cont, evalExprState)
      case Expr.Symbol("or", _) :: args =>
        SchemeInterpreterSpecialForms.evalOrState(args, env, macros, cont, evalExprState)
      case (symbol @ Expr.Symbol(name, _)) :: args =>
        evalMacroOrProcedureState(symbol, name, args, items, env, macros, pos, cont)
      case head :: args =>
        evalProcedureCallState(head, args, env, macros, pos, cont)

  private def evalMacroOrProcedureState(
    symbol: Expr.Symbol,
    name: String,
    args: List[Expr],
    items: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    cont: Resume
  ): EvalState =
    macros.lookup(name) match
      case Some(macroDef) =>
        val expanded = macroDef.expand(Expr.ListExpr(items, pos), pos)
        evalExprState(expanded, env, macros, cont)
      case None =>
        evalProcedureCallState(symbol, args, env, macros, pos, cont)

  private def evalProcedureCallState(
    procedureExpr: Expr,
    argExprs: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    cont: Resume
  ): EvalState =
    SchemeInterpreterProcedureCalls.evalProcedureCallState(
      procedureExpr,
      argExprs,
      env,
      macros,
      pos,
      cont,
      evalExprState,
      applyProcedureState
    )

  private def applyProcedureState(
    value: Value,
    args: List[Value],
    pos: SourcePos,
    cont: Resume
  ): EvalState =
    SchemeInterpreterProcedureCalls.applyProcedureState(value, args, pos, cont, evalSequenceState)

  private[ming] def applyProcedure(value: Value, args: List[Value], pos: SourcePos): Value =
    withCurrentRuntime { _ =>
      runState(applyProcedureState(value, args, pos, halt))
    }
