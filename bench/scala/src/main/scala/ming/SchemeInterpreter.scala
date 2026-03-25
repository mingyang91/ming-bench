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

  private[ming] def evalExpr(expr: Expr, env: Env, macros: MacroScope): Value =
    withCurrentRuntime { _ =>
      runState(evalExprState(expr, env, macros, halt))
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
        case EvalState.PopExceptionHandler(frame, value, cont) =>
          currentRuntime.popExceptionHandler(frame)
          state = cont(value)

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
        EvalState.EvaluateExpr(head, env, macros, Resume.Sequence(tail, env, macros, cont))

  private def evalExprState(expr: Expr, env: Env, macros: MacroScope, cont: Resume): EvalState =
    expr match
      case Expr.Number(value, _)    => cont(Value.Number(value))
      case Expr.Bool(value, _)      => cont(Value.Bool(value))
      case Expr.StringLit(value, _) => cont(Value.StringLit(value))
      case Expr.Character(value, _) => cont(Value.Character(value))
      case Expr.Symbol(name, pos)   => cont(env.lookup(name, pos))
      case Expr.VectorExpr(items, _) =>
        cont(Value.Vector(items.map(SchemeInterpreterSyntax.quote)))
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
      case head :: args =>
        evalNonEmptyListExpr(head, args, items, env, macros, pos, cont)

  private def evalNonEmptyListExpr(
    head: Expr,
    args: List[Expr],
    items: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    cont: Resume
  ): EvalState =
    head match
      case symbol: Expr.Symbol =>
        evalSymbolListExpr(symbol, args, items, env, macros, pos, cont)
      case _ =>
        evalProcedureCallState(head, args, env, macros, pos, cont)

  private def evalSymbolListExpr(
    symbol: Expr.Symbol,
    args: List[Expr],
    items: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    cont: Resume
  ): EvalState =
    SchemeInterpreterListDispatch.evalNamedFormState(
      symbol.name,
      args,
      env,
      macros,
      pos,
      cont,
      evalExprState,
      evalSequenceState,
      applyProcedureState
    ) match
      case Some(state) =>
        state
      case None =>
        evalMacroOrProcedureState(symbol, symbol.name, args, items, env, macros, pos, cont)

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
