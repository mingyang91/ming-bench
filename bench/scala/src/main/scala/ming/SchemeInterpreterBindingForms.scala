package ming

private[ming] object SchemeInterpreterBindingForms:

  import SchemeInterpreter.{EvalState, Expr, Resume, Value}
  import SchemeInterpreterSyntax.*

  private type EvalExprState =
    (Expr, Env, MacroScope, Resume) => EvalState

  private type EvalSequenceState =
    (List[Expr], Env, MacroScope, Resume) => EvalState

  private type ApplyProcedureState =
    (Value, List[Value], SourcePos, Resume) => EvalState

  def evalLetState(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    cont: Resume,
    evalExprState: EvalExprState,
    evalSequenceState: EvalSequenceState,
    applyProcedureState: ApplyProcedureState
  ): EvalState =
    args match
      case Expr.ListExpr(bindingsExpr, _) :: body if body.nonEmpty =>
        val bindings = readBindings(bindingsExpr)
        evalParallelBindingsState(
          bindings,
          env,
          macros,
          values =>
            val letEnv    = Env.child(env, bindings.map(_._1).zip(values))
            val letMacros = MacroScope.child(macros)
            evalSequenceState(body, letEnv, letMacros, cont)
          ,
          evalExprState
        )
      case Expr.Symbol(name, _) :: Expr.ListExpr(bindingsExpr, _) :: body if body.nonEmpty =>
        val bindings = readBindings(bindingsExpr)
        evalParallelBindingsState(
          bindings,
          env,
          macros,
          values =>
            val letEnv    = Env.child(env, Nil)
            val letMacros = MacroScope.child(macros)
            val closure   = Value.Closure(LambdaParams.fixed(bindings.map(_._1)), body, letEnv, letMacros)
            letEnv.define(name, closure)
            applyProcedureState(closure, values, pos, cont)
          ,
          evalExprState
        )
      case _ =>
        throw EvalError.at(pos, "invalid let")

  def evalLetStarState(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    cont: Resume,
    evalExprState: EvalExprState,
    evalSequenceState: EvalSequenceState
  ): EvalState =
    args match
      case Expr.ListExpr(bindingsExpr, _) :: body if body.nonEmpty =>
        val bindings      = readBindings(bindingsExpr)
        val letStarEnv    = Env.child(env, Nil)
        val letStarMacros = MacroScope.child(macros)
        evalLetStarBindingsState(bindings, letStarEnv, letStarMacros, body, cont, evalExprState, evalSequenceState)
      case _ =>
        throw EvalError.at(pos, "invalid let*")

  def evalLetrecState(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    cont: Resume,
    evalExprState: EvalExprState,
    evalSequenceState: EvalSequenceState
  ): EvalState =
    args match
      case Expr.ListExpr(bindingsExpr, _) :: body if body.nonEmpty =>
        val bindings     = readBindings(bindingsExpr)
        val letrecEnv    = Env.child(env, Nil)
        val letrecMacros = MacroScope.child(macros)

        bindings.foreach { case (name, _) =>
          letrecEnv.define(name, Value.Void)
        }

        evalParallelBindingsState(
          bindings,
          letrecEnv,
          letrecMacros,
          values =>
            bindings.zip(values).foreach { case ((name, _), value) =>
              letrecEnv.assign(name, value, pos)
            }
            evalSequenceState(body, letrecEnv, letrecMacros, cont)
          ,
          evalExprState
        )
      case _ =>
        throw EvalError.at(pos, "invalid letrec")

  def evalLetrecStarState(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    cont: Resume,
    evalExprState: EvalExprState,
    evalSequenceState: EvalSequenceState
  ): EvalState =
    args match
      case Expr.ListExpr(bindingsExpr, _) :: body if body.nonEmpty =>
        val bindings     = readBindings(bindingsExpr)
        val letrecEnv    = Env.child(env, Nil)
        val letrecMacros = MacroScope.child(macros)
        evalLetrecStarBindingsState(bindings, letrecEnv, letrecMacros, body, cont, evalExprState, evalSequenceState)
      case _ =>
        throw EvalError.at(pos, "invalid letrec*")

  private def evalLetStarBindingsState(
    bindings: List[(String, Expr)],
    env: Env,
    macros: MacroScope,
    body: List[Expr],
    cont: Resume,
    evalExprState: EvalExprState,
    evalSequenceState: EvalSequenceState
  ): EvalState =
    bindings match
      case Nil =>
        evalSequenceState(body, env, macros, cont)
      case (name, valueExpr) :: rest =>
        evalExprState(
          valueExpr,
          env,
          macros,
          value =>
            env.define(name, value)
            evalLetStarBindingsState(rest, env, macros, body, cont, evalExprState, evalSequenceState)
        )

  private def evalLetrecStarBindingsState(
    bindings: List[(String, Expr)],
    env: Env,
    macros: MacroScope,
    body: List[Expr],
    cont: Resume,
    evalExprState: EvalExprState,
    evalSequenceState: EvalSequenceState
  ): EvalState =
    bindings match
      case Nil =>
        evalSequenceState(body, env, macros, cont)
      case (name, valueExpr) :: rest =>
        env.define(name, Value.Void)
        evalExprState(
          valueExpr,
          env,
          macros,
          value =>
            env.assign(name, value, valueExpr.pos)
            evalLetrecStarBindingsState(rest, env, macros, body, cont, evalExprState, evalSequenceState)
        )

  private def evalParallelBindingsState(
    bindings: List[(String, Expr)],
    env: Env,
    macros: MacroScope,
    onComplete: List[Value] => EvalState,
    evalExprState: EvalExprState
  ): EvalState =
    evalParallelBindingsReversedState(bindings.reverse, env, macros, Nil, onComplete, evalExprState)

  private def evalParallelBindingsReversedState(
    remaining: List[(String, Expr)],
    env: Env,
    macros: MacroScope,
    values: List[Value],
    onComplete: List[Value] => EvalState,
    evalExprState: EvalExprState
  ): EvalState =
    remaining match
      case Nil =>
        onComplete(values)
      case (_, valueExpr) :: rest =>
        evalExprState(
          valueExpr,
          env,
          macros,
          value => evalParallelBindingsReversedState(rest, env, macros, value :: values, onComplete, evalExprState)
        )
