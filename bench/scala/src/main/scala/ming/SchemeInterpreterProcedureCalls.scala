package ming

private[ming] object SchemeInterpreterProcedureCalls:

  import BuiltinSupport.{asList, requireAtLeast, requireExactly}
  import SchemeInterpreter.{EvalState, Expr, Resume, Value}

  private type EvalExprState =
    (Expr, Env, MacroScope, Resume) => EvalState

  private type EvalSequenceState =
    (List[Expr], Env, MacroScope, Resume) => EvalState

  private type ApplyProcedureState =
    (Value, List[Value], SourcePos, Resume) => EvalState

  def evalProcedureCallState(
    procedureExpr: Expr,
    argExprs: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    cont: Resume,
    evalExprState: EvalExprState,
    applyProcedureState: ApplyProcedureState
  ): EvalState =
    evalExprState(
      procedureExpr,
      env,
      macros,
      procedure =>
        evalArgumentsState(procedure, argExprs.reverse, Nil, env, macros, pos, cont, evalExprState, applyProcedureState)
    )

  def applyProcedureState(
    value: Value,
    args: List[Value],
    pos: SourcePos,
    cont: Resume,
    evalSequenceState: EvalSequenceState
  ): EvalState =
    val runtime = SchemeInterpreter.currentRuntime

    value match
      case Value.Builtin(_, impl) =>
        cont(impl(args, pos))
      case Value.ApplyProcedureBuiltin =>
        requireAtLeast("apply", args, expected = 2, pos)
        val procedure = args.head
        val prefix    = args.tail.dropRight(1)
        val rest      = asList(args.last, "apply", pos)
        applyProcedureState(procedure, prefix ++ rest, pos, cont, evalSequenceState)
      case Value.MapProcedureBuiltin =>
        requireAtLeast("map", args, expected = 2, pos)
        val procedure = args.head
        val lists     = args.tail.map(asList(_, "map", pos))
        evalMapState(procedure, lists, Nil, pos, cont, evalSequenceState)
      case Value.ForEachProcedureBuiltin =>
        requireAtLeast("for-each", args, expected = 2, pos)
        val procedure = args.head
        val lists     = args.tail.map(asList(_, "for-each", pos))
        evalForEachState(procedure, lists, pos, cont, evalSequenceState)
      case Value.DynamicWindBuiltin =>
        SchemeDynamicWind.applyState(
          args,
          pos,
          cont,
          runtime,
          (procedure, callArgs, callPos, callCont) =>
            applyProcedureState(procedure, callArgs, callPos, callCont, evalSequenceState)
        )
      case Value.CallWithCurrentContinuation =>
        requireExactly("call/cc", args, expected = 1, pos)
        args match
          case procedure :: Nil =>
            applyProcedureState(
              procedure,
              List(SchemeDynamicWind.captureContinuation(cont, runtime)),
              pos,
              cont,
              evalSequenceState
            )
          case _ =>
            throw new IllegalStateException("unreachable")
      case continuation: Value.Continuation =>
        requireExactly("continuation", args, expected = 1, pos)
        args match
          case argument :: Nil =>
            SchemeDynamicWind.resumeContinuationState(
              continuation,
              argument,
              runtime,
              (procedure, callArgs, callPos, callCont) =>
                applyProcedureState(procedure, callArgs, callPos, callCont, evalSequenceState)
            )
          case _ => throw new IllegalStateException("unreachable")
      case Value.Closure(params, body, closureEnv, closureMacros) =>
        val prepared = SchemeProcedures.prepareUserProcedure(
          params,
          body,
          closureEnv,
          closureMacros,
          args,
          pos,
          "lambda"
        )
        evalSequenceState(prepared.body, prepared.env, prepared.macros, cont)
      case Value.CaseLambda(clauses, closureEnv, closureMacros) =>
        val Value.CaseLambdaClause(params, body) =
          SchemeProcedures.selectCaseLambdaClause(clauses, args.length, pos)
        val prepared = SchemeProcedures.prepareUserProcedure(
          params,
          body,
          closureEnv,
          closureMacros,
          args,
          pos,
          "case-lambda"
        )
        evalSequenceState(prepared.body, prepared.env, prepared.macros, cont)
      case other =>
        throw EvalError.at(pos, s"not a procedure: ${SchemeRendering.render(other)}")

  private def evalArgumentsState(
    procedure: Value,
    remainingReversed: List[Expr],
    values: List[Value],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    cont: Resume,
    evalExprState: EvalExprState,
    applyProcedureState: ApplyProcedureState
  ): EvalState =
    remainingReversed match
      case Nil =>
        applyProcedureState(procedure, values, pos, cont)
      case argumentExpr :: rest =>
        evalExprState(
          argumentExpr,
          env,
          macros,
          value =>
            evalArgumentsState(
              procedure,
              rest,
              value :: values,
              env,
              macros,
              pos,
              cont,
              evalExprState,
              applyProcedureState
            )
        )

  private def evalMapState(
    procedure: Value,
    lists: List[List[Value]],
    acc: List[Value],
    pos: SourcePos,
    cont: Resume,
    evalSequenceState: EvalSequenceState
  ): EvalState =
    if lists.exists(_.isEmpty) then cont(Value.list(acc.reverse))
    else
      val callArgs = lists.map(_.head)
      val next     = lists.map(_.tail)
      applyProcedureState(
        procedure,
        callArgs,
        pos,
        value => evalMapState(procedure, next, value :: acc, pos, cont, evalSequenceState),
        evalSequenceState
      )

  private def evalForEachState(
    procedure: Value,
    lists: List[List[Value]],
    pos: SourcePos,
    cont: Resume,
    evalSequenceState: EvalSequenceState
  ): EvalState =
    if lists.exists(_.isEmpty) then cont(Value.Void)
    else
      val callArgs = lists.map(_.head)
      val next     = lists.map(_.tail)
      applyProcedureState(
        procedure,
        callArgs,
        pos,
        _ => evalForEachState(procedure, next, pos, cont, evalSequenceState),
        evalSequenceState
      )
