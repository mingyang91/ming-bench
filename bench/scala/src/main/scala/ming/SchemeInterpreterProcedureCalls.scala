package ming

private[ming] object SchemeInterpreterProcedureCalls:

  import BuiltinSupport.{asList, requireAtLeast, singleArg, twoArgs}
  import SchemeValues.{pack, unpack}
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
    val runtime        = SchemeInterpreter.currentRuntime
    val recursiveApply = recursiveApplyState(evalSequenceState)

    value match
      case Value.Builtin(_, impl) =>
        cont(impl(args, pos))
      case Value.ApplyProcedureBuiltin =>
        requireAtLeast("apply", args, expected = 2, pos)
        val procedure = args.head
        val prefix    = args.tail.dropRight(1)
        val rest      = asList(args.last, "apply", pos)
        recursiveApply(procedure, prefix ++ rest, pos, cont)
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
      case Value.ValuesBuiltin =>
        cont(pack(args))
      case Value.CallWithValuesBuiltin =>
        applyCallWithValuesState(args, pos, cont, recursiveApply)
      case Value.DynamicWindBuiltin =>
        SchemeDynamicWind.applyState(
          args,
          pos,
          cont,
          runtime,
          recursiveApply
        )
      case Value.RaiseBuiltin =>
        SchemeExceptions.raiseState(
          args,
          pos,
          runtime,
          recursiveApply
        )
      case Value.WithExceptionHandlerBuiltin =>
        SchemeExceptions.withExceptionHandlerState(
          args,
          pos,
          cont,
          runtime,
          recursiveApply
        )
      case Value.CallWithCurrentContinuation =>
        applyCallWithCurrentContinuationState(args, pos, cont, runtime, recursiveApply)
      case continuation: Value.Continuation =>
        resumeContinuationState(continuation, args, pos, runtime, recursiveApply)
      case Value.Closure(params, body, closureEnv, closureMacros) =>
        applyUserProcedureState(
          params,
          body,
          closureEnv,
          closureMacros,
          args,
          pos,
          "lambda",
          cont,
          evalSequenceState
        )
      case Value.CaseLambda(clauses, closureEnv, closureMacros) =>
        applyCaseLambdaState(
          clauses,
          closureEnv,
          closureMacros,
          args,
          pos,
          cont,
          evalSequenceState
        )
      case other =>
        throw EvalError.at(pos, s"not a procedure: ${SchemeRendering.render(other)}")

  private def recursiveApplyState(evalSequenceState: EvalSequenceState): ApplyProcedureState =
    (procedure, callArgs, callPos, callCont) =>
      applyProcedureState(procedure, callArgs, callPos, callCont, evalSequenceState)

  private def applyCallWithValuesState(
    args: List[Value],
    pos: SourcePos,
    cont: Resume,
    recursiveApply: ApplyProcedureState
  ): EvalState =
    val (producer, consumer) = twoArgs("call-with-values", args, pos)
    recursiveApply(
      producer,
      Nil,
      pos,
      produced => recursiveApply(consumer, unpack(produced), pos, cont)
    )

  private def applyCallWithCurrentContinuationState(
    args: List[Value],
    pos: SourcePos,
    cont: Resume,
    runtime: Runtime,
    recursiveApply: ApplyProcedureState
  ): EvalState =
    val procedure = singleArg("call/cc", args, pos)
    recursiveApply(
      procedure,
      List(SchemeDynamicWind.captureContinuation(cont, runtime)),
      pos,
      cont
    )

  private def resumeContinuationState(
    continuation: Value.Continuation,
    args: List[Value],
    pos: SourcePos,
    runtime: Runtime,
    recursiveApply: ApplyProcedureState
  ): EvalState =
    val argument = singleArg("continuation", args, pos)
    SchemeDynamicWind.resumeContinuationState(
      continuation,
      argument,
      runtime,
      recursiveApply
    )

  private def applyUserProcedureState(
    params: LambdaParams,
    body: List[Expr],
    closureEnv: Env,
    closureMacros: MacroScope,
    args: List[Value],
    pos: SourcePos,
    name: String,
    cont: Resume,
    evalSequenceState: EvalSequenceState
  ): EvalState =
    val prepared = SchemeProcedures.prepareUserProcedure(
      params,
      body,
      closureEnv,
      closureMacros,
      args,
      pos,
      name
    )
    evalSequenceState(prepared.body, prepared.env, prepared.macros, cont)

  private def applyCaseLambdaState(
    clauses: List[Value.CaseLambdaClause],
    closureEnv: Env,
    closureMacros: MacroScope,
    args: List[Value],
    pos: SourcePos,
    cont: Resume,
    evalSequenceState: EvalSequenceState
  ): EvalState =
    val Value.CaseLambdaClause(params, body) =
      SchemeProcedures.selectCaseLambdaClause(clauses, args.length, pos)
    applyUserProcedureState(
      params,
      body,
      closureEnv,
      closureMacros,
      args,
      pos,
      "case-lambda",
      cont,
      evalSequenceState
    )

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
