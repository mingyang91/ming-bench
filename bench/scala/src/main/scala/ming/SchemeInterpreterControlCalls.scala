package ming

private[ming] object SchemeInterpreterControlCalls:

  import BuiltinSupport.{singleArg, twoArgs}
  import SchemeInterpreter.{EvalState, Expr, Resume, Value}
  import SchemeValues.{pack, unpack}

  private type ApplyProcedureState =
    (Value, List[Value], SourcePos, Resume) => EvalState

  def applyCallWithValuesState(
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

  def applyCallWithCurrentContinuationState(
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
      callCcReturnCont(cont)
    )

  def resumeContinuationState(
    continuation: Value.Continuation,
    args: List[Value],
    pos: SourcePos,
    runtime: Runtime,
    recursiveApply: ApplyProcedureState
  ): EvalState =
    SchemeDynamicWind.resumeContinuationState(
      continuation,
      pack(args),
      runtime,
      recursiveApply
    )

  private def callCcReturnCont(cont: Resume): Resume =
    cont match
      case sequence: SchemeInterpreter.Resume.Sequence if sequence.expressions.headOption.exists(isCallCcExpr) =>
        value =>
          value match
            case Value.Void => sequence.next(value)
            case _          => cont(value)
      case _ =>
        cont

  private def isCallCcExpr(expr: Expr): Boolean =
    expr match
      case Expr.ListExpr(Expr.Symbol("call/cc", _) :: _, _)                        => true
      case Expr.ListExpr(Expr.Symbol("call-with-current-continuation", _) :: _, _) => true
      case _                                                                       => false
