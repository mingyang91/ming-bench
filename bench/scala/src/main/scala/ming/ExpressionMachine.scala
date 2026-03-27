package ming

private[ming] enum MachineState:
  case EvalExpr(expr: Expr, env: Env)
  case ApplyValue(value: Value)
  case InvokeProcedure(procedure: Value, args: List[Value], pos: SourcePos)

final private[ming] class Machine(private[ming] val context: EvalContext):

  private[ming] var state: MachineState                     = MachineState.ApplyValue(Value.Void)
  private[ming] var frames: List[ContinuationFrame]         = Nil
  private[ming] var winds: List[DynamicWindContext]         = Nil
  private[ming] var handlers: List[ExceptionHandlerContext] = Nil

  def runExpr(expr: Expr, env: Env): Value =
    state = MachineState.EvalExpr(expr, env)
    runLoop()

  def runSequence(exprs: List[Expr], env: Env): Value =
    MachineExpressions.startSequence(this, exprs, env)
    runLoop()

  def runInvoke(procedure: Value, args: List[Value], pos: SourcePos): Value =
    state = MachineState.InvokeProcedure(procedure, args, pos)
    runLoop()

  private def runLoop(): Value =
    while true do
      state match
        case MachineState.EvalExpr(expr, env) =>
          MachineExpressions.evalExpr(this, expr, env)
        case MachineState.ApplyValue(value) =>
          frames match
            case Nil =>
              return value
            case frame :: rest =>
              frames = rest
              MachineContinuations.continueWith(this, frame, value)
        case MachineState.InvokeProcedure(procedure, args, pos) =>
          MachineProcedures.invokeProcedure(this, procedure, args, pos)

    throw IllegalStateException("unreachable")

  private[ming] def setExpr(expr: Expr, env: Env): Unit =
    state = MachineState.EvalExpr(expr, env)

  private[ming] def setValue(value: Value): Unit =
    state = MachineState.ApplyValue(value)

  private[ming] def setInvoke(procedure: Value, args: List[Value], pos: SourcePos): Unit =
    state = MachineState.InvokeProcedure(procedure, args, pos)

  private[ming] def push(frame: ContinuationFrame): Unit =
    frames = frame :: frames

  private[ming] def activateWind(wind: DynamicWindContext): Unit =
    winds = winds :+ wind

  private[ming] def deactivateWind(wind: DynamicWindContext): Unit =
    if winds.nonEmpty && (winds.last eq wind) then winds = winds.init
    else winds = winds.filterNot(existing => existing eq wind)

  private[ming] def activateHandler(handler: ExceptionHandlerContext): Unit =
    handlers = handler :: handlers

  private[ming] def deactivateHandler(handler: ExceptionHandlerContext): Unit =
    handlers match
      case current :: rest if current.eq(handler) =>
        handlers = rest
      case _ =>
        handlers = handlers.filterNot(existing => existing.eq(handler))
