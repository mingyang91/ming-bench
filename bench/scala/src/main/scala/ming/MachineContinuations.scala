package ming

import ContinuationFrame.*

private[ming] object MachineContinuations:

  def continueWith(machine: Machine, frame: ContinuationFrame, value: Value): Unit =
    frame match
      case Sequence(remaining, env) =>
        MachineExpressions.startSequence(machine, remaining, env)
      case IfBranch(thenExpr, elseExpr, env) =>
        continueIfBranch(machine, thenExpr, elseExpr, env, value)
      case DefineValue(name, env) =>
        env.define(name, value)
        machine.setValue(Value.Void)
      case SetValue(name, symbolPos, env) =>
        continueSetValue(machine, name, symbolPos, env, value)
      case CallHead(args, env, pos) =>
        continueCallHead(machine, args, env, pos, value)
      case CallWithValuesConsumer(consumer, pos) =>
        continueCallWithValuesConsumer(machine, consumer, pos, value)
      case CallArg(procedure, evaluatedRev, remaining, env, pos) =>
        continueCallArg(machine, procedure, evaluatedRev, remaining, env, pos, value)
      case And(remaining, env) =>
        continueAnd(machine, remaining, env, value)
      case Or(remaining, env) =>
        continueOr(machine, remaining, env, value)
      case CondClause(body, remainingClauses, env, pos) =>
        continueCondClause(machine, body, remainingClauses, env, pos, value)
      case CaseKey(clauses, env, pos) =>
        MachineExpressions.startCaseClauses(machine, value, clauses, env, pos)
      case LetrecSequentialValue(currentCell, remaining, recursiveEnv, body) =>
        continueLetrecSequentialValue(machine, currentCell, remaining, recursiveEnv, body, value)
      case LetrecParallelValue(currentCell, remaining, evaluatedRev, recursiveEnv, body) =>
        continueLetrecParallelValue(machine, currentCell, remaining, evaluatedRev, recursiveEnv, body, value)
      case MapResult(procedure, remainingRows, accRev, pos) =>
        continueMapResult(machine, procedure, remainingRows, accRev, pos, value)
      case ForEachResult(procedure, remainingRows, pos) =>
        continueForEachResult(machine, procedure, remainingRows, pos)
      case DynamicWindEntered(bodyThunk, wind) =>
        continueDynamicWindEntered(machine, bodyThunk, wind)
      case DynamicWindBodyResult(wind) =>
        MachineProcedures.continueDynamicWindBody(machine, wind, value)
      case DynamicWindOutResult(bodyResult) =>
        machine.setValue(bodyResult)
      case WithExceptionHandlerResult(handler) =>
        machine.deactivateHandler(handler)
        machine.setValue(value)
      case ExceptionHandlerReturned(raisePos) =>
        throw EvalError.at(raisePos, "raise handler returned")
      case ExceptionWindExit(remaining, entering, handler, exception, raisePos) =>
        MachineExceptions.continueRaiseAfterExit(machine, remaining, entering, handler, exception, raisePos)
      case ExceptionWindEnter(current, remaining, handler, exception, raisePos) =>
        continueExceptionWindEnter(machine, current, remaining, handler, exception, raisePos)
      case ContinuationWindExit(remaining, entering, snapshot, capturedValue) =>
        MachineProcedures.continueContinuationTransferAfterExit(machine, remaining, entering, snapshot, capturedValue)
      case ContinuationWindEnter(current, remaining, snapshot, capturedValue) =>
        continueContinuationWindEnter(machine, current, remaining, snapshot, capturedValue)

  private def continueIfBranch(
    machine: Machine,
    thenExpr: Expr,
    elseExpr: Option[Expr],
    env: Env,
    value: Value
  ): Unit =
    if ValueSemantics.isTruthy(value) then machine.setExpr(thenExpr, env)
    else
      elseExpr match
        case Some(expr) =>
          machine.setExpr(expr, env)
        case None =>
          machine.setValue(Value.Void)

  private def continueSetValue(
    machine: Machine,
    name: String,
    symbolPos: SourcePos,
    env: Env,
    value: Value
  ): Unit =
    val assignedValue = MultiValueSupport.requireSingle(value, symbolPos, "set!")
    if env.assign(name, assignedValue) then machine.setValue(Value.Void)
    else throw EvalError.at(symbolPos, s"unbound variable: $name")

  private def continueCallHead(machine: Machine, args: List[Expr], env: Env, pos: SourcePos, value: Value): Unit =
    val procedure = MultiValueSupport.requireSingle(value, pos, "procedure position")
    args match
      case Nil =>
        machine.setInvoke(procedure, Nil, pos)
      case _ =>
        machine.push(CallArg(procedure, Nil, args.dropRight(1).reverse, env, pos))
        machine.setExpr(args.last, env)

  private def continueCallWithValuesConsumer(
    machine: Machine,
    consumer: Value,
    pos: SourcePos,
    value: Value
  ): Unit =
    machine.setInvoke(consumer, MultiValueSupport.unpack(value), pos)

  private def continueCallArg(
    machine: Machine,
    procedure: Value,
    evaluatedRev: List[Value],
    remaining: List[Expr],
    env: Env,
    pos: SourcePos,
    value: Value
  ): Unit =
    val nextEvaluated = MultiValueSupport.requireSingle(value, pos, "procedure argument") :: evaluatedRev
    remaining match
      case Nil =>
        machine.setInvoke(procedure, nextEvaluated, pos)
      case head :: tail =>
        machine.push(CallArg(procedure, nextEvaluated, tail, env, pos))
        machine.setExpr(head, env)

  private def continueAnd(machine: Machine, remaining: List[Expr], env: Env, value: Value): Unit =
    if !ValueSemantics.isTruthy(value) then machine.setValue(value)
    else
      remaining match
        case Nil =>
          machine.setValue(value)
        case head :: Nil =>
          machine.setExpr(head, env)
        case head :: tail =>
          machine.push(And(tail, env))
          machine.setExpr(head, env)

  private def continueOr(machine: Machine, remaining: List[Expr], env: Env, value: Value): Unit =
    if ValueSemantics.isTruthy(value) then machine.setValue(value)
    else
      remaining match
        case Nil =>
          machine.setValue(value)
        case head :: Nil =>
          machine.setExpr(head, env)
        case head :: tail =>
          machine.push(Or(tail, env))
          machine.setExpr(head, env)

  private def continueCondClause(
    machine: Machine,
    body: List[Expr],
    remainingClauses: List[Expr],
    env: Env,
    pos: SourcePos,
    value: Value
  ): Unit =
    if ValueSemantics.isTruthy(value) then
      if body.isEmpty then machine.setValue(value)
      else MachineExpressions.startSequence(machine, body, env)
    else MachineExpressions.startCond(machine, remainingClauses, env, pos)

  private def continueLetrecSequentialValue(
    machine: Machine,
    currentCell: BindingCell,
    remaining: List[(LetBinding, BindingCell)],
    recursiveEnv: Env,
    body: List[Expr],
    value: Value
  ): Unit =
    currentCell.value = value
    remaining match
      case Nil =>
        MachineExpressions.startSequence(machine, body, recursiveEnv)
      case (binding, nextCell) :: tail =>
        machine.push(LetrecSequentialValue(nextCell, tail, recursiveEnv, body))
        machine.setExpr(binding.valueExpr, recursiveEnv)

  private def continueLetrecParallelValue(
    machine: Machine,
    currentCell: BindingCell,
    remaining: List[(LetBinding, BindingCell)],
    evaluatedRev: List[(BindingCell, Value)],
    recursiveEnv: Env,
    body: List[Expr],
    value: Value
  ): Unit =
    val nextEvaluated = (currentCell, value) :: evaluatedRev
    remaining match
      case Nil =>
        nextEvaluated.reverse.foreach { case (cell, bindingValue) =>
          cell.value = bindingValue
        }
        MachineExpressions.startSequence(machine, body, recursiveEnv)
      case (binding, nextCell) :: tail =>
        machine.push(LetrecParallelValue(nextCell, tail, nextEvaluated, recursiveEnv, body))
        machine.setExpr(binding.valueExpr, recursiveEnv)

  private def continueMapResult(
    machine: Machine,
    procedure: Value,
    remainingRows: List[List[Value]],
    accRev: List[Value],
    pos: SourcePos,
    value: Value
  ): Unit =
    val nextAcc = value :: accRev
    remainingRows match
      case Nil =>
        machine.setValue(ValueSemantics.listFrom(nextAcc.reverse))
      case row :: tail =>
        machine.push(MapResult(procedure, tail, nextAcc, pos))
        machine.setInvoke(procedure, row, pos)

  private def continueForEachResult(
    machine: Machine,
    procedure: Value,
    remainingRows: List[List[Value]],
    pos: SourcePos
  ): Unit =
    remainingRows match
      case Nil =>
        machine.setValue(Value.Void)
      case row :: tail =>
        machine.push(ForEachResult(procedure, tail, pos))
        machine.setInvoke(procedure, row, pos)

  private def continueDynamicWindEntered(machine: Machine, bodyThunk: Value, wind: DynamicWindContext): Unit =
    machine.activateWind(wind)
    machine.push(DynamicWindBodyResult(wind))
    machine.setInvoke(bodyThunk, Nil, wind.pos)

  private def continueExceptionWindEnter(
    machine: Machine,
    current: DynamicWindContext,
    remaining: List[DynamicWindContext],
    handler: ExceptionHandlerContext,
    exception: Value,
    raisePos: SourcePos
  ): Unit =
    machine.activateWind(current)
    MachineExceptions.continueRaiseAfterEnter(machine, remaining, handler, exception, raisePos)

  private def continueContinuationWindEnter(
    machine: Machine,
    current: DynamicWindContext,
    remaining: List[DynamicWindContext],
    snapshot: ContinuationSnapshot,
    capturedValue: Value
  ): Unit =
    machine.activateWind(current)
    MachineProcedures.continueContinuationTransferAfterEnter(machine, remaining, snapshot, capturedValue)
