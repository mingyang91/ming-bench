package ming

import scala.collection.immutable.Vector as IVector

private[ming] object SchemeDynamicWind:

  import BuiltinSupport.requireExactly
  import SchemeInterpreter.{DynamicWindFrame, EvalState, Resume, Value}

  private type ApplyProcedureState =
    (Value, List[Value], SourcePos, Resume) => EvalState

  def applyState(
    args: List[Value],
    pos: SourcePos,
    cont: Resume,
    runtime: Runtime,
    applyProcedureState: ApplyProcedureState
  ): EvalState =
    requireExactly("dynamic-wind", args, expected = 3, pos)
    args match
      case before :: body :: after :: Nil =>
        val frame = DynamicWindFrame(before, after, pos)
        applyProcedureState(
          before,
          Nil,
          pos,
          _ =>
            runtime.pushWindFrame(frame)
            applyProcedureState(
              body,
              Nil,
              pos,
              value => exitFrameState(frame, value, cont, runtime, applyProcedureState)
            )
        )
      case _ =>
        throw new IllegalStateException("unreachable")

  def captureContinuation(cont: Resume, runtime: Runtime): Value.Continuation =
    Value.Continuation(cont, runtime.windStack)

  def resumeContinuationState(
    continuation: Value.Continuation,
    argument: Value,
    runtime: Runtime,
    applyProcedureState: ApplyProcedureState
  ): EvalState =
    val current = runtime.windStack
    val target  = continuation.windStack
    val common  = commonPrefixLength(current, target)
    val exits   = current.drop(common).reverse.toList
    val enters  = target.drop(common).toList

    unwindState(
      exits,
      runtime,
      applyProcedureState,
      rewindState(
        enters,
        runtime,
        applyProcedureState, {
          runtime.setWindStack(target)
          continuation.resume(argument)
        }
      )
    )

  private def exitFrameState(
    frame: DynamicWindFrame,
    value: Value,
    cont: Resume,
    runtime: Runtime,
    applyProcedureState: ApplyProcedureState
  ): EvalState =
    runtime.popWindFrame(frame)
    applyProcedureState(frame.after, Nil, frame.pos, _ => cont(value))

  private def unwindState(
    frames: List[DynamicWindFrame],
    runtime: Runtime,
    applyProcedureState: ApplyProcedureState,
    next: => EvalState
  ): EvalState =
    frames match
      case Nil =>
        next
      case frame :: rest =>
        runtime.popWindFrame(frame)
        applyProcedureState(frame.after, Nil, frame.pos, _ => unwindState(rest, runtime, applyProcedureState, next))

  private def rewindState(
    frames: List[DynamicWindFrame],
    runtime: Runtime,
    applyProcedureState: ApplyProcedureState,
    next: => EvalState
  ): EvalState =
    frames match
      case Nil =>
        next
      case frame :: rest =>
        applyProcedureState(
          frame.before,
          Nil,
          frame.pos,
          _ =>
            runtime.pushWindFrame(frame)
            rewindState(rest, runtime, applyProcedureState, next)
        )

  private def commonPrefixLength(
    current: IVector[DynamicWindFrame],
    target: IVector[DynamicWindFrame]
  ): Int =
    val limit = math.min(current.length, target.length)
    var index = 0

    while index < limit && (current(index) eq target(index)) do index += 1

    index
