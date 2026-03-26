package ming

import java.util.WeakHashMap
import java.util.concurrent.atomic.AtomicLong

import SchemeBuiltinSupport.*
import SchemeDynamicContext.*
import SchemeEvaluatorState.*
import SchemeModel.*
import SchemeRuntime.*

private[ming] trait SchemeEvaluatorProcedureSupport:

  protected def applyProcedure(
    procedure: Value,
    args: List[Value],
    continuation: Continuation,
    pos: Option[SourcePos]
  ): Computation

  private enum CursorState:
    case Complete
    case Step(callArgs: List[Value], nextCursors: List[Value])

  private val capturedContinuations = new WeakHashMap[Value.Builtin, CapturedContinuation]()

  private val capturedContinuationImpl: List[Value] => Value =
    _ => throw new IllegalStateException("captured continuation should be handled by the evaluator")

  private val nextContinuationId = new AtomicLong()

  final protected def resetDynamicContext(): Unit =
    SchemeDynamicContext.reset()

  final protected def applyDynamicWind(
    args: List[Value],
    continuation: Continuation,
    pos: Option[SourcePos]
  ): Computation =
    requireArgCount("dynamic-wind", args, 3)
    val List(inThunk, bodyThunk, outThunk) = args
    val parentContext                      = current
    val frame                              = newFrame(inThunk, outThunk)

    applyProcedure(
      inThunk,
      Nil,
      _ =>
        evaluateDynamicWindBody(
          parentContext,
          frame,
          bodyThunk,
          outThunk,
          continuation,
          pos
        ),
      pos
    )

  private def evaluateDynamicWindBody(
    parentContext: Vector[WindFrame],
    frame: WindFrame,
    bodyThunk: Value,
    outThunk: Value,
    continuation: Continuation,
    pos: Option[SourcePos]
  ): Computation =
    replace(parentContext :+ frame)
    applyProcedure(
      bodyThunk,
      Nil,
      bodyValue => exitDynamicWind(parentContext, outThunk, bodyValue, continuation, pos),
      pos
    )

  private def exitDynamicWind(
    parentContext: Vector[WindFrame],
    outThunk: Value,
    bodyValue: Value,
    continuation: Continuation,
    pos: Option[SourcePos]
  ): Computation =
    replace(parentContext)
    applyProcedure(
      outThunk,
      Nil,
      _ => resume(continuation, bodyValue),
      pos
    )

  final protected def applyCallWithCurrentContinuation(
    name: String,
    args: List[Value],
    continuation: Continuation,
    pos: Option[SourcePos]
  ): Computation =
    requireArgCount(name, args, 1)
    applyProcedure(args.head, List(captureContinuation(continuation)), continuation, pos)

  final protected def applyBuiltinApply(
    args: List[Value],
    continuation: Continuation,
    pos: Option[SourcePos]
  ): Computation =
    requireMinArgCount("apply", args, 2)
    val procedure  = args.head
    val prefixArgs = args.slice(1, args.length - 1)
    val listArgs   = properListElements("apply", args.last)
    applyProcedure(procedure, prefixArgs ++ listArgs, continuation, pos)

  final protected def applyBuiltinMap(
    args: List[Value],
    continuation: Continuation,
    pos: Option[SourcePos]
  ): Computation =
    requireMinArgCount("map", args, 2)
    val procedure = args.head

    def loop(cursors: List[Value], reversedResults: List[Value]): Computation =
      nextCursorState("map", cursors) match
        case CursorState.Complete =>
          resume(continuation, makeList(reversedResults.reverse))
        case CursorState.Step(callArgs, nextCursors) =>
          applyProcedure(
            procedure,
            callArgs,
            mapped => suspend(loop(nextCursors, mapped :: reversedResults)),
            pos
          )

    loop(args.tail, Nil)

  final protected def applyBuiltinForEach(
    args: List[Value],
    continuation: Continuation,
    pos: Option[SourcePos]
  ): Computation =
    requireMinArgCount("for-each", args, 2)
    val procedure = args.head

    def loop(cursors: List[Value]): Computation =
      nextCursorState("for-each", cursors) match
        case CursorState.Complete =>
          resume(continuation, Value.VoidValue)
        case CursorState.Step(callArgs, nextCursors) =>
          applyProcedure(
            procedure,
            callArgs,
            _ => suspend(loop(nextCursors)),
            pos
          )

    loop(args.tail)

  final protected def applyCapturedContinuation(
    name: String,
    captured: CapturedContinuation,
    args: List[Value],
    pos: Option[SourcePos]
  ): Computation =
    requireArgCount(name, args, 1)
    val List(argument) = args
    transitionDynamicContext(captured.dynamicContext, pos) {
      suspend(captured.continuation(argument))
    }

  final protected def capturedContinuation(
    builtin: Value.Builtin
  ): Option[CapturedContinuation] =
    Option(capturedContinuations.get(builtin))

  private def nextCursorState(name: String, cursors: List[Value]): CursorState =
    if cursors.forall(_ == Value.NilValue) then CursorState.Complete
    else if cursors.exists(_ == Value.NilValue) then throw new EvalError(s"$name expected lists of equal length")
    else
      val (callArgs, nextCursors) = cursors.map {
        case Value.PairValue(car, cdr) => (car, cdr)
        case _                         => throw new EvalError(s"$name expected a proper list")
      }.unzip
      CursorState.Step(callArgs, nextCursors)

  private def captureContinuation(continuation: Continuation): Value =
    val builtin: Value.Builtin = Value.Builtin(
      s"continuation:${nextContinuationId.incrementAndGet()}",
      capturedContinuationImpl
    )
    capturedContinuations.put(
      builtin,
      CapturedContinuation(continuation, current)
    )
    builtin

  private def transitionDynamicContext(
    targetContext: Vector[WindFrame],
    pos: Option[SourcePos]
  )(next: => Computation): Computation =
    val sourceContext = current
    val sharedLength  = commonPrefixLength(sourceContext, targetContext)

    def exitFrames(sourceLength: Int): Computation =
      if sourceLength == sharedLength then enterFrames(sharedLength)
      else
        val frame = sourceContext(sourceLength - 1)
        replace(sourceContext.take(sourceLength - 1))
        applyProcedure(
          frame.outThunk,
          Nil,
          _ => suspend(exitFrames(sourceLength - 1)),
          pos
        )

    def enterFrames(targetIndex: Int): Computation =
      if targetIndex == targetContext.length then
        replace(targetContext)
        next
      else
        val frame = targetContext(targetIndex)
        replace(targetContext.take(targetIndex))
        applyProcedure(
          frame.inThunk,
          Nil,
          _ =>
            replace(targetContext.take(targetIndex + 1))
            suspend(enterFrames(targetIndex + 1))
          ,
          pos
        )

    exitFrames(sourceContext.length)
