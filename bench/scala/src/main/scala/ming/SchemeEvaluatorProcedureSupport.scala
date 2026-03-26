package ming

import java.util.WeakHashMap
import java.util.concurrent.atomic.AtomicLong

import SchemeBuiltinSupport.*
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

  private val capturedContinuations = new WeakHashMap[Value.Builtin, Continuation]()

  private val capturedContinuationImpl: List[Value] => Value =
    _ => throw new IllegalStateException("captured continuation should be handled by the evaluator")

  private val nextContinuationId = new AtomicLong()

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

  final protected def capturedContinuation(builtin: Value.Builtin): Option[Continuation] =
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
    capturedContinuations.put(builtin, continuation)
    builtin
