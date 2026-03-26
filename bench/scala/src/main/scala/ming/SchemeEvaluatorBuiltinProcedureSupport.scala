package ming

import SchemeBuiltinSupport.*
import SchemeEvaluatorState.*
import SchemeModel.*
import SchemeRuntime.*

private[ming] trait SchemeEvaluatorBuiltinProcedureSupport:

  protected def applyProcedure(
    procedure: Value,
    args: List[Value],
    continuation: Continuation,
    pos: Option[SourcePos]
  ): Computation

  private enum CursorState:
    case Complete
    case Step(callArgs: List[Value], nextCursors: List[Value])

  final protected def applyValues(
    args: List[Value],
    continuation: Continuation
  ): Computation =
    args match
      case single :: Nil =>
        resume(continuation, single)
      case _ =>
        resume(continuation, Value.MultiValues(args))

  final protected def applyCallWithValues(
    args: List[Value],
    continuation: Continuation,
    pos: Option[SourcePos]
  ): Computation =
    requireArgCount("call-with-values", args, 2)
    val List(producer, consumer) = args
    applyProcedure(
      producer,
      Nil,
      produced => applyProcedure(consumer, toValueList(produced), continuation, pos),
      pos
    )

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

  private def nextCursorState(name: String, cursors: List[Value]): CursorState =
    if cursors.forall(_ == Value.NilValue) then CursorState.Complete
    else if cursors.exists(_ == Value.NilValue) then throw new EvalError(s"$name expected lists of equal length")
    else
      val (callArgs, nextCursors) = cursors.map {
        case Value.PairValue(car, cdr) => (car, cdr)
        case _                         => throw new EvalError(s"$name expected a proper list")
      }.unzip
      CursorState.Step(callArgs, nextCursors)
