package ming

import scala.annotation.tailrec

import Evaluator.{Bounce, Done, EvalResult}

/** Procedure application and value utilities. */
object Apply:

  /** Apply a procedure, returning Bounce for lambda bodies (TCO). */
  private[ming] def applyProcTail(
    proc: Value,
    args: List[Value],
    pos: Option[(Int, Int)],
    out: String
  ): EvalResult =
    proc match
      case lam @ Value.LambdaVal(
            params,
            body,
            closureThunk,
            nameOpt,
            restParam
          ) =>
        val closure = closureThunk()
        val closureWithSelf = nameOpt match
          case Some(n) => closure.define(n, lam)
          case None    => closure
        val localEnv =
          closureWithSelf.extendVariadic(params, restParam, args, pos)
        Evaluator.evalBodyTail(body, localEnv, out)
      case Value.NativeProcVal(_, fn) =>
        Done(fn(args), Env(Map.empty, None), out)
      case Value.Symbol("apply", _) =>
        HigherOrder.evalApply(args, pos, out)
      case Value.Symbol("map", _) =>
        HigherOrder.evalMap(args, pos, out)
      case Value.Symbol("values", _) =>
        val result = args match
          case single :: Nil => single
          case _             => Value.MultipleValues(args)
        Done(result, Env(Map.empty, None), out)
      case Value.Symbol("call-with-values", _) =>
        applyCallWithValues(args, pos, out)
      case cv: Value.ContinuationVal =>
        if args.length != 1 then throw EvalError.withPos("continuation requires 1 argument", pos)
        throw new ContinuationInvoked(
          cv.tag,
          args.head,
          out,
          cv.remaining,
          cv.envThunk,
          cv.capturedOut,
          cv.bodyLevel,
          cv.windEntries
        )
      case Value.Symbol("call/cc" | "call-with-current-continuation", _) =>
        if args.length != 1 then throw EvalError.withPos("call/cc requires 1 argument", pos)
        throw new CallCCSetup(args.head, out, pos)
      case Value.Symbol(name, _) =>
        Done(
          Builtins.applyBuiltin(name, args, pos),
          Env(Map.empty, None),
          out
        )
      case _ =>
        throw EvalError.withPos(
          s"not a procedure: ${proc.display}",
          pos
        )

  private def applyCallWithValues(
    args: List[Value],
    pos: Option[(Int, Int)],
    out: String
  ): EvalResult =
    args match
      case producer :: consumer :: Nil =>
        val (produced, _, out2) = trampolineResult(
          applyProcTail(producer, Nil, pos, out)
        )
        val consumerArgs = produced match
          case Value.MultipleValues(vs) => vs
          case single                   => List(single)
        applyProcTail(consumer, consumerArgs, pos, out2)
      case _ =>
        throw EvalError.withPos(
          "call-with-values requires 2 arguments",
          pos
        )

  @tailrec
  private[ming] def trampolineResult(r: EvalResult): (Value, Env, String) =
    r match
      case Done(v, e, o)       => (v, e, o)
      case Bounce(e2, env2, o) => trampolineResult(Evaluator.evalStep(e2, env2, o))

  private[ming] def isFalsy(v: Value): Boolean = v match
    case Value.BoolVal(false) => true
    case _                    => false

  private[ming] def toList(v: Value): List[Value] = v match
    case Value.NilVal           => Nil
    case Value.PairVal(h, t, _) => h :: toList(t)
    case other                  => List(other)
