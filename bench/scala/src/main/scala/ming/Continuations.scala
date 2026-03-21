package ming

import Evaluator.{Bounce, Done, EvalResult}

/** Call/cc and continuation handling, extracted from Evaluator. */
object Continuations:

  /** Check if call/cc should replay or throw setup. */
  def handleCallCCForm(
    args: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    env.lookup("__callcc_replay__") match
      case Value.PairVal(v, _, _) =>
        env.set("__callcc_replay__", Value.NilVal, None)
        Done(v, env, out)
      case _ =>
        if args.length != 1 then throw new EvalError("call/cc requires 1 argument")
        val (proc, _, out2) = Evaluator.eval(args.head, env, out)
        throw new CallCCSetup(proc, out2, None)

  /** Handle CallCCSetup: create continuation, call lambda, replay. */
  def replayCallCC(
    setup: CallCCSetup,
    currentExpr: Value,
    remainingExprs: List[Value],
    env: Env,
    out: String
  ): (Value, Env, String) =
    val tag       = new Object()
    val remaining = currentExpr :: remainingExprs
    val cont      = Value.ContinuationVal(tag, remaining, () => env, out)
    val callccResult: Value =
      try
        Evaluator.applyProcTail(setup.proc, List(cont), setup.pos, setup.output) match
          case Done(v, _, _)       => v
          case Bounce(e2, env2, o) => Evaluator.eval(e2, env2, o)._1
      catch case ci: ContinuationInvoked if ci.tag eq tag => ci.value
    env.set("__callcc_replay__", Value.PairVal(callccResult, Value.NilVal), None)
    Evaluator.evalAll(remaining, env, out)

  /** Resume a saved continuation. */
  def resumeContinuation(
    ci: ContinuationInvoked
  ): (Value, Env, String) =
    val env = ci.envThunk()
    env.set("__callcc_replay__", Value.PairVal(ci.value, Value.NilVal), None)
    Evaluator.evalAll(ci.remaining, env, ci.capturedOut)
