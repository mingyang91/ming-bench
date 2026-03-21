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
    val tag         = new Object()
    val remaining   = currentExpr :: remainingExprs
    val windEntries = DynamicWind.windStackToEntries(DynamicWind.lookupWindStack(env))
    val cont        = Value.ContinuationVal(tag, remaining, () => env, out, windEntries = windEntries)
    val callccResult: Value =
      try
        Evaluator.applyProcTail(setup.proc, List(cont), setup.pos, setup.output) match
          case Done(v, _, _)             => v
          case Bounce(e2, env2, o)       => Evaluator.eval(e2, env2, o)._1
          case gb: Evaluator.GuardBounce => ExceptionHandling.guardLoop(gb)._1
      catch case ci: ContinuationInvoked if ci.tag eq tag => ci.value
    env.set("__callcc_replay__", Value.PairVal(callccResult, Value.NilVal), None)
    Evaluator.evalAll(remaining, env, out)

  /** Resume a saved continuation. */
  def resumeContinuation(
    ci: ContinuationInvoked
  ): (Value, Env, String) =
    val env = ci.envThunk()
    def doResume(out: String): (Value, Env, String) =
      env.set("__callcc_replay__", Value.PairVal(ci.value, Value.NilVal), None)
      if ci.bodyLevel then
        Evaluator.evalBodyTail(ci.remaining, env, out, replayMode = true) match
          case Done(v, e, o)             => (v, e, o)
          case Bounce(e2, env2, o)       => Evaluator.eval(e2, env2, o)
          case gb: Evaluator.GuardBounce => ExceptionHandling.guardLoop(gb)
      else Evaluator.evalAll(ci.remaining, env, out)
    if ci.windEntries.nonEmpty then DynamicWind.withRewind(ci.windEntries.reverse, env, ci.capturedOut, doResume)
    else doResume(ci.capturedOut)

  /** Check if an expression is a call/cc form. */
  private[ming] def isCallCCForm(v: Value): Boolean = v match
    case Value.PairVal(
          Value.Symbol("call/cc" | "call-with-current-continuation", _),
          _,
          _
        ) =>
      true
    case _ => false

  /** Handle call/cc directly in a body.
    *
    * In replayMode, skips fresh call/cc (returns void).
    */
  private[ming] def evalBodyCallCC(
    head: Value,
    tail: List[Value],
    env: Env,
    out: String,
    replayMode: Boolean = false
  ): (Value, Env, String) =
    try
      env.lookup("__callcc_replay__") match
        case Value.PairVal(v, _, _) =>
          env.set("__callcc_replay__", Value.NilVal, None)
          (v, env, out)
        case _ =>
          if replayMode then (Value.VoidVal, env, out)
          else freshBodyCallCC(head, tail, env, out)
    catch
      case _: EvalError =>
        if replayMode then (Value.VoidVal, env, out)
        else freshBodyCallCC(head, tail, env, out)

  private def freshBodyCallCC(
    head: Value,
    tail: List[Value],
    env: Env,
    out: String
  ): (Value, Env, String) =
    val args = Evaluator.toList(head).tail
    if args.length != 1 then throw new EvalError("call/cc requires 1 argument")
    val (proc, _, out2) = Evaluator.eval(args.head, env, out)
    val tag             = new Object()
    val remaining       = head :: tail
    val windEntries     = DynamicWind.windStackToEntries(DynamicWind.lookupWindStack(env))
    val cont =
      Value.ContinuationVal(tag, remaining, () => env, out, bodyLevel = true, windEntries = windEntries)
    val callccResult: Value =
      try
        Evaluator.applyProcTail(proc, List(cont), None, out2) match
          case Done(v, _, _)             => v
          case Bounce(e2, env2, o)       => Evaluator.eval(e2, env2, o)._1
          case gb: Evaluator.GuardBounce => ExceptionHandling.guardLoop(gb)._1
      catch case ci: ContinuationInvoked if ci.tag eq tag => ci.value
    (callccResult, env, out2)

  /** Handle define-syntax form. */
  private[ming] def handleDefineSyntax(
    args: List[Value],
    env: Env,
    pos: Option[(Int, Int)],
    out: String
  ): EvalResult =
    args match
      case Value.Symbol(name, _) :: transformer :: Nil =>
        if isSyntaxRulesForm(transformer) then
          val parsed = Macros.parseSyntaxRules(transformer, env)
          lazy val selfMacro: Value.MacroVal =
            Value.MacroVal(parsed.rules, parsed.literals, () => selfEnv)
          lazy val selfEnv: Env = env.define(name, selfMacro)
          Done(Value.VoidVal, selfEnv, out)
        else
          val (proc, _, out2) = Evaluator.eval(transformer, env, out)
          val macro_          = Value.TransformerMacroVal(proc, () => env)
          Done(Value.VoidVal, env.define(name, macro_), out2)
      case _ =>
        throw EvalError.withPos("bad define-syntax", pos)

  private def isSyntaxRulesForm(v: Value): Boolean =
    v match
      case Value.PairVal(Value.Symbol("syntax-rules", _), _, _) => true
      case _                                                    => false
