package ming

import scala.annotation.tailrec

/** Scheme interpreter entry point. */
object Evaluator:

  sealed private[ming] trait EvalResult
  private[ming] case class Done(value: Value, env: Env, out: String)  extends EvalResult
  private[ming] case class Bounce(expr: Value, env: Env, out: String) extends EvalResult

  private[ming] case class GuardBounce(
    bodyResult: EvalResult,
    clauses: List[Value],
    varName: String,
    guardEnv: Env
  ) extends EvalResult

  def evalStr(input: String): String =
    evalStrWithOutput(input)._1

  def evalStrWithOutput(input: String): (String, String) =
    val exprs = Parser.parse(input)
    if exprs.isEmpty then throw new EvalError("no expressions")
    val env = Builtins.defaultEnv
      .define("__callcc_replay__", Value.NilVal)
      .define("__wind_stack__", Value.NilVal)
    val (lastVal, _, output) = evalTopLevel(exprs, env, "")
    (lastVal.display, output)

  private def evalTopLevel(
    exprs: List[Value],
    env: Env,
    out: String
  ): (Value, Env, String) =
    try evalAll(exprs, env, out)
    catch
      case ci: ContinuationInvoked =>
        val capturedEnv = ci.envThunk()
        capturedEnv.set("__callcc_replay__", Value.PairVal(ci.value, Value.NilVal), None)
        evalTopLevel(ci.remaining, capturedEnv, ci.capturedOut)

  private[ming] def evalAll(
    exprs: List[Value],
    env: Env,
    out: String
  ): (Value, Env, String) =
    exprs match
      case Nil => (Value.VoidVal, env, out)
      case head :: Nil =>
        try eval(head, env, out)
        catch
          case setup: CallCCSetup =>
            Continuations.replayCallCC(setup, head, Nil, env, out)
          case ci: ContinuationInvoked =>
            Continuations.resumeContinuation(ci)
      case head :: tail =>
        val headEval: Either[(Value, Env, String), (Value, Env, String)] =
          try Right(eval(head, env, out))
          catch
            case setup: CallCCSetup =>
              Left(Continuations.replayCallCC(setup, head, tail, env, out))
            case ci: ContinuationInvoked =>
              val resumed = Continuations.resumeContinuation(ci)
              if ci.bodyLevel then Left(evalAll(tail, env, resumed._3))
              else Left(resumed)
        headEval match
          case Left(result) => result
          case Right((_, newEnv, out2)) =>
            val nextEnv = EnvOps.envAfterForm(head, env, newEnv)
            evalAll(tail, nextEnv, out2)

  private[ming] def evalBodyTail(
    exprs: List[Value],
    env: Env,
    out: String,
    replayMode: Boolean = false
  ): EvalResult =
    exprs match
      case Nil => Done(Value.VoidVal, env, out)
      case last :: Nil =>
        if replayMode && Continuations.isCallCCForm(last) then
          val (v, e, o) =
            Continuations.evalBodyCallCC(last, Nil, env, out, replayMode)
          Done(v, e, o)
        else Bounce(last, env, out)
      case head :: tail =>
        val headResult = head match
          case _ if Continuations.isCallCCForm(head) =>
            Continuations.evalBodyCallCC(head, tail, env, out, replayMode)
          case _ =>
            try eval(head, env, out)
            catch
              case ci: ContinuationInvoked if ci.bodyLevel =>
                val cEnv = ci.envThunk()
                cEnv.set("__callcc_replay__", Value.PairVal(ci.value, Value.NilVal), None)
                eval(ci.remaining.head, cEnv, ci.capturedOut)
        val (_, newEnv, out2) = headResult
        val nextEnv = head match
          case Value.PairVal(Value.Symbol("define", _), _, _)             => newEnv
          case Value.PairVal(Value.Symbol("define-syntax", _), _, _)      => newEnv
          case Value.PairVal(Value.Symbol("define-record-type", _), _, _) => newEnv
          case _ =>
            if newEnv.bindings.contains(EnvOps.MacroDefinedTag) then
              Env(newEnv.bindings - EnvOps.MacroDefinedTag, newEnv.parent)
            else env
        evalBodyTail(tail, nextEnv, out2, replayMode)

  private[ming] def eval(
    expr: Value,
    env: Env,
    out: String
  ): (Value, Env, String) =
    bounceLoop(evalStep(expr, env, out))

  @tailrec
  private def bounceLoop(result: EvalResult): (Value, Env, String) =
    result match
      case Done(v, e, o)       => (v, e, o)
      case gb: GuardBounce     => ExceptionHandling.guardLoop(gb)
      case Bounce(e2, env2, o) => bounceLoop(evalStep(e2, env2, o))

  private[ming] def evalStep(
    expr: Value,
    env: Env,
    out: String
  ): EvalResult =
    expr match
      case Value.IntVal(_)              => Done(expr, env, out)
      case Value.RationalVal(_, _)      => Done(expr, env, out)
      case Value.DoubleVal(_)           => Done(expr, env, out)
      case Value.BoolVal(_)             => Done(expr, env, out)
      case Value.StringVal(_)           => Done(expr, env, out)
      case Value.CharVal(_)             => Done(expr, env, out)
      case Value.NilVal                 => Done(expr, env, out)
      case Value.VoidVal                => Done(expr, env, out)
      case _: Value.MutableStringVal    => Done(expr, env, out)
      case _: Value.VectorVal           => Done(expr, env, out)
      case _: Value.MutablePairVal      => Done(expr, env, out)
      case _: Value.LambdaVal           => Done(expr, env, out)
      case _: Value.ContinuationVal     => Done(expr, env, out)
      case _: Value.MacroVal            => Done(expr, env, out)
      case _: Value.TransformerMacroVal => Done(expr, env, out)
      case _: Value.SyntaxBindingsVal   => Done(expr, env, out)
      case _: Value.MultipleValues      => Done(expr, env, out)
      case _: Value.RecordVal           => Done(expr, env, out)
      case _: Value.NativeProcVal       => Done(expr, env, out)
      case Value.Symbol(name, pos) =>
        Done(env.lookup(name, pos), env, out)
      case Value.PairVal(car, _, pos) =>
        val args = toList(expr).tail
        evalForm(car, args, env, pos, out)

  private def evalForm(
    op: Value,
    args: List[Value],
    env: Env,
    pos: Option[(Int, Int)],
    out: String
  ): EvalResult =
    op match
      case Value.Symbol("define", _) => Forms.evalDefine(args, env, pos, out)
      case Value.Symbol("if", _)     => Forms.evalIf(args, env, pos, out)
      case Value.Symbol("quote", _)  => Forms.evalQuote(args, env, out)
      case Value.Symbol("lambda", _) =>
        Done(Forms.makeLambda(args, env), env, out)
      case Value.Symbol("let", _)     => Forms.evalLet(args, env, out)
      case Value.Symbol("letrec", _)  => CondForms.evalLetrec(args, env, out)
      case Value.Symbol("letrec*", _) => CondForms.evalLetrecStar(args, env, out)
      case Value.Symbol("case", _)    => CondForms.evalCase(args, env, out)
      case Value.Symbol("set!", _)    => Forms.evalSet(args, env, pos, out)
      case Value.Symbol("begin", _)   => evalBodyTail(args, env, out)
      case Value.Symbol("cond", _)    => CondForms.evalCond(args, env, out)
      case Value.Symbol("and", _)     => Forms.evalAnd(args, env, out)
      case Value.Symbol("or", _)      => Forms.evalOr(args, env, out)
      case Value.Symbol("not", _) =>
        val (v, out2) = Forms.evalNotInner(args, env, out)
        Done(v, env, out2)
      case Value.Symbol("display", _) =>
        Forms.evalDisplayForm(args, env, out, _.displayRepr)
      case Value.Symbol("write", _) =>
        Forms.evalDisplayForm(args, env, out, _.display)
      case Value.Symbol("newline", _) =>
        Done(Value.VoidVal, env, out + "\n")
      case Value.Symbol("call/cc" | "call-with-current-continuation", _) =>
        Continuations.handleCallCCForm(args, env, out)
      case Value.Symbol("dynamic-wind", _) =>
        DynamicWind.evalDynamicWind(args, env, out)
      case Value.Symbol("raise", _) =>
        ExceptionHandling.evalRaise(args, env, out)
      case Value.Symbol("guard", _) =>
        ExceptionHandling.evalGuard(args, env, out)
      case Value.Symbol("with-exception-handler", _) =>
        ExceptionHandling.evalWithExceptionHandler(args, env, out)
      case Value.Symbol("define-syntax", _) =>
        Continuations.handleDefineSyntax(args, env, pos, out)
      case Value.Symbol("syntax-case", _) =>
        SyntaxCase.evalSyntaxCase(args, env, out)
      case Value.Symbol("syntax", _) =>
        SyntaxCase.evalSyntax(args, env, out)
      case Value.Symbol("with-syntax", _) =>
        SyntaxCase.evalWithSyntax(args, env, out)
      case Value.Symbol("define-record-type", _) =>
        Records.evalDefineRecordType(args, env, pos, out)
      case _ =>
        val (proc, _, out2) = eval(op, env, out)
        proc match
          case m: Value.MacroVal =>
            val (expanded, envBinds) = Macros.expand(m, op :: args)
            val expEnv = envBinds.foldLeft(env) { case (e, (k, v)) =>
              e.define(k, v)
            }
            expanded match
              case Value.PairVal(Value.Symbol("define" | "define-syntax" | "define-record-type", _), _, _) =>
                val (v, defEnv, out3) = eval(expanded, expEnv, out)
                Done(v, defEnv.define(EnvOps.MacroDefinedTag, Value.BoolVal(true)), out3)
              case _ =>
                Bounce(expanded, expEnv, out)
          case m: Value.TransformerMacroVal =>
            EnvOps.expandTransformerMacro(m, op :: args, env, out2)
          case Value.Symbol("call/cc" | "call-with-current-continuation", _) =>
            Continuations.handleCallCCForm(args, env, out2)
          case _ =>
            val (evaledArgs, out3) = evalArgs(args, env, out2)
            applyProcTail(proc, evaledArgs, pos, out3)

  private def evalArgs(
    args: List[Value],
    env: Env,
    out: String
  ): (List[Value], String) =
    args.foldLeft((List.empty[Value], out)) { case ((acc, o), arg) =>
      val (v, _, o2) = eval(arg, env, o)
      (acc :+ v, o2)
    }

  private[ming] def applyProcTail(
    proc: Value,
    args: List[Value],
    pos: Option[(Int, Int)],
    out: String
  ): EvalResult = Apply.applyProcTail(proc, args, pos, out)

  private[ming] def isFalsy(v: Value): Boolean = Apply.isFalsy(v)

  private[ming] def toList(v: Value): List[Value] = Apply.toList(v)
