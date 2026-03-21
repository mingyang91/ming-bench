package ming

import ming.Evaluator.{Bounce, Done, EvalResult, GuardBounce}

/** Environment management helpers: closure patching, post-form env selection, transformer macro expansion. */
private[ming] object EnvOps:

  val MacroDefinedTag: String = "__macro_defined__"

  /** Tie-the-knot: update named lambdas' closures so they can see all current bindings. */
  def patchClosures(env: Env): Env =
    val hasNamedLambda = env.bindings.exists { case (n, cell) =>
      cell(0) match
        case Value.LambdaVal(_, _, _, Some(ln), _) => ln == n
        case _                                     => false
    }
    if !hasNamedLambda then return env
    lazy val patched: Env = Env(
      env.bindings.map { case (n, cell) =>
        cell(0) match
          case Value.LambdaVal(ps, bd, _, ln @ Some(name), rp) if name == n =>
            n -> Array[Value](
              Value.LambdaVal(ps, bd, () => patched, ln, rp)
            )
          case _ => n -> cell
      },
      env.parent
    )
    patched

  /** Decide env after evaluating a form: use newEnv for defines. */
  def envAfterForm(
    head: Value,
    origEnv: Env,
    newEnv: Env
  ): Env =
    head match
      case Value.PairVal(Value.Symbol("define", _), _, _) =>
        patchClosures(newEnv)
      case Value.PairVal(Value.Symbol("define-syntax", _), _, _) =>
        newEnv
      case Value.PairVal(Value.Symbol("define-record-type", _), _, _) =>
        newEnv
      case _ =>
        if newEnv.bindings.contains(MacroDefinedTag) then
          val cleaned = Env(
            newEnv.bindings - MacroDefinedTag,
            newEnv.parent
          )
          patchClosures(cleaned)
        else origEnv

  def expandTransformerMacro(
    m: Value.TransformerMacroVal,
    inputElems: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    val inputForm = Macros.listToValue(inputElems)
    val result = Apply.applyProcTail(
      m.proc,
      List(inputForm),
      None,
      out
    )
    val (expanded, _, out2) = result match
      case Done(v, e, o)       => (v, e, o)
      case Bounce(e2, env2, o) => Evaluator.eval(e2, env2, o)
      case gb: GuardBounce     => ExceptionHandling.guardLoop(gb)
    expanded match
      case Value.PairVal(Value.Symbol("define" | "define-syntax", _), _, _) =>
        val (v, defEnv, out3) = Evaluator.eval(expanded, env, out2)
        Done(v, defEnv.define(MacroDefinedTag, Value.BoolVal(true)), out3)
      case _ =>
        Bounce(expanded, env, out2)
