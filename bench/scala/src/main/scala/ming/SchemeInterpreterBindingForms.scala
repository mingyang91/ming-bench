package ming

private[ming] object SchemeInterpreterBindingForms:

  import SchemeInterpreter.{continueSequence, EvalStep, Expr, Value}
  import SchemeInterpreterSyntax.*

  private type EvalExpr       = (Expr, Env, MacroScope) => Value
  private type ApplyProcedure = (Value, List[Value], SourcePos) => EvalStep

  def evalLet(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    evalExpr: EvalExpr,
    applyProcedure: ApplyProcedure
  ): EvalStep =
    args match
      case Expr.ListExpr(bindingsExpr, _) :: body if body.nonEmpty =>
        val (bindings, values) = evaluateBindings(bindingsExpr, env, macros, evalExpr)
        val letEnv             = Env.child(env, bindingNames(bindings).zip(values))
        val letMacros          = MacroScope.child(macros)
        continueSequence(body, letEnv, letMacros)
      case Expr.Symbol(name, _) :: Expr.ListExpr(bindingsExpr, _) :: body if body.nonEmpty =>
        val (bindings, values) = evaluateBindings(bindingsExpr, env, macros, evalExpr)
        val letEnv             = Env.child(env, Nil)
        val letMacros          = MacroScope.child(macros)
        val closure            = Value.Closure(LambdaParams.fixed(bindingNames(bindings)), body, letEnv, letMacros)
        letEnv.define(name, closure)
        applyProcedure(closure, values, pos)
      case _ =>
        throw EvalError.at(pos, "invalid let")

  def evalLetStar(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    evalExpr: EvalExpr
  ): EvalStep =
    args match
      case Expr.ListExpr(bindingsExpr, _) :: body if body.nonEmpty =>
        val bindings      = readBindings(bindingsExpr)
        val letStarEnv    = Env.child(env, Nil)
        val letStarMacros = MacroScope.child(macros)

        bindings.foreach { case (name, valueExpr) =>
          letStarEnv.define(name, evalExpr(valueExpr, letStarEnv, letStarMacros))
        }

        continueSequence(body, letStarEnv, letStarMacros)
      case _ =>
        throw EvalError.at(pos, "invalid let*")

  def evalLetrec(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    evalExpr: EvalExpr
  ): EvalStep =
    args match
      case Expr.ListExpr(bindingsExpr, _) :: body if body.nonEmpty =>
        val bindings     = readBindings(bindingsExpr)
        val letrecEnv    = Env.child(env, Nil)
        val letrecMacros = MacroScope.child(macros)

        bindings.foreach { case (name, _) =>
          letrecEnv.define(name, Value.Void)
        }

        val values = bindings.map { case (_, valueExpr) =>
          evalExpr(valueExpr, letrecEnv, letrecMacros)
        }

        bindings.zip(values).foreach { case ((name, _), value) =>
          letrecEnv.assign(name, value, pos)
        }

        continueSequence(body, letrecEnv, letrecMacros)
      case _ =>
        throw EvalError.at(pos, "invalid letrec")

  def evalLetrecStar(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    evalExpr: EvalExpr
  ): EvalStep =
    args match
      case Expr.ListExpr(bindingsExpr, _) :: body if body.nonEmpty =>
        val bindings     = readBindings(bindingsExpr)
        val letrecEnv    = Env.child(env, Nil)
        val letrecMacros = MacroScope.child(macros)

        bindings.foreach { case (name, valueExpr) =>
          letrecEnv.define(name, Value.Void)
          letrecEnv.assign(name, evalExpr(valueExpr, letrecEnv, letrecMacros), valueExpr.pos)
        }

        continueSequence(body, letrecEnv, letrecMacros)
      case _ =>
        throw EvalError.at(pos, "invalid letrec*")

  private def evaluateBindings(
    bindingsExpr: List[Expr],
    env: Env,
    macros: MacroScope,
    evalExpr: EvalExpr
  ): (List[(String, Expr)], List[Value]) =
    val bindings = readBindings(bindingsExpr)
    val values = bindings.map { case (_, valueExpr) =>
      evalExpr(valueExpr, env, macros)
    }
    (bindings, values)

  private def bindingNames(bindings: List[(String, Expr)]): List[String] =
    bindings.map(_._1)
