package ming

/** syntax-case, syntax (#'), and with-syntax evaluation. */
object SyntaxCase:

  case class Context(bindings: Macro.Bindings, defEnv: Env, defBound: Set[String])

  /** Thread-local stack of syntax-case pattern binding contexts. */
  val contextStack: ThreadLocal[List[Context]] =
    ThreadLocal.withInitial(() => Nil)

  /** Try a matched clause: push context, evaluate fender+body. Returns Some(result) on success. */
  private def tryClauseMatch(
    fender: Option[SchemeVal],
    bodyExpr: SchemeVal,
    newBindings: Macro.Bindings,
    env: Env
  ): Option[SchemeVal] =
    val stack      = contextStack.get()
    val currentCtx = stack.headOption
    val merged     = currentCtx.map(_.bindings).getOrElse(Map.empty) ++ newBindings
    val defEnv     = currentCtx.map(_.defEnv).getOrElse(env)
    val defBound   = currentCtx.map(_.defBound).getOrElse(Set.empty)
    val ctx        = Context(merged, defEnv, defBound)
    contextStack.set(ctx :: stack)
    try
      fender match
        case Some(f) if !Evaluator.isTruthy(Evaluator.eval(f, env)) =>
          contextStack.set(stack)
          None
        case _ =>
          val result = Evaluator.eval(bodyExpr, env)
          contextStack.set(stack)
          Some(result)
    catch
      case e: Throwable =>
        contextStack.set(stack)
        throw e

  /** Evaluate a syntax-case form: match stxVal against clauses. */
  def evalSyntaxCase(
    stxVal: SchemeVal,
    literals: Set[String],
    clauses: List[SchemeVal],
    env: Env
  ): SchemeVal =
    for clause <- clauses do
      clause match
        case SchemeVal.SList(elems) if elems.length >= 2 =>
          val pattern = elems.head
          val (fender, bodyExpr) =
            if elems.length == 3 then (Some(elems(1)), elems(2))
            else (None, elems(1))
          Macro.matchFull(pattern, stxVal, literals) match
            case Some(newBindings) =>
              tryClauseMatch(fender, bodyExpr, newBindings, env) match
                case Some(result) => return result
                case None         => () // fender failed, try next
            case None => () // pattern didn't match
        case _ => throw new EvalError("syntax-case: bad clause")
    throw new EvalError("syntax-case: no matching clause")

  /** Evaluate (syntax template): expand template using current pattern bindings. */
  def evalSyntax(template: SchemeVal): SchemeVal =
    val stack = contextStack.get()
    if stack.isEmpty then throw new EvalError("syntax: not in syntax-case context")
    val ctx = stack.head
    Macro.expandTemplate(template, ctx.bindings, ctx.defEnv, ctx.defBound)

  /** Evaluate (with-syntax ((pat expr) ...) body ...): bind patterns and evaluate body. */
  def evalWithSyntax(
    bindingForms: List[SchemeVal],
    body: List[SchemeVal],
    env: Env
  ): SchemeVal =
    val stack      = contextStack.get()
    val currentCtx = stack.headOption
    var merged     = currentCtx.map(_.bindings).getOrElse(Map.empty: Macro.Bindings)
    val defEnv     = currentCtx.map(_.defEnv).getOrElse(env)
    val defBound   = currentCtx.map(_.defBound).getOrElse(Set.empty)

    for bf <- bindingForms do
      bf match
        case SchemeVal.SList(pattern :: expr :: Nil) =>
          val value = Evaluator.eval(expr, env)
          Macro.matchFull(pattern, value, Set.empty) match
            case Some(b) => merged = merged ++ b
            case None    => throw new EvalError("with-syntax: pattern match failed")
        case _ => throw new EvalError("with-syntax: bad binding form")

    val ctx = Context(merged, defEnv, defBound)
    contextStack.set(ctx :: stack)
    try
      val result = Evaluator.evalBody(body, env)
      contextStack.set(stack)
      result
    catch
      case e: Throwable =>
        contextStack.set(stack)
        throw e
