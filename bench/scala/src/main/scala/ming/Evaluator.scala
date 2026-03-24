package ming

import scala.collection.mutable

object Evaluator:

  val outputBuffer: ThreadLocal[StringBuilder] = ThreadLocal.withInitial(() => new StringBuilder)

  private val contIdCounter                                  = new java.util.concurrent.atomic.AtomicLong(0)
  private[ming] def nextContId(): Long                       = contIdCounter.incrementAndGet()
  private[ming] val currentBodyCtx: ThreadLocal[BodyContext] = ThreadLocal.withInitial(() => null)
  private[ming] val pendingCCReturn: ThreadLocal[SchemeVal]  = ThreadLocal.withInitial(() => null)
  private[ming] val windStack: ThreadLocal[List[WindEntry]]  = ThreadLocal.withInitial(() => Nil)

  // Flag: true when we are re-evaluating a body due to continuation re-entry.
  // During re-entry, performCallCC skips creating new continuations to avoid
  // re-triggering side effects from call/cc lambdas that have already executed.
  private[ming] val inReentry: ThreadLocal[Boolean] = ThreadLocal.withInitial(() => false)

  // Track bodies currently being evaluated by evalBody (identity-based set).
  // Used by the trampoline to decide whether to handle ContinuationReturn locally.
  private[ming] val activeBodies: ThreadLocal[java.util.Set[List[Expr]]] =
    ThreadLocal.withInitial(() =>
      java.util.Collections.newSetFromMap(
        new java.util.IdentityHashMap[List[Expr], java.lang.Boolean]()
      )
    )

  private[ming] def isTruthy(v: SchemeVal): Boolean = v match
    case SchemeVal.BoolVal(false) => false
    case _                        => true

  private[ming] def evalBody(body: List[Expr], env: Env, startIdx: Int = 0): SchemeVal =
    val bodies = activeBodies.get()
    bodies.add(body)
    try
      var result: SchemeVal = SchemeVal.Void
      var i                 = startIdx
      while i < body.length do
        val prev = currentBodyCtx.get()
        currentBodyCtx.set(BodyContext(body, i, env))
        try
          result = eval(body(i), env)
          i += 1
        catch
          case cr: ContinuationReturn =>
            pendingCCReturn.set(cr.value)
            if (cr.body ne null) && (cr.body eq body) then i = cr.startIdx
            else if cr.body ne null then
              result = reenterInnerBody(cr)
              i += 1
            else throw cr
        finally currentBodyCtx.set(prev)
      result
    finally bodies.remove(body)

  private def reenterInnerBody(cr: ContinuationReturn): SchemeVal =
    val prevReentry = inReentry.get()
    inReentry.set(true)
    try
      val result = evalBody(cr.body, cr.bodyEnv, cr.startIdx)
      unwindToCallerWind(cr.callerWind)
      result
    finally inReentry.set(prevReentry)

  private def unwindToCallerWind(callerWind: List[WindEntry]): Unit =
    var cur = windStack.get()
    while (cur ne callerWind) && cur.nonEmpty do
      val entry = cur.head
      cur = cur.tail
      windStack.set(cur)
      applyProc(entry.outThunk, Nil)

  private[ming] def evalBodyTail(body: List[Expr], env: Env): SchemeVal =
    if body.isEmpty then SchemeVal.Void
    else
      for idx <- 0 until body.length - 1 do
        val prev = currentBodyCtx.get()
        currentBodyCtx.set(BodyContext(body, idx, env))
        try eval(body(idx), env)
        finally currentBodyCtx.set(prev)
      // For multi-expression bodies, set context for the tail position so that
      // call/cc in the tail expression captures this body's context (not the caller's).
      // Single-expression bodies inherit the caller's context, which allows
      // continuations to target the enclosing evalBody for proper re-entry.
      if body.length > 1 then currentBodyCtx.set(BodyContext(body, body.length - 1, env))
      SchemeVal.TailCall(body.last, env)

  private def posStr(expr: Expr): String =
    val p = Parser.positions.get(expr)
    if p != null then s"${p._1}:${p._2}" else "1:1"

  private val posPattern = ".*\\d+:\\d+.*".r

  private def trampoline(initial: SchemeVal): SchemeVal =
    var result = initial
    while result.isInstanceOf[SchemeVal.TailCall] do
      val SchemeVal.TailCall(e, env) = result: @unchecked
      try result = evalInner(e, env)
      catch
        case cr: ContinuationReturn if (cr.body ne null) && !activeBodies.get().contains(cr.body) =>
          pendingCCReturn.set(cr.value)
          result = reenterInnerBody(cr)
    result

  private[ming] def eval(expr: Expr, env: Env): SchemeVal =
    trampoline(evalInner(expr, env))

  private def evalCallCC(procExpr: Expr, env: Env): SchemeVal =
    val pending = pendingCCReturn.get()
    if pending != null then
      pendingCCReturn.set(null)
      pending
    else
      val proc = eval(procExpr, env)
      performCallCC(proc)

  private[ming] def performCallCC(proc: SchemeVal): SchemeVal =
    if inReentry.get() then
      // During continuation re-entry, skip creating new continuations.
      // This prevents re-triggering side effects (like yield-val) from
      // call/cc lambdas that were already executed in the original run.
      SchemeVal.Void
    else
      val ctx    = currentBodyCtx.get()
      val contId = nextContId()
      val cont = SchemeVal.ContinuationVal(
        contId,
        if ctx != null then ctx.body else null,
        if ctx != null then ctx.idx else 0,
        if ctx != null then ctx.env else null,
        windStack.get()
      )
      try applyProc(proc, List(cont))
      catch case cr: ContinuationReturn if cr.contId == contId => cr.value

  private def evalInner(expr: Expr, env: Env): SchemeVal =
    try
      expr match
        case Expr.IntLit(n)    => SchemeVal.IntVal(n)
        case Expr.FloatLit(d)  => SchemeVal.FloatVal(d)
        case Expr.RatLit(n, d) => SchemeNum.makeRational(n, d)
        case Expr.BoolLit(b)   => SchemeVal.BoolVal(b)
        case Expr.StrLit(s)    => SchemeVal.StrVal(s.toCharArray)
        case Expr.CharLit(c)   => SchemeVal.CharVal(c)
        case Expr.Symbol(name) => env.lookup(name)
        case Expr.SList(elems) => evalSList(elems, expr, env)
    catch
      case cr: ContinuationReturn => throw cr
      case e: EvalError =>
        val msg = e.getMessage
        if posPattern.matches(msg) then throw e
        else throw new EvalError(s"$msg at ${posStr(expr)}")

  private def evalSList(elems: List[Expr], expr: Expr, env: Env): SchemeVal = elems match
    case Nil => SchemeVal.ListVal(Nil)
    case Expr.Symbol("quote") :: arg :: Nil =>
      EvalHelpers.quoteToVal(arg)
    case Expr.Symbol("if") :: cond :: thenBr :: elseBr :: Nil =>
      if isTruthy(eval(cond, env)) then SchemeVal.TailCall(thenBr, env)
      else SchemeVal.TailCall(elseBr, env)
    case Expr.Symbol("if") :: cond :: thenBr :: Nil =>
      if isTruthy(eval(cond, env)) then SchemeVal.TailCall(thenBr, env)
      else SchemeVal.Void
    case Expr.Symbol("define") :: Expr.SList(Expr.Symbol(name) :: params) :: body =>
      val (paramNames, restParam) = EvalHelpers.parseParams(params)
      env.define(name, SchemeVal.Procedure(paramNames, restParam, body, env))
      SchemeVal.Void
    case Expr.Symbol("define") :: Expr.Symbol(name) :: value :: Nil =>
      env.define(name, eval(value, env))
      SchemeVal.Void
    case Expr.Symbol("set!") :: Expr.Symbol(name) :: value :: Nil =>
      env.set(name, eval(value, env))
      SchemeVal.Void
    case Expr.Symbol("lambda") :: Expr.SList(params) :: body =>
      val (paramNames, restParam) = EvalHelpers.parseParams(params)
      SchemeVal.Procedure(paramNames, restParam, body, env)
    case Expr.Symbol("lambda") :: Expr.Symbol(restName) :: body =>
      SchemeVal.Procedure(Nil, Some(restName), body, env)
    case Expr.Symbol("let") :: Expr.Symbol(name) :: Expr.SList(bindings) :: body =>
      EvalForms.evalNamedLet(name, bindings, body, env)
    case Expr.Symbol("let") :: Expr.SList(bindings) :: body =>
      EvalForms.evalLet(bindings, body, env)
    case Expr.Symbol("let*") :: Expr.SList(bindings) :: body =>
      EvalForms.evalLetStar(bindings, body, env)
    case Expr.Symbol("begin") :: exprs =>
      evalBodyTail(exprs, env)
    case Expr.Symbol("cond") :: clauses =>
      EvalForms.evalCond(clauses, env)
    case Expr.Symbol("and") :: args =>
      EvalForms.evalAnd(args, env)
    case Expr.Symbol("or") :: args =>
      EvalForms.evalOr(args, env)
    case Expr.Symbol("when") :: test :: body =>
      if isTruthy(eval(test, env)) then evalBodyTail(body, env) else SchemeVal.Void
    case Expr.Symbol("unless") :: test :: body =>
      if !isTruthy(eval(test, env)) then evalBodyTail(body, env) else SchemeVal.Void
    case Expr.Symbol("define-syntax") :: Expr.Symbol(name) :: Expr.SList(
          Expr.Symbol("syntax-rules") :: Expr.SList(lits) :: rules
        ) :: Nil =>
      MacroExpander.defineFromSyntaxRules(name, lits, rules, env)
    case Expr.Symbol("define-syntax") :: Expr.Symbol(name) :: transformerExpr :: Nil =>
      val proc = eval(transformerExpr, env)
      env.define(name, SchemeVal.MacroTransformer(proc, env))
      SchemeVal.Void
    case Expr.Symbol("define-record-type") :: Expr.Symbol(typeName) ::
        Expr.SList(Expr.Symbol(ctorName) :: ctorFields) ::
        Expr.Symbol(predName) :: fieldDefs =>
      RecordType.defineRecordType(typeName, ctorName, ctorFields, predName, fieldDefs, env)
    case Expr.Symbol("letrec") :: Expr.SList(bindings) :: body =>
      EvalForms.evalLetrec(bindings, body, env)
    case Expr.Symbol("letrec*") :: Expr.SList(bindings) :: body =>
      EvalForms.evalLetrecStar(bindings, body, env)
    case Expr.Symbol("case") :: key :: clauses =>
      EvalForms.evalCase(eval(key, env), clauses, env)
    case Expr.Symbol("do") :: Expr.SList(varClauses) :: Expr.SList(testAndResult) :: bodyExprs =>
      EvalForms.evalDo(varClauses, testAndResult, bodyExprs, env)
    case Expr.Symbol("case-lambda") :: clauseExprs =>
      EvalForms.evalCaseLambda(clauseExprs, env)
    case Expr.Symbol("call/cc") :: procExpr :: Nil =>
      evalCallCC(procExpr, env)
    case Expr.Symbol("call-with-current-continuation") :: procExpr :: Nil =>
      evalCallCC(procExpr, env)
    case Expr.Symbol("dynamic-wind") :: inExpr :: bodyExpr :: outExpr :: Nil =>
      DynamicWind.evalDynamicWind(eval(inExpr, env), eval(bodyExpr, env), eval(outExpr, env))
    case Expr.Symbol("guard") :: Expr.SList(Expr.Symbol(varName) :: clauses) :: body =>
      EvalForms.evalGuard(varName, clauses, body, env)
    case Expr.Symbol("syntax-case") :: stxExpr :: Expr.SList(lits) :: clauses =>
      SyntaxCaseEval.evalSyntaxCase(stxExpr, lits, clauses, env)
    case Expr.Symbol("syntax") :: tmpl :: Nil =>
      SyntaxCaseEval.evalSyntaxForm(tmpl, env)
    case Expr.Symbol("with-syntax") :: Expr.SList(bindings) :: body =>
      SyntaxCaseEval.evalWithSyntax(bindings, body, env)
    case Expr.Symbol(name) :: _ if MacroExpander.isMacro(name, env) =>
      env.lookup(name) match
        case m: SchemeVal.Macro             => MacroExpander.expandAndEval(expr, name, m, env, eval)
        case mt: SchemeVal.MacroTransformer => SyntaxCaseEval.expandSyntaxCaseMacro(expr, name, mt, env)
        case _                              => throw new EvalError(s"$name: expected macro")
    case head :: args =>
      applyProcInner(eval(head, env), args.map(a => eval(a, env)))

  private def applyProcInner(fn: SchemeVal, evaledArgs: List[SchemeVal]): SchemeVal =
    fn match
      case SchemeVal.BuiltinProc(_, f) => f(evaledArgs)
      case SchemeVal.Procedure(params, restParam, body, closureEnv) =>
        val newEnv = new Env(mutable.Map.empty, Some(closureEnv))
        if restParam.isDefined then
          if evaledArgs.length < params.length then
            throw new EvalError(s"expected at least ${params.length} arguments, got ${evaledArgs.length}")
          params.zip(evaledArgs).foreach((p, v) => newEnv.define(p, v))
          newEnv.define(restParam.get, SchemeVal.schemeList(evaledArgs.drop(params.length)))
        else
          if evaledArgs.length != params.length then
            throw new EvalError(s"expected ${params.length} arguments, got ${evaledArgs.length}")
          params.zip(evaledArgs).foreach((p, v) => newEnv.define(p, v))
        evalBodyTail(body, newEnv)
      case SchemeVal.CaseLambda(clauses) =>
        val matching = clauses.find { (params, restParam, _, _) =>
          if restParam.isDefined then evaledArgs.length >= params.length
          else evaledArgs.length == params.length
        }
        matching match
          case Some((params, restParam, body, closureEnv)) =>
            applyProcInner(SchemeVal.Procedure(params, restParam, body, closureEnv), evaledArgs)
          case None =>
            throw new EvalError(s"case-lambda: no matching clause for ${evaledArgs.length} arguments")
      case SchemeVal.ContinuationVal(id, body, startIdx, bodyEnv, savedWind) =>
        if evaledArgs.length != 1 then
          throw new EvalError(s"continuation: expected 1 argument, got ${evaledArgs.length}")
        // Save caller's wind before transition, perform wind transition, then jump
        val callerWind = windStack.get()
        DynamicWind.doWindTransition(callerWind, savedWind)
        throw new ContinuationReturn(id, evaledArgs.head, body, startIdx, bodyEnv, callerWind)
      case other => throw new EvalError(s"not a procedure: ${other.display}")

  def applyProc(fn: SchemeVal, evaledArgs: List[SchemeVal]): SchemeVal =
    trampoline(applyProcInner(fn, evaledArgs))

  def evalStr(input: String): String =
    val exprs = Parser.parse(input)
    val env   = Builtins.makeGlobalEnv()
    evalBody(exprs, env).display

  def evalStrWithOutput(input: String): (String, String) =
    val buf = outputBuffer.get()
    buf.clear()
    val exprs  = Parser.parse(input)
    val env    = Builtins.makeGlobalEnv()
    val result = evalBody(exprs, env).display
    val output = buf.toString
    buf.clear()
    (result, output)
