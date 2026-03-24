package ming

import scala.collection.mutable

object Evaluator:

  val outputBuffer: ThreadLocal[StringBuilder] = ThreadLocal.withInitial(() => new StringBuilder)

  // --- call/cc support ---
  private val contIdCounter                                  = new java.util.concurrent.atomic.AtomicLong(0)
  private[ming] def nextContId(): Long                       = contIdCounter.incrementAndGet()
  private[ming] val currentBodyCtx: ThreadLocal[BodyContext] = ThreadLocal.withInitial(() => null)
  private[ming] val pendingCCReturn: ThreadLocal[SchemeVal]  = ThreadLocal.withInitial(() => null)

  private[ming] def isTruthy(v: SchemeVal): Boolean = v match
    case SchemeVal.BoolVal(false) => false
    case _                        => true

  /** Evaluate all body exprs, returning the last value (fully evaluated). Handles ContinuationReturn for reentrant
    * continuation support.
    */
  private[ming] def evalBody(body: List[Expr], env: Env, startIdx: Int = 0): SchemeVal =
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
          if (cr.body ne null) && (cr.body eq body) then
            // Reentry targets this body: restart from the stored index
            i = cr.startIdx
          else if cr.body ne null then
            // Reentry targets an inner body: evaluate it directly
            result = evalBody(cr.body, cr.bodyEnv, cr.startIdx)
            i += 1
          else throw cr
      finally currentBodyCtx.set(prev)
    result

  /** Evaluate all but last body expr; return TailCall for the last (for TCO). Pushes body context for each expression
    * so call/cc can capture it.
    */
  private[ming] def evalBodyTail(body: List[Expr], env: Env): SchemeVal =
    if body.isEmpty then SchemeVal.Void
    else
      for idx <- 0 until body.length - 1 do
        val prev = currentBodyCtx.get()
        currentBodyCtx.set(BodyContext(body, idx, env))
        try eval(body(idx), env)
        finally currentBodyCtx.set(prev)
      SchemeVal.TailCall(body.last, env)

  private def posStr(expr: Expr): String =
    val p = Parser.positions.get(expr)
    if p != null then s"${p._1}:${p._2}" else "1:1"

  private val posPattern = ".*\\d+:\\d+.*".r

  private def evalDefineSyntax(
    name: String,
    lits: List[Expr],
    rules: List[Expr],
    env: Env
  ): SchemeVal =
    val literals = lits.map {
      case Expr.Symbol(n) => n
      case _              => throw new EvalError("syntax-rules: literals must be identifiers")
    }.toSet
    val ruleList = rules.map {
      case Expr.SList(pat :: tmpl :: Nil) => (pat, tmpl)
      case _                              => throw new EvalError("syntax-rules: invalid rule")
    }
    env.define(name, SchemeVal.Macro(literals, ruleList, env))
    SchemeVal.Void

  /** Trampoline: resolve TailCall chain into a final value */
  private def trampoline(initial: SchemeVal): SchemeVal =
    var result = initial
    while result.isInstanceOf[SchemeVal.TailCall] do
      val SchemeVal.TailCall(e, env) = result: @unchecked
      result = evalInner(e, env)
    result

  /** Public eval: always returns a fully evaluated value (trampolines internally) */
  private[ming] def eval(expr: Expr, env: Env): SchemeVal =
    trampoline(evalInner(expr, env))

  /** Handle call/cc as a special form */
  private def evalCallCC(procExpr: Expr, env: Env): SchemeVal =
    val pending = pendingCCReturn.get()
    if pending != null then
      pendingCCReturn.set(null)
      pending
    else
      val proc = eval(procExpr, env)
      performCallCC(proc)

  /** Shared call/cc logic: create continuation, call proc, handle escape */
  private[ming] def performCallCC(proc: SchemeVal): SchemeVal =
    val ctx    = currentBodyCtx.get()
    val contId = nextContId()
    val cont = SchemeVal.ContinuationVal(
      contId,
      if ctx != null then ctx.body else null,
      if ctx != null then ctx.idx else 0,
      if ctx != null then ctx.env else null
    )
    try applyProc(proc, List(cont))
    catch case cr: ContinuationReturn if cr.contId == contId => cr.value

  /** Inner eval: may return TailCall for tail positions */
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

  /** Dispatch on S-expression forms */
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
      evalDefineSyntax(name, lits, rules, env)
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
      evalCaseLambda(clauseExprs, env)
    case Expr.Symbol("call/cc") :: procExpr :: Nil =>
      evalCallCC(procExpr, env)
    case Expr.Symbol("call-with-current-continuation") :: procExpr :: Nil =>
      evalCallCC(procExpr, env)
    case Expr.Symbol(name) :: _ if MacroExpander.isMacro(name, env) =>
      env.lookup(name) match
        case m: SchemeVal.Macro => MacroExpander.expandAndEval(expr, name, m, env, eval)
        case _                  => throw new EvalError(s"$name: expected macro")
    case head :: args =>
      applyProcInner(eval(head, env), args.map(a => eval(a, env)))

  private def evalCaseLambda(clauseExprs: List[Expr], env: Env): SchemeVal =
    val clauses = clauseExprs.map {
      case Expr.SList(Expr.SList(params) :: body) =>
        val (paramNames, restParam) = EvalHelpers.parseParams(params)
        (paramNames, restParam, body, env)
      case _ => throw new EvalError("case-lambda: invalid clause")
    }
    SchemeVal.CaseLambda(clauses)

  /** Inner apply: may return TailCall for procedure bodies (used from evalInner) */
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
      case SchemeVal.ContinuationVal(id, body, startIdx, bodyEnv) =>
        if evaledArgs.length != 1 then
          throw new EvalError(s"continuation: expected 1 argument, got ${evaledArgs.length}")
        throw new ContinuationReturn(id, evaledArgs.head, body, startIdx, bodyEnv)
      case other => throw new EvalError(s"not a procedure: ${other.display}")

  /** Public apply: always fully evaluates (trampolines TailCall) */
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
