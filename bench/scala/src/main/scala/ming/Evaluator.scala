package ming

/** Scheme interpreter — CEK machine with first-class continuations. */
object Evaluator:

  /** Thread-local dynamic-wind stack (head = innermost extent). */
  private[ming] val windStack: ThreadLocal[List[WindEntry]] =
    ThreadLocal.withInitial(() => Nil)

  /** Thread-local exception handler stack (head = innermost handler). */
  private[ming] val handlerStack: ThreadLocal[List[ExceptionHandler]] =
    ThreadLocal.withInitial(() => Nil)

  private[ming] enum State:
    case Ev(expr: SchemeVal, env: Env, k: Cont)
    case Ko(value: SchemeVal, k: Cont)

  private[ming] def isTruthy(v: SchemeVal): Boolean = v match
    case SchemeVal.SBool(false) => false
    case _                      => true

  private[ming] def evalBody(body: List[SchemeVal], env: Env): SchemeVal =
    body match
      case Nil           => SchemeVal.SVoid
      case last :: Nil   => run(State.Ev(last, env, Cont.Halt))
      case first :: rest => run(State.Ev(first, env, Cont.SeqK(rest, env, Cont.Halt)))

  def eval(initExpr: SchemeVal, initEnv: Env): SchemeVal =
    run(State.Ev(initExpr, initEnv, Cont.Halt))

  private def run(initState: State): SchemeVal =
    var state                = initState
    var lastPos: Option[Pos] = None
    while true do
      state match
        case State.Ko(v, Cont.Halt) => return v
        case _                      =>
          // Track position from the most recent compound expression
          state match
            case State.Ev(expr @ SchemeVal.SList(_), _, _) if expr.pos.isDefined =>
              lastPos = expr.pos
            case _ => ()
          try state = step(state)
          catch
            case e: EvalError if !e.hasPosition =>
              lastPos match
                case Some(p) =>
                  throw new EvalError(s"${e.getMessage} at ${p.line}:${p.col}", hasPosition = true)
                case None => throw e
    throw new RuntimeException("unreachable")

  private def step(s: State): State = s match
    case State.Ev(expr, env, k) => evalStep(expr, env, k)
    case State.Ko(v, k)         => kontStep(v, k)

  private def evalStep(expr: SchemeVal, env: Env, k: Cont): State =
    expr match
      case SchemeVal.SInt(_) | SchemeVal.SFloat(_) | SchemeVal.SRational(_, _) | SchemeVal.SBool(_) |
          SchemeVal.SString(_, _) | SchemeVal.SChar(_) | SchemeVal.SVoid | SchemeVal.SPair(_) | SchemeVal.SVector(_) =>
        State.Ko(expr, k)
      case SchemeVal.SSymbol(name) => State.Ko(env.get(name), k)
      case SchemeVal.SList(elems)  => dispatchList(elems, env, k)
      case other                   => State.Ko(other, k)

  private def dispatchList(elems: List[SchemeVal], env: Env, k: Cont): State =
    elems match
      case Nil => throw new EvalError("empty application")
      case SchemeVal.SSymbol("quote") :: args =>
        if args.length != 1 then throw new EvalError("quote: expected 1 argument")
        State.Ko(args.head, k)
      case SchemeVal.SSymbol("if") :: args =>
        if args.length < 2 || args.length > 3 then throw new EvalError("if: expected 2 or 3 arguments")
        State.Ev(args(0), env, Cont.IfK(args(1), args.lift(2), env, k))
      case SchemeVal.SSymbol("define") :: args =>
        LetForms.evalDefineStep(args, env, k)
      case SchemeVal.SSymbol("lambda") :: args =>
        State.Ko(DefineForms.evalLambda(args, env), k)
      case SchemeVal.SSymbol("and") :: args =>
        if args.isEmpty then State.Ko(SchemeVal.SBool(true), k)
        else if args.length == 1 then State.Ev(args.head, env, k)
        else State.Ev(args.head, env, Cont.AndK(args.tail, env, k))
      case SchemeVal.SSymbol("or") :: args =>
        if args.isEmpty then State.Ko(SchemeVal.SBool(false), k)
        else if args.length == 1 then State.Ev(args.head, env, k)
        else State.Ev(args.head, env, Cont.OrK(args.tail, env, k))
      case SchemeVal.SSymbol("let") :: args =>
        LetForms.evalLetStep(args, env, k)
      case SchemeVal.SSymbol("set!") :: args =>
        args match
          case SchemeVal.SSymbol(name) :: valueExpr :: Nil =>
            State.Ev(valueExpr, env, Cont.SetValK(name, env, k))
          case _ => throw new EvalError("set!: bad syntax")
      case SchemeVal.SSymbol("begin") :: args =>
        evalBodyCek(args, env, k)
      case SchemeVal.SSymbol("cond") :: clauses =>
        LetForms.evalCondStep(clauses, env, k)
      case SchemeVal.SSymbol("define-syntax") :: args =>
        State.Ko(DefineForms.evalDefineSyntax(args, env), k)
      case SchemeVal.SSymbol("case-lambda") :: args =>
        State.Ko(BindingForms.evalCaseLambda(args, env), k)
      case SchemeVal.SSymbol("define-record-type") :: args =>
        State.Ko(RecordOps.evalDefineRecordType(args, env), k)
      case SchemeVal.SSymbol("letrec") :: args =>
        LetForms.evalLetrecStep(args, env, k)
      case SchemeVal.SSymbol("letrec*") :: args =>
        LetForms.evalLetrecStarStep(args, env, k)
      case SchemeVal.SSymbol("case") :: args =>
        State.Ko(BindingForms.evalCase(args, env), k)
      case SchemeVal.SSymbol("do") :: args =>
        State.Ko(BindingForms.evalDo(args, env), k)
      case SchemeVal.SSymbol("let*") :: args =>
        LetForms.evalLetStarStep(args, env, k)
      case SchemeVal.SSymbol("guard") :: args =>
        args match
          case SchemeVal.SList(SchemeVal.SSymbol(exnVar) :: clauses) :: body if body.nonEmpty =>
            val handler = ExceptionHandler.Guard(exnVar, clauses, env, k, windStack.get())
            handlerStack.set(handler :: handlerStack.get())
            evalBodyCek(body, env, Cont.WithHandlerK(k))
          case _ => throw new EvalError("guard: bad syntax")
      case SchemeVal.SSymbol(name) :: _ if env.lookup(name).exists(isMacro) =>
        val macro_   = env.get(name).asMatchedMacro
        val expanded = Macro.expand(macro_, SchemeVal.SList(elems))
        State.Ev(expanded, env, k)
      case head :: args =>
        State.Ev(head, env, Cont.EvOpK(args, env, k))

  private[ming] def evalBodyCek(body: List[SchemeVal], env: Env, k: Cont): State =
    body match
      case Nil           => State.Ko(SchemeVal.SVoid, k)
      case last :: Nil   => State.Ev(last, env, k)
      case first :: rest => State.Ev(first, env, Cont.SeqK(rest, env, k))

  private def kontStep(v: SchemeVal, k: Cont): State = k match
    case Cont.Halt => State.Ko(v, k)
    case Cont.IfK(thenE, elseE, env, k2) =>
      if isTruthy(v) then State.Ev(thenE, env, k2)
      else
        elseE match
          case Some(e) => State.Ev(e, env, k2)
          case None    => State.Ko(SchemeVal.SVoid, k2)
    case Cont.SeqK(rest, env, k2) =>
      rest match
        case last :: Nil  => State.Ev(last, env, k2)
        case next :: more => State.Ev(next, env, Cont.SeqK(more, env, k2))
        case Nil          => State.Ko(SchemeVal.SVoid, k2)
    case Cont.DefValK(name, env, k2) =>
      env.define(name, v)
      State.Ko(SchemeVal.SVoid, k2)
    case Cont.SetValK(name, env, k2) =>
      env.set(name, v)
      State.Ko(SchemeVal.SVoid, k2)
    case Cont.EvOpK(argExprs, env, k2) =>
      if argExprs.isEmpty then performApply(v, Nil, k2)
      else
        // Right-to-left argument evaluation (matches Chez Scheme)
        val rev = argExprs.reverse
        State.Ev(rev.head, env, Cont.EvArgK(v, Nil, rev.tail, env, k2))
    case Cont.EvArgK(op, done, rest, env, k2) =>
      val newDone = v :: done // prepend; with reversed eval order, this builds correct order
      if rest.isEmpty then performApply(op, newDone, k2)
      else State.Ev(rest.head, env, Cont.EvArgK(op, newDone, rest.tail, env, k2))
    case Cont.AndK(rest, env, k2) =>
      if !isTruthy(v) then State.Ko(v, k2)
      else
        rest match
          case last :: Nil  => State.Ev(last, env, k2)
          case next :: more => State.Ev(next, env, Cont.AndK(more, env, k2))
          case Nil          => State.Ko(v, k2)
    case Cont.OrK(rest, env, k2) =>
      if isTruthy(v) then State.Ko(v, k2)
      else
        rest match
          case last :: Nil  => State.Ev(last, env, k2)
          case next :: more => State.Ev(next, env, Cont.OrK(more, env, k2))
          case Nil          => State.Ko(v, k2)
    case Cont.CondK(body, remaining, env, k2) =>
      if isTruthy(v) then
        if body.isEmpty then State.Ko(v, k2)
        else evalBodyCek(body, env, k2)
      else LetForms.evalCondStep(remaining, env, k2)
    // dynamic-wind normal flow
    case Cont.DynWindAfterInK(bodyThunk, entry, k2) =>
      // in-thunk done; push entry onto wind stack, call body
      windStack.set(entry :: windStack.get())
      performApply(bodyThunk, Nil, Cont.DynWindAfterBodyK(entry, k2))
    case Cont.DynWindAfterBodyK(entry, k2) =>
      // body done; pop entry from wind stack, call out-thunk
      val ws = windStack.get()
      if ws.nonEmpty && (ws.head eq entry) then windStack.set(ws.tail)
      performApply(entry.outThunk, Nil, Cont.DynWindAfterOutK(v, k2))
    case Cont.DynWindAfterOutK(bodyValue, k2) =>
      // out-thunk done; return body value
      State.Ko(bodyValue, k2)
    // continuation wind/unwind steps
    case Cont.WindContinueK(remaining, finalValue, targetK, targetWinds) =>
      DynWind.startWindActions(remaining, finalValue, targetK, targetWinds)(performApply)
    case Cont.WindPushK(entry, remaining, finalValue, targetK, targetWinds) =>
      // in-thunk done during rewind; push entry onto wind stack
      windStack.set(entry :: windStack.get())
      DynWind.startWindActions(remaining, finalValue, targetK, targetWinds)(performApply)
    // exception handling (L20)
    case Cont.WithHandlerK(k2) =>
      // Body/thunk completed normally — pop handler, return value
      val hs = handlerStack.get()
      if hs.nonEmpty then handlerStack.set(hs.tail)
      State.Ko(v, k2)
    case Cont.RaiseReturnErrorK =>
      throw new EvalError("handler returned from non-continuable exception")
    case Cont.GuardAfterWindK(clauses, exnValue, env, guardK) =>
      ExceptionOps.evalGuardClauses(clauses, exnValue, env, guardK, performApply)
    case Cont.GuardTestK(body, remaining, exnValue, env, guardK) =>
      if isTruthy(v) then
        if body.isEmpty then State.Ko(v, guardK)
        else evalBodyCek(body, env, guardK)
      else ExceptionOps.evalGuardClauses(remaining, exnValue, env, guardK, performApply)

  private def performApply(op: SchemeVal, args: List[SchemeVal], k: Cont): State =
    op match
      case SchemeVal.SLambda(params, restParam, body, closure) =>
        val callEnv = Apply.setupCallEnv(params, restParam, args, closure)
        evalBodyCek(body, callEnv, k)
      case SchemeVal.SCaseLambda(clauses, closure) =>
        val (cparams, crest, cbody) = Apply.findClause(clauses, args)
        val callEnv                 = Apply.setupCallEnv(cparams, crest, args, closure)
        evalBodyCek(cbody, callEnv, k)
      case SchemeVal.SContinuation(savedK, savedWinds) =>
        if args.length != 1 then throw new EvalError("continuation: expected 1 argument")
        val currentWinds = windStack.get()
        val actions      = DynWind.computeWindActions(currentWinds, savedWinds)
        if actions.isEmpty then State.Ko(args.head, savedK)
        else DynWind.startWindActions(actions, args.head, savedK, savedWinds)(performApply)
      case SchemeVal.SSymbol(name) if name == "call/cc" || name == "call-with-current-continuation" =>
        if args.length != 1 then throw new EvalError("call/cc: expected 1 argument")
        val contVal = SchemeVal.SContinuation(k, windStack.get())
        performApply(args.head, List(contVal), k)
      case SchemeVal.SSymbol("dynamic-wind") =>
        if args.length != 3 then throw new EvalError("dynamic-wind: expected 3 arguments")
        val (inThunk, bodyThunk, outThunk) = (args(0), args(1), args(2))
        val entry                          = new WindEntry(inThunk, outThunk)
        performApply(inThunk, Nil, Cont.DynWindAfterInK(bodyThunk, entry, k))
      case SchemeVal.SSymbol("raise") =>
        if args.length != 1 then throw new EvalError("raise: expected 1 argument")
        ExceptionOps.handleRaise(args.head, k, performApply)
      case SchemeVal.SSymbol("with-exception-handler") =>
        if args.length != 2 then throw new EvalError("with-exception-handler: expected 2 arguments")
        val (handler, thunk) = (args(0), args(1))
        handlerStack.set(ExceptionHandler.Proc(handler, windStack.get()) :: handlerStack.get())
        performApply(thunk, Nil, Cont.WithHandlerK(k))
      case SchemeVal.SSymbol(name)
          if name.startsWith("__record-ctor__:") ||
            name.startsWith("__record-pred__:") ||
            name.startsWith("__record-acc__:") =>
        State.Ko(RecordOps.applyRecordOp(name, args), k)
      case SchemeVal.SSymbol(name) =>
        State.Ko(Apply.applyBuiltinOrHOF(name, args), k)
      case _ => throw new EvalError(s"not a procedure: ${op.display}")

  private def isMacro(v: SchemeVal): Boolean = v match
    case _: SchemeVal.SMacro => true
    case _                   => false

  private def makeGlobalEnv(): Env =
    val env = Env()
    for name <- Builtins.names do env.define(name, SchemeVal.SSymbol(name))
    env

  def evalStr(input: String): String =
    windStack.set(Nil)
    handlerStack.set(Nil)
    val exprs = Parser.parseAll(input)
    if exprs.isEmpty then throw new EvalError("empty input")
    val env = makeGlobalEnv()
    evalBody(exprs, env).display

  def evalStrWithOutput(input: String): (String, String) =
    windStack.set(Nil)
    handlerStack.set(Nil)
    val buf = HigherOrder.outputBuffer.get()
    buf.clear()
    val exprs = Parser.parseAll(input)
    if exprs.isEmpty then throw new EvalError("empty input")
    val env    = makeGlobalEnv()
    val result = evalBody(exprs, env).display
    val output = buf.toString
    buf.clear()
    (result, output)
