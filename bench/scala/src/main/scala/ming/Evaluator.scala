package ming

object Evaluator:

  def evalStr(input: String): String =
    val tokens = Tokenizer.tokenize(input)
    val exprs  = Parser.parseAll(tokens)
    if exprs.isEmpty then throw new EvalError("empty input")
    windStack = Nil
    handlerStack = Nil
    val env = GlobalEnv.create()
    evalSequence(exprs, env).display

  def evalStrWithLimit(input: String, maxSteps: Int): String =
    val tokens = Tokenizer.tokenize(input)
    val exprs  = Parser.parseAll(tokens)
    if exprs.isEmpty then throw new EvalError("empty input")
    windStack = Nil
    handlerStack = Nil
    val env = GlobalEnv.create()
    runWithLimit(
      if exprs.size == 1 then SEval(exprs.head, env, HaltK)
      else SEval(exprs.head, env, SeqK(exprs.tail, env, HaltK)),
      maxSteps
    ).display

  def evalStrWithOutput(input: String): (String, String) =
    val tokens = Tokenizer.tokenize(input)
    val exprs  = Parser.parseAll(tokens)
    if exprs.isEmpty then throw new EvalError("empty input")
    windStack = Nil
    handlerStack = Nil
    val output = new StringBuilder
    val env    = GlobalEnv.create(output)
    val result = evalSequence(exprs, env)
    (result.display, output.toString)

  private[ming] def isFalsy(v: SchemeVal): Boolean = v match
    case SchemeBool(false) => true
    case _                 => false

  private[ming] def eval(expr: Expr, env: Env): SchemeVal =
    run(SEval(expr, env, HaltK))

  private[ming] def applyProc(op: SchemeVal, args: List[SchemeVal]): SchemeVal =
    run(ProcApply.applyFunction(op, args, HaltK))

  private[ming] def applyProcSafe(op: SchemeVal, args: List[SchemeVal]): SchemeVal =
    applyProc(op, args)

  private[ming] def evalBody(exprs: List[Expr], env: Env): SchemeVal =
    if exprs.isEmpty then SchemeVoid
    else evalSequence(exprs, env)

  private def evalSequence(exprs: List[Expr], env: Env): SchemeVal =
    if exprs.isEmpty then SchemeVoid
    else if exprs.size == 1 then run(SEval(exprs.head, env, HaltK))
    else run(SEval(exprs.head, env, SeqK(exprs.tail, env, HaltK)))

  private val _lastPos: ThreadLocal[Pos] = ThreadLocal.withInitial(() => Pos.zero)
  private def lastPos: Pos = _lastPos.get()
  private def lastPos_=(p: Pos): Unit = _lastPos.set(p)

  private val _windStack: ThreadLocal[List[(SchemeVal, SchemeVal)]] = ThreadLocal.withInitial(() => Nil)
  private[ming] def windStack: List[(SchemeVal, SchemeVal)] = _windStack.get()
  private[ming] def windStack_=(v: List[(SchemeVal, SchemeVal)]): Unit = _windStack.set(v)

  private val _handlerStack: ThreadLocal[List[ExceptionHandler]] = ThreadLocal.withInitial(() => Nil)
  private[ming] def handlerStack: List[ExceptionHandler] = _handlerStack.get()
  private[ming] def handlerStack_=(v: List[ExceptionHandler]): Unit = _handlerStack.set(v)

  private def run(initial: MState): SchemeVal =
    var state = initial
    while true do
      state match
        case SApply(v, HaltK) => return v
        case _ =>
          try state = step(state)
          catch
            case raised: SchemeRaisedException =>
              if handlerStack.nonEmpty then
                val handler = handlerStack.head
                handlerStack = handlerStack.tail
                state = handler match
                  case WHHandler(proc) =>
                    ProcApply.applyFunction(proc, List(raised.value), RaiseReturnCheckK)
                  case GuardExHandler(variable, clauses, genv, guardK, savedWind) =>
                    val common   = EvalHelpers.commonWindTail(windStack, savedWind)
                    val toUnwind = windStack.take(windStack.length - common.length)
                    val toRewind = savedWind.take(savedWind.length - common.length).reverse
                    val actions: List[WindAction] =
                      toUnwind.map(e => DoUnwind(e._2)) ++ toRewind.map(e => DoRewind(e._1, e))
                    val clauseK = GuardClauseK(variable, raised.value, clauses, genv, guardK)
                    ProcApply.processWindActions(actions, raised.value, clauseK)
              else throw new EvalError(s"unhandled exception: ${raised.value.display}")
            case e: EvalError =>
              val msg = e.getMessage
              if msg.matches(".*\\d+:\\d+.*") then throw e
              else throw new EvalError(s"$lastPos: $msg")
    throw new AssertionError("unreachable")

  private def runWithLimit(initial: MState, maxSteps: Int): SchemeVal =
    var state = initial
    var steps = 0
    while true do
      state match
        case SApply(v, HaltK) => return v
        case _ =>
          steps += 1
          if steps > maxSteps then throw new EvalError("step limit exceeded")
          try state = step(state)
          catch
            case raised: SchemeRaisedException =>
              if handlerStack.nonEmpty then
                val handler = handlerStack.head
                handlerStack = handlerStack.tail
                state = handler match
                  case WHHandler(proc) =>
                    ProcApply.applyFunction(proc, List(raised.value), RaiseReturnCheckK)
                  case GuardExHandler(variable, clauses, genv, guardK, savedWind) =>
                    val common   = EvalHelpers.commonWindTail(windStack, savedWind)
                    val toUnwind = windStack.take(windStack.length - common.length)
                    val toRewind = savedWind.take(savedWind.length - common.length).reverse
                    val actions: List[WindAction] =
                      toUnwind.map(e => DoUnwind(e._2)) ++ toRewind.map(e => DoRewind(e._1, e))
                    val clauseK = GuardClauseK(variable, raised.value, clauses, genv, guardK)
                    ProcApply.processWindActions(actions, raised.value, clauseK)
              else throw new EvalError(s"unhandled exception: ${raised.value.display}")
            case e: EvalError =>
              val msg = e.getMessage
              if msg.matches(".*\\d+:\\d+.*") then throw e
              else throw new EvalError(s"$lastPos: $msg")
    throw new AssertionError("unreachable")

  private def step(s: MState): MState =
    s match
      case SEval(expr, env, k) =>
        lastPos = expr.pos
        evalStep(expr, env, k)
      case SApply(v, k) => applyKont(v, k)

  private def evalStep(expr: Expr, env: Env, k: Kont): MState = expr match
    case IntLit(v, _)         => SApply(SchemeInt(v), k)
    case FloatLit(v, _)       => SApply(SchemeFloat(v), k)
    case RationalLit(n, d, _) => SApply(SchemeRational(n, d), k)
    case BoolLit(v, _)        => SApply(SchemeBool(v), k)
    case StringLit(v, _)      => SApply(SchemeString(v), k)
    case CharLit(v, _)        => SApply(SchemeChar(v), k)
    case Symbol(name, _)      => SApply(env.get(name), k)
    case SList(Nil, _)        => throw new EvalError("empty application")
    case SList(elems, _)      => dispatchForm(elems, env, k)

  private def dispatchForm(elems: List[Expr], env: Env, k: Kont): MState =
    elems.head match
      case Symbol("if", _)         => SpecialForms.evalIfForm(elems.tail, env, k)
      case Symbol("define", _)     => SpecialForms.evalDefineForm(elems.tail, env, k)
      case Symbol("set!", _)       => SpecialForms.evalSetForm(elems.tail, env, k)
      case Symbol("begin", _)      => SpecialForms.evalBeginForm(elems.tail, env, k)
      case Symbol("quote", _)      => SApply(SpecialForms.evalQuote(elems.tail), k)
      case Symbol("quasiquote", _) => SpecialForms.evalQuasiquote(elems.tail, env, k)
      case Symbol("lambda", _)     => SApply(SpecialForms.makeLambda(elems.tail, env), k)
      case Symbol("and", _)        => SpecialForms.evalAndForm(elems.tail, env, k)
      case Symbol("or", _)         => SpecialForms.evalOrForm(elems.tail, env, k)
      case Symbol("call/cc", _)    => SpecialForms.evalCallCCForm(elems.tail, env, k)
      case Symbol("call-with-current-continuation", _) =>
        SpecialForms.evalCallCCForm(elems.tail, env, k)
      case Symbol("cond", _)        => SpecialForms.evalCondForm(elems.tail, env, k)
      case Symbol("case", _)        => SpecialForms.evalCaseForm(elems.tail, env, k)
      case Symbol("let", _)         => SpecialForms.evalLetForm(elems.tail, env, k)
      case Symbol("let*", _)        => SpecialForms.evalLetStarForm(elems.tail, env, k)
      case Symbol("letrec", _)      => SpecialForms.evalLetrecForm(elems.tail, env, k)
      case Symbol("letrec*", _)     => SpecialForms.evalLetrecStarForm(elems.tail, env, k)
      case Symbol("do", _)          => SpecialForms.evalDoForm(elems.tail, env, k)
      case Symbol("when", _)        => SpecialForms.evalWhenForm(elems.tail, env, k)
      case Symbol("unless", _)      => SpecialForms.evalUnlessForm(elems.tail, env, k)
      case Symbol("case-lambda", _) => SApply(SpecialForms.makeCaseLambda(elems.tail, env), k)
      case Symbol("define-syntax", _) =>
        SpecialForms.evalDefineSyntax(elems.tail, env)
        SApply(SchemeVoid, k)
      case Symbol("syntax-case", _) =>
        SyntaxCaseEval.evalSyntaxCaseForm(elems.tail, env, k)
      case Symbol("syntax-quote", _) =>
        if elems.tail.size != 1 then throw new EvalError("syntax-quote: expected 1 argument")
        SApply(Macros.expandSyntaxQuote(elems.tail.head, env), k)
      case Symbol("with-syntax", _) =>
        SyntaxCaseEval.evalWithSyntaxForm(elems.tail, env, k)
      case Symbol("define-record-type", _) =>
        SApply(Records.evalDefineRecordType(elems.tail, env), k)
      case Symbol("guard", _) => ProcApply.evalGuardForm(elems.tail, env, k)
      case _                  => dispatchMacroOrApply(elems, env, k)

  private def dispatchMacroOrApply(elems: List[Expr], env: Env, k: Kont): MState =
    val macroVal = elems.head match
      case Symbol(name, _) =>
        try
          env.get(name) match
            case m: SchemeMacro            => Some(m)
            case m: SchemeTransformerMacro => Some(m)
            case _                         => None
        catch case _: EvalError => None
      case _ => None
    macroVal match
      case Some(m: SchemeMacro) =>
        val expanded = Macros.expand(m, SList(elems, elems.head.pos), env)
        SEval(expanded, env, k)
      case Some(tm: SchemeTransformerMacro) =>
        val inputSyntax = SchemeSyntax(SList(elems, elems.head.pos))
        val result      = applyProc(tm.proc, List(inputSyntax))
        result match
          case SchemeSyntax(outExpr) => SEval(outExpr, env, k)
          case other =>
            throw new EvalError(s"syntax-case: transformer must return syntax, got ${other.display}")
      case _ =>
        SEval(elems.head, env, EvFunK(elems.tail, env, k))

  private def applyKont(value: SchemeVal, k: Kont): MState = k match
    case HaltK => SApply(value, HaltK)

    case IfK(thenE, elseE, env, k2) =>
      if !isFalsy(value) then SEval(thenE, env, k2)
      else
        elseE match
          case Some(e) => SEval(e, env, k2)
          case None    => SApply(SchemeVoid, k2)

    case SeqK(remaining, env, k2) =>
      if remaining.size == 1 then SEval(remaining.head, env, k2)
      else SEval(remaining.head, env, SeqK(remaining.tail, env, k2))

    case DefineK(name, env, k2) =>
      env.set(name, value)
      SApply(SchemeVoid, k2)

    case SetBangK(name, env, k2) =>
      env.update(name, value)
      SApply(SchemeVoid, k2)

    case EvFunK(argExprs, env, k2) =>
      if argExprs.isEmpty then ProcApply.applyFunction(value, Nil, k2)
      else
        val rev = argExprs.reverse
        SEval(rev.head, env, EvArgsK(value, Nil, rev.tail, env, k2))

    case EvArgsK(op, done, remaining, env, k2) =>
      val newDone = value :: done
      if remaining.isEmpty then ProcApply.applyFunction(op, newDone, k2)
      else SEval(remaining.head, env, EvArgsK(op, newDone, remaining.tail, env, k2))

    case AndK(remaining, env, k2) =>
      if isFalsy(value) then SApply(value, k2)
      else if remaining.isEmpty then SApply(value, k2)
      else if remaining.size == 1 then SEval(remaining.head, env, k2)
      else SEval(remaining.head, env, AndK(remaining.tail, env, k2))

    case OrK(remaining, env, k2) =>
      if !isFalsy(value) then SApply(value, k2)
      else if remaining.isEmpty then SApply(value, k2)
      else if remaining.size == 1 then SEval(remaining.head, env, k2)
      else SEval(remaining.head, env, OrK(remaining.tail, env, k2))

    case CallCCK(k2) =>
      val cont = new SchemeContinuation(k2, windStack)
      ProcApply.applyFunction(value, List(cont), k2)

    case CondTestK(body, remaining, env, k2) =>
      if !isFalsy(value) then
        body match
          case Symbol("=>", _) :: proc :: Nil =>
            SEval(proc, env, CondArrowK(value, k2))
          case Nil => SApply(value, k2)
          case _   => evalBodyCEK(body, env, k2)
      else SpecialForms.evalCondForm(remaining, env, k2)

    case CondArrowK(testVal, k2) =>
      ProcApply.applyFunction(value, List(testVal), k2)

    case CaseK(clauses, env, k2) =>
      SpecialForms.matchCaseClauses(value, clauses, env, k2)

    case LetrecBindK(name, remaining, localEnv, body, k2) =>
      localEnv.set(name, value)
      if remaining.isEmpty then evalBodyCEK(body, localEnv, k2)
      else
        SEval(
          remaining.head._2,
          localEnv,
          LetrecBindK(remaining.head._1, remaining.tail, localEnv, body, k2)
        )

    case TestBodyK(body, invert, env, k2) =>
      val shouldRun = if invert then isFalsy(value) else !isFalsy(value)
      if shouldRun then evalBodyCEK(body, env, k2)
      else SApply(SchemeVoid, k2)

    case other => applyExtendedKont(value, other)

  private def applyExtendedKont(value: SchemeVal, k: Kont): MState = k match
    case DynWindAfterInK(inThunk, bodyThunk, outThunk, k2) =>
      val entry = (inThunk, outThunk)
      windStack = entry :: windStack
      ProcApply.applyFunction(bodyThunk, Nil, DynWindAfterBodyK(outThunk, k2))

    case DynWindAfterBodyK(outThunk, k2) =>
      windStack = windStack.tail
      ProcApply.applyFunction(outThunk, Nil, DynWindAfterOutK(value, k2))

    case DynWindAfterOutK(bodyVal, k2) =>
      SApply(bodyVal, k2)

    case WindContK(actions, savedVal, targetK) =>
      ProcApply.processWindActions(actions, savedVal, targetK)

    case WindPushK(entry, actions, savedVal, targetK) =>
      windStack = entry :: windStack
      ProcApply.processWindActions(actions, savedVal, targetK)

    case ExHandlerPopK(k2) =>
      handlerStack = handlerStack.tail
      SApply(value, k2)

    case GuardBodyPopK(k2) =>
      handlerStack = handlerStack.tail
      SApply(value, k2)

    case GuardClauseK(variable, exnVal, clauses, env, k2) =>
      ProcApply.evaluateGuardClauses(exnVal, variable, clauses, env, k2)

    case GuardTestK(variable, exnVal, body, remaining, env, k2) =>
      if !isFalsy(value) then
        if body.isEmpty then SApply(value, k2)
        else
          val localEnv = new Env(scala.collection.mutable.Map(variable -> exnVal), Some(env))
          evalBodyCEK(body, localEnv, k2)
      else ProcApply.evaluateGuardClauses(exnVal, variable, remaining, env, k2)

    case CallWithValuesK(consumer, k2) =>
      val args = value match
        case SchemeValues(vals) => vals
        case single             => List(single)
      ProcApply.applyFunction(consumer, args, k2)

    case RaiseReturnCheckK =>
      throw new EvalError("handler returned from non-continuable exception")

    case SyntaxCaseK(literals, clauses, env, k2) =>
      SyntaxCaseEval.applySyntaxCase(value, literals, clauses, env, k2)

    case _ => throw new AssertionError(s"unhandled continuation: $k")

  private[ming] def evalBodyCEK(exprs: List[Expr], env: Env, k: Kont): MState =
    if exprs.isEmpty then SApply(SchemeVoid, k)
    else if exprs.size == 1 then SEval(exprs.head, env, k)
    else SEval(exprs.head, env, SeqK(exprs.tail, env, k))
