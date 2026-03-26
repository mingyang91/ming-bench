package ming

import scala.collection.mutable

object Evaluator:

  def evalStr(input: String): String =
    val tokens = Tokenizer.tokenize(input)
    val exprs  = Parser.parseAll(tokens)
    if exprs.isEmpty then throw new EvalError("empty input")
    windStack = Nil
    val env = makeGlobalEnv()
    evalSequence(exprs, env).display

  def evalStrWithOutput(input: String): (String, String) =
    val tokens = Tokenizer.tokenize(input)
    val exprs  = Parser.parseAll(tokens)
    if exprs.isEmpty then throw new EvalError("empty input")
    windStack = Nil
    val output = new StringBuilder
    val env    = makeGlobalEnv(output)
    val result = evalSequence(exprs, env)
    (result.display, output.toString)

  private def makeGlobalEnv(output: StringBuilder = new StringBuilder): Env =
    val env = new Env(mutable.Map.empty, None)
    Builtins.install(env, output)
    env.set("call/cc", SchemeCallCC)
    env.set("call-with-current-continuation", SchemeCallCC)
    env.set("dynamic-wind", SchemeDynamicWind)
    env

  private[ming] def isFalsy(v: SchemeVal): Boolean = v match
    case SchemeBool(false) => true
    case _                 => false

  private[ming] def eval(expr: Expr, env: Env): SchemeVal =
    run(SEval(expr, env, HaltK))

  private[ming] def applyProc(op: SchemeVal, args: List[SchemeVal]): SchemeVal =
    run(applyFunction(op, args, HaltK))

  private[ming] def applyProcSafe(op: SchemeVal, args: List[SchemeVal]): SchemeVal =
    applyProc(op, args)

  private[ming] def evalBody(exprs: List[Expr], env: Env): SchemeVal =
    if exprs.isEmpty then SchemeVoid
    else evalSequence(exprs, env)

  private def evalSequence(exprs: List[Expr], env: Env): SchemeVal =
    if exprs.isEmpty then SchemeVoid
    else if exprs.size == 1 then run(SEval(exprs.head, env, HaltK))
    else run(SEval(exprs.head, env, SeqK(exprs.tail, env, HaltK)))

  private var lastPos: Pos                            = Pos.zero
  private var windStack: List[(SchemeVal, SchemeVal)] = Nil

  private def run(initial: MState): SchemeVal =
    var state = initial
    while true do
      state match
        case SApply(v, HaltK) => return v
        case _ =>
          try state = step(state)
          catch
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
      case Symbol("if", _)      => SpecialForms.evalIfForm(elems.tail, env, k)
      case Symbol("define", _)  => SpecialForms.evalDefineForm(elems.tail, env, k)
      case Symbol("set!", _)    => SpecialForms.evalSetForm(elems.tail, env, k)
      case Symbol("begin", _)   => SpecialForms.evalBeginForm(elems.tail, env, k)
      case Symbol("quote", _)   => SApply(SpecialForms.evalQuote(elems.tail), k)
      case Symbol("lambda", _)  => SApply(SpecialForms.makeLambda(elems.tail, env), k)
      case Symbol("and", _)     => SpecialForms.evalAndForm(elems.tail, env, k)
      case Symbol("or", _)      => SpecialForms.evalOrForm(elems.tail, env, k)
      case Symbol("call/cc", _) => SpecialForms.evalCallCCForm(elems.tail, env, k)
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
      case Symbol("define-record-type", _) =>
        SApply(Records.evalDefineRecordType(elems.tail, env), k)
      case _ => dispatchMacroOrApply(elems, env, k)

  private def dispatchMacroOrApply(elems: List[Expr], env: Env, k: Kont): MState =
    val macroVal = elems.head match
      case Symbol(name, _) =>
        try
          env.get(name) match
            case m: SchemeMacro => Some(m)
            case _              => None
        catch case _: EvalError => None
      case _ => None
    macroVal match
      case Some(m) =>
        val expanded = Macros.expand(m, SList(elems, elems.head.pos), env)
        SEval(expanded, env, k)
      case None =>
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
      if argExprs.isEmpty then applyFunction(value, Nil, k2)
      else
        val rev = argExprs.reverse
        SEval(rev.head, env, EvArgsK(value, Nil, rev.tail, env, k2))

    case EvArgsK(op, done, remaining, env, k2) =>
      val newDone = value :: done
      if remaining.isEmpty then applyFunction(op, newDone, k2)
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
      applyFunction(value, List(cont), k2)

    case CondTestK(body, remaining, env, k2) =>
      if !isFalsy(value) then
        if body.isEmpty then SApply(value, k2)
        else evalBodyCEK(body, env, k2)
      else SpecialForms.evalCondForm(remaining, env, k2)

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

    case DynWindAfterInK(inThunk, bodyThunk, outThunk, k2) =>
      val entry = (inThunk, outThunk)
      windStack = entry :: windStack
      applyFunction(bodyThunk, Nil, DynWindAfterBodyK(outThunk, k2))

    case DynWindAfterBodyK(outThunk, k2) =>
      windStack = windStack.tail
      applyFunction(outThunk, Nil, DynWindAfterOutK(value, k2))

    case DynWindAfterOutK(bodyVal, k2) =>
      SApply(bodyVal, k2)

    case WindContK(actions, savedVal, targetK) =>
      processWindActions(actions, savedVal, targetK)

    case WindPushK(entry, actions, savedVal, targetK) =>
      windStack = entry :: windStack
      processWindActions(actions, savedVal, targetK)

  private[ming] def applyFunction(op: SchemeVal, args: List[SchemeVal], k: Kont): MState =
    op match
      case SchemeBuiltin("apply", _) =>
        if args.size < 2 then throw new EvalError("apply: expected at least 2 arguments")
        val proc = args.head
        val lastList = SchemeListOps
          .toScalaList(args.last)
          .getOrElse(throw new EvalError("apply: last argument must be a list"))
        val allArgs = args.slice(1, args.size - 1) ++ lastList
        applyFunction(proc, allArgs, k)

      case SchemeBuiltin(_, fn) =>
        SApply(fn(args), k)

      case SchemeLambda(params, restParam, body, closureEnv) =>
        val localEnv = EvalHelpers.bindArgs(params, restParam, args, closureEnv)
        evalBodyCEK(body, localEnv, k)

      case SchemeCaseLambda(clauses) =>
        val matching = clauses.find { lam =>
          lam.restParam match
            case None    => args.size == lam.params.size
            case Some(_) => args.size >= lam.params.size
        }
        matching match
          case Some(lam) => applyFunction(lam, args, k)
          case None      => throw new EvalError(s"no matching clause for ${args.size} arguments")

      case SchemeCallCC =>
        if args.size != 1 then throw new EvalError("call/cc: expected 1 argument")
        val cont = new SchemeContinuation(k, windStack)
        applyFunction(args.head, List(cont), k)

      case SchemeDynamicWind =>
        if args.size != 3 then throw new EvalError("dynamic-wind: expected 3 arguments")
        val List(inThunk, bodyThunk, outThunk) = args
        applyFunction(inThunk, Nil, DynWindAfterInK(inThunk, bodyThunk, outThunk, k))

      case cont: SchemeContinuation =>
        if args.size != 1 then throw new EvalError("continuation: expected 1 argument")
        cont.savedK match
          case targetK: Kont =>
            val targetWind = cont.savedWind
            val v          = args.head
            val common     = EvalHelpers.commonWindTail(windStack, targetWind)
            val toUnwind   = windStack.take(windStack.length - common.length)
            val toRewind   = targetWind.take(targetWind.length - common.length).reverse
            val actions: List[WindAction] =
              toUnwind.map(e => DoUnwind(e._2)) ++ toRewind.map(e => DoRewind(e._1, e))
            processWindActions(actions, v, targetK)
          case _ => throw new EvalError("invalid continuation")

      case _ => throw new EvalError(s"not a procedure: ${op.display}")

  private[ming] def evalBodyCEK(exprs: List[Expr], env: Env, k: Kont): MState =
    if exprs.isEmpty then SApply(SchemeVoid, k)
    else if exprs.size == 1 then SEval(exprs.head, env, k)
    else SEval(exprs.head, env, SeqK(exprs.tail, env, k))

  private def processWindActions(
    actions: List[WindAction],
    savedVal: SchemeVal,
    targetK: Kont
  ): MState =
    actions match
      case Nil => SApply(savedVal, targetK)
      case DoUnwind(outThunk) :: rest =>
        windStack = windStack.tail
        applyFunction(outThunk, Nil, WindContK(rest, savedVal, targetK))
      case DoRewind(inThunk, entry) :: rest =>
        applyFunction(inThunk, Nil, WindPushK(entry, rest, savedVal, targetK))
