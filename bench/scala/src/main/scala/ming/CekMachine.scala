package ming

object CekMachine:

  import Builtins.isFalsy
  import CekApply.*

  def eval(expr: Expr, env: Env): Expr = run(expr, env, HaltK)

  def evalBody(exprs: List[Expr], env: Env): Expr =
    if exprs.isEmpty then Expr.Bool(false)
    else if exprs.length == 1 then run(exprs.head, env, HaltK)
    else run(exprs.head, env, SeqK(exprs.tail, env, HaltK))

  def evalBodyWithLimit(exprs: List[Expr], env: Env, maxSteps: Int): Expr =
    if exprs.isEmpty then Expr.Bool(false)
    else if exprs.length == 1 then runWithLimit(exprs.head, env, HaltK, maxSteps)
    else runWithLimit(exprs.head, env, SeqK(exprs.tail, env, HaltK), maxSteps)

  private def stepEval(s: CekState): Unit =
    val curExpr = s.expr
    val curEnv  = s.env
    curExpr match
      case _: Expr.Num | _: Expr.Rational | _: Expr.Real | _: Expr.Bool | _: Expr.Str | _: Expr.Chr | _: Expr.Lambda |
          _: Expr.Pair | _: Expr.Macro | _: Expr.Record | _: Expr.CaseLambda | _: Expr.Vec | _: Expr.Cont |
          _: Expr.Values | _: Expr.TransformerMacro | _: Expr.SyntaxExpanded =>
        s.value = curExpr
        s.evaluating = false

      case Expr.Sym(name) =>
        s.value = curEnv.lookup(name)
        s.evaluating = false

      case Expr.Lst(Nil) => throw EvalError("empty application")

      case Expr.Lst(Expr.Sym("quote") :: args) =>
        if args.length != 1 then throw EvalError("quote: need exactly 1 argument")
        s.value = args.head
        s.evaluating = false

      case Expr.Lst(List(Expr.Sym("quasiquote"), tmpl)) =>
        s.expr = Quasiquote.expand(tmpl)
        // re-evaluate the expanded expression

      case Expr.Lst(Expr.Sym("if") :: args) =>
        if args.length < 2 || args.length > 3 then throw EvalError("if: need 2 or 3 arguments")
        s.k = IfK(args(1), if args.length == 3 then Some(args(2)) else None, curEnv, s.k)
        s.expr = args.head

      case Expr.Lst(Expr.Sym("define") :: rest) =>
        stepDefine(s, rest, curEnv)

      case Expr.Lst(Expr.Sym("set!") :: rest) =>
        rest match
          case Expr.Sym(name) :: valExpr :: Nil =>
            s.k = SetK(name, curEnv, s.k)
            s.expr = valExpr
          case _ => throw EvalError("set!: invalid syntax")

      case Expr.Lst(Expr.Sym("lambda") :: rest) =>
        s.value = SpecialForms.evalLambda(rest, curEnv)
        s.evaluating = false

      case Expr.Lst(Expr.Sym("begin") :: rest) =>
        if rest.isEmpty then throw EvalError("begin: need at least 1 expression")
        setupBody(s, rest, curEnv, s.k)

      case Expr.Lst(Expr.Sym("let") :: rest)     => stepLet(s, rest, curEnv)
      case Expr.Lst(Expr.Sym("let*") :: rest)    => stepLetStar(s, rest, curEnv, "let*")
      case Expr.Lst(Expr.Sym("letrec") :: rest)  => stepLetrec(s, rest, curEnv)
      case Expr.Lst(Expr.Sym("letrec*") :: rest) => stepLetStar(s, rest, curEnv, "letrec*")
      case Expr.Lst(Expr.Sym("cond") :: clauses) => evalCondClauses(s, clauses, curEnv, s.k)
      case Expr.Lst(Expr.Sym("and") :: args)     => CekHelpers.stepLogical(s, args, curEnv, isAnd = true)
      case Expr.Lst(Expr.Sym("or") :: args)      => CekHelpers.stepLogical(s, args, curEnv, isAnd = false)

      case Expr.Lst(Expr.Sym("case") :: rest) =>
        if rest.isEmpty then throw EvalError("case: need key expression")
        s.k = CaseKeyK(rest.tail, curEnv, s.k)
        s.expr = rest.head

      case Expr.Lst(Expr.Sym("define-syntax") :: rest) =>
        s.value = SpecialForms.evalDefineSyntax(rest, curEnv)
        s.evaluating = false

      case Expr.Lst(Expr.Sym("syntax-case") :: stxExpr :: Expr.Lst(literals) :: clauses) =>
        CekSyntaxSteps.stepEvalSyntaxCase(s, stxExpr, literals, clauses)

      case Expr.Lst(Expr.Sym("syntax") :: template :: Nil) =>
        CekSyntaxSteps.stepEvalSyntax(s, template, curEnv)

      case Expr.Lst(Expr.Sym("with-syntax") :: Expr.Lst(bindings) :: body) if body.nonEmpty =>
        CekSyntaxSteps.stepEvalWithSyntax(s, bindings, body, curEnv)

      case Expr.Lst(Expr.Sym("define-record-type") :: rest) =>
        s.value = RecordOps.evalDefineRecordType(rest, curEnv)
        s.evaluating = false

      case Expr.Lst(Expr.Sym("case-lambda") :: clauses) =>
        s.value = SpecialForms.evalCaseLambda(clauses, curEnv)
        s.evaluating = false

      case Expr.Lst(Expr.Sym("do") :: rest) =>
        s.value = SpecialForms.evalDo(rest, curEnv)
        s.evaluating = false

      case Expr.Lst(Expr.Sym("guard") :: rest) =>
        CekHelpers.stepGuard(s, rest, curEnv)

      case lst @ Expr.Lst(Expr.Sym(name) :: _) if SpecialForms.isMacro(name, curEnv) =>
        CekSyntaxSteps.stepEvalMacro(s, name, lst, curEnv)

      case Expr.Lst(op :: args) =>
        s.appPosExpr = curExpr
        s.k = EvFunK(args, curEnv, s.k)
        s.expr = op

  private def stepKont(s: CekState): Expr =
    s.k match
      case HaltK => return s.value

      case IfK(thenE, elseE, e, kk) =>
        s.evaluating = true
        s.env = e
        s.k = kk
        if !isFalsy(s.value) then s.expr = thenE
        else
          elseE match
            case Some(el) => s.expr = el
            case None =>
              s.value = Expr.Bool(false)
              s.evaluating = false

      case SeqK(remaining, e, kk) =>
        s.evaluating = true
        s.env = e
        if remaining.length == 1 then
          s.expr = remaining.head
          s.k = kk
        else
          s.expr = remaining.head
          s.k = SeqK(remaining.tail, e, kk)

      case DefineK(name, e, kk) =>
        e.define(name, s.value)
        s.value = Expr.Bool(false)
        s.k = kk

      case SetK(name, e, kk) =>
        e.set(name, s.value)
        s.value = Expr.Bool(false)
        s.k = kk

      case k: EvFunK  => stepEvFunK(s, k)
      case k: EvArgsK => stepEvArgsK(s, k)

      case _: BindK | _: NamedLetBindK =>
        CekHelpers.stepBindKont(s, s.k)

      case AndK(remaining, e, kk) =>
        if isFalsy(s.value) then s.k = kk
        else CekHelpers.stepLogicalKont(s, remaining, e, kk, isAnd = true)

      case OrK(remaining, e, kk) =>
        if !isFalsy(s.value) then s.k = kk
        else CekHelpers.stepLogicalKont(s, remaining, e, kk, isAnd = false)

      case CondK(body, remaining, e, kk) =>
        if !isFalsy(s.value) then
          if body.isEmpty then s.k = kk
          else setupBody(s, body, e, kk)
        else evalCondClauses(s, remaining, e, kk)

      case k: CondArrowK                 => stepCondArrowK(s, k)
      case CondArrowApplyK(testVal, kk)  => applyFunc(s, s.value, List(testVal), kk)
      case CaseKeyK(clauses, e, kk)      => evalCaseClauses(s, s.value, clauses, e, kk)
      case k: DynWindAfterInK            => CekHelpers.stepDynWind(s, k)
      case k: DynWindAfterBodyK          => CekHelpers.stepDynWind(s, k)
      case k: DynWindAfterOutK           => CekHelpers.stepDynWind(s, k)
      case k: DynWindTransferK           => CekHelpers.stepDynWind(s, k)
      case PopExnHandlerK(kk)            => s.exnHandlers = s.exnHandlers.tail; s.k = kk
      case RaiseReturnK                  => throw EvalError("raise: handler returned")
      case CallExnHandlerK(h, v, afterK) => applyFunc(s, h, List(v), afterK)
      case GuardStartK(vn, cls, env, ek) => evalGuardClauses(s, vn, cls, env, ek)
      case k: GuardCondK                 => stepGuardCondK(s, k)
      case k: GuardCondArrowK            => stepGuardCondArrowK(s, k)
      case k: CallWithValuesConsumerK    => stepCallWithValuesK(s, k)
      case SyntaxCaseMatchK(lits, cls, env, kk) =>
        CekSyntaxSteps.stepKontSyntaxCaseMatch(s, lits, cls, env, kk)
      case SyntaxCaseCleanupK(kk) => s.k = kk
      case TransformerMacroReturnK(useEnv, kk) =>
        CekSyntaxSteps.stepKontTransformerMacroReturn(s, useEnv, kk)

    null // signal: keep looping

  private def stepCondArrowK(s: CekState, k: CondArrowK): Unit =
    if !isFalsy(s.value) then
      val testVal = s.value
      s.k = CondArrowApplyK(testVal, k.k)
      s.expr = k.procExpr
      s.env = k.env
      s.evaluating = true
    else evalCondClauses(s, k.remaining, k.env, k.k)

  private def stepGuardCondK(s: CekState, k: GuardCondK): Unit =
    if !isFalsy(s.value) then
      if k.body.isEmpty then s.k = k.exitK
      else setupBody(s, k.body, k.env, k.exitK)
    else evalGuardClauses(s, k.varName, k.remaining, k.env, k.exitK)

  private def stepGuardCondArrowK(s: CekState, k: GuardCondArrowK): Unit =
    if !isFalsy(s.value) then
      val testVal = s.value
      s.k = CondArrowApplyK(testVal, k.exitK)
      s.expr = k.procExpr
      s.env = k.env
      s.evaluating = true
    else evalGuardClauses(s, k.varName, k.remaining, k.env, k.exitK)

  private def stepCallWithValuesK(s: CekState, k: CallWithValuesConsumerK): Unit =
    val args = s.value match
      case Expr.Values(elems) => elems
      case single             => List(single)
    applyFunc(s, k.consumer, args, k.k)

  private def stepEvFunK(s: CekState, k: EvFunK): Unit =
    if k.argExprs.isEmpty then applyFunc(s, s.value, Nil, k.k)
    else
      val revArgs = k.argExprs.reverse
      s.k = EvArgsK(s.value, Nil, revArgs.tail, k.env, k.k)
      s.expr = revArgs.head
      s.env = k.env
      s.evaluating = true

  private def stepEvArgsK(s: CekState, k: EvArgsK): Unit =
    val newEvaled = s.value :: k.evaledInOrder
    if k.remaining.isEmpty then applyFunc(s, k.func, newEvaled, k.k)
    else
      s.k = EvArgsK(k.func, newEvaled, k.remaining.tail, k.env, k.k)
      s.expr = k.remaining.head
      s.env = k.env
      s.evaluating = true

  private def run(expr0: Expr, env0: Env, k0: Kont): Expr =
    val s = new CekState
    s.expr = expr0
    s.env = env0
    s.k = k0
    s.evaluating = true
    var posExpr: Expr = null

    while true do
      try
        if s.evaluating then
          posExpr = s.expr
          stepEval(s)
        else
          val result = stepKont(s)
          if result != null then return result
      catch
        case e: EvalError if !e.getMessage.matches(".*\\d+:\\d+.*") =>
          val pe =
            if s.evaluating && posExpr != null && posExpr.line > 0 then posExpr
            else if s.appPosExpr != null && s.appPosExpr.line > 0 then s.appPosExpr
            else null
          if pe != null then throw EvalError(s"${pe.line}:${pe.col}: ${e.getMessage}")
          else throw e
    throw RuntimeException("unreachable")

  private def runWithLimit(expr0: Expr, env0: Env, k0: Kont, maxSteps: Int): Expr =
    val s = new CekState
    s.expr = expr0
    s.env = env0
    s.k = k0
    s.evaluating = true
    s.stepLimit = maxSteps
    var posExpr: Expr = null

    while true do
      try
        if s.evaluating then
          s.stepCount += 1
          if s.stepCount > s.stepLimit then throw EvalError("step limit exceeded")
          posExpr = s.expr
          stepEval(s)
        else
          val result = stepKont(s)
          if result != null then return result
      catch
        case e: EvalError if !e.getMessage.matches(".*\\d+:\\d+.*") =>
          val pe =
            if s.evaluating && posExpr != null && posExpr.line > 0 then posExpr
            else if s.appPosExpr != null && s.appPosExpr.line > 0 then s.appPosExpr
            else null
          if pe != null then throw EvalError(s"${pe.line}:${pe.col}: ${e.getMessage}")
          else throw e
    throw RuntimeException("unreachable")
