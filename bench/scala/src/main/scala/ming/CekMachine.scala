package ming

object CekMachine:

  import Builtins.isFalsy
  import CekApply.*

  def eval(expr: Expr, env: Env): Expr = run(expr, env, HaltK)

  def evalBody(exprs: List[Expr], env: Env): Expr =
    if exprs.isEmpty then Expr.Bool(false)
    else if exprs.length == 1 then run(exprs.head, env, HaltK)
    else run(exprs.head, env, SeqK(exprs.tail, env, HaltK))

  private def stepEval(s: CekState): Unit =
    val curExpr = s.expr
    val curEnv  = s.env
    curExpr match
      case _: Expr.Num | _: Expr.Rational | _: Expr.Real | _: Expr.Bool | _: Expr.Str | _: Expr.Chr | _: Expr.Lambda |
          _: Expr.Pair | _: Expr.Macro | _: Expr.Record | _: Expr.CaseLambda | _: Expr.Vec | _: Expr.Cont =>
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

      case Expr.Lst(Expr.Sym("let") :: rest) =>
        stepLet(s, rest, curEnv)

      case Expr.Lst(Expr.Sym("let*") :: rest) =>
        stepLetStar(s, rest, curEnv, "let*")

      case Expr.Lst(Expr.Sym("letrec") :: rest) =>
        stepLetrec(s, rest, curEnv)

      case Expr.Lst(Expr.Sym("letrec*") :: rest) =>
        stepLetStar(s, rest, curEnv, "letrec*")

      case Expr.Lst(Expr.Sym("cond") :: clauses) =>
        evalCondClauses(s, clauses, curEnv, s.k)

      case Expr.Lst(Expr.Sym("and") :: args) =>
        stepLogical(s, args, curEnv, isAnd = true)

      case Expr.Lst(Expr.Sym("or") :: args) =>
        stepLogical(s, args, curEnv, isAnd = false)

      case Expr.Lst(Expr.Sym("case") :: rest) =>
        if rest.isEmpty then throw EvalError("case: need key expression")
        s.k = CaseKeyK(rest.tail, curEnv, s.k)
        s.expr = rest.head

      case Expr.Lst(Expr.Sym("define-syntax") :: rest) =>
        s.value = SpecialForms.evalDefineSyntax(rest, curEnv)
        s.evaluating = false

      case Expr.Lst(Expr.Sym("define-record-type") :: rest) =>
        s.value = RecordOps.evalDefineRecordType(rest, curEnv)
        s.evaluating = false

      case Expr.Lst(Expr.Sym("case-lambda") :: clauses) =>
        s.value = SpecialForms.evalCaseLambda(clauses, curEnv)
        s.evaluating = false

      case Expr.Lst(Expr.Sym("do") :: rest) =>
        s.value = SpecialForms.evalDo(rest, curEnv)
        s.evaluating = false

      case lst @ Expr.Lst(Expr.Sym(name) :: _) if SpecialForms.isMacro(name, curEnv) =>
        val mac = SpecialForms.lookupMacro(name, curEnv)
        val (expanded, hygieneEnv) =
          Macros.expandMacro(mac.literals, mac.rules, lst.elems, mac.defEnv, curEnv)
        s.expr = expanded
        s.env = hygieneEnv

      case Expr.Lst(op :: args) =>
        s.appPosExpr = curExpr
        s.k = EvFunK(args, curEnv, s.k)
        s.expr = op

  private def stepLogical(s: CekState, args: List[Expr], curEnv: Env, isAnd: Boolean): Unit =
    if args.isEmpty then
      s.value = Expr.Bool(isAnd)
      s.evaluating = false
    else if args.length == 1 then s.expr = args.head
    else
      s.k = if isAnd then AndK(args.tail, curEnv, s.k) else OrK(args.tail, curEnv, s.k)
      s.expr = args.head

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

      case EvFunK(argExprs, e, kk) =>
        if argExprs.isEmpty then applyFunc(s, s.value, Nil, kk)
        else
          val revArgs = argExprs.reverse
          s.k = EvArgsK(s.value, Nil, revArgs.tail, e, kk)
          s.expr = revArgs.head
          s.env = e
          s.evaluating = true

      case EvArgsK(func, evaledInOrder, remaining, e, kk) =>
        val newEvaled = s.value :: evaledInOrder
        if remaining.isEmpty then applyFunc(s, func, newEvaled, kk)
        else
          s.k = EvArgsK(func, newEvaled, remaining.tail, e, kk)
          s.expr = remaining.head
          s.env = e
          s.evaluating = true

      case BindK(name, remaining, body, bindEnv, evalEnv, kk) =>
        bindEnv.define(name, s.value)
        if remaining.isEmpty then setupBody(s, body, bindEnv, kk)
        else
          s.k = BindK(remaining.head._1, remaining.tail, body, bindEnv, evalEnv, kk)
          s.expr = remaining.head._2
          s.env = evalEnv
          s.evaluating = true

      case NamedLetBindK(name, paramNames, evaledRev, remaining, body, evalEnv, kk) =>
        val newEvaled = s.value :: evaledRev
        if remaining.isEmpty then
          val initVals = newEvaled.reverse
          val letEnv   = evalEnv.child()
          letEnv.define(name, Expr.Lambda(paramNames, None, body, letEnv))
          val localEnv = Evaluator.bindLambdaParams(paramNames, None, initVals, letEnv)
          setupBody(s, body, localEnv, kk)
        else
          s.k = NamedLetBindK(name, paramNames, newEvaled, remaining.tail, body, evalEnv, kk)
          s.expr = remaining.head
          s.env = evalEnv
          s.evaluating = true

      case AndK(remaining, e, kk) =>
        if isFalsy(s.value) then s.k = kk
        else stepLogicalKont(s, remaining, e, kk, isAnd = true)

      case OrK(remaining, e, kk) =>
        if !isFalsy(s.value) then s.k = kk
        else stepLogicalKont(s, remaining, e, kk, isAnd = false)

      case CondK(body, remaining, e, kk) =>
        if !isFalsy(s.value) then
          if body.isEmpty then s.k = kk
          else setupBody(s, body, e, kk)
        else evalCondClauses(s, remaining, e, kk)

      case CaseKeyK(clauses, e, kk) =>
        evalCaseClauses(s, s.value, clauses, e, kk)

    null // signal: keep looping

  private def stepLogicalKont(
    s: CekState,
    remaining: List[Expr],
    e: Env,
    kk: Kont,
    isAnd: Boolean
  ): Unit =
    if remaining.length == 1 then
      s.expr = remaining.head
      s.env = e
      s.k = kk
      s.evaluating = true
    else
      s.k = if isAnd then AndK(remaining.tail, e, kk) else OrK(remaining.tail, e, kk)
      s.expr = remaining.head
      s.env = e
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
