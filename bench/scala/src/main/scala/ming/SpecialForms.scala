package ming

import scala.collection.mutable

private[ming] object SpecialForms:

  def evalIfForm(args: List[Expr], env: Env, k: Kont): MState =
    args match
      case cond :: thenE :: elseE :: Nil =>
        SEval(cond, env, IfK(thenE, Some(elseE), env, k))
      case cond :: thenE :: Nil =>
        SEval(cond, env, IfK(thenE, None, env, k))
      case _ => throw new EvalError("if: bad syntax")

  def evalDefineForm(args: List[Expr], env: Env, k: Kont): MState =
    args match
      case SList(Symbol(name, _) :: paramExprs, _) :: body =>
        val (params, rest) = parseParams(paramExprs)
        env.set(name, SchemeLambda(params, rest, body, env))
        SApply(SchemeVoid, k)
      case Symbol(name, _) :: valueExpr :: Nil =>
        SEval(valueExpr, env, DefineK(name, env, k))
      case _ => throw new EvalError("define: bad syntax")

  def evalSetForm(args: List[Expr], env: Env, k: Kont): MState =
    args match
      case Symbol(name, _) :: valueExpr :: Nil =>
        SEval(valueExpr, env, SetBangK(name, env, k))
      case _ => throw new EvalError("set!: bad syntax")

  def evalBeginForm(exprs: List[Expr], env: Env, k: Kont): MState =
    if exprs.isEmpty then SApply(SchemeVoid, k)
    else Evaluator.evalBodyCEK(exprs, env, k)

  def evalAndForm(exprs: List[Expr], env: Env, k: Kont): MState =
    if exprs.isEmpty then SApply(SchemeBool(true), k)
    else if exprs.size == 1 then SEval(exprs.head, env, k)
    else SEval(exprs.head, env, AndK(exprs.tail, env, k))

  def evalOrForm(exprs: List[Expr], env: Env, k: Kont): MState =
    if exprs.isEmpty then SApply(SchemeBool(false), k)
    else if exprs.size == 1 then SEval(exprs.head, env, k)
    else SEval(exprs.head, env, OrK(exprs.tail, env, k))

  def evalCallCCForm(args: List[Expr], env: Env, k: Kont): MState =
    if args.size != 1 then throw new EvalError("call/cc: expected 1 argument")
    SEval(args.head, env, CallCCK(k))

  def evalCondForm(clauses: List[Expr], env: Env, k: Kont): MState =
    if clauses.isEmpty then SApply(SchemeVoid, k)
    else
      clauses.head match
        case SList(Symbol("else", _) :: body, _) =>
          Evaluator.evalBodyCEK(body, env, k)
        case SList(test :: body, _) =>
          SEval(test, env, CondTestK(body, clauses.tail, env, k))
        case _ => throw new EvalError("cond: bad clause")

  def evalCaseForm(args: List[Expr], env: Env, k: Kont): MState =
    if args.isEmpty then throw new EvalError("case: bad syntax")
    SEval(args.head, env, CaseK(args.tail, env, k))

  def matchCaseClauses(key: SchemeVal, clauses: List[Expr], env: Env, k: Kont): MState =
    if clauses.isEmpty then SApply(SchemeVoid, k)
    else
      clauses.head match
        case SList(Symbol("else", _) :: body, _) =>
          Evaluator.evalBodyCEK(body, env, k)
        case SList(SList(datums, _) :: body, _) =>
          val matched = datums.exists(d => ListBuiltins.schemeEqv(key, EvalHelpers.exprToVal(d)))
          if matched then
            if body.isEmpty then SApply(SchemeVoid, k)
            else Evaluator.evalBodyCEK(body, env, k)
          else matchCaseClauses(key, clauses.tail, env, k)
        case _ => throw new EvalError("case: bad clause")

  def evalLetForm(args: List[Expr], env: Env, k: Kont): MState =
    args match
      case Symbol(name, _) :: SList(bindings, _) :: body if body.nonEmpty =>
        val parsed   = parseLetBindings(bindings)
        val localEnv = new Env(mutable.Map.empty, Some(env))
        val lambda   = SchemeLambda(parsed.map(_._1), None, body, localEnv)
        localEnv.set(name, lambda)
        val inits = parsed.map(_._2)
        if inits.isEmpty then Evaluator.applyFunction(lambda, Nil, k)
        else
          val rev = inits.reverse
          SEval(rev.head, env, EvArgsK(lambda, Nil, rev.tail, env, k))
      case SList(bindings, _) :: body if body.nonEmpty =>
        val parsed     = parseLetBindings(bindings)
        val paramNames = parsed.map(_._1)
        val inits      = parsed.map(_._2)
        val lambda     = SchemeLambda(paramNames, None, body, env)
        if inits.isEmpty then Evaluator.applyFunction(lambda, Nil, k)
        else
          val rev = inits.reverse
          SEval(rev.head, env, EvArgsK(lambda, Nil, rev.tail, env, k))
      case _ => throw new EvalError("let: bad syntax")

  def evalLetStarForm(args: List[Expr], env: Env, k: Kont): MState =
    args match
      case SList(bindings, _) :: body if body.nonEmpty =>
        val localEnv = new Env(mutable.Map.empty, Some(env))
        val parsed   = parseLetBindings(bindings)
        if parsed.isEmpty then Evaluator.evalBodyCEK(body, localEnv, k)
        else
          SEval(
            parsed.head._2,
            localEnv,
            LetrecBindK(parsed.head._1, parsed.tail, localEnv, body, k)
          )
      case _ => throw new EvalError("let*: bad syntax")

  def evalLetrecForm(args: List[Expr], env: Env, k: Kont): MState =
    args match
      case SList(bindings, _) :: body if body.nonEmpty =>
        val localEnv = new Env(mutable.Map.empty, Some(env))
        val parsed   = parseLetBindings(bindings)
        for (n, _) <- parsed do localEnv.set(n, SchemeVoid)
        if parsed.isEmpty then Evaluator.evalBodyCEK(body, localEnv, k)
        else
          SEval(
            parsed.head._2,
            localEnv,
            LetrecBindK(parsed.head._1, parsed.tail, localEnv, body, k)
          )
      case _ => throw new EvalError("letrec: bad syntax")

  def evalLetrecStarForm(args: List[Expr], env: Env, k: Kont): MState =
    args match
      case SList(bindings, _) :: body if body.nonEmpty =>
        val localEnv = new Env(mutable.Map.empty, Some(env))
        val parsed   = parseLetBindings(bindings)
        if parsed.isEmpty then Evaluator.evalBodyCEK(body, localEnv, k)
        else
          SEval(
            parsed.head._2,
            localEnv,
            LetrecBindK(parsed.head._1, parsed.tail, localEnv, body, k)
          )
      case _ => throw new EvalError("letrec*: bad syntax")

  def evalDoForm(args: List[Expr], env: Env, k: Kont): MState =
    args match
      case SList(varSpecs, _) :: SList(testAndExprs, _) :: commands =>
        if testAndExprs.isEmpty then throw new EvalError("do: bad syntax")
        val test        = testAndExprs.head
        val resultExprs = testAndExprs.tail

        case class DoVar(name: String, init: Expr, step: Option[Expr])
        val vars = varSpecs.map {
          case SList(Symbol(n, _) :: init :: step :: Nil, _) => DoVar(n, init, Some(step))
          case SList(Symbol(n, _) :: init :: Nil, _)         => DoVar(n, init, None)
          case _                                             => throw new EvalError("do: bad variable spec")
        }

        val loopName   = "$do"
        val paramNames = vars.map(_.name)
        val inits      = vars.map(_.init)
        val steps      = vars.map(v => v.step.getOrElse(Symbol(v.name, Pos.zero)))

        val loopCall   = SList(Symbol(loopName, Pos.zero) :: steps, Pos.zero)
        val elseBranch = SList(Symbol("begin", Pos.zero) :: (commands :+ loopCall), Pos.zero)
        val thenBranch =
          if resultExprs.isEmpty then SList(List(Symbol("begin", Pos.zero)), Pos.zero)
          else SList(Symbol("begin", Pos.zero) :: resultExprs, Pos.zero)
        val ifExpr = SList(List(Symbol("if", Pos.zero), test, thenBranch, elseBranch), Pos.zero)

        val localEnv = new Env(mutable.Map.empty, Some(env))
        val lambda   = SchemeLambda(paramNames, None, List(ifExpr), localEnv)
        localEnv.set(loopName, lambda)

        if inits.isEmpty then Evaluator.applyFunction(lambda, Nil, k)
        else
          val rev = inits.reverse
          SEval(rev.head, env, EvArgsK(lambda, Nil, rev.tail, env, k))
      case _ => throw new EvalError("do: bad syntax")

  def evalWhenForm(args: List[Expr], env: Env, k: Kont): MState =
    if args.size < 2 then throw new EvalError("when: bad syntax")
    SEval(args.head, env, TestBodyK(args.tail, false, env, k))

  def evalUnlessForm(args: List[Expr], env: Env, k: Kont): MState =
    if args.size < 2 then throw new EvalError("unless: bad syntax")
    SEval(args.head, env, TestBodyK(args.tail, true, env, k))

  def makeLambda(args: List[Expr], env: Env): SchemeVal =
    args match
      case Symbol(name, _) :: body if body.nonEmpty =>
        SchemeLambda(Nil, Some(name), body, env)
      case SList(paramExprs, _) :: body if body.nonEmpty =>
        val (params, rest) = parseParams(paramExprs)
        SchemeLambda(params, rest, body, env)
      case _ => throw new EvalError("lambda: bad syntax")

  def makeCaseLambda(clauses: List[Expr], env: Env): SchemeVal =
    val lambdas = clauses.map {
      case SList(SList(paramExprs, _) :: body, _) if body.nonEmpty =>
        val (params, rest) = parseParams(paramExprs)
        SchemeLambda(params, rest, body, env)
      case _ => throw new EvalError("case-lambda: bad clause")
    }
    SchemeCaseLambda(lambdas)

  def evalQuote(args: List[Expr]): SchemeVal =
    if args.size != 1 then throw new EvalError("quote: expected 1 argument")
    EvalHelpers.exprToVal(args.head)

  def evalDefineSyntax(args: List[Expr], env: Env): Unit =
    args match
      case Symbol(name, _) :: SList(
            Symbol("syntax-rules", _) :: SList(literals, _) :: rules,
            _
          ) :: Nil =>
        val litNames = literals.map {
          case Symbol(n, _) => n
          case _            => throw new EvalError("syntax-rules: expected literal name")
        }
        val parsedRules = rules.map {
          case SList(pattern :: template :: Nil, _) => (pattern, template)
          case _                                    => throw new EvalError("syntax-rules: bad rule")
        }
        env.set(name, SchemeMacro(litNames, parsedRules, env))
      case _ => throw new EvalError("define-syntax: bad syntax")

  // ── Parsing Helpers ──────────────────────────────────────────────

  private def parseLetBindings(bindings: List[Expr]): List[(String, Expr)] =
    bindings.map {
      case SList(Symbol(n, _) :: init :: Nil, _) => (n, init)
      case _                                     => throw new EvalError("bad binding")
    }

  def parseParams(paramExprs: List[Expr]): (List[String], Option[String]) =
    val dotIdx = paramExprs.indexWhere { case Symbol(".", _) => true; case _ => false }
    if dotIdx < 0 then
      val params = paramExprs.map {
        case Symbol(n, _) => n
        case _            => throw new EvalError("expected parameter name")
      }
      (params, None)
    else
      if dotIdx + 1 >= paramExprs.size then throw new EvalError("bad dot syntax")
      val before = paramExprs.take(dotIdx).map {
        case Symbol(n, _) => n
        case _            => throw new EvalError("expected parameter name")
      }
      val rest = paramExprs(dotIdx + 1) match
        case Symbol(n, _) => n
        case _            => throw new EvalError("expected parameter name after dot")
      (before, Some(rest))
