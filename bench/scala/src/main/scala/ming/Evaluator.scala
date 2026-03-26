package ming

import scala.collection.mutable
import scala.util.boundary
import scala.util.boundary.break

object Evaluator:

  // ── Tail-call trampoline signal ──────────────────────────────────────
  private class TailCallSignal(val expr: Expr, val env: Env) extends Exception(null, null, true, false)

  private def throwTailCall(expr: Expr, env: Env): Nothing =
    throw new TailCallSignal(expr, env)

  // ── Public API ───────────────────────────────────────────────────────

  def evalStr(input: String): String =
    val tokens = Tokenizer.tokenize(input)
    val exprs  = Parser.parseAll(tokens)
    if exprs.isEmpty then throw new EvalError("empty input")
    val env               = makeGlobalEnv()
    var result: SchemeVal = SchemeVoid
    for expr <- exprs do result = eval(expr, env)
    result.display

  def evalStrWithOutput(input: String): (String, String) =
    val tokens = Tokenizer.tokenize(input)
    val exprs  = Parser.parseAll(tokens)
    if exprs.isEmpty then throw new EvalError("empty input")
    val output            = new StringBuilder
    val env               = makeGlobalEnv(output)
    var result: SchemeVal = SchemeVoid
    for expr <- exprs do result = eval(expr, env)
    (result.display, output.toString)

  private def makeGlobalEnv(output: StringBuilder = new StringBuilder): Env =
    val env = new Env(mutable.Map.empty, None)
    Builtins.install(env, output)
    env

  private[ming] def isFalsy(v: SchemeVal): Boolean = v match
    case SchemeBool(false) => true
    case _                 => false

  // ── Core eval with trampoline ────────────────────────────────────────

  private[ming] def eval(expr: Expr, env: Env): SchemeVal =
    var curExpr = expr
    var curEnv  = env
    while true do
      try
        val result = curExpr match
          case IntLit(v, _)         => SchemeInt(v)
          case FloatLit(v, _)       => SchemeFloat(v)
          case RationalLit(n, d, _) => SchemeRational(n, d)
          case BoolLit(v, _)        => SchemeBool(v)
          case StringLit(v, _)      => SchemeString(v)
          case CharLit(v, _)        => SchemeChar(v)
          case Symbol(name, _)      => curEnv.get(name)
          case SList(Nil, _)        => throw new EvalError("empty application")
          case SList(elems, _)      => evalApplication(elems, curEnv)
        return result
      catch
        case tc: TailCallSignal =>
          curExpr = tc.expr
          curEnv = tc.env
        case e: EvalError =>
          val msg = e.getMessage
          if msg.matches(".*\\d+:\\d+.*") then throw e
          else throw new EvalError(s"${curExpr.pos}: $msg")
    throw new AssertionError("unreachable")

  // ── Application dispatch ─────────────────────────────────────────────

  private def evalApplication(elems: List[Expr], env: Env): SchemeVal =
    elems.head match
      case Symbol("and", _)                => evalAnd(elems.tail, env)
      case Symbol("or", _)                 => evalOr(elems.tail, env)
      case Symbol("define", _)             => evalDefine(elems.tail, env)
      case Symbol("if", _)                 => evalIf(elems.tail, env)
      case Symbol("quote", _)              => evalQuote(elems.tail)
      case Symbol("lambda", _)             => evalLambda(elems.tail, env)
      case Symbol("begin", _)              => evalBegin(elems.tail, env)
      case Symbol("let", _)                => BindingForms.evalLet(elems.tail, env)
      case Symbol("cond", _)               => BindingForms.evalCond(elems.tail, env)
      case Symbol("set!", _)               => evalSet(elems.tail, env)
      case Symbol("define-syntax", _)      => evalDefineSyntax(elems.tail, env)
      case Symbol("define-record-type", _) => Records.evalDefineRecordType(elems.tail, env)
      case Symbol("case-lambda", _)        => evalCaseLambda(elems.tail, env)
      case Symbol("letrec", _)             => BindingForms.evalLetrec(elems.tail, env)
      case Symbol("letrec*", _)            => BindingForms.evalLetrecStar(elems.tail, env)
      case Symbol("case", _)               => BindingForms.evalCase(elems.tail, env)
      case Symbol("do", _)                 => BindingForms.evalDo(elems.tail, env)
      case Symbol("let*", _)               => BindingForms.evalLetStar(elems.tail, env)
      case Symbol("when", _)               => BindingForms.evalWhen(elems.tail, env)
      case Symbol("unless", _)             => BindingForms.evalUnless(elems.tail, env)
      case _ =>
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
            throwTailCall(expanded, env)
          case None =>
            val op   = eval(elems.head, env)
            val args = elems.tail.map(e => eval(e, env))
            applyProc(op, args)

  // ── Special forms with TCO ───────────────────────────────────────────

  private def evalAnd(exprs: List[Expr], env: Env): SchemeVal =
    if exprs.isEmpty then return SchemeBool(true)
    for expr <- exprs.init do
      val v = eval(expr, env)
      if isFalsy(v) then return v
    throwTailCall(exprs.last, env)

  private def evalOr(exprs: List[Expr], env: Env): SchemeVal =
    if exprs.isEmpty then return SchemeBool(false)
    for expr <- exprs.init do
      val v = eval(expr, env)
      if !isFalsy(v) then return v
    throwTailCall(exprs.last, env)

  private[ming] def applyProc(op: SchemeVal, args: List[SchemeVal]): SchemeVal =
    op match
      case SchemeBuiltin(_, fn) => fn(args)
      case SchemeLambda(params, restParam, body, closureEnv) =>
        val localEnv = restParam match
          case None =>
            if params.size != args.size then throw new EvalError(s"expected ${params.size} arguments, got ${args.size}")
            new Env(mutable.Map.from(params.zip(args)), Some(closureEnv))
          case Some(rest) =>
            if args.size < params.size then
              throw new EvalError(s"expected at least ${params.size} arguments, got ${args.size}")
            val (required, extra) = args.splitAt(params.size)
            val bindings          = mutable.Map.from(params.zip(required))
            bindings(rest) = SchemeListOps.makeList(extra)
            new Env(bindings, Some(closureEnv))
        for expr <- body.init do eval(expr, localEnv)
        throwTailCall(body.last, localEnv)
      case SchemeCaseLambda(clauses) =>
        val matching = clauses.find { lam =>
          lam.restParam match
            case None    => args.size == lam.params.size
            case Some(_) => args.size >= lam.params.size
        }
        matching match
          case Some(lam) => applyProc(lam, args)
          case None      => throw new EvalError(s"no matching clause for ${args.size} arguments")
      case _ => throw new EvalError(s"not a procedure: ${op.display}")

  /** Like applyProc but always returns a resolved value (catches tail calls). */
  private[ming] def applyProcSafe(op: SchemeVal, args: List[SchemeVal]): SchemeVal =
    try applyProc(op, args)
    catch
      case tc: TailCallSignal =>
        eval(tc.expr, tc.env)

  private def evalDefine(args: List[Expr], env: Env): SchemeVal =
    args match
      case SList(Symbol(name, _) :: params, _) :: body =>
        val (paramNames, rest) = parseParams(params)
        env.set(name, SchemeLambda(paramNames, rest, body, env))
        SchemeVoid
      case Symbol(name, _) :: expr :: Nil =>
        env.set(name, eval(expr, env))
        SchemeVoid
      case _ => throw new EvalError("define: bad syntax")

  private def evalIf(args: List[Expr], env: Env): SchemeVal =
    args match
      case cond :: thenExpr :: elseExpr :: Nil =>
        if !isFalsy(eval(cond, env)) then throwTailCall(thenExpr, env)
        else throwTailCall(elseExpr, env)
      case cond :: thenExpr :: Nil =>
        if !isFalsy(eval(cond, env)) then throwTailCall(thenExpr, env)
        else SchemeVoid
      case _ => throw new EvalError("if: bad syntax")

  private def evalQuote(args: List[Expr]): SchemeVal =
    if args.size != 1 then throw new EvalError("quote: expected 1 argument")
    exprToVal(args.head)

  private[ming] def exprToVal(expr: Expr): SchemeVal =
    expr match
      case IntLit(v, _)         => SchemeInt(v)
      case FloatLit(v, _)       => SchemeFloat(v)
      case RationalLit(n, d, _) => SchemeRational(n, d)
      case BoolLit(v, _)        => SchemeBool(v)
      case StringLit(v, _)      => SchemeString(v)
      case CharLit(v, _)        => SchemeChar(v)
      case Symbol(name, _)      => SchemeSymbol(name)
      case SList(elems, _)      => SchemeListOps.makeList(elems.map(exprToVal))

  private def parseParams(paramExprs: List[Expr]): (List[String], Option[String]) =
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

  private def evalLambda(args: List[Expr], env: Env): SchemeVal =
    args match
      case Symbol(name, _) :: body if body.nonEmpty =>
        SchemeLambda(Nil, Some(name), body, env)
      case SList(paramExprs, _) :: body if body.nonEmpty =>
        val (params, rest) = parseParams(paramExprs)
        SchemeLambda(params, rest, body, env)
      case _ => throw new EvalError("lambda: bad syntax")

  private def evalBegin(exprs: List[Expr], env: Env): SchemeVal =
    if exprs.isEmpty then SchemeVoid
    else
      for expr <- exprs.init do eval(expr, env)
      throwTailCall(exprs.last, env)

  private def evalSet(args: List[Expr], env: Env): SchemeVal =
    args match
      case Symbol(name, _) :: valueExpr :: Nil =>
        env.update(name, eval(valueExpr, env))
        SchemeVoid
      case _ => throw new EvalError("set!: bad syntax")

  private def evalDefineSyntax(args: List[Expr], env: Env): SchemeVal =
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
        SchemeVoid
      case _ => throw new EvalError("define-syntax: bad syntax")

  private def evalCaseLambda(clauses: List[Expr], env: Env): SchemeVal =
    val lambdas = clauses.map {
      case SList(SList(paramExprs, _) :: body, _) if body.nonEmpty =>
        val (params, rest) = parseParams(paramExprs)
        SchemeLambda(params, rest, body, env)
      case _ => throw new EvalError("case-lambda: bad clause")
    }
    SchemeCaseLambda(lambdas)

  private[ming] def evalBody(exprs: List[Expr], env: Env): SchemeVal =
    if exprs.isEmpty then SchemeVoid
    else
      for expr <- exprs.init do eval(expr, env)
      throwTailCall(exprs.last, env)
