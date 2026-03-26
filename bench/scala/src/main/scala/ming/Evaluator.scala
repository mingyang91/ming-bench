package ming

import SchemeTypes.{builtinNames, display, errAt, isTruthy, Env, Pos, Value}

object Evaluator:

  // ── Position helpers ────────────────────────────────────────────────
  private def posOf(expr: Expr): Pos = expr match
    case Expr.Num(_, p)    => p
    case Expr.Flt(_, p)    => p
    case Expr.Rat(_, _, p) => p
    case Expr.Bool(_, p)   => p
    case Expr.Str(_, p)    => p
    case Expr.Chr(_, p)    => p
    case Expr.Symbol(_, p) => p
    case Expr.SList(_, p)  => p

  // ── Environment ──────────────────────────────────────────────────────
  private def defaultEnv(
    output: StringBuilder = new StringBuilder
  ): Env =
    val env =
      Env(scala.collection.mutable.Map.empty, None, output)
    for name <- builtinNames do env.define(name, Value.VBuiltin(name))
    env

  // ── Eval (trampoline for TCO) ─────────────────────────────────────
  private def eval(expr0: Expr, env0: Env): Value =
    var curExpr: Expr = expr0
    var curEnv: Env   = env0

    while true do
      val tco: TcoResult = curExpr match
        case Expr.Num(n, _)       => return Value.VNum(n)
        case Expr.Flt(d, _)       => return Value.VFloat(d)
        case Expr.Rat(n, d, _)    => return Value.VRational(n, d)
        case Expr.Bool(b, _)      => return Value.VBool(b)
        case Expr.Str(s, _)       => return Value.VStr(s.toCharArray, mutable = false)
        case Expr.Chr(c, _)       => return Value.VChar(c)
        case Expr.Symbol(name, p) => return curEnv.lookup(name, p)
        case Expr.SList(Nil, p)   => throw errAt(p, "empty application")
        case Expr.SList(Expr.Symbol("quote", _) :: arg :: Nil, _) =>
          return EvalForms.quoteToValue(arg)
        case Expr.SList(Expr.Symbol("define", _) :: rest, p) =>
          return evalDefine(rest, curEnv, p)
        case Expr.SList(Expr.Symbol("if", _) :: rest, p) =>
          EvalTail.evalIf(rest, curEnv, p, eval)
        case Expr.SList(Expr.Symbol("lambda", _) :: rest, p) =>
          return evalLambda(rest, curEnv, p)
        case Expr.SList(Expr.Symbol("and", _) :: args, _) =>
          EvalTail.evalAnd(args, curEnv, eval)
        case Expr.SList(Expr.Symbol("or", _) :: args, _) =>
          EvalTail.evalOr(args, curEnv, eval)
        case Expr.SList(Expr.Symbol("let", _) :: rest, p) =>
          EvalTail.evalLet(rest, curEnv, p, eval)
        case Expr.SList(Expr.Symbol("begin", _) :: body, _) =>
          EvalTail.evalBegin(body, curEnv, eval)
        case Expr.SList(Expr.Symbol("cond", _) :: clauses, _) =>
          EvalTail.evalCond(clauses, curEnv, eval, posOf)
        case Expr.SList(Expr.Symbol("set!", _) :: rest, p) =>
          return evalSet(rest, curEnv, p)
        case Expr.SList(Expr.Symbol("define-syntax", _) :: rest, p) =>
          return EvalForms.evalDefineSyntax(rest, curEnv, p)
        case Expr.SList(Expr.Symbol("case-lambda", _) :: clauses, p) =>
          return EvalForms.evalCaseLambda(clauses, curEnv, p)
        case Expr.SList(Expr.Symbol("define-record-type", _) :: rest, p) =>
          return EvalForms.evalDefineRecordType(rest, curEnv, p)
        case Expr.SList(Expr.Symbol("letrec", _) :: rest, p) =>
          return EvalCompound.evalLetrec(rest, curEnv, p, eval, evalBody)
        case Expr.SList(Expr.Symbol("letrec*", _) :: rest, p) =>
          return EvalCompound.evalLetrecStar(rest, curEnv, p, eval, evalBody)
        case Expr.SList(Expr.Symbol("case", _) :: rest, p) =>
          return EvalCompound.evalCase(rest, curEnv, p, eval, evalBody, posOf)
        case Expr.SList(Expr.Symbol("do", _) :: rest, p) =>
          return EvalCompound.evalDo(rest, curEnv, p, eval, evalBody, posOf)
        case Expr.SList(head :: args, p) =>
          EvalTail.evalApp(head, args, curEnv, p, eval)
      tco match
        case TcoResult.Done(v)        => return v
        case TcoResult.Bounce(e, env) => curExpr = e; curEnv = env
    throw EvalError("unreachable")

  private def evalBody(body: List[Expr], env: Env): Value =
    body.foldLeft(Value.VVoid: Value)((_, e) => eval(e, env))

  private[ming] def applyFunc(
    func: Value,
    args: List[Value],
    pos: Pos,
    env: Env
  ): Value = func match
    case Value.VBuiltin(name) => Builtins(name, args, pos, env)
    case Value.VLambda(params, restParam, body, closure) =>
      val callEnv = closure.child()
      EvalTail.bindArgs(params, restParam, args, callEnv, pos)
      evalBody(body, callEnv)
    case Value.VCaseLambda(clauses) =>
      val matched = clauses.find { case (params, restParam, _, _) =>
        restParam match
          case None    => args.length == params.length
          case Some(_) => args.length >= params.length
      }
      matched match
        case Some((params, restParam, body, closure)) =>
          val callEnv = closure.child()
          EvalTail.bindArgs(params, restParam, args, callEnv, pos)
          evalBody(body, callEnv)
        case None => throw errAt(pos, "wrong number of arguments")
    case _ => throw errAt(pos, "not a procedure")

  private def evalDefine(
    rest: List[Expr],
    env: Env,
    pos: Pos
  ): Value = rest match
    case Expr.Symbol(name, _) :: valueExpr :: Nil =>
      env.define(name, eval(valueExpr, env))
      Value.VVoid
    case Expr.SList(Expr.Symbol(name, _) :: params, _) :: body =>
      val (paramNames, restParam) = EvalForms.parseParams(params, pos)
      env.define(
        name,
        Value.VLambda(paramNames, restParam, body, env)
      )
      Value.VVoid
    case _ => throw errAt(pos, "invalid define")

  private def evalLambda(
    rest: List[Expr],
    env: Env,
    pos: Pos
  ): Value = rest match
    case Expr.SList(params, _) :: body =>
      val (paramNames, restParam) = EvalForms.parseParams(params, pos)
      Value.VLambda(paramNames, restParam, body, env)
    case Expr.Symbol(name, _) :: body =>
      Value.VLambda(Nil, Some(name), body, env)
    case _ => throw errAt(pos, "invalid lambda")

  private def evalSet(
    rest: List[Expr],
    env: Env,
    pos: Pos
  ): Value = rest match
    case Expr.Symbol(name, p) :: valueExpr :: Nil =>
      env.set(name, eval(valueExpr, env), p)
      Value.VVoid
    case _ => throw errAt(pos, "invalid set!")

  // ── Public API ───────────────────────────────────────────────────────
  def evalStr(input: String): String =
    val exprs = Parser.parseAll(input)
    if exprs.isEmpty then throw EvalError("no expressions")
    val env = defaultEnv()
    display(evalBody(exprs, env))

  def evalStrWithOutput(input: String): (String, String) =
    val exprs = Parser.parseAll(input)
    if exprs.isEmpty then throw EvalError("no expressions")
    val output = new StringBuilder
    val env    = defaultEnv(output)
    val result = display(evalBody(exprs, env))
    (result, output.toString)
