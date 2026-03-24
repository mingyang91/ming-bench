package ming

import scala.collection.mutable

object Evaluator:

  val outputBuffer: ThreadLocal[StringBuilder] = ThreadLocal.withInitial(() => new StringBuilder)

  /** Parse a parameter list, handling dot notation for rest params */
  private def parseParams(params: List[Expr]): (List[String], Option[String]) =
    val dotIdx = params.indexWhere {
      case Expr.Symbol(".") => true
      case _                => false
    }
    if dotIdx >= 0 then
      if dotIdx != params.length - 2 then throw new EvalError("lambda: invalid dot notation in parameters")
      val fixed = params.take(dotIdx).map {
        case Expr.Symbol(n) => n
        case _              => throw new EvalError("lambda: expected parameter name")
      }
      val rest = params(dotIdx + 1) match
        case Expr.Symbol(n) => n
        case _              => throw new EvalError("lambda: expected parameter name after dot")
      (fixed, Some(rest))
    else
      val names = params.map {
        case Expr.Symbol(n) => n
        case _              => throw new EvalError("lambda: expected parameter name")
      }
      (names, None)

  private def isTruthy(v: SchemeVal): Boolean = v match
    case SchemeVal.BoolVal(false) => false
    case _                        => true

  private def quoteToVal(expr: Expr): SchemeVal = expr match
    case Expr.IntLit(n)  => SchemeVal.IntVal(n)
    case Expr.BoolLit(b) => SchemeVal.BoolVal(b)
    case Expr.StrLit(s)  => SchemeVal.StrVal(s.toCharArray)
    case Expr.CharLit(c) => SchemeVal.CharVal(c)
    case Expr.Symbol(n)  => SchemeVal.SymVal(n)
    case Expr.SList(es)  => SchemeVal.ListVal(es.map(quoteToVal))

  private def evalBody(body: List[Expr], env: Env): SchemeVal =
    body.foldLeft[SchemeVal](SchemeVal.Void)((_, e) => eval(e, env))

  private def posStr(expr: Expr): String =
    val p = Parser.positions.get(expr)
    if p != null then s"${p._1}:${p._2}" else "1:1"

  private val posPattern = ".*\\d+:\\d+.*".r

  private def eval(expr: Expr, env: Env): SchemeVal =
    try
      expr match
        case Expr.IntLit(n)    => SchemeVal.IntVal(n)
        case Expr.BoolLit(b)   => SchemeVal.BoolVal(b)
        case Expr.StrLit(s)    => SchemeVal.StrVal(s.toCharArray)
        case Expr.CharLit(c)   => SchemeVal.CharVal(c)
        case Expr.Symbol(name) => env.lookup(name)
        case Expr.SList(Nil)   => SchemeVal.ListVal(Nil)
        case Expr.SList(Expr.Symbol("quote") :: arg :: Nil) =>
          quoteToVal(arg)
        case Expr.SList(Expr.Symbol("if") :: cond :: thenBr :: elseBr :: Nil) =>
          if isTruthy(eval(cond, env)) then eval(thenBr, env)
          else eval(elseBr, env)
        case Expr.SList(Expr.Symbol("if") :: cond :: thenBr :: Nil) =>
          if isTruthy(eval(cond, env)) then eval(thenBr, env) else SchemeVal.Void
        case Expr.SList(
              Expr.Symbol("define") :: Expr.SList(
                Expr.Symbol(name) :: params
              ) :: body
            ) =>
          val (paramNames, restParam) = parseParams(params)
          env.define(name, SchemeVal.Procedure(paramNames, restParam, body, env))
          SchemeVal.Void
        case Expr.SList(
              Expr.Symbol("define") :: Expr.Symbol(name) :: value :: Nil
            ) =>
          env.define(name, eval(value, env))
          SchemeVal.Void
        case Expr.SList(
              Expr.Symbol("set!") :: Expr.Symbol(name) :: value :: Nil
            ) =>
          env.set(name, eval(value, env))
          SchemeVal.Void
        case Expr.SList(Expr.Symbol("lambda") :: Expr.SList(params) :: body) =>
          val (paramNames, restParam) = parseParams(params)
          SchemeVal.Procedure(paramNames, restParam, body, env)
        case Expr.SList(
              Expr.Symbol("let") :: Expr.Symbol(name) :: Expr.SList(
                bindings
              ) :: body
            ) =>
          evalNamedLet(name, bindings, body, env)
        case Expr.SList(
              Expr.Symbol("let") :: Expr.SList(bindings) :: body
            ) =>
          evalLet(bindings, body, env)
        case Expr.SList(Expr.Symbol("begin") :: exprs) =>
          evalBody(exprs, env)
        case Expr.SList(Expr.Symbol("cond") :: clauses) =>
          evalCond(clauses, env)
        case Expr.SList(Expr.Symbol("and") :: args) =>
          evalAnd(args, env)
        case Expr.SList(Expr.Symbol("or") :: args) =>
          evalOr(args, env)
        case Expr.SList(head :: args) =>
          applyProc(eval(head, env), args.map(a => eval(a, env)))
    catch
      case e: EvalError =>
        val msg = e.getMessage
        if posPattern.matches(msg) then throw e
        else throw new EvalError(s"$msg at ${posStr(expr)}")

  def applyProc(fn: SchemeVal, evaledArgs: List[SchemeVal]): SchemeVal =
    fn match
      case SchemeVal.BuiltinProc(_, f) => f(evaledArgs)
      case SchemeVal.Procedure(params, restParam, body, closureEnv) =>
        val newEnv = new Env(mutable.Map.empty, Some(closureEnv))
        if restParam.isDefined then
          if evaledArgs.length < params.length then
            throw new EvalError(s"expected at least ${params.length} arguments, got ${evaledArgs.length}")
          params.zip(evaledArgs).foreach((p, v) => newEnv.define(p, v))
          newEnv.define(restParam.get, SchemeVal.ListVal(evaledArgs.drop(params.length)))
        else
          if evaledArgs.length != params.length then
            throw new EvalError(s"expected ${params.length} arguments, got ${evaledArgs.length}")
          params.zip(evaledArgs).foreach((p, v) => newEnv.define(p, v))
        evalBody(body, newEnv)
      case other => throw new EvalError(s"not a procedure: ${other.display}")

  private def evalNamedLet(
    name: String,
    bindings: List[Expr],
    body: List[Expr],
    env: Env
  ): SchemeVal =
    val (paramNames, initExprs) = bindings.map {
      case Expr.SList(Expr.Symbol(p) :: v :: Nil) => (p, v)
      case _                                      => throw new EvalError("let: invalid binding")
    }.unzip
    val letEnv = new Env(mutable.Map.empty, Some(env))
    val proc   = SchemeVal.Procedure(paramNames, None, body, letEnv)
    letEnv.define(name, proc)
    val initVals = initExprs.map(e => eval(e, env))
    val callEnv  = new Env(mutable.Map.empty, Some(letEnv))
    paramNames.zip(initVals).foreach((p, v) => callEnv.define(p, v))
    evalBody(body, callEnv)

  private def evalLet(
    bindings: List[Expr],
    body: List[Expr],
    env: Env
  ): SchemeVal =
    val letEnv = new Env(mutable.Map.empty, Some(env))
    for b <- bindings do
      b match
        case Expr.SList(Expr.Symbol(name) :: valExpr :: Nil) =>
          letEnv.define(name, eval(valExpr, env))
        case _ => throw new EvalError("let: invalid binding")
    evalBody(body, letEnv)

  @scala.annotation.tailrec
  private def evalCond(clauses: List[Expr], env: Env): SchemeVal =
    clauses match
      case Nil => SchemeVal.Void
      case Expr.SList(Expr.Symbol("else") :: body) :: _ =>
        evalBody(body, env)
      case Expr.SList(test :: body) :: rest =>
        if isTruthy(eval(test, env)) then evalBody(body, env)
        else evalCond(rest, env)
      case _ => throw new EvalError("cond: invalid clause")

  @scala.annotation.tailrec
  private def evalAnd(
    args: List[Expr],
    env: Env,
    last: SchemeVal = SchemeVal.BoolVal(true)
  ): SchemeVal =
    args match
      case Nil => last
      case head :: tail =>
        val v = eval(head, env)
        if !isTruthy(v) then v
        else evalAnd(tail, env, v)

  @scala.annotation.tailrec
  private def evalOr(
    args: List[Expr],
    env: Env,
    last: SchemeVal = SchemeVal.BoolVal(false)
  ): SchemeVal =
    args match
      case Nil => last
      case head :: tail =>
        val v = eval(head, env)
        if isTruthy(v) then v
        else evalOr(tail, env, v)

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
