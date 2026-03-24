package ming

/** Scheme interpreter entry point. */
object Evaluator:

  enum Val:
    case Num(n: Long)
    case Bool(b: Boolean)
    case Str(chars: Array[Char])
    case Symbol(name: String)
    case Pair(car: Val, cdr: Val)
    case Nil
    case SchemeChar(c: scala.Char)
    case Void
    case Rational(num: Long, den: Long)
    case Inexact(d: Double)
    case Builtin(f: List[Val] => Val)
    case MacroTransformer(expand: Val => Val)
    case Record(tag: String, fields: Array[Val])
    case Vector(elems: Array[Val])

    case Closure(
      params: List[String],
      restParam: Option[String],
      body: List[Val],
      closureEnv: Env
    )

  import Val.*
  import TcoForms.TcoResult
  import TcoForms.TcoResult.*

  private[ming] def mkStr(s: String): Val = Str(s.toCharArray)

  /** Create a rational, simplifying and converting to Num if denominator is 1. */
  private[ming] def mkRational(num: Long, den: Long): Val =
    if den == 0 then throw new EvalError("division by zero")
    val sign = if den < 0 then -1 else 1
    val n    = num * sign
    val d    = den * sign
    val g    = gcd(math.abs(n), d)
    val sn   = n / g
    val sd   = d / g
    if sd == 1 then Num(sn) else Rational(sn, sd)

  private def gcd(a: Long, b: Long): Long =
    if b == 0 then a else gcd(b, a % b)

  private[ming] def strValue(v: Val): String = v match
    case Str(chars) => new String(chars)
    case _          => throw new EvalError(s"not a string: ${Display.write(v)}")

  // --- Output capture ---
  private[ming] val outputBuffer = new StringBuilder

  // --- Mutable string tracking (string-copy creates mutable strings) ---
  private[ming] val mutableStrings: java.util.Set[Array[Char]] =
    java.util.Collections.newSetFromMap(
      new java.util.IdentityHashMap[Array[Char], java.lang.Boolean]()
    )

  // --- Position tracking ---
  private var lastPos = "1:1"

  private def error(msg: String): Nothing =
    throw new EvalError(s"$lastPos: $msg")

  // --- Helper: set up closure call environment ---
  private[ming] def setupClosureEnv(
    params: List[String],
    restParam: Option[String],
    body: List[Val],
    closureEnv: Env,
    args: List[Val]
  ): Env =
    if restParam.isDefined then
      if args.length < params.length then
        error(s"lambda: expected at least ${params.length} arguments, got ${args.length}")
    else if args.length != params.length then error(s"lambda: expected ${params.length} arguments, got ${args.length}")
    val callEnv = Env.empty(Some(closureEnv))
    params.zip(args).foreach((p, a) => callEnv.define(p, a))
    restParam.foreach { rp =>
      val restArgs = args.drop(params.length)
      callEnv.define(rp, restArgs.foldRight(Nil: Val)((a, acc) => Pair(a, acc)))
    }
    callEnv

  /** Dispatch a TcoResult: either continue the trampoline or return a value. */
  private inline def dispatchTco(
    result: TcoResult,
    curExpr: Array[Val],
    curEnv: Array[Env]
  ): Val | scala.Null =
    result match
      case Continue(e, env) => curExpr(0) = e; curEnv(0) = env; null
      case Done(v)          => v

  // --- Evaluator (trampoline for TCO) ---
  private def eval(expr0: Val, env0: Env): Val =
    val curExprArr = Array[Val](expr0)
    val curEnvArr  = Array[Env](env0)

    while true do
      val curExpr = curExprArr(0)
      val curEnv  = curEnvArr(0)
      val result  = evalStep(curExpr, curEnv)
      if result != null then
        val v = dispatchTco(result, curExprArr, curEnvArr)
        if v != null then return v
      else return curExpr

    throw new RuntimeException("unreachable")

  /** Evaluate one step. Returns null for self-evaluating, TcoResult otherwise. */
  private def evalStep(curExpr: Val, curEnv: Env): TcoResult | scala.Null =
    curExpr match
      case Num(_) | Bool(_) | Str(_) | SchemeChar(_) | Builtin(_) | MacroTransformer(_) | Rational(_, _) | Inexact(_) |
          Record(_, _) | Vector(_) | Closure(_, _, _, _) =>
        null
      case Nil  => null
      case Void => null
      case Symbol(name) =>
        Done(curEnv.lookup(name).getOrElse(error(s"unbound variable: $name")))
      case Pair(Symbol("quote"), Pair(datum, Nil)) => Done(datum)
      case Pair(Symbol("define"), rest)            => Done(evalDefine(rest, curEnv))
      case Pair(Symbol("set!"), Pair(Symbol(name), Pair(valueExpr, Nil))) =>
        val v = eval(valueExpr, curEnv)
        if !curEnv.set(name, v) then error(s"unbound variable: $name")
        Done(Void)
      case Pair(Symbol("if"), rest) =>
        TcoForms.evalIf(rest, curEnv, eval, error)
      case Pair(Symbol("lambda"), rest) => Done(evalLambda(rest, curEnv))
      case Pair(Symbol("begin"), body) =>
        val exprs = toList(body)
        if exprs.isEmpty then Done(Void)
        else
          for e <- exprs.init do eval(e, curEnv)
          Continue(exprs.last, curEnv)
      case Pair(Symbol("cond"), clauses) =>
        TcoForms.evalCond(clauses, curEnv, eval, error)
      case Pair(Symbol("let"), rest) =>
        TcoForms.evalLet(rest, curEnv, eval, error)
      case Pair(Symbol("and"), args) =>
        TcoForms.evalAnd(args, curEnv, eval)
      case Pair(Symbol("or"), args) =>
        TcoForms.evalOr(args, curEnv, eval)
      case Pair(Symbol("define-record-type"), rest) =>
        Done(Records.evalDefineRecordType(rest, curEnv, error))
      case Pair(Symbol("case-lambda"), clausesList) =>
        Done(SpecialForms.evalCaseLambda(clausesList, curEnv, eval, parseParams, error))
      case Pair(Symbol("let*"), rest) =>
        Done(SpecialForms.evalLetStar(rest, curEnv, eval, error))
      case Pair(Symbol("letrec"), rest) =>
        Done(SpecialForms.evalLetrec(rest, curEnv, eval, error))
      case Pair(Symbol("letrec*"), rest) =>
        Done(SpecialForms.evalLetrecStar(rest, curEnv, eval, error))
      case Pair(Symbol("case"), rest) =>
        Done(SpecialForms.evalCase(rest, curEnv, eval, error))
      case Pair(Symbol("do"), rest) =>
        Done(SpecialForms.evalDo(rest, curEnv, eval, error))
      case Pair(Symbol("define-syntax"), Pair(Symbol(name), Pair(sr, Nil))) =>
        Done(Macros.evalDefineSyntax(name, sr, curEnv))
      case p @ Pair(Symbol(name), _) =>
        curEnv.lookup(name) match
          case Some(MacroTransformer(expand)) => Continue(expand(p), curEnv)
          case _ =>
            val func           = eval(Symbol(name), curEnv)
            val Pair(_, pArgs) = p: @unchecked
            val argList        = toList(pArgs).map(a => eval(a, curEnv))
            TcoForms.applyFuncTco(func, argList, eval, error, applyBuiltinChecked)
      case Pair(head, args) =>
        val func    = eval(head, curEnv)
        val argList = toList(args).map(a => eval(a, curEnv))
        TcoForms.applyFuncTco(func, argList, eval, error, applyBuiltinChecked)

  private def evalDefine(rest: Val, env: Env): Val =
    rest match
      case Pair(Pair(Symbol(name), params), body) =>
        val lambdaExpr = Pair(Symbol("lambda"), Pair(params, body))
        env.define(name, eval(lambdaExpr, env))
        Void
      case Pair(Symbol(name), Pair(valueExpr, Nil)) =>
        env.define(name, eval(valueExpr, env))
        Void
      case _ => error("bad define syntax")

  /** Parse parameter list, returning (fixed params, optional rest param). */
  private def parseParams(params: Val): (List[String], Option[String]) =
    params match
      case Nil          => (List.empty, None)
      case Symbol(name) => (List.empty, Some(name))
      case Pair(Symbol(name), rest) =>
        rest match
          case Symbol(restName) => (List(name), Some(restName))
          case _ =>
            val (more, restParam) = parseParams(rest)
            (name :: more, restParam)
      case _ => error(s"bad parameter: ${Display.write(params)}")

  private def evalLambda(rest: Val, env: Env): Val =
    rest match
      case Pair(params, body) =>
        val (paramNames, restParam) = parseParams(params)
        val bodyList                = toList(body)
        if bodyList.isEmpty then error("lambda: empty body")
        Closure(paramNames, restParam, bodyList, env)
      case _ => error("bad lambda syntax")

  private[ming] def toList(v: Val): List[Val] = v match
    case Nil            => List.empty
    case Pair(car, cdr) => car :: toList(cdr)
    case _              => error("improper list")

  private def applyBuiltinChecked(f: List[Val] => Val, args: List[Val]): Val =
    try f(args)
    catch
      case e: EvalError =>
        if !e.getMessage.matches(".*\\d+:\\d+.*") then error(e.getMessage)
        else throw e

  private[ming] def applyFunc(func: Val, args: List[Val]): Val = func match
    case Closure(params, restParam, body, closureEnv) =>
      val callEnv     = setupClosureEnv(params, restParam, body, closureEnv, args)
      var result: Val = Void
      for expr <- body do result = eval(expr, callEnv)
      result
    case Builtin(f) => applyBuiltinChecked(f, args)
    case _          => error(s"not a procedure: ${Display.write(func)}")

  private def defaultEnv(): Env =
    val env = Env.empty()
    Builtins.all.foreach((name, v) => env.define(name, v))
    env

  // --- Public API ---
  def evalStr(input: String): String =
    val parser = new Parser(input)
    val exprs  = parser.parseAllWithPositions()
    if exprs.isEmpty then throw new EvalError("no expressions")
    val env         = defaultEnv()
    var result: Val = Void
    for (expr, line, col) <- exprs do
      lastPos = s"$line:$col"
      result = eval(expr, env)
    Display.write(result)

  def evalStrWithOutput(input: String): (String, String) =
    outputBuffer.clear()
    val parser = new Parser(input)
    val exprs  = parser.parseAllWithPositions()
    if exprs.isEmpty then throw new EvalError("no expressions")
    val env         = defaultEnv()
    var result: Val = Void
    for (expr, line, col) <- exprs do
      lastPos = s"$line:$col"
      result = eval(expr, env)
    val output = outputBuffer.toString
    outputBuffer.clear()
    (Display.write(result), output)
