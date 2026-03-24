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
    case Rational(num: Long, den: Long) // exact rational, always simplified, den > 1
    case Inexact(d: Double)             // inexact number
    case Builtin(f: List[Val] => Val)
    case MacroTransformer(expand: Val => Val)
    case Record(tag: String, fields: Array[Val])
    case Vector(elems: Array[Val])

  import Val.*

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
    java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[Array[Char], java.lang.Boolean]())

  // --- Position tracking ---
  private var lastPos = "1:1"

  private def error(msg: String): Nothing =
    throw new EvalError(s"$lastPos: $msg")

  // --- Evaluator ---
  private def eval(expr: Val, env: Env): Val =
    expr match
      case Num(_) | Bool(_) | Str(_) | SchemeChar(_) | Builtin(_) | MacroTransformer(_) | Rational(_, _) | Inexact(_) |
          Record(_, _) | Vector(_) =>
        expr
      case Nil => Nil
      case Symbol(name) =>
        env.lookup(name) match
          case Some(v) => v
          case None    => error(s"unbound variable: $name")
      case Pair(Symbol("quote"), Pair(datum, Nil)) => datum
      case Pair(Symbol("define"), rest)            => evalDefine(rest, env)
      case Pair(Symbol("if"), rest)                => evalIf(rest, env)
      case Pair(Symbol("lambda"), rest)            => evalLambda(rest, env)
      case Pair(Symbol("begin"), body)             => evalBegin(body, env)
      case Pair(Symbol("cond"), clauses)           => evalCond(clauses, env)
      case Pair(Symbol("let"), rest)               => evalLet(rest, env)
      case Pair(Symbol("set!"), Pair(Symbol(name), Pair(valueExpr, Nil))) =>
        val v = eval(valueExpr, env)
        if !env.set(name, v) then error(s"unbound variable: $name")
        Void
      case Pair(Symbol("and"), args)                => evalAnd(args, env)
      case Pair(Symbol("or"), args)                 => evalOr(args, env)
      case Pair(Symbol("define-record-type"), rest) => Records.evalDefineRecordType(rest, env, error)
      case Pair(Symbol("case-lambda"), clausesList) =>
        SpecialForms.evalCaseLambda(clausesList, env, eval, parseParams, error)
      case Pair(Symbol("let*"), rest) =>
        SpecialForms.evalLetStar(rest, env, eval, error)
      case Pair(Symbol("letrec"), rest) =>
        SpecialForms.evalLetrec(rest, env, eval, error)
      case Pair(Symbol("letrec*"), rest) =>
        SpecialForms.evalLetrecStar(rest, env, eval, error)
      case Pair(Symbol("case"), rest) =>
        SpecialForms.evalCase(rest, env, eval, error)
      case Pair(Symbol("do"), rest) =>
        SpecialForms.evalDo(rest, env, eval, error)
      case Pair(Symbol("define-syntax"), Pair(Symbol(name), Pair(sr, Nil))) =>
        Macros.evalDefineSyntax(name, sr, env)
      case p @ Pair(Symbol(name), _) =>
        env.lookup(name) match
          case Some(MacroTransformer(expand)) =>
            eval(expand(p), env)
          case _ =>
            val func = eval(Symbol(name), env)
            val argList = toList(p match
              case Pair(_, a) => a;
              case _          => Nil).map(a => eval(a, env))
            applyFunc(func, argList)
      case Pair(head, args) =>
        val func    = eval(head, env)
        val argList = toList(args).map(a => eval(a, env))
        applyFunc(func, argList)
      case Void => Void

  private def evalDefine(rest: Val, env: Env): Val =
    rest match
      // (define (f params...) body...) => (define f (lambda (params...) body...))
      case Pair(Pair(Symbol(name), params), body) =>
        val lambdaExpr = Pair(Symbol("lambda"), Pair(params, body))
        val v          = eval(lambdaExpr, env)
        env.define(name, v)
        Void
      // (define x expr)
      case Pair(Symbol(name), Pair(valueExpr, Nil)) =>
        val v = eval(valueExpr, env)
        env.define(name, v)
        Void
      case _ => error("bad define syntax")

  private def evalIf(rest: Val, env: Env): Val =
    rest match
      case Pair(cond, Pair(thenExpr, Pair(elseExpr, Nil))) =>
        val condVal = eval(cond, env)
        if condVal != Bool(false) then eval(thenExpr, env) else eval(elseExpr, env)
      case Pair(cond, Pair(thenExpr, Nil)) =>
        val condVal = eval(cond, env)
        if condVal != Bool(false) then eval(thenExpr, env) else Void
      case _ => error("bad if syntax")

  /** Parse parameter list, returning (fixed params, optional rest param). */
  private def parseParams(params: Val): (List[String], Option[String]) =
    params match
      case Nil          => (List.empty, None)
      case Symbol(name) => (List.empty, Some(name)) // (lambda args body)
      case Pair(Symbol(name), rest) =>
        rest match
          case Symbol(restName) => (List(name), Some(restName)) // last dotted element
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
        Builtin { args =>
          if restParam.isDefined then
            if args.length < paramNames.length then
              error(s"lambda: expected at least ${paramNames.length} arguments, got ${args.length}")
          else if args.length != paramNames.length then
            error(s"lambda: expected ${paramNames.length} arguments, got ${args.length}")
          val childEnv = Env.empty(Some(env))
          paramNames.zip(args).foreach((p, a) => childEnv.define(p, a))
          restParam.foreach { rp =>
            val restArgs = args.drop(paramNames.length)
            childEnv.define(rp, restArgs.foldRight(Nil: Val)((a, acc) => Pair(a, acc)))
          }
          var result: Val = Void
          for expr <- bodyList do result = eval(expr, childEnv)
          result
        }
      case _ => error("bad lambda syntax")

  private def evalAnd(args: Val, env: Env): Val =
    args match
      case Nil          => Bool(true)
      case Pair(x, Nil) => eval(x, env)
      case Pair(x, rest) =>
        val v = eval(x, env)
        v match
          case Bool(false) => Bool(false)
          case _           => evalAnd(rest, env)
      case _ => error("bad and syntax")

  private def evalOr(args: Val, env: Env): Val =
    args match
      case Nil          => Bool(false)
      case Pair(x, Nil) => eval(x, env)
      case Pair(x, rest) =>
        val v = eval(x, env)
        v match
          case Bool(false) => evalOr(rest, env)
          case _           => v
      case _ => error("bad or syntax")

  private def evalBegin(body: Val, env: Env): Val =
    val exprs = toList(body)
    if exprs.isEmpty then Void
    else
      var result: Val = Void
      for expr <- exprs do result = eval(expr, env)
      result

  private def evalCond(clauses: Val, env: Env): Val =
    LetForms.evalCond(clauses, env, eval, error)

  private def evalLet(rest: Val, env: Env): Val =
    LetForms.evalLet(rest, env, eval, error)

  private[ming] def toList(v: Val): List[Val] = v match
    case Nil            => List.empty
    case Pair(car, cdr) => car :: toList(cdr)
    case _              => error("improper list")

  private[ming] def applyFunc(func: Val, args: List[Val]): Val = func match
    case Builtin(f) =>
      try f(args)
      catch
        case e: EvalError =>
          if !e.getMessage.matches(".*\\d+:\\d+.*") then error(e.getMessage)
          else throw e
    case _ => error(s"not a procedure: ${Display.write(func)}")

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
