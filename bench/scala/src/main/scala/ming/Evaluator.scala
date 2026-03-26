package ming

object Evaluator:

  // ── Values ───────────────────────────────────────────────────────────
  private[ming] enum Value:
    case VNum(n: Long)
    case VBool(b: Boolean)
    case VStr(chars: Array[Char])
    case VChar(c: Char)
    case VList(elems: List[Value])
    case VSymbol(name: String)
    case VBuiltin(name: String)
    case VLambda(params: List[String], body: List[Expr], closure: Env)
    case VVoid

  private[ming] type Pos = ming.Pos

  private[ming] def display(v: Value): String = v match
    case Value.VNum(n)          => n.toString
    case Value.VBool(true)      => "#t"
    case Value.VBool(false)     => "#f"
    case Value.VStr(chars)      => s"\"${new String(chars)}\""
    case Value.VChar(c)         => s"#\\$c"
    case Value.VList(elems)     => "(" + elems.map(display).mkString(" ") + ")"
    case Value.VSymbol(n)       => n
    case Value.VBuiltin(n)      => s"#<procedure $n>"
    case Value.VLambda(_, _, _) => "#<procedure>"
    case Value.VVoid            => ""

  /** display-style output: no quotes on strings */
  private[ming] def displayStr(v: Value): String = v match
    case Value.VStr(chars)  => new String(chars)
    case Value.VList(elems) => "(" + elems.map(displayStr).mkString(" ") + ")"
    case other              => display(other)

  // ── Position helpers ────────────────────────────────────────────────
  private def posOf(expr: Expr): Pos = expr match
    case Expr.Num(_, p)    => p
    case Expr.Bool(_, p)   => p
    case Expr.Str(_, p)    => p
    case Expr.Chr(_, p)    => p
    case Expr.Symbol(_, p) => p
    case Expr.SList(_, p)  => p

  private[ming] def errAt(pos: Pos, msg: String): EvalError =
    EvalError(s"$pos: $msg")

  // ── Environment ──────────────────────────────────────────────────────
  private[ming] class Env(
    val bindings: scala.collection.mutable.Map[String, Value],
    val parent: Option[Env],
    val output: StringBuilder
  ):

    def lookup(name: String, pos: Pos): Value =
      bindings
        .get(name)
        .orElse(parent.flatMap(p => scala.util.Try(p.lookup(name, pos)).toOption))
        .getOrElse(throw errAt(pos, s"unbound variable: $name"))
    def define(name: String, value: Value): Unit = bindings(name) = value
    def child(): Env                             = Env(scala.collection.mutable.Map.empty, Some(this), output)

  private def defaultEnv(output: StringBuilder = new StringBuilder): Env =
    val env = Env(scala.collection.mutable.Map.empty, None, output)
    for name <- List(
        "+",
        "-",
        "*",
        "/",
        "<",
        ">",
        "=",
        "<=",
        ">=",
        "not",
        "cons",
        "car",
        "cdr",
        "null?",
        "list",
        "length",
        "append",
        "number?",
        "string?",
        "boolean?",
        "pair?",
        "symbol?",
        "char?",
        "display",
        "write",
        "newline",
        "string-append",
        "string-length",
        "substring",
        "string->number",
        "number->string",
        "symbol->string",
        "string->symbol",
        "string-ref",
        "string-copy",
        "string-set!"
      )
    do env.define(name, Value.VBuiltin(name))
    env

  // ── Eval ─────────────────────────────────────────────────────────────
  private def eval(expr: Expr, env: Env): Value = expr match
    case Expr.Num(n, _)                                       => Value.VNum(n)
    case Expr.Bool(b, _)                                      => Value.VBool(b)
    case Expr.Str(s, _)                                       => Value.VStr(s.toCharArray)
    case Expr.Chr(c, _)                                       => Value.VChar(c)
    case Expr.Symbol(name, p)                                 => env.lookup(name, p)
    case Expr.SList(Nil, p)                                   => throw errAt(p, "empty application")
    case Expr.SList(Expr.Symbol("quote", _) :: arg :: Nil, _) => quoteToValue(arg)
    case Expr.SList(Expr.Symbol("define", _) :: rest, p)      => evalDefine(rest, env, p)
    case Expr.SList(Expr.Symbol("if", _) :: rest, p)          => evalIf(rest, env, p)
    case Expr.SList(Expr.Symbol("lambda", _) :: rest, p)      => evalLambda(rest, env, p)
    case Expr.SList(Expr.Symbol("and", _) :: args, _)         => evalAnd(args, env)
    case Expr.SList(Expr.Symbol("or", _) :: args, _)          => evalOr(args, env)
    case Expr.SList(Expr.Symbol("let", _) :: rest, p)         => evalLet(rest, env, p)
    case Expr.SList(Expr.Symbol("begin", _) :: body, _)       => evalBegin(body, env)
    case Expr.SList(Expr.Symbol("cond", _) :: clauses, _)     => evalCond(clauses, env)
    case Expr.SList(head :: args, p) =>
      val func = eval(head, env)
      applyFunc(func, args.map(a => eval(a, env)), p, env)

  private def evalBody(body: List[Expr], env: Env): Value =
    body.foldLeft(Value.VVoid: Value)((_, e) => eval(e, env))

  private def applyFunc(func: Value, args: List[Value], pos: Pos, env: Env): Value = func match
    case Value.VBuiltin(name) => Builtins(name, args, pos, env)
    case Value.VLambda(params, body, closure) =>
      if args.length != params.length then throw errAt(pos, "wrong number of arguments")
      val callEnv = closure.child()
      params.zip(args).foreach((p, a) => callEnv.define(p, a))
      evalBody(body, callEnv)
    case _ => throw errAt(pos, "not a procedure")

  private def quoteToValue(expr: Expr): Value = expr match
    case Expr.Num(n, _)       => Value.VNum(n)
    case Expr.Bool(b, _)      => Value.VBool(b)
    case Expr.Str(s, _)       => Value.VStr(s.toCharArray)
    case Expr.Chr(c, _)       => Value.VChar(c)
    case Expr.Symbol(name, _) => Value.VSymbol(name)
    case Expr.SList(elems, _) => Value.VList(elems.map(quoteToValue))

  private def evalDefine(rest: List[Expr], env: Env, pos: Pos): Value = rest match
    case Expr.Symbol(name, _) :: valueExpr :: Nil =>
      env.define(name, eval(valueExpr, env))
      Value.VVoid
    case Expr.SList(Expr.Symbol(name, _) :: params, _) :: body =>
      val paramNames = params.map {
        case Expr.Symbol(n, _) => n
        case _                 => throw errAt(pos, "invalid parameter")
      }
      env.define(name, Value.VLambda(paramNames, body, env))
      Value.VVoid
    case _ => throw errAt(pos, "invalid define")

  private def evalIf(rest: List[Expr], env: Env, pos: Pos): Value = rest match
    case cond :: thenExpr :: elseExpr :: Nil =>
      if isTruthy(eval(cond, env)) then eval(thenExpr, env) else eval(elseExpr, env)
    case cond :: thenExpr :: Nil =>
      if isTruthy(eval(cond, env)) then eval(thenExpr, env) else Value.VVoid
    case _ => throw errAt(pos, "invalid if")

  private def evalLambda(rest: List[Expr], env: Env, pos: Pos): Value = rest match
    case Expr.SList(params, _) :: body =>
      val paramNames = params.map {
        case Expr.Symbol(n, _) => n
        case _                 => throw errAt(pos, "invalid parameter")
      }
      Value.VLambda(paramNames, body, env)
    case _ => throw errAt(pos, "invalid lambda")

  private def evalAnd(args: List[Expr], env: Env): Value =
    args match
      case Nil         => Value.VBool(true)
      case last :: Nil => eval(last, env)
      case head :: tail =>
        eval(head, env) match
          case Value.VBool(false) => Value.VBool(false)
          case _                  => evalAnd(tail, env)

  private def evalOr(args: List[Expr], env: Env): Value =
    args match
      case Nil         => Value.VBool(false)
      case last :: Nil => eval(last, env)
      case head :: tail =>
        val result = eval(head, env)
        if isTruthy(result) then result else evalOr(tail, env)

  private def evalLet(rest: List[Expr], env: Env, pos: Pos): Value = rest match
    case Expr.Symbol(name, _) :: Expr.SList(bindings, _) :: body if body.nonEmpty =>
      val paramNames = bindings.map {
        case Expr.SList(Expr.Symbol(n, _) :: _ :: Nil, _) => n
        case _                                            => throw errAt(pos, "invalid let binding")
      }
      val initVals = bindings.map {
        case Expr.SList(_ :: initExpr :: Nil, _) => eval(initExpr, env)
        case _                                   => throw errAt(pos, "invalid let binding")
      }
      val loopEnv = env.child()
      val lambda  = Value.VLambda(paramNames, body, loopEnv)
      loopEnv.define(name, lambda)
      applyFunc(lambda, initVals, pos, loopEnv)
    case Expr.SList(bindings, _) :: body if body.nonEmpty =>
      val letEnv = env.child()
      for b <- bindings do
        b match
          case Expr.SList(Expr.Symbol(name, _) :: initExpr :: Nil, _) =>
            letEnv.define(name, eval(initExpr, env))
          case _ => throw errAt(pos, "invalid let binding")
      evalBody(body, letEnv)
    case _ => throw errAt(pos, "invalid let")

  private def evalBegin(body: List[Expr], env: Env): Value =
    if body.isEmpty then Value.VVoid
    else evalBody(body, env)

  private def evalCond(clauses: List[Expr], env: Env): Value = clauses match
    case Nil                                                => Value.VVoid
    case Expr.SList(Expr.Symbol("else", _) :: body, _) :: _ => evalBody(body, env)
    case Expr.SList(test :: body, _) :: rest =>
      if isTruthy(eval(test, env)) then evalBody(body, env)
      else evalCond(rest, env)
    case e :: _ => throw errAt(posOf(e), "invalid cond")

  private[ming] def isTruthy(v: Value): Boolean = v match
    case Value.VBool(false) => false
    case _                  => true

  private[ming] def asNum(v: Value, pos: Pos): Long = v match
    case Value.VNum(n) => n
    case _             => throw errAt(pos, "expected number")

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
