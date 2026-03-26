package ming

object Evaluator:

  // ── Values ───────────────────────────────────────────────────────────
  private enum Value:
    case VNum(n: Long)
    case VBool(b: Boolean)
    case VStr(s: String)
    case VList(elems: List[Value])
    case VSymbol(name: String)
    case VBuiltin(name: String)
    case VLambda(params: List[String], body: List[Expr], closure: Env)
    case VVoid

  private def display(v: Value): String = v match
    case Value.VNum(n)          => n.toString
    case Value.VBool(true)      => "#t"
    case Value.VBool(false)     => "#f"
    case Value.VStr(s)          => s"\"$s\""
    case Value.VList(elems)     => "(" + elems.map(display).mkString(" ") + ")"
    case Value.VSymbol(n)       => n
    case Value.VBuiltin(n)      => s"#<procedure $n>"
    case Value.VLambda(_, _, _) => "#<procedure>"
    case Value.VVoid            => ""

  // ── Environment ──────────────────────────────────────────────────────
  private class Env(val bindings: scala.collection.mutable.Map[String, Value], val parent: Option[Env]):

    def lookup(name: String): Value =
      bindings
        .get(name)
        .orElse(parent.flatMap(p => scala.util.Try(p.lookup(name)).toOption))
        .getOrElse(throw EvalError(s"unbound variable: $name"))
    def define(name: String, value: Value): Unit = bindings(name) = value
    def child(): Env                             = Env(scala.collection.mutable.Map.empty, Some(this))

  private def defaultEnv: Env =
    val env = Env(scala.collection.mutable.Map.empty, None)
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
        "symbol?"
      )
    do env.define(name, Value.VBuiltin(name))
    env

  // ── Eval ─────────────────────────────────────────────────────────────
  private def eval(expr: Expr, env: Env): Value = expr match
    case Expr.Num(n)                                    => Value.VNum(n)
    case Expr.Bool(b)                                   => Value.VBool(b)
    case Expr.Str(s)                                    => Value.VStr(s)
    case Expr.Symbol(name)                              => env.lookup(name)
    case Expr.SList(Nil)                                => throw EvalError("empty application")
    case Expr.SList(Expr.Symbol("quote") :: arg :: Nil) => quoteToValue(arg)
    case Expr.SList(Expr.Symbol("define") :: rest)      => evalDefine(rest, env)
    case Expr.SList(Expr.Symbol("if") :: rest)          => evalIf(rest, env)
    case Expr.SList(Expr.Symbol("lambda") :: rest)      => evalLambda(rest, env)
    case Expr.SList(Expr.Symbol("and") :: args)         => evalAnd(args, env)
    case Expr.SList(Expr.Symbol("or") :: args)          => evalOr(args, env)
    case Expr.SList(Expr.Symbol("let") :: rest)         => evalLet(rest, env)
    case Expr.SList(Expr.Symbol("begin") :: body)       => evalBegin(body, env)
    case Expr.SList(Expr.Symbol("cond") :: clauses)     => evalCond(clauses, env)
    case Expr.SList(head :: args) =>
      val func = eval(head, env)
      applyFunc(func, args.map(a => eval(a, env)))

  private def evalBody(body: List[Expr], env: Env): Value =
    body.foldLeft(Value.VVoid: Value)((_, e) => eval(e, env))

  private def applyFunc(func: Value, args: List[Value]): Value = func match
    case Value.VBuiltin(name) => applyBuiltin(name, args)
    case Value.VLambda(params, body, closure) =>
      if args.length != params.length then throw EvalError("wrong number of arguments")
      val callEnv = closure.child()
      params.zip(args).foreach((p, a) => callEnv.define(p, a))
      evalBody(body, callEnv)
    case _ => throw EvalError("not a procedure")

  private def quoteToValue(expr: Expr): Value = expr match
    case Expr.Num(n)       => Value.VNum(n)
    case Expr.Bool(b)      => Value.VBool(b)
    case Expr.Str(s)       => Value.VStr(s)
    case Expr.Symbol(name) => Value.VSymbol(name)
    case Expr.SList(elems) => Value.VList(elems.map(quoteToValue))

  private def evalDefine(rest: List[Expr], env: Env): Value = rest match
    case Expr.Symbol(name) :: valueExpr :: Nil =>
      env.define(name, eval(valueExpr, env))
      Value.VVoid
    case Expr.SList(Expr.Symbol(name) :: params) :: body =>
      val paramNames = params.map { case Expr.Symbol(n) => n; case _ => throw EvalError("invalid parameter") }
      env.define(name, Value.VLambda(paramNames, body, env))
      Value.VVoid
    case _ => throw EvalError("invalid define")

  private def evalIf(rest: List[Expr], env: Env): Value = rest match
    case cond :: thenExpr :: elseExpr :: Nil =>
      if isTruthy(eval(cond, env)) then eval(thenExpr, env) else eval(elseExpr, env)
    case cond :: thenExpr :: Nil =>
      if isTruthy(eval(cond, env)) then eval(thenExpr, env) else Value.VVoid
    case _ => throw EvalError("invalid if")

  private def evalLambda(rest: List[Expr], env: Env): Value = rest match
    case Expr.SList(params) :: body =>
      val paramNames = params.map { case Expr.Symbol(n) => n; case _ => throw EvalError("invalid parameter") }
      Value.VLambda(paramNames, body, env)
    case _ => throw EvalError("invalid lambda")

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

  private def evalLet(rest: List[Expr], env: Env): Value = rest match
    case Expr.Symbol(name) :: Expr.SList(bindings) :: body if body.nonEmpty =>
      val paramNames = bindings.map {
        case Expr.SList(Expr.Symbol(n) :: _ :: Nil) => n
        case _                                      => throw EvalError("invalid let binding")
      }
      val initVals = bindings.map {
        case Expr.SList(_ :: initExpr :: Nil) => eval(initExpr, env)
        case _                                => throw EvalError("invalid let binding")
      }
      val loopEnv = env.child()
      val lambda  = Value.VLambda(paramNames, body, loopEnv)
      loopEnv.define(name, lambda)
      applyFunc(lambda, initVals)
    case Expr.SList(bindings) :: body if body.nonEmpty =>
      val letEnv = env.child()
      for b <- bindings do
        b match
          case Expr.SList(Expr.Symbol(name) :: initExpr :: Nil) =>
            letEnv.define(name, eval(initExpr, env))
          case _ => throw EvalError("invalid let binding")
      evalBody(body, letEnv)
    case _ => throw EvalError("invalid let")

  private def evalBegin(body: List[Expr], env: Env): Value =
    if body.isEmpty then Value.VVoid
    else evalBody(body, env)

  private def evalCond(clauses: List[Expr], env: Env): Value = clauses match
    case Nil                                          => Value.VVoid
    case Expr.SList(Expr.Symbol("else") :: body) :: _ => evalBody(body, env)
    case Expr.SList(test :: body) :: rest =>
      if isTruthy(eval(test, env)) then evalBody(body, env)
      else evalCond(rest, env)
    case _ => throw EvalError("invalid cond")

  private def isTruthy(v: Value): Boolean = v match
    case Value.VBool(false) => false
    case _                  => true

  private def asNum(v: Value): Long = v match
    case Value.VNum(n) => n
    case _             => throw EvalError("expected number")

  private def applyBuiltin(name: String, args: List[Value]): Value = name match
    case "+" => Value.VNum(args.map(asNum).sum)
    case "*" => Value.VNum(args.map(asNum).product)
    case "-" =>
      if args.isEmpty then throw EvalError("- requires at least 1 argument")
      else if args.length == 1 then Value.VNum(-asNum(args.head))
      else Value.VNum(args.map(asNum).reduceLeft(_ - _))
    case "/" =>
      if args.isEmpty then throw EvalError("/ requires at least 1 argument")
      else if args.length == 1 then Value.VNum(1 / asNum(args.head))
      else
        val nums = args.map(asNum)
        if nums.tail.contains(0L) then throw EvalError("division by zero")
        Value.VNum(nums.reduceLeft(_ / _))
    case "<" =>
      val nums = args.map(asNum)
      Value.VBool(nums.zip(nums.tail).forall((a, b) => a < b))
    case ">" =>
      val nums = args.map(asNum)
      Value.VBool(nums.zip(nums.tail).forall((a, b) => a > b))
    case "=" =>
      val nums = args.map(asNum)
      Value.VBool(nums.zip(nums.tail).forall((a, b) => a == b))
    case "<=" =>
      val nums = args.map(asNum)
      Value.VBool(nums.zip(nums.tail).forall((a, b) => a <= b))
    case ">=" =>
      val nums = args.map(asNum)
      Value.VBool(nums.zip(nums.tail).forall((a, b) => a >= b))
    case "not" =>
      if args.length != 1 then throw EvalError("not requires 1 argument")
      Value.VBool(!isTruthy(args.head))
    case "cons" =>
      if args.length != 2 then throw EvalError("cons requires 2 arguments")
      args(1) match
        case Value.VList(elems) => Value.VList(args(0) :: elems)
        case _                  => Value.VList(List(args(0), args(1)))
    case "car" =>
      if args.length != 1 then throw EvalError("car requires 1 argument")
      args.head match
        case Value.VList(h :: _) => h
        case _                   => throw EvalError("car: not a pair")
    case "cdr" =>
      if args.length != 1 then throw EvalError("cdr requires 1 argument")
      args.head match
        case Value.VList(_ :: t) => Value.VList(t)
        case _                   => throw EvalError("cdr: not a pair")
    case "null?" =>
      if args.length != 1 then throw EvalError("null? requires 1 argument")
      Value.VBool(args.head == Value.VList(Nil))
    case "list" => Value.VList(args)
    case "length" =>
      if args.length != 1 then throw EvalError("length requires 1 argument")
      args.head match
        case Value.VList(elems) => Value.VNum(elems.length.toLong)
        case _                  => throw EvalError("length: not a list")
    case "append" =>
      args.foldRight(Value.VList(Nil): Value) { (arg, acc) =>
        (arg, acc) match
          case (Value.VList(elems), Value.VList(accElems)) => Value.VList(elems ++ accElems)
          case _                                           => throw EvalError("append: not a list")
      }
    case "number?" =>
      Value.VBool(args.length == 1 && args.head.isInstanceOf[Value.VNum])
    case "string?" =>
      Value.VBool(args.length == 1 && args.head.isInstanceOf[Value.VStr])
    case "boolean?" =>
      Value.VBool(args.length == 1 && args.head.isInstanceOf[Value.VBool])
    case "pair?" =>
      Value.VBool(args.length == 1 && (args.head match
        case Value.VList(_ :: _) => true
        case _                   => false))
    case "symbol?" =>
      Value.VBool(args.length == 1 && args.head.isInstanceOf[Value.VSymbol])
    case _ => throw EvalError(s"unknown builtin: $name")

  // ── Public API ───────────────────────────────────────────────────────
  def evalStr(input: String): String =
    val tokens = Parser.tokenize(input)
    val exprs  = Parser.parseAll(tokens)
    if exprs.isEmpty then throw EvalError("no expressions")
    val env = defaultEnv
    display(evalBody(exprs, env))

  def evalStrWithOutput(input: String): (String, String) =
    (evalStr(input), "")
