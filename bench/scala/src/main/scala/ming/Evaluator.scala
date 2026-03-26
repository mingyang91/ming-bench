package ming

object Evaluator:

  // ── AST ──────────────────────────────────────────────────────────────
  private enum Expr:
    case Num(value: Long)
    case Bool(value: Boolean)
    case Str(value: String)
    case Symbol(name: String)
    case SList(elems: List[Expr])

  // ── Tokeniser ────────────────────────────────────────────────────────
  private def readWord(input: String, start: Int): (String, Int) =
    val sb = new StringBuilder
    var i  = start
    while i < input.length && !input(i).isWhitespace && input(i) != '(' && input(i) != ')' do
      sb += input(i); i += 1
    (sb.toString, i)

  private def readString(input: String, start: Int): (String, Int) =
    val sb = new StringBuilder("\"")
    var i  = start + 1
    while i < input.length && input(i) != '"' do
      if input(i) == '\\' then
        sb += input(i); i += 1
        if i < input.length then
          sb += input(i); i += 1
      else
        sb += input(i); i += 1
    if i < input.length then
      sb += '"'; i += 1
    (sb.toString, i)

  private def readHash(input: String, start: Int): (String, Int) =
    if start + 1 >= input.length then ("#", start + 1)
    else
      val next = input(start + 1)
      if next == 't' || next == 'f' then (input.substring(start, start + 2), start + 2)
      else readWord(input, start)

  private def skipComment(input: String, start: Int): Int =
    var i = start
    while i < input.length && input(i) != '\n' do i += 1
    i

  private def tokenize(input: String): List[String] =
    val tokens = scala.collection.mutable.ListBuffer[String]()
    var i      = 0
    while i < input.length do
      input(i) match
        case c if c.isWhitespace => i += 1
        case ';'                 => i = skipComment(input, i)
        case '(' | ')' | '\'' =>
          tokens += input(i).toString; i += 1
        case '"' =>
          val (tok, next) = readString(input, i)
          tokens += tok; i = next
        case '#' =>
          val (tok, next) = readHash(input, i)
          tokens += tok; i = next
        case _ =>
          val (tok, next) = readWord(input, i)
          tokens += tok; i = next
    tokens.toList

  // ── Parser ───────────────────────────────────────────────────────────
  private def parseAll(tokens: List[String]): List[Expr] =
    val exprs = scala.collection.mutable.ListBuffer[Expr]()
    var rest  = tokens
    while rest.nonEmpty do
      val (expr, remaining) = parseExpr(rest)
      exprs += expr
      rest = remaining
    exprs.toList

  private def parseExpr(tokens: List[String]): (Expr, List[String]) = tokens match
    case Nil => throw EvalError("unexpected end of input")
    case "(" :: rest =>
      val (elems, remaining) = parseList(rest)
      (Expr.SList(elems), remaining)
    case "'" :: rest =>
      val (expr, remaining) = parseExpr(rest)
      (Expr.SList(List(Expr.Symbol("quote"), expr)), remaining)
    case ")" :: _      => throw EvalError("unexpected )")
    case token :: rest => (parseAtom(token), rest)

  private def parseList(tokens: List[String]): (List[Expr], List[String]) =
    val elems = scala.collection.mutable.ListBuffer[Expr]()
    var rest  = tokens
    while rest.nonEmpty && rest.head != ")" do
      val (expr, remaining) = parseExpr(rest)
      elems += expr
      rest = remaining
    if rest.isEmpty then throw EvalError("missing )")
    (elems.toList, rest.tail) // skip ")"

  private def parseAtom(token: String): Expr =
    if token == "#t" then Expr.Bool(true)
    else if token == "#f" then Expr.Bool(false)
    else if token.startsWith("\"") then Expr.Str(token.substring(1, token.length - 1))
    else
      token.toLongOption match
        case Some(n) => Expr.Num(n)
        case None    => Expr.Symbol(token)

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
    for name <- List("+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not") do env.define(name, Value.VBuiltin(name))
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
    case Expr.SList(head :: args) =>
      val func = eval(head, env)
      applyFunc(func, args.map(a => eval(a, env)))

  private def applyFunc(func: Value, args: List[Value]): Value = func match
    case Value.VBuiltin(name) => applyBuiltin(name, args)
    case Value.VLambda(params, body, closure) =>
      if args.length != params.length then throw EvalError("wrong number of arguments")
      val callEnv = closure.child()
      params.zip(args).foreach((p, a) => callEnv.define(p, a))
      var result: Value = Value.VVoid
      for e <- body do result = eval(e, callEnv)
      result
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
    case _ => throw EvalError(s"unknown builtin: $name")

  // ── Public API ───────────────────────────────────────────────────────
  def evalStr(input: String): String =
    val tokens = tokenize(input)
    val exprs  = parseAll(tokens)
    if exprs.isEmpty then throw EvalError("no expressions")
    var result: Value = Value.VVoid
    val env           = defaultEnv
    for expr <- exprs do result = eval(expr, env)
    display(result)

  def evalStrWithOutput(input: String): (String, String) =
    (evalStr(input), "")
