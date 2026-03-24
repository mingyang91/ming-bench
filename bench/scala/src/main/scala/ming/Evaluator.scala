package ming

import scala.collection.mutable

object Evaluator:

  // --- AST ---
  private enum Expr:
    case Num(value: Long)
    case Bool(value: Boolean)
    case Str(value: String)
    case Sym(name: String)
    case Lst(elems: List[Expr])
    case Lambda(params: List[String], body: List[Expr], closure: Env)

  // --- Environment ---
  private class Env(val bindings: mutable.Map[String, Expr], val parent: Option[Env]):

    def lookup(name: String): Expr =
      bindings.get(name) match
        case Some(v) => v
        case None =>
          parent match
            case Some(p) => p.lookup(name)
            case None    => throw EvalError(s"unbound variable: $name")

    def define(name: String, value: Expr): Unit =
      bindings(name) = value

    def child(): Env = Env(mutable.Map.empty, Some(this))

  // --- Parser ---
  private class Parser(input: String):
    private var pos: Int = 0

    private def peek: Char = if pos < input.length then input(pos) else '\u0000'

    private def advance(): Char =
      val c = input(pos)
      pos += 1
      c

    private def skipWhitespaceAndComments(): Unit =
      while pos < input.length do
        val c = input(pos)
        if c.isWhitespace then pos += 1
        else if c == ';' then while pos < input.length && input(pos) != '\n' do pos += 1
        else return

    def parseAll(): List[Expr] =
      val exprs = List.newBuilder[Expr]
      skipWhitespaceAndComments()
      while pos < input.length do
        exprs += parseExpr()
        skipWhitespaceAndComments()
      exprs.result()

    private def parseExpr(): Expr =
      skipWhitespaceAndComments()
      if pos >= input.length then throw EvalError("unexpected end of input")
      peek match
        case '(' => parseList()
        case '"' => parseString()
        case '\'' => // quote shorthand
          advance()
          val inner = parseExpr()
          Expr.Lst(List(Expr.Sym("quote"), inner))
        case '#' => parseHash()
        case _   => parseAtom()

    private def parseList(): Expr =
      advance() // skip '('
      val elems = List.newBuilder[Expr]
      skipWhitespaceAndComments()
      while pos < input.length && peek != ')' do
        elems += parseExpr()
        skipWhitespaceAndComments()
      if pos >= input.length then throw EvalError("unmatched (")
      advance() // skip ')'
      Expr.Lst(elems.result())

    private def parseString(): Expr =
      advance() // skip opening "
      val sb = StringBuilder()
      while pos < input.length && peek != '"' do
        val c = advance()
        if c == '\\' && pos < input.length then
          val esc = advance()
          esc match
            case 'n'   => sb.append('\n')
            case 't'   => sb.append('\t')
            case '\\'  => sb.append('\\')
            case '"'   => sb.append('"')
            case other => sb.append('\\'); sb.append(other)
        else sb.append(c)
      if pos >= input.length then throw EvalError("unterminated string")
      advance() // skip closing "
      Expr.Str(sb.result())

    private def parseHash(): Expr =
      advance() // skip '#'
      if pos >= input.length then throw EvalError("unexpected end after #")
      val c = advance()
      c match
        case 't' =>
          if pos < input.length && !isDelimiter(peek) then throw EvalError(s"unexpected character after #t")
          Expr.Bool(true)
        case 'f' =>
          if pos < input.length && !isDelimiter(peek) then throw EvalError(s"unexpected character after #f")
          Expr.Bool(false)
        case _ => throw EvalError(s"unexpected #$c")

    private def isDelimiter(c: Char): Boolean =
      c.isWhitespace || c == '(' || c == ')' || c == '"' || c == ';'

    private def parseAtom(): Expr =
      val start = pos
      while pos < input.length && !isDelimiter(peek) do pos += 1
      val token = input.substring(start, pos)
      if token.isEmpty then throw EvalError(s"unexpected character: ${peek}")
      token.toLongOption match
        case Some(n) => Expr.Num(n)
        case None    => Expr.Sym(token)

  // --- Evaluation ---
  private def eval(expr: Expr, env: Env): Expr = expr match
    case Expr.Num(_) | Expr.Bool(_) | Expr.Str(_) | Expr.Lambda(_, _, _) => expr
    case Expr.Sym(name)                                                  => env.lookup(name)
    case Expr.Lst(Nil)                                                   => throw EvalError("empty application")
    case Expr.Lst(Expr.Sym("quote") :: args) =>
      if args.length != 1 then throw EvalError("quote: need exactly 1 argument")
      args.head
    case Expr.Lst(Expr.Sym("if") :: args) =>
      if args.length < 2 || args.length > 3 then throw EvalError("if: need 2 or 3 arguments")
      val cond = eval(args.head, env)
      if !isFalsy(cond) then eval(args(1), env)
      else if args.length == 3 then eval(args(2), env)
      else Expr.Bool(false) // unspecified
    case Expr.Lst(Expr.Sym("define") :: args) =>
      args match
        case Expr.Sym(name) :: value :: Nil =>
          env.define(name, eval(value, env))
          Expr.Bool(false) // unspecified
        case Expr.Lst(Expr.Sym(name) :: params) :: body if body.nonEmpty =>
          val paramNames = params.map {
            case Expr.Sym(p) => p
            case other       => throw EvalError(s"define: invalid parameter: ${display(other)}")
          }
          val lambda = Expr.Lambda(paramNames, body, env)
          env.define(name, lambda)
          Expr.Bool(false)
        case _ => throw EvalError("define: invalid syntax")
    case Expr.Lst(Expr.Sym("lambda") :: args) =>
      args match
        case Expr.Lst(params) :: body if body.nonEmpty =>
          val paramNames = params.map {
            case Expr.Sym(p) => p
            case other       => throw EvalError(s"lambda: invalid parameter: ${display(other)}")
          }
          Expr.Lambda(paramNames, body, env)
        case _ => throw EvalError("lambda: invalid syntax")
    case Expr.Lst(Expr.Sym("and") :: args) => evalAnd(args, env)
    case Expr.Lst(Expr.Sym("or") :: args)  => evalOr(args, env)
    case Expr.Lst(op :: args) =>
      val func          = eval(op, env)
      val evaluatedArgs = args.map(a => eval(a, env))
      applyProc(func, evaluatedArgs)

  private def evalAnd(args: List[Expr], env: Env): Expr = args match
    case Nil         => Expr.Bool(true)
    case last :: Nil => eval(last, env)
    case head :: tail =>
      val v = eval(head, env)
      if isFalsy(v) then v else evalAnd(tail, env)

  private def evalOr(args: List[Expr], env: Env): Expr = args match
    case Nil         => Expr.Bool(false)
    case last :: Nil => eval(last, env)
    case head :: tail =>
      val v = eval(head, env)
      if !isFalsy(v) then v else evalOr(tail, env)

  private def isFalsy(e: Expr): Boolean = e match
    case Expr.Bool(false) => true
    case _                => false

  private def applyProc(func: Expr, args: List[Expr]): Expr = func match
    case Expr.Sym(name) => applyBuiltin(name, args)
    case Expr.Lambda(params, body, closure) =>
      if params.length != args.length then
        throw EvalError(s"lambda: expected ${params.length} arguments, got ${args.length}")
      val localEnv = closure.child()
      params.zip(args).foreach((p, a) => localEnv.define(p, a))
      var result: Expr = Expr.Bool(false)
      for e <- body do result = eval(e, localEnv)
      result
    case _ => throw EvalError(s"not a procedure: ${display(func)}")

  private def applyBuiltin(name: String, args: List[Expr]): Expr = name match
    case "+" =>
      val nums = args.map(asNum)
      Expr.Num(nums.sum)
    case "-" =>
      if args.isEmpty then throw EvalError("-: need at least 1 argument")
      val nums = args.map(asNum)
      if nums.length == 1 then Expr.Num(-nums.head)
      else Expr.Num(nums.reduceLeft(_ - _))
    case "*" =>
      val nums = args.map(asNum)
      Expr.Num(nums.product)
    case "/" =>
      if args.length < 2 then throw EvalError("/: need at least 2 arguments")
      val nums = args.map(asNum)
      if nums.tail.contains(0L) then throw EvalError("division by zero")
      Expr.Num(nums.reduceLeft(_ / _))
    case "<" =>
      checkBinaryComparison(name, args)
      Expr.Bool(asNum(args(0)) < asNum(args(1)))
    case ">" =>
      checkBinaryComparison(name, args)
      Expr.Bool(asNum(args(0)) > asNum(args(1)))
    case "=" =>
      checkBinaryComparison(name, args)
      Expr.Bool(asNum(args(0)) == asNum(args(1)))
    case "<=" =>
      checkBinaryComparison(name, args)
      Expr.Bool(asNum(args(0)) <= asNum(args(1)))
    case ">=" =>
      checkBinaryComparison(name, args)
      Expr.Bool(asNum(args(0)) >= asNum(args(1)))
    case "not" =>
      if args.length != 1 then throw EvalError("not: need exactly 1 argument")
      Expr.Bool(isFalsy(args.head))
    case _ => throw EvalError(s"unknown procedure: $name")

  private def checkBinaryComparison(name: String, args: List[Expr]): Unit =
    if args.length != 2 then throw EvalError(s"$name: need exactly 2 arguments")

  private def asNum(e: Expr): Long = e match
    case Expr.Num(n) => n
    case _           => throw EvalError(s"expected number, got ${display(e)}")

  private def display(e: Expr): String = e match
    case Expr.Num(n)          => n.toString
    case Expr.Bool(true)      => "#t"
    case Expr.Bool(false)     => "#f"
    case Expr.Str(s)          => "\"" + s + "\""
    case Expr.Sym(name)       => name
    case Expr.Lst(elems)      => "(" + elems.map(display).mkString(" ") + ")"
    case Expr.Lambda(_, _, _) => "#<procedure>"

  // --- Public API ---
  private def makeTopLevelEnv(): Env =
    val env      = Env(mutable.Map.empty, None)
    val builtins = List("+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not")
    for name <- builtins do env.define(name, Expr.Sym(name))
    env

  def evalStr(input: String): String =
    val parser = Parser(input)
    val exprs  = parser.parseAll()
    if exprs.isEmpty then throw EvalError("no expressions")
    val env          = makeTopLevelEnv()
    var result: Expr = Expr.Bool(false)
    for e <- exprs do result = eval(e, env)
    display(result)

  def evalStrWithOutput(input: String): (String, String) =
    (evalStr(input), "")
