package ming

import scala.collection.mutable

// --- AST ---
enum Expr:
  case IntLit(value: Long)
  case BoolLit(value: Boolean)
  case StrLit(value: String)
  case Symbol(name: String)
  case SList(elems: List[Expr])

// --- Parser ---
object Parser:

  def parse(input: String): List[Expr] =
    val tokens     = tokenize(input)
    val (exprs, _) = parseAll(tokens, 0)
    exprs

  private def tokenizeString(input: String, start: Int): (String, Int) =
    val sb = new StringBuilder("\"")
    var i  = start
    while i < input.length && input(i) != '"' do
      if input(i) == '\\' then
        i += 1
        if i < input.length then
          input(i) match
            case 'n'   => sb.append('\n')
            case 't'   => sb.append('\t')
            case '\\'  => sb.append('\\')
            case '"'   => sb.append('"')
            case other => sb.append('\\'); sb.append(other)
          i += 1
        end if
      else
        sb.append(input(i))
        i += 1
    end while
    if i < input.length then i += 1 // closing quote
    sb.append('"')
    (sb.toString, i)

  private def isDelimiter(c: Char): Boolean =
    c.isWhitespace || c == '(' || c == ')' || c == '"' || c == ';'

  private def tokenizeBare(input: String, start: Int): (String, Int) =
    val sb = new StringBuilder
    var i  = start
    while i < input.length && !isDelimiter(input(i)) do
      sb.append(input(i))
      i += 1
    end while
    (sb.toString, i)

  private def tokenize(input: String): Array[String] =
    val buf = mutable.ArrayBuffer[String]()
    var i   = 0
    while i < input.length do
      input(i) match
        case c if c.isWhitespace => i += 1
        case ';'                 => while i < input.length && input(i) != '\n' do i += 1
        case '('                 => buf += "("; i += 1
        case ')'                 => buf += ")"; i += 1
        case '\''                => buf += "'"; i += 1
        case '"' =>
          val (tok, next) = tokenizeString(input, i + 1)
          buf += tok
          i = next
        case _ =>
          val (tok, next) = tokenizeBare(input, i)
          buf += tok
          i = next
    end while
    buf.toArray

  private def parseAll(tokens: Array[String], pos: Int): (List[Expr], Int) =
    val buf = mutable.ListBuffer[Expr]()
    var i   = pos
    while i < tokens.length do
      val (expr, next) = parseExpr(tokens, i)
      buf += expr
      i = next
    end while
    (buf.toList, i)

  private def parseExpr(tokens: Array[String], pos: Int): (Expr, Int) =
    if pos >= tokens.length then throw new EvalError("unexpected end of input")
    val tok = tokens(pos)
    tok match
      case "(" =>
        val buf = mutable.ListBuffer[Expr]()
        var i   = pos + 1
        while i < tokens.length && tokens(i) != ")" do
          val (expr, next) = parseExpr(tokens, i)
          buf += expr
          i = next
        end while
        if i >= tokens.length then throw new EvalError("missing closing parenthesis")
        (Expr.SList(buf.toList), i + 1)
      case ")" =>
        throw new EvalError("unexpected )")
      case "'" =>
        val (expr, next) = parseExpr(tokens, pos + 1)
        (Expr.SList(List(Expr.Symbol("quote"), expr)), next)
      case _ =>
        (parseAtom(tok), pos + 1)

  private def parseAtom(tok: String): Expr =
    if tok == "#t" then Expr.BoolLit(true)
    else if tok == "#f" then Expr.BoolLit(false)
    else if tok.startsWith("\"") && tok.endsWith("\"") then Expr.StrLit(tok.substring(1, tok.length - 1))
    else
      tok.toLongOption match
        case Some(n) => Expr.IntLit(n)
        case None    => Expr.Symbol(tok)

// --- Values ---
enum SchemeVal:
  case IntVal(value: Long)
  case BoolVal(value: Boolean)
  case StrVal(value: String)
  case SymVal(name: String)
  case ListVal(elems: List[SchemeVal])
  case Procedure(params: List[String], body: List[Expr], env: Env)
  case BuiltinProc(name: String, fn: List[SchemeVal] => SchemeVal)
  case Void

  def display: String = this match
    case IntVal(n)            => n.toString
    case BoolVal(b)           => if b then "#t" else "#f"
    case StrVal(s)            => "\"" + s + "\""
    case SymVal(n)            => n
    case ListVal(Nil)         => "()"
    case ListVal(elems)       => "(" + elems.map(_.display).mkString(" ") + ")"
    case Procedure(_, _, _)   => "#<procedure>"
    case BuiltinProc(name, _) => s"#<procedure:$name>"
    case Void                 => "#<void>"

// --- Environment ---
class Env(val bindings: mutable.Map[String, SchemeVal], val parent: Option[Env]):

  def lookup(name: String): SchemeVal =
    bindings.get(name) match
      case Some(v) => v
      case None =>
        parent match
          case Some(p) => p.lookup(name)
          case None    => throw new EvalError(s"unbound variable: $name")

  def define(name: String, value: SchemeVal): Unit =
    bindings(name) = value

// --- Evaluator ---
object Evaluator:

  private def isTruthy(v: SchemeVal): Boolean = v match
    case SchemeVal.BoolVal(false) => false
    case _                        => true

  private def requireNums(name: String, args: List[SchemeVal]): List[Long] =
    args.map {
      case SchemeVal.IntVal(n) => n
      case other               => throw new EvalError(s"$name: expected number, got ${other.display}")
    }

  private def numericCmp(name: String, op: (Long, Long) => Boolean): (String, SchemeVal) =
    name -> SchemeVal.BuiltinProc(
      name,
      args =>
        val nums = requireNums(name, args)
        if nums.length < 2 then throw new EvalError(s"$name: expected at least 2 arguments")
        SchemeVal.BoolVal(nums.sliding(2).forall(w => op(w(0), w(1))))
    )

  private def makeGlobalEnv(): Env =
    val env = new Env(mutable.Map.empty, None)

    env.define("+", SchemeVal.BuiltinProc("+", args => SchemeVal.IntVal(requireNums("+", args).sum)))

    env.define(
      "-",
      SchemeVal.BuiltinProc(
        "-",
        args =>
          val nums = requireNums("-", args)
          if nums.isEmpty then throw new EvalError("-: expected at least 1 argument")
          if nums.length == 1 then SchemeVal.IntVal(-nums.head)
          else SchemeVal.IntVal(nums.reduce(_ - _))
      )
    )

    env.define("*", SchemeVal.BuiltinProc("*", args => SchemeVal.IntVal(requireNums("*", args).product)))

    env.define(
      "/",
      SchemeVal.BuiltinProc(
        "/",
        args =>
          val nums = requireNums("/", args)
          if nums.isEmpty then throw new EvalError("/: expected at least 1 argument")
          if nums.length == 1 then SchemeVal.IntVal(1 / nums.head)
          else
            nums.tail.foreach(d => if d == 0 then throw new EvalError("division by zero"))
            SchemeVal.IntVal(nums.reduce(_ / _))
      )
    )

    for (name, proc) <- List(
        numericCmp("<", _ < _),
        numericCmp(">", _ > _),
        numericCmp("=", _ == _),
        numericCmp("<=", _ <= _),
        numericCmp(">=", _ >= _)
      )
    do env.define(name, proc)

    env.define(
      "not",
      SchemeVal.BuiltinProc(
        "not",
        {
          case List(arg) => SchemeVal.BoolVal(!isTruthy(arg))
          case args      => throw new EvalError(s"not: expected 1 argument, got ${args.length}")
        }
      )
    )

    env

  private def eval(expr: Expr, env: Env): SchemeVal = expr match
    case Expr.IntLit(n)    => SchemeVal.IntVal(n)
    case Expr.BoolLit(b)   => SchemeVal.BoolVal(b)
    case Expr.StrLit(s)    => SchemeVal.StrVal(s)
    case Expr.Symbol(name) => env.lookup(name)
    case Expr.SList(Nil)   => SchemeVal.ListVal(Nil)
    case Expr.SList(Expr.Symbol("and") :: args) =>
      evalAnd(args, env)
    case Expr.SList(Expr.Symbol("or") :: args) =>
      evalOr(args, env)
    case Expr.SList(head :: args) =>
      val fn         = eval(head, env)
      val evaledArgs = args.map(a => eval(a, env))
      fn match
        case SchemeVal.BuiltinProc(_, f) => f(evaledArgs)
        case SchemeVal.Procedure(params, body, closureEnv) =>
          val newEnv = new Env(mutable.Map.empty, Some(closureEnv))
          params.zip(evaledArgs).foreach((p, v) => newEnv.define(p, v))
          var result: SchemeVal = SchemeVal.Void
          for e <- body do result = eval(e, newEnv)
          result
        case other => throw new EvalError(s"not a procedure: ${other.display}")

  @scala.annotation.tailrec
  private def evalAnd(args: List[Expr], env: Env, last: SchemeVal = SchemeVal.BoolVal(true)): SchemeVal =
    args match
      case Nil => last
      case head :: tail =>
        val v = eval(head, env)
        if !isTruthy(v) then v
        else evalAnd(tail, env, v)

  @scala.annotation.tailrec
  private def evalOr(args: List[Expr], env: Env, last: SchemeVal = SchemeVal.BoolVal(false)): SchemeVal =
    args match
      case Nil => last
      case head :: tail =>
        val v = eval(head, env)
        if isTruthy(v) then v
        else evalOr(tail, env, v)

  def evalStr(input: String): String =
    val exprs             = Parser.parse(input)
    val env               = makeGlobalEnv()
    var result: SchemeVal = SchemeVal.Void
    for expr <- exprs do result = eval(expr, env)
    result.display

  def evalStrWithOutput(input: String): (String, String) =
    val result = evalStr(input)
    (result, "")
