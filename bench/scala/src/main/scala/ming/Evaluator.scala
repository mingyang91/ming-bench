package ming

/** Scheme interpreter entry point. */
object Evaluator:

  // --- Value types ---
  enum Val:
    case Num(n: Long)
    case Bool(b: Boolean)
    case Str(s: String)
    case Symbol(name: String)
    case Pair(car: Val, cdr: Val)
    case Nil
    case Void
    case Builtin(f: List[Val] => Val)

  import Val.*

  // --- Parser ---
  private class Parser(input: String):
    private var pos = 0

    def parseAll(): List[Val] =
      val exprs = scala.collection.mutable.ListBuffer[Val]()
      while
        skipWhitespace()
        pos < input.length
      do exprs += parseExpr()
      exprs.toList

    private def skipWhitespace(): Unit =
      while pos < input.length && (input(pos).isWhitespace || input(pos) == ';') do
        if input(pos) == ';' then while pos < input.length && input(pos) != '\n' do pos += 1
        else pos += 1

    private def parseExpr(): Val =
      skipWhitespace()
      if pos >= input.length then throw new EvalError("unexpected end of input")
      input(pos) match
        case '(' =>
          pos += 1
          parseList()
        case '\'' =>
          pos += 1
          val e = parseExpr()
          Pair(Symbol("quote"), Pair(e, Nil))
        case '"' =>
          parseString()
        case '#' =>
          pos += 1
          if pos >= input.length then throw new EvalError("unexpected end of input after #")
          input(pos) match
            case 't'   => pos += 1; Bool(true)
            case 'f'   => pos += 1; Bool(false)
            case other => throw new EvalError(s"unexpected character after #: $other")
        case _ =>
          parseAtom()

    private def parseList(): Val =
      skipWhitespace()
      if pos >= input.length then throw new EvalError("unexpected end of input in list")
      if input(pos) == ')' then
        pos += 1
        Nil
      else
        val first = parseExpr()
        skipWhitespace()
        if pos < input.length && input(pos) == '.' then
          pos += 1
          val rest = parseExpr()
          skipWhitespace()
          if pos >= input.length || input(pos) != ')' then throw new EvalError("expected ) after dotted pair")
          pos += 1
          Pair(first, rest)
        else
          val rest = parseList()
          Pair(first, rest)

    private def parseString(): Val =
      pos += 1 // skip opening "
      val sb = new StringBuilder
      while pos < input.length && input(pos) != '"' do
        if input(pos) == '\\' then
          pos += 1
          if pos >= input.length then throw new EvalError("unterminated string")
          input(pos) match
            case 'n'  => sb += '\n'
            case 't'  => sb += '\t'
            case '\\' => sb += '\\'
            case '"'  => sb += '"'
            case c    => sb += '\\'; sb += c
        else sb += input(pos)
        pos += 1
      if pos >= input.length then throw new EvalError("unterminated string")
      pos += 1 // skip closing "
      Str(sb.toString)

    private def parseAtom(): Val =
      val start = pos
      while pos < input.length && !input(pos).isWhitespace && !"()\"';".contains(input(pos)) do pos += 1
      val token = input.substring(start, pos)
      if token.isEmpty then throw new EvalError(s"unexpected character: ${input(pos)}")
      token.toLongOption match
        case Some(n) => Num(n)
        case None    => Symbol(token)

  // --- Evaluator ---
  private def eval(expr: Val, env: Env): Val =
    expr match
      case Num(_) | Bool(_) | Str(_) => expr
      case Nil                       => Nil
      case Symbol(name) =>
        env.lookup(name) match
          case Some(v) => v
          case None    => throw new EvalError(s"unbound variable: $name")
      case Pair(Symbol("quote"), Pair(datum, Nil)) => datum
      case Pair(Symbol("and"), args)               => evalAnd(args, env)
      case Pair(Symbol("or"), args)                => evalOr(args, env)
      case Pair(head, args) =>
        val func    = eval(head, env)
        val argList = toList(args).map(a => eval(a, env))
        applyFunc(func, argList)
      case Void => Void

  private def evalAnd(args: Val, env: Env): Val =
    args match
      case Nil          => Bool(true)
      case Pair(x, Nil) => eval(x, env)
      case Pair(x, rest) =>
        val v = eval(x, env)
        v match
          case Bool(false) => Bool(false)
          case _           => evalAnd(rest, env)
      case _ => throw new EvalError("bad and syntax")

  private def evalOr(args: Val, env: Env): Val =
    args match
      case Nil          => Bool(false)
      case Pair(x, Nil) => eval(x, env)
      case Pair(x, rest) =>
        val v = eval(x, env)
        v match
          case Bool(false) => evalOr(rest, env)
          case _           => v
      case _ => throw new EvalError("bad or syntax")

  private def toList(v: Val): List[Val] = v match
    case Nil            => List.empty
    case Pair(car, cdr) => car :: toList(cdr)
    case _              => throw new EvalError("improper list")

  private def applyFunc(func: Val, args: List[Val]): Val = func match
    case Builtin(f) => f(args)
    case _          => throw new EvalError(s"not a procedure: ${display(func)}")

  // --- Environment ---
  private class Env(bindings: scala.collection.mutable.Map[String, Val], parent: Option[Env]):

    def lookup(name: String): Option[Val] =
      bindings.get(name).orElse(parent.flatMap(_.lookup(name)))
    def define(name: String, value: Val): Unit = bindings(name) = value

  private def defaultEnv(): Env =
    val m = scala.collection.mutable.Map[String, Val]()
    def numericBinop(op: (Long, Long) => Long): Val = Builtin { args =>
      val nums = args.map { case Num(n) => n; case v => throw new EvalError(s"not a number: ${display(v)}") }
      Num(nums.reduce(op))
    }

    m("+") = Builtin { args =>
      val nums = args.map { case Num(n) => n; case v => throw new EvalError(s"not a number: ${display(v)}") }
      Num(nums.sum)
    }
    m("-") = Builtin { args =>
      val nums = args.map { case Num(n) => n; case v => throw new EvalError(s"not a number: ${display(v)}") }
      if nums.length == 1 then Num(-nums.head)
      else Num(nums.reduce(_ - _))
    }
    m("*") = Builtin { args =>
      val nums = args.map { case Num(n) => n; case v => throw new EvalError(s"not a number: ${display(v)}") }
      Num(nums.product)
    }
    m("/") = Builtin { args =>
      val nums = args.map { case Num(n) => n; case v => throw new EvalError(s"not a number: ${display(v)}") }
      if nums.length < 2 then throw new EvalError("/ requires at least 2 arguments")
      if nums.tail.contains(0L) then throw new EvalError("division by zero")
      Num(nums.reduce(_ / _))
    }
    m("<") = Builtin { args =>
      val nums = args.map { case Num(n) => n; case v => throw new EvalError(s"not a number: ${display(v)}") }
      Bool(nums.sliding(2).forall { case Seq(a, b) => a < b; case _ => true })
    }
    m(">") = Builtin { args =>
      val nums = args.map { case Num(n) => n; case v => throw new EvalError(s"not a number: ${display(v)}") }
      Bool(nums.sliding(2).forall { case Seq(a, b) => a > b; case _ => true })
    }
    m("=") = Builtin { args =>
      val nums = args.map { case Num(n) => n; case v => throw new EvalError(s"not a number: ${display(v)}") }
      Bool(nums.sliding(2).forall { case Seq(a, b) => a == b; case _ => true })
    }
    m("<=") = Builtin { args =>
      val nums = args.map { case Num(n) => n; case v => throw new EvalError(s"not a number: ${display(v)}") }
      Bool(nums.sliding(2).forall { case Seq(a, b) => a <= b; case _ => true })
    }
    m(">=") = Builtin { args =>
      val nums = args.map { case Num(n) => n; case v => throw new EvalError(s"not a number: ${display(v)}") }
      Bool(nums.sliding(2).forall { case Seq(a, b) => a >= b; case _ => true })
    }
    m("not") = Builtin {
      case List(Bool(false)) => Bool(true)
      case List(_)           => Bool(false)
      case _                 => throw new EvalError("not requires 1 argument")
    }
    new Env(m, None)

  // --- Display ---
  private def display(v: Val): String = v match
    case Num(n)       => n.toString
    case Bool(true)   => "#t"
    case Bool(false)  => "#f"
    case Str(s)       => "\"" + s + "\""
    case Symbol(name) => name
    case Nil          => "()"
    case Void         => "#<void>"
    case Pair(_, _)   => displayList(v)
    case Builtin(_)   => "#<procedure>"

  private def displayList(v: Val): String =
    val sb      = new StringBuilder("(")
    var current = v
    var first   = true
    while current.isInstanceOf[Pair] do
      val Pair(car, cdr) = current: @unchecked
      if !first then sb.append(" ")
      sb.append(display(car))
      first = false
      current = cdr
    current match
      case Nil => ()
      case _   => sb.append(" . "); sb.append(display(current))
    sb.append(")")
    sb.toString

  // --- Public API ---
  def evalStr(input: String): String =
    val parser = new Parser(input)
    val exprs  = parser.parseAll()
    if exprs.isEmpty then throw new EvalError("no expressions")
    val env         = defaultEnv()
    var result: Val = Void
    for expr <- exprs do result = eval(expr, env)
    display(result)

  def evalStrWithOutput(input: String): (String, String) =
    val result = evalStr(input)
    (result, "")
