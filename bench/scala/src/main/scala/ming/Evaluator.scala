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
    case SchemeChar(c: scala.Char)
    case Void
    case Builtin(f: List[Val] => Val)

  import Val.*

  // --- Output capture ---
  private[ming] val outputBuffer = new StringBuilder

  // --- Position tracking ---
  private var lastPos = "1:1"

  private def error(msg: String): Nothing =
    throw new EvalError(s"$lastPos: $msg")

  // --- Evaluator ---
  private def eval(expr: Val, env: Env): Val =
    expr match
      case Num(_) | Bool(_) | Str(_) => expr
      case Nil                       => Nil
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
      case Pair(Symbol("and"), args)               => evalAnd(args, env)
      case Pair(Symbol("or"), args)                => evalOr(args, env)
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

  private def evalLambda(rest: Val, env: Env): Val =
    rest match
      case Pair(params, body) =>
        val paramNames = toList(params).map {
          case Symbol(s) => s; case v => error(s"bad parameter: ${display(v)}")
        }
        val bodyList = toList(body)
        if bodyList.isEmpty then error("lambda: empty body")
        Builtin { args =>
          if args.length != paramNames.length then
            error(s"lambda: expected ${paramNames.length} arguments, got ${args.length}")
          val childEnv = new Env(scala.collection.mutable.Map[String, Val](), Some(env))
          paramNames.zip(args).foreach((p, a) => childEnv.define(p, a))
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
    clauses match
      case Nil => Void
      case Pair(clause, rest) =>
        val clauseList = toList(clause)
        if clauseList.isEmpty then error("bad cond clause")
        clauseList.head match
          case Symbol("else") =>
            var result: Val = Void
            for expr <- clauseList.tail do result = eval(expr, env)
            result
          case test =>
            val v = eval(test, env)
            if v != Bool(false) then
              if clauseList.tail.isEmpty then v
              else
                var result: Val = Void
                for expr <- clauseList.tail do result = eval(expr, env)
                result
            else evalCond(rest, env)
      case _ => error("bad cond syntax")

  private def evalLet(rest: Val, env: Env): Val =
    rest match
      // Named let: (let name ((var init) ...) body ...)
      case Pair(Symbol(name), Pair(bindings, body)) =>
        val bindingList = toList(bindings)
        val paramNames = bindingList.map {
          case Pair(Symbol(p), Pair(_, Nil)) => p
          case _                             => error("bad named let binding")
        }
        val initVals = bindingList.map {
          case Pair(_, Pair(v, Nil)) => eval(v, env)
          case _                     => error("bad named let binding")
        }
        val bodyList = toList(body)
        if bodyList.isEmpty then error("let: empty body")
        val childEnv = new Env(scala.collection.mutable.Map[String, Val](), Some(env))
        val loopFunc = Builtin { args =>
          if args.length != paramNames.length then
            error(s"named let $name: expected ${paramNames.length} arguments, got ${args.length}")
          val loopEnv = new Env(scala.collection.mutable.Map[String, Val](), Some(childEnv))
          paramNames.zip(args).foreach((p, a) => loopEnv.define(p, a))
          var result: Val = Void
          for expr <- bodyList do result = eval(expr, loopEnv)
          result
        }
        childEnv.define(name, loopFunc)
        // Initial call
        val loopEnv = new Env(scala.collection.mutable.Map[String, Val](), Some(childEnv))
        paramNames.zip(initVals).foreach((p, a) => loopEnv.define(p, a))
        var result: Val = Void
        for expr <- bodyList do result = eval(expr, loopEnv)
        result
      // Regular let: (let ((var init) ...) body ...)
      case Pair(bindings, body) =>
        val childEnv = new Env(scala.collection.mutable.Map[String, Val](), Some(env))
        for binding <- toList(bindings) do
          binding match
            case Pair(Symbol(name), Pair(valueExpr, Nil)) =>
              childEnv.define(name, eval(valueExpr, env))
            case _ => error("bad let binding")
        val bodyList = toList(body)
        if bodyList.isEmpty then error("let: empty body")
        var result: Val = Void
        for expr <- bodyList do result = eval(expr, childEnv)
        result
      case _ => error("bad let syntax")

  private def toList(v: Val): List[Val] = v match
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
    case _ => error(s"not a procedure: ${display(func)}")

  // --- Environment ---
  private class Env(bindings: scala.collection.mutable.Map[String, Val], parent: Option[Env]):

    def lookup(name: String): Option[Val] =
      bindings.get(name).orElse(parent.flatMap(_.lookup(name)))
    def define(name: String, value: Val): Unit = bindings(name) = value

  private def defaultEnv(): Env =
    val m = scala.collection.mutable.Map[String, Val]()
    Builtins.all.foreach((name, v) => m(name) = v)
    new Env(m, None)

  // --- Display (write-style, with quotes) ---
  private[ming] def display(v: Val): String = v match
    case Num(n)        => n.toString
    case Bool(true)    => "#t"
    case Bool(false)   => "#f"
    case Str(s)        => "\"" + s + "\""
    case SchemeChar(c) => s"#\\$c"
    case Symbol(name)  => name
    case Nil           => "()"
    case Void          => "#<void>"
    case Pair(_, _)    => displayList(v)
    case Builtin(_)    => "#<procedure>"

  // --- Display (display-style, no quotes on strings) ---
  private[ming] def displayVal(v: Val): String = v match
    case Str(s)        => s
    case SchemeChar(c) => c.toString
    case Pair(_, _)    => displayListVal(v)
    case _             => display(v)

  private def displayListVal(v: Val): String =
    val sb = new StringBuilder("(")
    @scala.annotation.tailrec
    def loop(current: Val, first: Boolean): Unit = current match
      case Pair(car, cdr) =>
        if !first then sb.append(" ")
        sb.append(displayVal(car))
        loop(cdr, first = false)
      case Nil => ()
      case _   => sb.append(" . "); sb.append(displayVal(current))
    loop(v, first = true)
    sb.append(")")
    sb.toString

  private def displayList(v: Val): String =
    val sb = new StringBuilder("(")
    @scala.annotation.tailrec
    def loop(current: Val, first: Boolean): Unit = current match
      case Pair(car, cdr) =>
        if !first then sb.append(" ")
        sb.append(display(car))
        loop(cdr, first = false)
      case Nil => ()
      case _   => sb.append(" . "); sb.append(display(current))
    loop(v, first = true)
    sb.append(")")
    sb.toString

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
    display(result)

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
    (display(result), output)
