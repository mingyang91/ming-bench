package ming

import scala.annotation.tailrec

/** Scheme interpreter entry point. */
object Evaluator:

  def evalStr(input: String): String =
    val tokens = Tokenizer.tokenize(input)
    val exprs  = Parser.parseAll(tokens)
    val env    = Env.default()
    val result = exprs.foldLeft(SchemeVal.Void: SchemeVal)((_, e) => eval(e, env))
    SchemeVal.display(result)

  def evalStrWithOutput(input: String): (String, String) =
    val tokens = Tokenizer.tokenize(input)
    val exprs  = Parser.parseAll(tokens)
    val output = new StringBuilder
    val env    = Env.defaultWithOutput(output)
    val result = exprs.foldLeft(SchemeVal.Void: SchemeVal)((_, e) => eval(e, env))
    (SchemeVal.display(result), output.toString)

  def eval(expr: SchemeVal, env: Env): SchemeVal =
    try
      expr match
        case SchemeVal.IntVal(_)    => expr
        case SchemeVal.BoolVal(_)   => expr
        case SchemeVal.StringVal(_) => expr
        case SchemeVal.CharVal(_)   => expr
        case SchemeVal.Symbol(name) =>
          env.lookup(name) match
            case Some(v) => v
            case None    => throw new EvalError(s"unbound variable: $name")
        case SchemeVal.SList(elems) if elems.isEmpty =>
          throw new EvalError("empty application")
        case SchemeVal.SList(elems) =>
          elems.head match
            case SchemeVal.Symbol("define") => evalDefine(elems.tail, env)
            case SchemeVal.Symbol("if")     => evalIf(elems.tail, env)
            case SchemeVal.Symbol("quote") =>
              if elems.tail.size != 1 then throw new EvalError("quote: expected 1 argument")
              elems.tail.head
            case SchemeVal.Symbol("lambda") => evalLambda(elems.tail, env)
            case SchemeVal.Symbol("and")    => evalAnd(elems.tail, env)
            case SchemeVal.Symbol("or")     => evalOr(elems.tail, env)
            case SchemeVal.Symbol("begin")  => evalBegin(elems.tail, env)
            case SchemeVal.Symbol("let")    => evalLet(elems.tail, env)
            case SchemeVal.Symbol("cond")   => evalCond(elems.tail, env)
            case head =>
              val proc = eval(head, env)
              val args = elems.tail.map(a => eval(a, env))
              apply(proc, args)
        case _ => throw new EvalError(s"cannot evaluate: $expr")
    catch
      case e: EvalError =>
        val (line, col) = expr.pos
        if line > 0 && !e.getMessage.matches(".*\\d+:\\d+.*") then throw new EvalError(s"$line:$col: ${e.getMessage}")
        else throw e

  private def evalDefine(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.Symbol(name) :: value :: Nil =>
        env.define(name, eval(value, env))
        SchemeVal.Void
      case SchemeVal.SList(elems) :: body if elems.nonEmpty && body.nonEmpty =>
        elems.head match
          case SchemeVal.Symbol(name) =>
            val params = elems.tail.map {
              case SchemeVal.Symbol(p) => p
              case other               => throw new EvalError(s"define: expected parameter name, got $other")
            }
            env.define(name, SchemeVal.LambdaProc(params, body, env))
            SchemeVal.Void
          case other => throw new EvalError(s"define: expected name, got $other")
      case _ => throw new EvalError("define: bad syntax")

  private def evalIf(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case cond :: thenBranch :: elseBranch :: Nil =>
        if isTruthy(eval(cond, env)) then eval(thenBranch, env)
        else eval(elseBranch, env)
      case cond :: thenBranch :: Nil =>
        if isTruthy(eval(cond, env)) then eval(thenBranch, env)
        else SchemeVal.Void
      case _ => throw new EvalError("if: bad syntax")

  private def evalLambda(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.SList(paramList) :: body if body.nonEmpty =>
        val params = paramList.map {
          case SchemeVal.Symbol(p) => p
          case other               => throw new EvalError(s"lambda: expected parameter name, got $other")
        }
        SchemeVal.LambdaProc(params, body, env)
      case _ => throw new EvalError("lambda: bad syntax")

  @tailrec
  private def evalAnd(exprs: List[SchemeVal], env: Env): SchemeVal =
    exprs match
      case Nil         => SchemeVal.BoolVal(true)
      case last :: Nil => eval(last, env)
      case head :: tail =>
        val result = eval(head, env)
        if !isTruthy(result) then result else evalAnd(tail, env)

  @tailrec
  private def evalOr(exprs: List[SchemeVal], env: Env): SchemeVal =
    exprs match
      case Nil         => SchemeVal.BoolVal(false)
      case last :: Nil => eval(last, env)
      case head :: tail =>
        val result = eval(head, env)
        if isTruthy(result) then result else evalOr(tail, env)

  private def evalBegin(exprs: List[SchemeVal], env: Env): SchemeVal =
    if exprs.isEmpty then SchemeVal.Void
    else exprs.foldLeft(SchemeVal.Void: SchemeVal)((_, e) => eval(e, env))

  private def evalLet(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.Symbol(name) :: SchemeVal.SList(bindings) :: body if body.nonEmpty =>
        val (params, inits) = parseBindings(bindings, env)
        val localEnv        = new Env(scala.collection.mutable.Map.empty, Some(env))
        val proc            = SchemeVal.LambdaProc(params, body, localEnv)
        localEnv.define(name, proc)
        params.zip(inits).foreach((p, v) => localEnv.define(p, v))
        body.foldLeft(SchemeVal.Void: SchemeVal)((_, e) => eval(e, localEnv))
      case SchemeVal.SList(bindings) :: body if body.nonEmpty =>
        val localEnv = new Env(scala.collection.mutable.Map.empty, Some(env))
        for b <- bindings do
          b match
            case SchemeVal.SList(List(SchemeVal.Symbol(name), valueExpr)) =>
              localEnv.define(name, eval(valueExpr, env))
            case _ => throw new EvalError("let: bad binding syntax")
        body.foldLeft(SchemeVal.Void: SchemeVal)((_, e) => eval(e, localEnv))
      case _ => throw new EvalError("let: bad syntax")

  private def parseBindings(
    bindings: List[SchemeVal],
    env: Env
  ): (List[String], List[SchemeVal]) =
    val pairs = bindings.map {
      case SchemeVal.SList(List(SchemeVal.Symbol(p), valueExpr)) => (p, eval(valueExpr, env))
      case _                                                     => throw new EvalError("let: bad binding syntax")
    }
    pairs.unzip

  @tailrec
  private def evalCond(clauses: List[SchemeVal], env: Env): SchemeVal =
    clauses match
      case Nil => SchemeVal.Void
      case clause :: rest =>
        clause match
          case SchemeVal.SList(elems) if elems.nonEmpty =>
            elems.head match
              case SchemeVal.Symbol("else") =>
                elems.tail.foldLeft(SchemeVal.Void: SchemeVal)((_, e) => eval(e, env))
              case test =>
                val testVal = eval(test, env)
                if isTruthy(testVal) then
                  if elems.tail.isEmpty then testVal
                  else elems.tail.foldLeft(SchemeVal.Void: SchemeVal)((_, e) => eval(e, env))
                else evalCond(rest, env)
          case _ => throw new EvalError("cond: bad clause")

  private def isTruthy(v: SchemeVal): Boolean = v match
    case SchemeVal.BoolVal(false) => false
    case _                        => true

  def apply(proc: SchemeVal, args: List[SchemeVal]): SchemeVal = proc match
    case SchemeVal.BuiltinProc(_, f) => f(args)
    case SchemeVal.LambdaProc(params, body, closure) =>
      if args.size != params.size then throw new EvalError(s"expected ${params.size} arguments, got ${args.size}")
      val localEnv = new Env(scala.collection.mutable.Map.empty, Some(closure))
      params.zip(args).foreach((p, a) => localEnv.define(p, a))
      body.foldLeft(SchemeVal.Void: SchemeVal)((_, e) => eval(e, localEnv))
    case _ => throw new EvalError(s"not a procedure: $proc")
