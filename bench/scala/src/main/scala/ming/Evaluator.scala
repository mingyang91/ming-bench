package ming

import SchemeValue.*

/** Scheme interpreter entry point. Agents implement this object. */
object Evaluator:

  /** Evaluate one or more Scheme expressions and return the string representation of the last result. */
  def evalStr(input: String): String =
    val exprs = Parser.parse(input)
    if exprs.isEmpty then throw new EvalError("empty input")
    val (lastVal, _) = evalSequence(exprs, Env.empty)
    lastVal.display

  /** Evaluate Scheme expressions and return both the result string and any captured output. */
  def evalStrWithOutput(input: String): (String, String) =
    val result = evalStr(input)
    (result, "")

  private def evalSequence(
    exprs: List[SchemeValue],
    env: Env
  ): (SchemeValue, Env) =
    val (defines, body) = collectDefines(exprs, Nil)
    val bodyEnv =
      if defines.isEmpty then env
      else
        val frame = new Env.LetrecFrame(defines, env)
        frame.init()
        frame
    evalSequenceSimple(body, bodyEnv)

  @scala.annotation.tailrec
  private def collectDefines(
    exprs: List[SchemeValue],
    acc: List[(String, List[String], List[SchemeValue])]
  ): (List[(String, List[String], List[SchemeValue])], List[SchemeValue]) =
    exprs match
      case (defExpr @ SchemeList(SchemeSymbol("define") :: rest)) :: tail =>
        rest match
          case SchemeSymbol(name) :: valueExpr :: Nil =>
            collectDefines(tail, (name, Nil, List(valueExpr)) :: acc)
          case SchemeList(SchemeSymbol(name) :: params) :: body =>
            val paramNames = params.map {
              case SchemeSymbol(n) => n
              case other           => throw new EvalError(s"bad parameter: ${other.display}")
            }
            collectDefines(tail, (name, paramNames, body) :: acc)
          case _ => throw new EvalError("bad define syntax", defExpr.pos)
      case _ => (acc.reverse, exprs)

  @scala.annotation.tailrec
  private def evalSequenceSimple(
    exprs: List[SchemeValue],
    env: Env
  ): (SchemeValue, Env) =
    exprs match
      case Nil         => (SchemeVoid, env)
      case last :: Nil => evalWithEnv(last, env)
      case head :: tail =>
        val (_, nextEnv) = evalWithEnv(head, env)
        evalSequenceSimple(tail, nextEnv)

  private def evalWithEnv(
    expr: SchemeValue,
    env: Env
  ): (SchemeValue, Env) = expr match
    case SchemeInt(_)          => (expr, env)
    case SchemeBool(_)         => (expr, env)
    case SchemeString(_)       => (expr, env)
    case SchemeVoid            => (expr, env)
    case SchemeLambda(_, _, _) => (expr, env)
    case SchemeSymbol(name) =>
      try (env.lookup(name), env)
      catch
        case e: EvalError if e.sourcePos == SourcePos.None =>
          throw new EvalError(e.baseMessage, expr.pos)
    case SchemeList(Nil) =>
      throw new EvalError("empty application", expr.pos)
    case SchemeList(SchemeSymbol(op) :: args) =>
      try evalSpecialOrCall(op, args, env)
      catch
        case e: EvalError if e.sourcePos == SourcePos.None =>
          throw new EvalError(e.baseMessage, expr.pos)
    case SchemeList(head :: args) =>
      try
        val (proc, _)  = evalWithEnv(head, env)
        val evaledArgs = args.map(a => eval(a, env))
        (applyProc(proc, evaledArgs), env)
      catch
        case e: EvalError if e.sourcePos == SourcePos.None =>
          throw new EvalError(e.baseMessage, expr.pos)
    case other =>
      throw new EvalError(s"cannot evaluate: ${other.display}", other.pos)

  private[ming] def eval(expr: SchemeValue, env: Env): SchemeValue =
    evalWithEnv(expr, env)._1

  private def evalSpecialOrCall(
    op: String,
    args: List[SchemeValue],
    env: Env
  ): (SchemeValue, Env) = op match
    case "define" => evalDefine(args, env)
    case "if"     => (evalIf(args, env), env)
    case "quote"  => evalQuote(args, env)
    case "lambda" => (evalLambda(args, env), env)
    case "and"    => (evalAnd(args, env), env)
    case "or"     => (evalOr(args, env), env)
    case "not"    => (evalNot(args, env), env)
    case "let"    => (evalLet(args, env), env)
    case "begin"  => evalBegin(args, env)
    case "cond"   => (evalCond(args, env), env)
    case _ =>
      val evaledArgs = args.map(a => eval(a, env))
      env.get(op) match
        case Some(proc) => (applyProc(proc, evaledArgs), env)
        case None       => (Builtins.evalBuiltin(op, evaledArgs), env)

  private def applyProc(
    proc: SchemeValue,
    args: List[SchemeValue]
  ): SchemeValue = proc match
    case SchemeLambda(params, body, closure) =>
      if params.length != args.length then
        throw new EvalError(
          s"expected ${params.length} arguments, got ${args.length}"
        )
      val localEnv    = Env.Frame(params.zip(args).toMap, closure)
      val (result, _) = evalSequence(body, localEnv)
      result
    case other =>
      throw new EvalError(s"not a procedure: ${other.display}")

  private def evalDefine(
    args: List[SchemeValue],
    env: Env
  ): (SchemeValue, Env) = args match
    case SchemeSymbol(name) :: valueExpr :: Nil =>
      val v = eval(valueExpr, env)
      (SchemeVoid, env.extend(name, v))
    case SchemeList(SchemeSymbol(name) :: params) :: body =>
      val paramNames = params.map {
        case SchemeSymbol(n) => n
        case other =>
          throw new EvalError(s"bad parameter: ${other.display}")
      }
      val recEnv = Env.RecursiveFrame(
        name,
        closure => SchemeLambda(paramNames, body, closure),
        env
      )
      (SchemeVoid, recEnv)
    case _ => throw new EvalError("bad define syntax")

  private def evalIf(
    args: List[SchemeValue],
    env: Env
  ): SchemeValue =
    args match
      case cond :: thenBranch :: elseBranch :: Nil =>
        val v = eval(cond, env)
        if isFalsy(v) then eval(elseBranch, env) else eval(thenBranch, env)
      case cond :: thenBranch :: Nil =>
        val v = eval(cond, env)
        if isFalsy(v) then SchemeVoid else eval(thenBranch, env)
      case _ => throw new EvalError("if: bad syntax")

  private def evalQuote(
    args: List[SchemeValue],
    env: Env
  ): (SchemeValue, Env) =
    if args.length != 1 then throw new EvalError("quote: expected 1 argument")
    (args.head, env)

  private def evalLambda(
    args: List[SchemeValue],
    env: Env
  ): SchemeValue = args match
    case SchemeList(params) :: body if body.nonEmpty =>
      val paramNames = params.map {
        case SchemeSymbol(n) => n
        case other =>
          throw new EvalError(s"bad parameter: ${other.display}")
      }
      SchemeLambda(paramNames, body, env)
    case _ => throw new EvalError("lambda: bad syntax")

  private def evalAnd(
    args: List[SchemeValue],
    env: Env
  ): SchemeValue = args match
    case Nil         => SchemeBool(true)
    case last :: Nil => eval(last, env)
    case head :: tail =>
      val v = eval(head, env)
      if isFalsy(v) then v else evalAnd(tail, env)

  private def evalOr(
    args: List[SchemeValue],
    env: Env
  ): SchemeValue = args match
    case Nil         => SchemeBool(false)
    case last :: Nil => eval(last, env)
    case head :: tail =>
      val v = eval(head, env)
      if isFalsy(v) then evalOr(tail, env) else v

  private def evalNot(
    args: List[SchemeValue],
    env: Env
  ): SchemeValue =
    if args.length != 1 then throw new EvalError("not: expected 1 argument")
    val v = eval(args.head, env)
    SchemeBool(isFalsy(v))

  private def evalLet(
    args: List[SchemeValue],
    env: Env
  ): SchemeValue = args match
    // Named let: (let loop ((var init) ...) body ...)
    case SchemeSymbol(name) :: SchemeList(bindings) :: body if body.nonEmpty =>
      val (params, inits) = parseBindings(bindings)
      val evaledInits     = inits.map(i => eval(i, env))
      val recEnv = Env.RecursiveFrame(
        name,
        closure => SchemeLambda(params, body, closure),
        env
      )
      val localEnv    = recEnv.extend(params, evaledInits)
      val (result, _) = evalSequence(body, localEnv)
      result
    // Regular let: (let ((var init) ...) body ...)
    case SchemeList(bindings) :: body if body.nonEmpty =>
      val (params, inits) = parseBindings(bindings)
      val evaledInits     = inits.map(i => eval(i, env))
      val localEnv        = env.extend(params, evaledInits)
      val (result, _)     = evalSequence(body, localEnv)
      result
    case _ => throw new EvalError("let: bad syntax")

  private def parseBindings(
    bindings: List[SchemeValue]
  ): (List[String], List[SchemeValue]) =
    bindings.map {
      case SchemeList(SchemeSymbol(name) :: init :: Nil) => (name, init)
      case other                                         => throw new EvalError(s"let: bad binding: ${other.display}")
    }.unzip

  private def evalBegin(
    args: List[SchemeValue],
    env: Env
  ): (SchemeValue, Env) =
    evalSequence(args, env)

  private def evalCond(
    args: List[SchemeValue],
    env: Env
  ): SchemeValue =
    evalCondClauses(args, env)

  @scala.annotation.tailrec
  private def evalCondClauses(
    clauses: List[SchemeValue],
    env: Env
  ): SchemeValue = clauses match
    case Nil => SchemeVoid
    case SchemeList(SchemeSymbol("else") :: body) :: _ =>
      val (result, _) = evalSequence(body, env)
      result
    case SchemeList(test :: body) :: rest =>
      val v = eval(test, env)
      if isFalsy(v) then evalCondClauses(rest, env)
      else if body.isEmpty then v
      else
        val (result, _) = evalSequence(body, env)
        result
    case other :: _ => throw new EvalError(s"cond: bad clause: ${other.display}")

  private def isFalsy(v: SchemeValue): Boolean = v match
    case SchemeBool(false) => true
    case _                 => false
