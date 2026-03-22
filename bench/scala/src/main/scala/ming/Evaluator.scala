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
    exprs match
      case Nil         => (SchemeVoid, env)
      case last :: Nil => evalWithEnv(last, env)
      case head :: tail =>
        val (_, nextEnv) = evalWithEnv(head, env)
        evalSequence(tail, nextEnv)

  private def evalWithEnv(
    expr: SchemeValue,
    env: Env
  ): (SchemeValue, Env) = expr match
    case SchemeInt(_)          => (expr, env)
    case SchemeBool(_)         => (expr, env)
    case SchemeString(_)       => (expr, env)
    case SchemeVoid            => (expr, env)
    case SchemeLambda(_, _, _) => (expr, env)
    case SchemeSymbol(name)    => (env.lookup(name), env)
    case SchemeList(Nil)       => throw new EvalError("empty application")
    case SchemeList(SchemeSymbol(op) :: args) =>
      evalSpecialOrCall(op, args, env)
    case SchemeList(head :: args) =>
      val (proc, _)  = evalWithEnv(head, env)
      val evaledArgs = args.map(a => eval(a, env))
      (applyProc(proc, evaledArgs), env)

  private def eval(expr: SchemeValue, env: Env): SchemeValue =
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
    case _ =>
      val evaledArgs = args.map(a => eval(a, env))
      env.get(op) match
        case Some(proc) => (applyProc(proc, evaledArgs), env)
        case None       => (evalBuiltin(op, evaledArgs), env)

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

  private def isFalsy(v: SchemeValue): Boolean = v match
    case SchemeBool(false) => true
    case _                 => false

  private def evalBuiltin(
    op: String,
    args: List[SchemeValue]
  ): SchemeValue = op match
    case "+" => SchemeInt(args.map(asInt).sum)
    case "-" =>
      if args.isEmpty then throw new EvalError("-: expected at least 1 argument")
      else if args.length == 1 then SchemeInt(-asInt(args.head))
      else SchemeInt(args.map(asInt).reduceLeft(_ - _))
    case "*" => SchemeInt(args.map(asInt).product)
    case "/" =>
      if args.length < 2 then throw new EvalError("/: expected at least 2 arguments")
      else SchemeInt(args.map(asInt).reduceLeft(_ / _))
    case "<"  => compareOp(args, _ < _)
    case ">"  => compareOp(args, _ > _)
    case "="  => compareOp(args, _ == _)
    case "<=" => compareOp(args, _ <= _)
    case ">=" => compareOp(args, _ >= _)
    case _    => throw new EvalError(s"unknown procedure: $op")

  private def compareOp(
    args: List[SchemeValue],
    cmp: (Long, Long) => Boolean
  ): SchemeValue =
    if args.length != 2 then throw new EvalError("comparison: expected 2 arguments")
    SchemeBool(cmp(asInt(args.head), asInt(args(1))))

  private def asInt(v: SchemeValue): Long = v match
    case SchemeInt(n) => n
    case other        => throw new EvalError(s"expected integer, got: ${other.display}")
