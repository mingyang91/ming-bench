package ming

import SchemeValue.*

/** Scheme interpreter entry point. Agents implement this object. */
object Evaluator:

  /** Evaluate one or more Scheme expressions and return the string representation of the last result. */
  def evalStr(input: String): String =
    evalStrWithOutput(input)._1

  /** Evaluate Scheme expressions and return both the result string and any captured output. */
  def evalStrWithOutput(input: String): (String, String) =
    val exprs = Parser.parse(input)
    if exprs.isEmpty then throw new EvalError("empty input")
    val (lastVal, _, output) = evalSequence(exprs, Env.empty)
    (lastVal.display, output)

  private[ming] def evalSequence(
    exprs: List[SchemeValue],
    env: Env
  ): (SchemeValue, Env, String) =
    val (defines, body) = collectDefines(exprs, Nil)
    val bodyEnv =
      if defines.isEmpty then env
      else
        val frame = new Env.LetrecFrame(defines, env)
        frame.init()
        frame
    evalSequenceSimple(body, bodyEnv, "")

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
    env: Env,
    accOutput: String
  ): (SchemeValue, Env, String) =
    exprs match
      case Nil => (SchemeVoid, env, accOutput)
      case last :: Nil =>
        val (v, e, o) = evalWithEnv(last, env)
        (v, e, accOutput + o)
      case head :: tail =>
        val (_, nextEnv, o) = evalWithEnv(head, env)
        evalSequenceSimple(tail, nextEnv, accOutput + o)

  private[ming] def evalWithEnv(
    expr: SchemeValue,
    env: Env
  ): (SchemeValue, Env, String) = expr match
    case SchemeInt(_)          => (expr, env, "")
    case SchemeBool(_)         => (expr, env, "")
    case SchemeString(_)       => (expr, env, "")
    case SchemeChar(_)         => (expr, env, "")
    case SchemeVoid            => (expr, env, "")
    case SchemeLambda(_, _, _) => (expr, env, "")
    case SchemeSymbol(name) =>
      try (env.lookup(name), env, "")
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
        val (proc, _, headOut)    = evalWithEnv(head, env)
        val (evaledArgs, argsOut) = evalArgs(args, env)
        val (result, bodyOut)     = applyProc(proc, evaledArgs)
        (result, env, headOut + argsOut + bodyOut)
      catch
        case e: EvalError if e.sourcePos == SourcePos.None =>
          throw new EvalError(e.baseMessage, expr.pos)
    case other =>
      throw new EvalError(s"cannot evaluate: ${other.display}", other.pos)

  private[ming] def eval(expr: SchemeValue, env: Env): SchemeValue =
    evalWithEnv(expr, env)._1

  private[ming] def evalArgs(
    args: List[SchemeValue],
    env: Env
  ): (List[SchemeValue], String) =
    val (reversedVals, out) = args.foldLeft((List.empty[SchemeValue], "")) { case ((vals, accOut), arg) =>
      val (v, _, o) = evalWithEnv(arg, env)
      (v :: vals, accOut + o)
    }
    (reversedVals.reverse, out)

  private def evalSpecialOrCall(
    op: String,
    args: List[SchemeValue],
    env: Env
  ): (SchemeValue, Env, String) = op match
    case "define" => evalDefine(args, env)
    case "if"     => evalIf(args, env)
    case "quote"  => evalQuote(args, env)
    case "lambda" => (evalLambda(args, env), env, "")
    case "and"    => evalAnd(args, env)
    case "or"     => evalOr(args, env)
    case "not"    => evalNot(args, env)
    case "let"    => evalLet(args, env)
    case "begin"  => evalBegin(args, env)
    case "cond"   => evalCond(args, env)
    case _ =>
      val (evaledArgs, argsOut) = evalArgs(args, env)
      env.get(op) match
        case Some(proc) =>
          val (result, bodyOut) = applyProc(proc, evaledArgs)
          (result, env, argsOut + bodyOut)
        case None =>
          val (result, builtinOut) = Builtins.evalBuiltin(op, evaledArgs)
          (result, env, argsOut + builtinOut)

  private def applyProc(
    proc: SchemeValue,
    args: List[SchemeValue]
  ): (SchemeValue, String) = proc match
    case SchemeLambda(params, body, closure) =>
      if params.length != args.length then
        throw new EvalError(
          s"expected ${params.length} arguments, got ${args.length}"
        )
      val localEnv            = Env.Frame(params.zip(args).toMap, closure)
      val (result, _, output) = evalSequence(body, localEnv)
      (result, output)
    case other =>
      throw new EvalError(s"not a procedure: ${other.display}")

  private def evalDefine(
    args: List[SchemeValue],
    env: Env
  ): (SchemeValue, Env, String) = args match
    case SchemeSymbol(name) :: valueExpr :: Nil =>
      val (v, _, o) = evalWithEnv(valueExpr, env)
      (SchemeVoid, env.extend(name, v), o)
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
      (SchemeVoid, recEnv, "")
    case _ => throw new EvalError("bad define syntax")

  private def evalIf(
    args: List[SchemeValue],
    env: Env
  ): (SchemeValue, Env, String) =
    args match
      case cond :: thenBranch :: elseBranch :: Nil =>
        val (cv, _, co) = evalWithEnv(cond, env)
        val (rv, _, ro) =
          if isFalsy(cv) then evalWithEnv(elseBranch, env)
          else evalWithEnv(thenBranch, env)
        (rv, env, co + ro)
      case cond :: thenBranch :: Nil =>
        val (cv, _, co) = evalWithEnv(cond, env)
        if isFalsy(cv) then (SchemeVoid, env, co)
        else
          val (rv, _, ro) = evalWithEnv(thenBranch, env)
          (rv, env, co + ro)
      case _ => throw new EvalError("if: bad syntax")

  private def evalQuote(
    args: List[SchemeValue],
    env: Env
  ): (SchemeValue, Env, String) =
    if args.length != 1 then throw new EvalError("quote: expected 1 argument")
    (args.head, env, "")

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
  ): (SchemeValue, Env, String) = args match
    case Nil => (SchemeBool(true), env, "")
    case last :: Nil =>
      val (v, _, o) = evalWithEnv(last, env)
      (v, env, o)
    case head :: tail =>
      val (v, _, o) = evalWithEnv(head, env)
      if isFalsy(v) then (v, env, o)
      else
        val (rv, re, ro) = evalAnd(tail, env)
        (rv, re, o + ro)

  private def evalOr(
    args: List[SchemeValue],
    env: Env
  ): (SchemeValue, Env, String) = args match
    case Nil => (SchemeBool(false), env, "")
    case last :: Nil =>
      val (v, _, o) = evalWithEnv(last, env)
      (v, env, o)
    case head :: tail =>
      val (v, _, o) = evalWithEnv(head, env)
      if isFalsy(v) then
        val (rv, re, ro) = evalOr(tail, env)
        (rv, re, o + ro)
      else (v, env, o)

  private def evalNot(
    args: List[SchemeValue],
    env: Env
  ): (SchemeValue, Env, String) =
    if args.length != 1 then throw new EvalError("not: expected 1 argument")
    val (v, _, o) = evalWithEnv(args.head, env)
    (SchemeBool(isFalsy(v)), env, o)

  private def evalLet(
    args: List[SchemeValue],
    env: Env
  ): (SchemeValue, Env, String) =
    SpecialForms.evalLet(args, env)

  private def evalBegin(
    args: List[SchemeValue],
    env: Env
  ): (SchemeValue, Env, String) =
    evalSequence(args, env)

  private def evalCond(
    args: List[SchemeValue],
    env: Env
  ): (SchemeValue, Env, String) =
    SpecialForms.evalCond(args, env)

  private[ming] def isFalsy(v: SchemeValue): Boolean = v match
    case SchemeBool(false) => true
    case _                 => false
