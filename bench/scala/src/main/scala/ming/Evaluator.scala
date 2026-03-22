package ming

import SchemeValue.*

/** Scheme interpreter entry point with trampoline-based TCO. */
object Evaluator:

  // --- Trampoline types (package-private for SpecialForms) ---
  sealed private[ming] trait EvalResult
  private[ming] case class Done(value: SchemeValue, env: Env, output: String) extends EvalResult

  private[ming] case class Bounce(
    expr: SchemeValue,
    env: Env,
    accOutput: String
  ) extends EvalResult

  // --- Public API ---

  def evalStr(input: String): String =
    evalStrWithOutput(input)._1

  def evalStrWithOutput(input: String): (String, String) =
    val exprs = Parser.parse(input)
    if exprs.isEmpty then throw new EvalError("empty input")
    val (lastVal, _, output) = evalSequence(exprs, Env.empty)
    (lastVal.display, output)

  // --- Sequence evaluation (top-level / body with defines) ---

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
    evalSeqLoop(body, bodyEnv, "")

  @scala.annotation.tailrec
  private def collectDefines(
    exprs: List[SchemeValue],
    acc: List[(String, List[String], List[SchemeValue])]
  ): (
    List[(String, List[String], List[SchemeValue])],
    List[SchemeValue]
  ) =
    exprs match
      case (defExpr @ SchemeList(
            SchemeSymbol("define") :: rest
          )) :: tail =>
        rest match
          case SchemeSymbol(name) :: valueExpr :: Nil =>
            collectDefines(tail, (name, Nil, List(valueExpr)) :: acc)
          case SchemeList(SchemeSymbol(name) :: params) :: body =>
            val paramNames = params.map {
              case SchemeSymbol(n) => n
              case other =>
                throw new EvalError(
                  s"bad parameter: ${other.display}"
                )
            }
            collectDefines(
              tail,
              (name, paramNames, body) :: acc
            )
          case _ =>
            throw new EvalError("bad define syntax", defExpr.pos)
      case _ => (acc.reverse, exprs)

  @scala.annotation.tailrec
  private def evalSeqLoop(
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
        evalSeqLoop(tail, nextEnv, accOutput + o)

  // --- Trampoline-based eval ---

  private[ming] def evalWithEnv(
    expr: SchemeValue,
    env: Env
  ): (SchemeValue, Env, String) =
    trampoline(evalOnce(expr, env, ""))

  @scala.annotation.tailrec
  private def trampoline(
    result: EvalResult
  ): (SchemeValue, Env, String) = result match
    case Done(v, e, o)          => (v, e, o)
    case Bounce(expr, env, acc) => trampoline(evalOnce(expr, env, acc))

  private[ming] def eval(
    expr: SchemeValue,
    env: Env
  ): SchemeValue =
    evalWithEnv(expr, env)._1

  private[ming] def evalArgs(
    args: List[SchemeValue],
    env: Env
  ): (List[SchemeValue], String) =
    val (reversedVals, out) =
      args.foldLeft((List.empty[SchemeValue], "")) { case ((vals, accOut), arg) =>
        val (v, _, o) = evalWithEnv(arg, env)
        (v :: vals, accOut + o)
      }
    (reversedVals.reverse, out)

  // --- Single evaluation step (returns Bounce for tail positions) ---

  private def evalOnce(
    expr: SchemeValue,
    env: Env,
    accOut: String
  ): EvalResult = expr match
    case SchemeInt(_)           => Done(expr, env, accOut)
    case SchemeBool(_)          => Done(expr, env, accOut)
    case SchemeString(_)        => Done(expr, env, accOut)
    case _: SchemeMutableString => Done(expr, env, accOut)
    case SchemeChar(_)          => Done(expr, env, accOut)
    case SchemeVoid             => Done(expr, env, accOut)
    case SchemeLambda(_, _, _)  => Done(expr, env, accOut)
    case SchemeSymbol(name) =>
      try Done(env.lookup(name), env, accOut)
      catch
        case e: EvalError if e.sourcePos == SourcePos.None =>
          throw new EvalError(e.baseMessage, expr.pos)
    case SchemeList(Nil) =>
      throw new EvalError("empty application", expr.pos)
    case SchemeList(SchemeSymbol(op) :: args) =>
      try evalSpecialOrCallOnce(op, args, env, accOut)
      catch
        case e: EvalError if e.sourcePos == SourcePos.None =>
          throw new EvalError(e.baseMessage, expr.pos)
    case SchemeList(head :: args) =>
      try
        val (proc, _, ho)       = evalWithEnv(head, env)
        val (evaledArgs, aoStr) = evalArgs(args, env)
        applyProcTail(proc, evaledArgs, accOut + ho + aoStr)
      catch
        case e: EvalError if e.sourcePos == SourcePos.None =>
          throw new EvalError(e.baseMessage, expr.pos)
    case other =>
      throw new EvalError(
        s"cannot evaluate: ${other.display}",
        other.pos
      )

  // --- Special form / call dispatch (tail-aware) ---

  private def evalSpecialOrCallOnce(
    op: String,
    args: List[SchemeValue],
    env: Env,
    accOut: String
  ): EvalResult = op match
    case "define" =>
      val (v, e, o) = SpecialForms.evalDefine(args, env)
      Done(v, e, accOut + o)
    case "if"     => SpecialForms.evalIfOnce(args, env, accOut)
    case "quote"  => SpecialForms.evalQuoteOnce(args, env, accOut)
    case "lambda" => Done(SpecialForms.evalLambda(args, env), env, accOut)
    case "and"    => SpecialForms.evalAndOnce(args, env, accOut)
    case "or"     => SpecialForms.evalOrOnce(args, env, accOut)
    case "not"    => SpecialForms.evalNotOnce(args, env, accOut)
    case "let"    => SpecialForms.evalLetOnce(args, env, accOut)
    case "begin"  => SpecialForms.evalBeginOnce(args, env, accOut)
    case "cond"   => SpecialForms.evalCondOnce(args, env, accOut)
    case _ =>
      val (evaledArgs, ao) = evalArgs(args, env)
      env.get(op) match
        case Some(proc) =>
          applyProcTail(proc, evaledArgs, accOut + ao)
        case None =>
          val (result, bo) =
            Builtins.evalBuiltin(op, evaledArgs)
          Done(result, env, accOut + ao + bo)

  // --- Procedure application (tail-aware) ---

  private def applyProcTail(
    proc: SchemeValue,
    args: List[SchemeValue],
    accOut: String
  ): EvalResult = proc match
    case SchemeLambda(params, body, closure) =>
      if params.length != args.length then
        throw new EvalError(
          s"expected ${params.length} arguments, got ${args.length}"
        )
      val localEnv = Env.Frame(params.zip(args).toMap, closure)
      evalSequenceOnce(body, localEnv, accOut)
    case other =>
      throw new EvalError(s"not a procedure: ${other.display}")

  // --- Tail-aware sequence (with defines) ---

  private[ming] def evalSequenceOnce(
    exprs: List[SchemeValue],
    env: Env,
    accOut: String
  ): EvalResult =
    val (defines, body) = collectDefines(exprs, Nil)
    val bodyEnv =
      if defines.isEmpty then env
      else
        val frame = new Env.LetrecFrame(defines, env)
        frame.init()
        frame
    evalBodyOnce(body, bodyEnv, accOut)

  @scala.annotation.tailrec
  private[ming] def evalBodyOnce(
    exprs: List[SchemeValue],
    env: Env,
    accOut: String
  ): EvalResult =
    exprs match
      case Nil         => Done(SchemeVoid, env, accOut)
      case last :: Nil => Bounce(last, env, accOut)
      case head :: tail =>
        val (_, nextEnv, o) = evalWithEnv(head, env)
        evalBodyOnce(tail, nextEnv, accOut + o)
