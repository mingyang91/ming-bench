package ming

import scala.annotation.tailrec

/** Core evaluation logic for the Scheme interpreter. */
object Interpreter:

  import SchemeValue.*

  private[ming] enum EvalResult:
    case Done(value: SchemeValue, env: Environment, output: String)
    case Bounce(expr: SchemeValue, env: Environment, output: String)

  /** Recursively strip all SchemeLocated wrappers from a value. */
  private def strip(v: SchemeValue): SchemeValue = v match
    case SchemeLocated(inner, _, _) => strip(inner)
    case SchemeList(elems)          => SchemeList(elems.map(strip))
    case other                      => other

  private[ming] def prependOut(r: EvalResult, prefix: String): EvalResult =
    if prefix.isEmpty then r
    else
      r match
        case EvalResult.Done(v, e, o)   => EvalResult.Done(v, e, prefix + o)
        case EvalResult.Bounce(x, e, o) => EvalResult.Bounce(x, e, prefix + o)

  /** Evaluate an expression, returning the result, environment, and output. */
  def eval(
    expr: SchemeValue,
    env: Environment
  ): (SchemeValue, Environment, String) =
    @tailrec
    def trampoline(result: EvalResult, accOut: String): (SchemeValue, Environment, String) =
      result match
        case EvalResult.Done(v, e, o) => (v, e, accOut + o)
        case EvalResult.Bounce(nextExpr, nextEnv, o) =>
          trampoline(evalStep(nextExpr, nextEnv), accOut + o)
    trampoline(evalStep(expr, env), "")

  /** One evaluation step — may return Bounce for tail-position expressions. */
  private def evalStep(
    expr: SchemeValue,
    env: Environment
  ): EvalResult =
    expr match
      case SchemeInt(_) | SchemeBool(_) | SchemeString(_) | _: SchemeMutableString | SchemeNil | SchemeVoid |
          _: SchemeLambda | SchemeChar(_) =>
        EvalResult.Done(expr, env, "")
      case SchemeLocated(inner, line, col) =>
        try evalStep(inner, env)
        catch
          case e: EvalError if !e.hasPosition =>
            throw new EvalError(
              s"$line:$col: ${e.getMessage}",
              hasPosition = true
            )
      case SchemeSymbol(name) =>
        env.lookup(name) match
          case Some(v) => EvalResult.Done(v, env, "")
          case None =>
            throw new EvalError(s"unbound variable: $name")
      case SchemeList(elements) => evalListStep(elements.map(strip), env)

  private def evalListStep(
    elements: List[SchemeValue],
    env: Environment
  ): EvalResult =
    elements match
      case Nil                            => EvalResult.Done(SchemeNil, env, "")
      case SchemeSymbol("define") :: rest => evalDefineStep(rest, env)
      case SchemeSymbol("if") :: rest     => evalIfStep(rest, env)
      case SchemeSymbol("quote") :: rest  => EvalResult.Done(evalQuote(rest), env, "")
      case SchemeSymbol("and") :: args    => evalAndStep(args, env)
      case SchemeSymbol("or") :: args     => evalOrStep(args, env)
      case SchemeSymbol("lambda") :: rest =>
        EvalResult.Done(evalLambda(rest, env), env, "")
      case SchemeSymbol("let") :: rest   => SpecialForms.evalLetStep(rest, env)
      case SchemeSymbol("begin") :: rest => evalBeginStep(rest, env, "")
      case SchemeSymbol("cond") :: rest  => SpecialForms.evalCondStep(rest, env, "")
      case head :: args =>
        val (func, _, funcOut)       = eval(head, env)
        val (evaluatedArgs, argsOut) = evalArgs(args, env)
        applyProcStep(func, evaluatedArgs, env, funcOut + argsOut)

  private[ming] def applyProcStep(
    func: SchemeValue,
    args: List[SchemeValue],
    callerEnv: Environment,
    prefixOut: String
  ): EvalResult =
    func match
      case lam @ SchemeLambda(params, body, closure, nameOpt) =>
        if params.length != args.length then
          throw new EvalError(
            s"expected ${params.length} args, got ${args.length}"
          )
        val closureWithSelf = nameOpt match
          case Some(n) => closure.define(n, lam)
          case None    => closure
        val innerEnv = closureWithSelf
          .extend(params, args)
          .copy(fallback = Some(callerEnv))
        evalBodyBounce(body, innerEnv, prefixOut)
      case _ =>
        val (result, resultOut) = Builtins.applyProc(func, args)
        EvalResult.Done(result, callerEnv, prefixOut + resultOut)

  @tailrec
  private[ming] def evalBodyBounce(
    body: List[SchemeValue],
    env: Environment,
    accOut: String
  ): EvalResult =
    body match
      case Nil         => EvalResult.Done(SchemeVoid, env, accOut)
      case last :: Nil => EvalResult.Bounce(last, env, accOut)
      case head :: tail =>
        val (_, newEnv, o) = eval(head, env)
        evalBodyBounce(tail, newEnv, accOut + o)

  private def evalDefineStep(
    args: List[SchemeValue],
    env: Environment
  ): EvalResult =
    args match
      case SchemeList(SchemeSymbol(name) :: params) :: body =>
        val paramNames = extractParamNames(params)
        val lambda =
          SchemeLambda(paramNames, body, env, Some(name))
        val newEnv = env.define(name, lambda)
        EvalResult.Done(SchemeVoid, newEnv, "")
      case SchemeSymbol(name) :: valueExpr :: Nil =>
        val (value, _, out) = eval(valueExpr, env)
        val named = value match
          case SchemeLambda(p, b, c, None) =>
            SchemeLambda(p, b, c, Some(name))
          case other => other
        val newEnv = env.define(name, named)
        EvalResult.Done(SchemeVoid, newEnv, out)
      case _ =>
        throw new EvalError("bad define syntax")

  private def extractParamNames(
    params: List[SchemeValue]
  ): List[String] =
    params.map {
      case SchemeSymbol(n) => n
      case other =>
        throw new EvalError(
          s"expected parameter name, got: ${other.display}"
        )
    }

  /** If: evaluate condition, then bounce into the chosen branch. */
  private def evalIfStep(
    args: List[SchemeValue],
    env: Environment
  ): EvalResult =
    args match
      case cond :: thenBranch :: elseBranch =>
        val (condVal, _, condOut) = eval(cond, env)
        val isFalse = condVal match
          case SchemeBool(false) => true
          case _                 => false
        if isFalse then
          elseBranch match
            case elseExpr :: Nil =>
              prependOut(evalStep(elseExpr, env), condOut)
            case Nil => EvalResult.Done(SchemeVoid, env, condOut)
            case _ =>
              throw new EvalError("if: too many arguments")
        else prependOut(evalStep(thenBranch, env), condOut)
      case _ =>
        throw new EvalError("if requires at least 2 arguments")

  private def evalQuote(args: List[SchemeValue]): SchemeValue =
    args match
      case List(value) => value
      case _ =>
        throw new EvalError("quote expects exactly 1 argument")

  /** And: short-circuit, tail-call last argument. */
  private def evalAndStep(
    args: List[SchemeValue],
    env: Environment
  ): EvalResult =
    args match
      case Nil => EvalResult.Done(SchemeBool(true), env, "")
      case last :: Nil =>
        evalStep(last, env)
      case head :: tail =>
        val (v, _, o) = eval(head, env)
        v match
          case SchemeBool(false) => EvalResult.Done(SchemeBool(false), env, o)
          case _ =>
            prependOut(evalAndStep(tail, env), o)

  /** Or: short-circuit, tail-call last argument. */
  private def evalOrStep(
    args: List[SchemeValue],
    env: Environment
  ): EvalResult =
    args match
      case Nil => EvalResult.Done(SchemeBool(false), env, "")
      case last :: Nil =>
        evalStep(last, env)
      case head :: tail =>
        val (result, _, o) = eval(head, env)
        result match
          case SchemeBool(false) =>
            prependOut(evalOrStep(tail, env), o)
          case _ => EvalResult.Done(result, env, o)

  private def evalLambda(
    args: List[SchemeValue],
    env: Environment
  ): SchemeValue =
    args match
      case SchemeList(params) :: body if body.nonEmpty =>
        val paramNames = extractParamNames(params)
        SchemeLambda(paramNames, body, env)
      case _ => throw new EvalError("bad lambda syntax")

  /** Begin: evaluate all-but-last, bounce last. */
  @tailrec
  private def evalBeginStep(
    exprs: List[SchemeValue],
    env: Environment,
    accOut: String
  ): EvalResult =
    exprs match
      case Nil => EvalResult.Done(SchemeVoid, env, accOut)
      case last :: Nil =>
        prependOut(evalStep(last, env), accOut)
      case head :: tail =>
        val (_, newEnv, o) = eval(head, env)
        evalBeginStep(tail, newEnv, accOut + o)

  /** Evaluate a body (list of expressions) fully — no bouncing. Used by external callers. */
  def evalBody(
    body: List[SchemeValue],
    env: Environment
  ): (SchemeValue, String) =
    body match
      case Nil => (SchemeVoid, "")
      case last :: Nil =>
        val (v, _, o) = eval(last, env)
        (v, o)
      case head :: tail =>
        val (_, newEnv, o) = eval(head, env)
        val (v, tailOut)   = evalBody(tail, newEnv)
        (v, o + tailOut)

  private[ming] def evalArgs(
    args: List[SchemeValue],
    env: Environment
  ): (List[SchemeValue], String) =
    val (revVals, out) = args.foldLeft((List.empty[SchemeValue], "")) { case ((vals, accOut), arg) =>
      val (v, _, o) = eval(arg, env)
      (v :: vals, accOut + o)
    }
    (revVals.reverse, out)
