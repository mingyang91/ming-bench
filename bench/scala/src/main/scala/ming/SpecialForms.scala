package ming

import SchemeValue.*
import Evaluator.{Bounce, Done, EvalResult}

/** Tail-aware special forms extracted from Evaluator. */
private[ming] object SpecialForms:

  def evalDefine(
    args: List[SchemeValue],
    env: Env
  ): (SchemeValue, Env, String) = args match
    case SchemeSymbol(name) :: valueExpr :: Nil =>
      val (v, _, o) = Evaluator.evalWithEnv(valueExpr, env)
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

  def evalIfOnce(
    args: List[SchemeValue],
    env: Env,
    accOut: String
  ): EvalResult = args match
    case cond :: thenBranch :: elseBranch :: Nil =>
      val (cv, _, co) = Evaluator.evalWithEnv(cond, env)
      if isFalsy(cv) then Bounce(elseBranch, env, accOut + co)
      else Bounce(thenBranch, env, accOut + co)
    case cond :: thenBranch :: Nil =>
      val (cv, _, co) = Evaluator.evalWithEnv(cond, env)
      if isFalsy(cv) then Done(SchemeVoid, env, accOut + co)
      else Bounce(thenBranch, env, accOut + co)
    case _ => throw new EvalError("if: bad syntax")

  def evalQuoteOnce(
    args: List[SchemeValue],
    env: Env,
    accOut: String
  ): EvalResult =
    if args.length != 1 then throw new EvalError("quote: expected 1 argument")
    Done(args.head, env, accOut)

  def evalLambda(
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

  @scala.annotation.tailrec
  def evalAndOnce(
    args: List[SchemeValue],
    env: Env,
    accOut: String
  ): EvalResult = args match
    case Nil         => Done(SchemeBool(true), env, accOut)
    case last :: Nil => Bounce(last, env, accOut)
    case head :: tail =>
      val (v, _, o) = Evaluator.evalWithEnv(head, env)
      if isFalsy(v) then Done(v, env, accOut + o)
      else evalAndOnce(tail, env, accOut + o)

  @scala.annotation.tailrec
  def evalOrOnce(
    args: List[SchemeValue],
    env: Env,
    accOut: String
  ): EvalResult = args match
    case Nil         => Done(SchemeBool(false), env, accOut)
    case last :: Nil => Bounce(last, env, accOut)
    case head :: tail =>
      val (v, _, o) = Evaluator.evalWithEnv(head, env)
      if isFalsy(v) then evalOrOnce(tail, env, accOut + o)
      else Done(v, env, accOut + o)

  def evalNotOnce(
    args: List[SchemeValue],
    env: Env,
    accOut: String
  ): EvalResult =
    if args.length != 1 then throw new EvalError("not: expected 1 argument")
    val (v, _, o) = Evaluator.evalWithEnv(args.head, env)
    Done(SchemeBool(isFalsy(v)), env, accOut + o)

  def evalLetOnce(
    args: List[SchemeValue],
    env: Env,
    accOut: String
  ): EvalResult = args match
    case SchemeSymbol(name) :: SchemeList(bindings) :: body if body.nonEmpty =>
      val (params, inits)    = parseBindings(bindings)
      val (evaledInits, ioS) = Evaluator.evalArgs(inits, env)
      val recEnv = Env.RecursiveFrame(
        name,
        closure => SchemeLambda(params, body, closure),
        env
      )
      val localEnv = recEnv.extend(params, evaledInits)
      Evaluator.evalSequenceOnce(body, localEnv, accOut + ioS)
    case SchemeList(bindings) :: body if body.nonEmpty =>
      val (params, inits)    = parseBindings(bindings)
      val (evaledInits, ioS) = Evaluator.evalArgs(inits, env)
      val localEnv           = env.extend(params, evaledInits)
      Evaluator.evalSequenceOnce(body, localEnv, accOut + ioS)
    case _ => throw new EvalError("let: bad syntax")

  def evalSetOnce(
    args: List[SchemeValue],
    env: Env,
    accOut: String
  ): EvalResult = args match
    case SchemeSymbol(name) :: valueExpr :: Nil =>
      val (v, _, o) = Evaluator.evalWithEnv(valueExpr, env)
      env.set(name, v)
      Done(SchemeVoid, env, accOut + o)
    case _ => throw new EvalError("set!: bad syntax")

  def evalBeginOnce(
    args: List[SchemeValue],
    env: Env,
    accOut: String
  ): EvalResult =
    Evaluator.evalSequenceOnce(args, env, accOut)

  @scala.annotation.tailrec
  def evalCondOnce(
    clauses: List[SchemeValue],
    env: Env,
    accOut: String
  ): EvalResult = clauses match
    case Nil => Done(SchemeVoid, env, accOut)
    case SchemeList(SchemeSymbol("else") :: body) :: _ =>
      Evaluator.evalBodyOnce(body, env, accOut)
    case SchemeList(test :: body) :: rest =>
      val (v, _, to) = Evaluator.evalWithEnv(test, env)
      if isFalsy(v) then evalCondOnce(rest, env, accOut + to)
      else if body.isEmpty then Done(v, env, accOut + to)
      else Evaluator.evalBodyOnce(body, env, accOut + to)
    case other :: _ =>
      throw new EvalError(s"cond: bad clause: ${other.display}")

  def parseBindings(
    bindings: List[SchemeValue]
  ): (List[String], List[SchemeValue]) =
    bindings.map {
      case SchemeList(SchemeSymbol(name) :: init :: Nil) => (name, init)
      case other =>
        throw new EvalError(s"let: bad binding: ${other.display}")
    }.unzip

  def isFalsy(v: SchemeValue): Boolean = v match
    case SchemeBool(false) => true
    case _                 => false
