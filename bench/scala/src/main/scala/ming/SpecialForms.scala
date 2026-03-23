package ming

import SchemeValue.*
import Interpreter.EvalResult
import Interpreter.EvalResult.*

/** Special form evaluators extracted from Interpreter. */
object SpecialForms:

  /** Format an error message with optional position info. */
  private def posMsg(msg: String, pos: Option[SourcePos]): String =
    pos match
      case Some(p) => s"$msg [$p]"
      case None    => msg

  def evalLet(
    args: List[SchemeValue],
    pos: Option[SourcePos],
    env: Environment
  ): EvalResult =
    args match
      // Named let: (let name ((var init) ...) body ...)
      case SymbolVal(name, _) :: ListVal(bindings, _) :: body if body.nonEmpty =>
        evalNamedLet(name, bindings, body, pos, env)
      // Regular let: (let ((var init) ...) body ...)
      case ListVal(bindings, _) :: body if body.nonEmpty =>
        val childEnv = env.child()
        bindings.foreach {
          case ListVal(SymbolVal(n, _) :: value :: Nil, _) =>
            childEnv.define(n, Interpreter.eval(value, env))
          case _ => throw new EvalError(posMsg("let: bad binding", pos))
        }
        Interpreter.evalBodyInit(body, childEnv)
        TailCall(body.last, childEnv)
      case _ => throw new EvalError(posMsg("let: bad syntax", pos))

  private def evalNamedLet(
    name: String,
    bindings: List[SchemeValue],
    body: List[SchemeValue],
    pos: Option[SourcePos],
    env: Environment
  ): EvalResult =
    val paramNames = bindings.map {
      case ListVal(SymbolVal(p, _) :: _ :: Nil, _) => p
      case _                                       => throw new EvalError(posMsg("let: bad binding", pos))
    }
    val initVals = bindings.map {
      case ListVal(_ :: v :: Nil, _) => Interpreter.eval(v, env)
      case _                         => throw new EvalError(posMsg("let: bad binding", pos))
    }
    val childEnv = env.child()
    val lambda   = LambdaVal(paramNames, None, body, childEnv)
    childEnv.define(name, lambda)
    if initVals.length != paramNames.length then
      throw new EvalError(
        posMsg(s"wrong number of arguments: expected ${paramNames.length}, got ${initVals.length}", pos)
      )
    val callEnv = childEnv.child()
    paramNames.zip(initVals).foreach((p, v) => callEnv.define(p, v))
    Interpreter.evalBodyInit(body, callEnv)
    TailCall(body.last, callEnv)

  def evalCond(clauses: List[SchemeValue], env: Environment): EvalResult =
    var remaining = clauses
    while remaining.nonEmpty do
      remaining.head match
        case ListVal(SymbolVal("else", _) :: body, _) =>
          if body.isEmpty then throw new EvalError("cond: else with no body")
          Interpreter.evalBodyInit(body, env)
          return TailCall(body.last, env)
        case ListVal(test :: body, _) =>
          val result = Interpreter.eval(test, env)
          if result.isTruthy then
            if body.isEmpty then return Done(result)
            Interpreter.evalBodyInit(body, env)
            return TailCall(body.last, env)
          remaining = remaining.tail
        case _ => throw new EvalError("cond: bad clause")
    Done(Void)

  def evalSet(args: List[SchemeValue], pos: Option[SourcePos], env: Environment): SchemeValue =
    args match
      case SymbolVal(name, namePos) :: value :: Nil =>
        val v = Interpreter.eval(value, env)
        if !env.set(name, v) then throw new EvalError(posMsg(s"set!: unbound variable: $name", namePos.orElse(pos)))
        Void
      case _ => throw new EvalError(posMsg("set!: bad syntax", pos))

  def evalDefine(args: List[SchemeValue], pos: Option[SourcePos], env: Environment): SchemeValue =
    args match
      case SymbolVal(name, _) :: value :: Nil =>
        env.define(name, Interpreter.eval(value, env))
        Void
      case ListVal(SymbolVal(name, _) :: params, _) :: body =>
        val (paramNames, restParam) = parseParams(params, pos)
        env.define(name, LambdaVal(paramNames, restParam, body, env))
        Void
      case _ => throw new EvalError(posMsg("define: bad syntax", pos))

  def evalQuote(args: List[SchemeValue], pos: Option[SourcePos]): SchemeValue =
    args match
      case expr :: Nil => expr
      case _           => throw new EvalError(posMsg("quote: requires 1 argument", pos))

  def evalLambda(
    args: List[SchemeValue],
    pos: Option[SourcePos],
    env: Environment
  ): SchemeValue =
    args match
      case ListVal(params, _) :: body if body.nonEmpty =>
        val (paramNames, restParam) = parseParams(params, pos)
        LambdaVal(paramNames, restParam, body, env)
      case SymbolVal(restName, _) :: body if body.nonEmpty =>
        LambdaVal(Nil, Some(restName), body, env)
      case _ => throw new EvalError(posMsg("lambda: bad syntax", pos))

  /** Parse a parameter list, handling dot notation for rest params. */
  def parseParams(
    params: List[SchemeValue],
    pos: Option[SourcePos]
  ): (List[String], Option[String]) =
    val dotIdx = params.indexWhere {
      case SymbolVal(".", _) => true
      case _                 => false
    }
    if dotIdx < 0 then
      val names = params.map {
        case SymbolVal(p, _) => p
        case _               => throw new EvalError(posMsg("expected parameter name", pos))
      }
      (names, None)
    else
      if dotIdx + 2 != params.length then throw new EvalError(posMsg("bad dot syntax in parameter list", pos))
      val fixed = params.take(dotIdx).map {
        case SymbolVal(p, _) => p
        case _               => throw new EvalError(posMsg("expected parameter name", pos))
      }
      val rest = params(dotIdx + 1) match
        case SymbolVal(p, _) => p
        case _               => throw new EvalError(posMsg("expected parameter name after dot", pos))
      (fixed, Some(rest))
