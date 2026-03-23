package ming

import SchemeValue.*

/** Core eval logic with trampoline-based TCO. */
object Interpreter:

  /** Result of a special form: either a final value or a tail-call continuation. */
  private enum EvalResult:
    case Done(value: SchemeValue)
    case TailCall(expr: SchemeValue, env: Environment)

  import EvalResult.*

  /** Format an error message with optional position info. */
  private def posMsg(msg: String, pos: Option[SourcePos]): String =
    pos match
      case Some(p) => s"$msg [$p]"
      case None    => msg

  /** Evaluate all but the last expression in a body, returning the last unevaluated. */
  private def evalBodyInit(body: List[SchemeValue], env: Environment): Unit =
    var remaining = body
    while remaining.tail.nonEmpty do
      eval(remaining.head, env)
      remaining = remaining.tail

  def eval(expr0: SchemeValue, env0: Environment): SchemeValue =
    var curExpr: SchemeValue = expr0
    var curEnv: Environment  = env0

    while true do
      curExpr match
        // Self-evaluating
        case IntVal(_, _) | BoolVal(_, _) | StringVal(_, _) | MutableStringVal(_, _) | CharVal(_, _) | PairVal(_, _) |
            LambdaVal(_, _, _) | BuiltinVal(_, _) | Void =>
          return curExpr

        // Symbol lookup
        case SymbolVal(name, pos) =>
          return curEnv.get(name).getOrElse(throw new EvalError(posMsg(s"unbound variable: $name", pos)))

        // Empty application
        case ListVal(Nil, pos) =>
          throw new EvalError(posMsg("empty application", pos))

        // Special forms
        case ListVal(SymbolVal("if", _) :: args, pos) =>
          evalIf(args, pos, curEnv) match
            case Done(v)          => return v
            case TailCall(e, env) => curExpr = e; curEnv = env

        case ListVal(SymbolVal("define", _) :: args, pos) =>
          return evalDefine(args, pos, curEnv)

        case ListVal(SymbolVal("quote", _) :: args, pos) =>
          return evalQuote(args, pos)

        case ListVal(SymbolVal("lambda", _) :: args, pos) =>
          return evalLambda(args, pos, curEnv)

        case ListVal(SymbolVal("set!", _) :: args, pos) =>
          return evalSet(args, pos, curEnv)

        case ListVal(SymbolVal("begin", _) :: args, _) =>
          if args.isEmpty then return Void
          evalBodyInit(args, curEnv)
          curExpr = args.last

        case ListVal(SymbolVal("and", _) :: args, _) =>
          evalAnd(args, curEnv) match
            case Done(v)          => return v
            case TailCall(e, env) => curExpr = e; curEnv = env

        case ListVal(SymbolVal("or", _) :: args, _) =>
          evalOr(args, curEnv) match
            case Done(v)          => return v
            case TailCall(e, env) => curExpr = e; curEnv = env

        case ListVal(SymbolVal("let", _) :: args, pos) =>
          evalLet(args, pos, curEnv) match
            case Done(v)          => return v
            case TailCall(e, env) => curExpr = e; curEnv = env

        case ListVal(SymbolVal("cond", _) :: args, _) =>
          evalCond(args, curEnv) match
            case Done(v)          => return v
            case TailCall(e, env) => curExpr = e; curEnv = env

        // Procedure application
        case ListVal(head :: args, pos) =>
          evalApplication(head, args, pos, curEnv) match
            case Done(v)          => return v
            case TailCall(e, env) => curExpr = e; curEnv = env

    // Unreachable but needed for type checker
    throw new AssertionError("unreachable")

  def applyProc(
    proc: SchemeValue,
    args: List[SchemeValue],
    callPos: Option[SourcePos] = None
  ): SchemeValue =
    proc match
      case LambdaVal(params, body, closure) =>
        if args.length != params.length then
          throw new EvalError(
            posMsg(s"wrong number of arguments: expected ${params.length}, got ${args.length}", callPos)
          )
        val childEnv = closure.child()
        params.zip(args).foreach((p, v) => childEnv.define(p, v))
        evalBodyInit(body, childEnv)
        eval(body.last, childEnv)
      case BuiltinVal(_, func) =>
        try func(args)
        catch
          case e: EvalError =>
            if callPos.isDefined && !e.getMessage.matches(".*\\d+:\\d+.*") then
              throw new EvalError(posMsg(e.getMessage, callPos))
            else throw e
      case _ =>
        throw new EvalError(posMsg("not a procedure", callPos))

  private def evalIf(args: List[SchemeValue], pos: Option[SourcePos], env: Environment): EvalResult =
    args match
      case cond :: thenBr :: elseBr :: Nil =>
        if eval(cond, env).isTruthy then TailCall(thenBr, env)
        else TailCall(elseBr, env)
      case cond :: thenBr :: Nil =>
        if eval(cond, env).isTruthy then TailCall(thenBr, env)
        else Done(Void)
      case _ => throw new EvalError(posMsg("if: bad syntax", pos))

  private def evalAnd(args: List[SchemeValue], env: Environment): EvalResult =
    args match
      case Nil         => Done(BoolVal(true))
      case last :: Nil => TailCall(last, env)
      case _ =>
        var remaining = args
        while remaining.tail.nonEmpty do
          val v = eval(remaining.head, env)
          if !v.isTruthy then return Done(v)
          remaining = remaining.tail
        TailCall(remaining.head, env)

  private def evalOr(args: List[SchemeValue], env: Environment): EvalResult =
    args match
      case Nil         => Done(BoolVal(false))
      case last :: Nil => TailCall(last, env)
      case _ =>
        var remaining = args
        while remaining.tail.nonEmpty do
          val v = eval(remaining.head, env)
          if v.isTruthy then return Done(v)
          remaining = remaining.tail
        TailCall(remaining.head, env)

  private def evalLet(
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
            childEnv.define(n, eval(value, env))
          case _ => throw new EvalError(posMsg("let: bad binding", pos))
        }
        evalBodyInit(body, childEnv)
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
      case ListVal(_ :: v :: Nil, _) => eval(v, env)
      case _                         => throw new EvalError(posMsg("let: bad binding", pos))
    }
    val childEnv = env.child()
    val lambda   = LambdaVal(paramNames, body, childEnv)
    childEnv.define(name, lambda)
    if initVals.length != paramNames.length then
      throw new EvalError(
        posMsg(s"wrong number of arguments: expected ${paramNames.length}, got ${initVals.length}", pos)
      )
    val callEnv = childEnv.child()
    paramNames.zip(initVals).foreach((p, v) => callEnv.define(p, v))
    evalBodyInit(body, callEnv)
    TailCall(body.last, callEnv)

  private def evalCond(clauses: List[SchemeValue], env: Environment): EvalResult =
    var remaining = clauses
    while remaining.nonEmpty do
      remaining.head match
        case ListVal(SymbolVal("else", _) :: body, _) =>
          if body.isEmpty then throw new EvalError("cond: else with no body")
          evalBodyInit(body, env)
          return TailCall(body.last, env)
        case ListVal(test :: body, _) =>
          val result = eval(test, env)
          if result.isTruthy then
            if body.isEmpty then return Done(result)
            evalBodyInit(body, env)
            return TailCall(body.last, env)
          remaining = remaining.tail
        case _ => throw new EvalError("cond: bad clause")
    Done(Void)

  private def evalApplication(
    head: SchemeValue,
    args: List[SchemeValue],
    pos: Option[SourcePos],
    env: Environment
  ): EvalResult =
    val proc       = eval(head, env)
    val evaledArgs = args.map(eval(_, env))
    proc match
      case LambdaVal(params, body, closure) =>
        if evaledArgs.length != params.length then
          throw new EvalError(
            posMsg(s"wrong number of arguments: expected ${params.length}, got ${evaledArgs.length}", pos)
          )
        val childEnv = closure.child()
        params.zip(evaledArgs).foreach((p, v) => childEnv.define(p, v))
        evalBodyInit(body, childEnv)
        TailCall(body.last, childEnv)
      case BuiltinVal(_, func) =>
        try Done(func(evaledArgs))
        catch
          case e: EvalError =>
            if pos.isDefined && !e.getMessage.matches(".*\\d+:\\d+.*") then
              throw new EvalError(posMsg(e.getMessage, pos))
            else throw e
      case _ =>
        throw new EvalError(posMsg("not a procedure", pos))

  private def evalSet(args: List[SchemeValue], pos: Option[SourcePos], env: Environment): SchemeValue =
    args match
      case SymbolVal(name, namePos) :: value :: Nil =>
        val v = eval(value, env)
        if !env.set(name, v) then throw new EvalError(posMsg(s"set!: unbound variable: $name", namePos.orElse(pos)))
        Void
      case _ => throw new EvalError(posMsg("set!: bad syntax", pos))

  private def evalDefine(args: List[SchemeValue], pos: Option[SourcePos], env: Environment): SchemeValue =
    args match
      case SymbolVal(name, _) :: value :: Nil =>
        env.define(name, eval(value, env))
        Void
      case ListVal(SymbolVal(name, _) :: params, _) :: body =>
        val paramNames = params.map {
          case SymbolVal(p, _) => p
          case _               => throw new EvalError(posMsg("define: expected parameter name", pos))
        }
        env.define(name, LambdaVal(paramNames, body, env))
        Void
      case _ => throw new EvalError(posMsg("define: bad syntax", pos))

  private def evalQuote(args: List[SchemeValue], pos: Option[SourcePos]): SchemeValue =
    args match
      case expr :: Nil => expr
      case _           => throw new EvalError(posMsg("quote: requires 1 argument", pos))

  private def evalLambda(args: List[SchemeValue], pos: Option[SourcePos], env: Environment): SchemeValue =
    args match
      case ListVal(params, _) :: body if body.nonEmpty =>
        val paramNames = params.map {
          case SymbolVal(p, _) => p
          case _               => throw new EvalError(posMsg("lambda: expected parameter name", pos))
        }
        LambdaVal(paramNames, body, env)
      case _ => throw new EvalError(posMsg("lambda: bad syntax", pos))
