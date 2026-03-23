package ming

import SchemeValue.*

/** Core eval logic. */
object Interpreter:

  /** Format an error message with optional position info. */
  private def posMsg(msg: String, pos: Option[SourcePos]): String =
    pos match
      case Some(p) => s"$msg [$p]"
      case None    => msg

  def eval(expr: SchemeValue, env: Environment): SchemeValue =
    expr match
      case IntVal(_, _) | BoolVal(_, _) | StringVal(_, _) | MutableStringVal(_, _) | CharVal(_, _) | PairVal(_, _) |
          LambdaVal(_, _, _) | BuiltinVal(_, _) | Void =>
        expr
      case SymbolVal(name, pos) =>
        env.get(name).getOrElse(throw new EvalError(posMsg(s"unbound variable: $name", pos)))
      case ListVal(Nil, pos) =>
        throw new EvalError(posMsg("empty application", pos))
      case ListVal(SymbolVal(op, _) :: args, pos) if isSpecial(op) =>
        evalSpecial(op, args, pos, env)
      case ListVal(head :: args, pos) =>
        val proc       = eval(head, env)
        val evaledArgs = args.map(eval(_, env))
        applyProc(proc, evaledArgs, pos)

  private def isSpecial(op: String): Boolean =
    op match
      case "and" | "or" | "if" | "define" | "quote" | "lambda" | "let" | "begin" | "cond" => true
      case _                                                                              => false

  private def evalSpecial(
    op: String,
    args: List[SchemeValue],
    pos: Option[SourcePos],
    env: Environment
  ): SchemeValue =
    op match
      case "and"    => evalAnd(args, env)
      case "or"     => evalOr(args, env)
      case "if"     => evalIf(args, pos, env)
      case "define" => evalDefine(args, pos, env)
      case "quote"  => evalQuote(args, pos)
      case "lambda" => evalLambda(args, pos, env)
      case "let"    => evalLet(args, pos, env)
      case "begin"  => evalBegin(args, env)
      case "cond"   => evalCond(args, env)
      case _        => throw new EvalError(posMsg(s"unknown special form: $op", pos))

  private def evalIf(args: List[SchemeValue], pos: Option[SourcePos], env: Environment): SchemeValue =
    args match
      case cond :: thenBr :: elseBr :: Nil =>
        if eval(cond, env).isTruthy then eval(thenBr, env)
        else eval(elseBr, env)
      case cond :: thenBr :: Nil =>
        if eval(cond, env).isTruthy then eval(thenBr, env)
        else Void
      case _ => throw new EvalError(posMsg("if: bad syntax", pos))

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

  def applyProc(proc: SchemeValue, args: List[SchemeValue], callPos: Option[SourcePos] = None): SchemeValue =
    proc match
      case LambdaVal(params, body, closure) =>
        if args.length != params.length then
          throw new EvalError(
            posMsg(s"wrong number of arguments: expected ${params.length}, got ${args.length}", callPos)
          )
        val childEnv = closure.child()
        params.zip(args).foreach((p, v) => childEnv.define(p, v))
        body.map(eval(_, childEnv)).last
      case BuiltinVal(_, func) =>
        try func(args)
        catch
          case e: EvalError =>
            // Re-throw with position if it doesn't already have one
            if callPos.isDefined && !e.getMessage.matches(".*\\d+:\\d+.*") then
              throw new EvalError(posMsg(e.getMessage, callPos))
            else throw e
      case _ =>
        throw new EvalError(posMsg("not a procedure", callPos))

  private def evalAnd(args: List[SchemeValue], env: Environment): SchemeValue =
    args match
      case Nil         => BoolVal(true)
      case last :: Nil => eval(last, env)
      case head :: tail =>
        val v = eval(head, env)
        if !v.isTruthy then v else evalAnd(tail, env)

  private def evalOr(args: List[SchemeValue], env: Environment): SchemeValue =
    args match
      case Nil         => BoolVal(false)
      case last :: Nil => eval(last, env)
      case head :: tail =>
        val v = eval(head, env)
        if v.isTruthy then v else evalOr(tail, env)

  private def evalLet(args: List[SchemeValue], pos: Option[SourcePos], env: Environment): SchemeValue =
    args match
      // Named let: (let name ((var init) ...) body ...)
      case SymbolVal(name, _) :: ListVal(bindings, _) :: body if body.nonEmpty =>
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
        applyProc(lambda, initVals, pos)
      // Regular let: (let ((var init) ...) body ...)
      case ListVal(bindings, _) :: body if body.nonEmpty =>
        val childEnv = env.child()
        bindings.foreach {
          case ListVal(SymbolVal(n, _) :: value :: Nil, _) =>
            childEnv.define(n, eval(value, env))
          case _ => throw new EvalError(posMsg("let: bad binding", pos))
        }
        body.map(eval(_, childEnv)).last
      case _ => throw new EvalError(posMsg("let: bad syntax", pos))

  private def evalBegin(args: List[SchemeValue], env: Environment): SchemeValue =
    if args.isEmpty then Void
    else args.map(eval(_, env)).last

  private def evalCond(args: List[SchemeValue], env: Environment): SchemeValue =
    args match
      case Nil => Void
      case ListVal(SymbolVal("else", _) :: body, _) :: _ =>
        if body.isEmpty then throw new EvalError("cond: else with no body")
        body.map(eval(_, env)).last
      case ListVal(test :: body, _) :: rest =>
        val result = eval(test, env)
        if result.isTruthy then
          if body.isEmpty then result
          else body.map(eval(_, env)).last
        else evalCond(rest, env)
      case _ => throw new EvalError("cond: bad clause")
