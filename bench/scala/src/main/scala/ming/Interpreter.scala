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
      case IntVal(_, _) | BoolVal(_, _) | StringVal(_, _) | PairVal(_, _) | LambdaVal(_, _, _) | BuiltinVal(_, _) |
          Void =>
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

  // --- Builtin operations (exposed for Evaluator to wire into global env) ---

  def arith(
    args: List[SchemeValue],
    op: (Long, Long) => Long,
    identity: Long
  ): SchemeValue =
    IntVal(args.foldLeft(identity) {
      case (acc, IntVal(n, _)) => op(acc, n)
      case _                   => throw new EvalError("arithmetic: expected number")
    })

  def subtractOp(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil                 => throw new EvalError("-: requires at least 1 argument")
      case IntVal(n, _) :: Nil => IntVal(-n)
      case IntVal(first, _) :: rest =>
        IntVal(rest.foldLeft(first) {
          case (acc, IntVal(n, _)) => acc - n
          case _                   => throw new EvalError("-: expected number")
        })
      case _ => throw new EvalError("-: expected number")

  def divideOp(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil => throw new EvalError("/: requires at least 1 argument")
      case IntVal(first, _) :: rest =>
        IntVal(rest.foldLeft(first) {
          case (acc, IntVal(n, _)) =>
            if n == 0 then throw new EvalError("division by zero")
            acc / n
          case _ => throw new EvalError("/: expected number")
        })
      case _ => throw new EvalError("/: expected number")

  def compare(
    args: List[SchemeValue],
    cmp: (Long, Long) => Boolean
  ): SchemeValue =
    args match
      case IntVal(a, _) :: IntVal(b, _) :: Nil => BoolVal(cmp(a, b))
      case _                                   => throw new EvalError("comparison: expected 2 numbers")

  def notOp(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil => BoolVal(!v.isTruthy)
      case _        => throw new EvalError("not: requires 1 argument")

  def consOp(args: List[SchemeValue]): SchemeValue =
    args match
      case car :: cdr :: Nil =>
        cdr match
          case ListVal(es, _) => ListVal(car :: es)
          case _              => PairVal(car, cdr)
      case _ => throw new EvalError("cons: requires 2 arguments")

  def carOp(args: List[SchemeValue]): SchemeValue =
    args match
      case PairVal(car, _) :: Nil       => car
      case ListVal(head :: _, _) :: Nil => head
      case ListVal(Nil, _) :: Nil       => throw new EvalError("car: empty list")
      case _ :: Nil                     => throw new EvalError("car: not a pair")
      case _                            => throw new EvalError("car: requires 1 argument")

  def cdrOp(args: List[SchemeValue]): SchemeValue =
    args match
      case PairVal(_, cdr) :: Nil       => cdr
      case ListVal(_ :: tail, _) :: Nil => ListVal(tail)
      case ListVal(Nil, _) :: Nil       => throw new EvalError("cdr: empty list")
      case _ :: Nil                     => throw new EvalError("cdr: not a pair")
      case _                            => throw new EvalError("cdr: requires 1 argument")

  def nullCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case ListVal(Nil, _) :: Nil => BoolVal(true)
      case _ :: Nil               => BoolVal(false)
      case _                      => throw new EvalError("null?: requires 1 argument")

  def lengthOp(args: List[SchemeValue]): SchemeValue =
    args match
      case ListVal(es, _) :: Nil => IntVal(es.length.toLong)
      case _                     => throw new EvalError("length: expected list")

  def typeCheck(args: List[SchemeValue], pred: SchemeValue => Boolean): SchemeValue =
    args match
      case v :: Nil => BoolVal(pred(v))
      case _        => throw new EvalError("type predicate: requires 1 argument")

  def appendOp(args: List[SchemeValue]): SchemeValue =
    args.foldRight(ListVal(Nil): SchemeValue) {
      case (ListVal(es, _), ListVal(acc, _)) => ListVal(es ++ acc)
      case (ListVal(es, _), acc)             => es.foldRight(acc)((e, a) => consOp(List(e, a)))
      case (other, _)                        => throw new EvalError("append: expected list")
    }

  def pairCheck(args: List[SchemeValue]): SchemeValue =
    args match
      case PairVal(_, _) :: Nil      => BoolVal(true)
      case ListVal(_ :: _, _) :: Nil => BoolVal(true)
      case _ :: Nil                  => BoolVal(false)
      case _                         => throw new EvalError("pair?: requires 1 argument")
