package ming

import SchemeValue.*

/** Core eval logic. */
object Interpreter:

  def eval(expr: SchemeValue, env: Environment): SchemeValue =
    expr match
      case IntVal(_) | BoolVal(_) | StringVal(_) | LambdaVal(_, _, _) | BuiltinVal(_, _) | Void =>
        expr
      case SymbolVal(name) =>
        env.get(name).getOrElse(throw new EvalError(s"unbound variable: $name"))
      case ListVal(Nil) =>
        throw new EvalError("empty application")
      case ListVal(SymbolVal(op) :: args) if isSpecial(op) =>
        evalSpecial(op, args, env)
      case ListVal(head :: args) =>
        val proc       = eval(head, env)
        val evaledArgs = args.map(eval(_, env))
        applyProc(proc, evaledArgs)

  private def isSpecial(op: String): Boolean =
    op match
      case "and" | "or" | "if" | "define" | "quote" | "lambda" => true
      case _                                                   => false

  private def evalSpecial(
    op: String,
    args: List[SchemeValue],
    env: Environment
  ): SchemeValue =
    op match
      case "and"    => evalAnd(args, env)
      case "or"     => evalOr(args, env)
      case "if"     => evalIf(args, env)
      case "define" => evalDefine(args, env)
      case "quote"  => evalQuote(args)
      case "lambda" => evalLambda(args, env)
      case _        => throw new EvalError(s"unknown special form: $op")

  private def evalIf(args: List[SchemeValue], env: Environment): SchemeValue =
    args match
      case cond :: thenBr :: elseBr :: Nil =>
        if eval(cond, env).isTruthy then eval(thenBr, env)
        else eval(elseBr, env)
      case cond :: thenBr :: Nil =>
        if eval(cond, env).isTruthy then eval(thenBr, env)
        else Void
      case _ => throw new EvalError("if: bad syntax")

  private def evalDefine(args: List[SchemeValue], env: Environment): SchemeValue =
    args match
      case SymbolVal(name) :: value :: Nil =>
        env.define(name, eval(value, env))
        Void
      case ListVal(SymbolVal(name) :: params) :: body =>
        val paramNames = params.map {
          case SymbolVal(p) => p
          case _            => throw new EvalError("define: expected parameter name")
        }
        env.define(name, LambdaVal(paramNames, body, env))
        Void
      case _ => throw new EvalError("define: bad syntax")

  private def evalQuote(args: List[SchemeValue]): SchemeValue =
    args match
      case expr :: Nil => expr
      case _           => throw new EvalError("quote: requires 1 argument")

  private def evalLambda(args: List[SchemeValue], env: Environment): SchemeValue =
    args match
      case ListVal(params) :: body if body.nonEmpty =>
        val paramNames = params.map {
          case SymbolVal(p) => p
          case _            => throw new EvalError("lambda: expected parameter name")
        }
        LambdaVal(paramNames, body, env)
      case _ => throw new EvalError("lambda: bad syntax")

  def applyProc(proc: SchemeValue, args: List[SchemeValue]): SchemeValue =
    proc match
      case LambdaVal(params, body, closure) =>
        if args.length != params.length then
          throw new EvalError(s"wrong number of arguments: expected ${params.length}, got ${args.length}")
        val childEnv = closure.child()
        params.zip(args).foreach((p, v) => childEnv.define(p, v))
        body.map(eval(_, childEnv)).last
      case BuiltinVal(_, func) =>
        func(args)
      case _ =>
        throw new EvalError("not a procedure")

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

  // --- Builtin operations (exposed for Evaluator to wire into global env) ---

  def arith(
    args: List[SchemeValue],
    op: (Long, Long) => Long,
    identity: Long
  ): SchemeValue =
    IntVal(args.foldLeft(identity) {
      case (acc, IntVal(n)) => op(acc, n)
      case _                => throw new EvalError("arithmetic: expected number")
    })

  def subtractOp(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil              => throw new EvalError("-: requires at least 1 argument")
      case IntVal(n) :: Nil => IntVal(-n)
      case IntVal(first) :: rest =>
        IntVal(rest.foldLeft(first) {
          case (acc, IntVal(n)) => acc - n
          case _                => throw new EvalError("-: expected number")
        })
      case _ => throw new EvalError("-: expected number")

  def divideOp(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil => throw new EvalError("/: requires at least 1 argument")
      case IntVal(first) :: rest =>
        IntVal(rest.foldLeft(first) {
          case (acc, IntVal(n)) =>
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
      case IntVal(a) :: IntVal(b) :: Nil => BoolVal(cmp(a, b))
      case _                             => throw new EvalError("comparison: expected 2 numbers")

  def notOp(args: List[SchemeValue]): SchemeValue =
    args match
      case v :: Nil => BoolVal(!v.isTruthy)
      case _        => throw new EvalError("not: requires 1 argument")
