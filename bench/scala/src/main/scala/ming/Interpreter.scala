package ming

import SchemeValue.*

object Interpreter:

  type Env = Map[String, SchemeValue]

  val defaultEnv: Env = Map.empty

  /** Evaluate an expression, returning (result, updated-env). */
  def eval(expr: SchemeValue, env: Env): (SchemeValue, Env) =
    expr match
      case IntVal(_)    => (expr, env)
      case BoolVal(_)   => (expr, env)
      case StringVal(_) => (expr, env)
      case Void         => (expr, env)
      case SymbolVal(name) =>
        (env.getOrElse(name, throw new EvalError(s"unbound variable: $name")), env)
      case ListVal(Nil) => (expr, env)

      // quote
      case ListVal(SymbolVal("quote") :: arg :: Nil) => (arg, env)

      // if
      case ListVal(SymbolVal("if") :: cond :: thenBr :: elseBr :: Nil) =>
        val (cv, _) = eval(cond, env)
        if cv.isTruthy then eval(thenBr, env) else eval(elseBr, env)
      case ListVal(SymbolVal("if") :: cond :: thenBr :: Nil) =>
        val (cv, _) = eval(cond, env)
        if cv.isTruthy then eval(thenBr, env) else (Void, env)

      // define variable
      case ListVal(SymbolVal("define") :: SymbolVal(name) :: value :: Nil) =>
        val (v, _) = eval(value, env)
        val bound = v match
          case LambdaVal(params, body, closure, _) =>
            LambdaVal(params, body, closure, Some(name))
          case other => other
        (Void, env + (name -> bound))

      // define function shorthand
      case ListVal(SymbolVal("define") :: ListVal(SymbolVal(name) :: params) :: body) =>
        val paramNames = params.map {
          case SymbolVal(n) => n
          case other        => throw new EvalError(s"invalid parameter: ${other.display}")
        }
        val lambda = LambdaVal(paramNames, body, env, Some(name))
        (Void, env + (name -> lambda))

      // lambda
      case ListVal(SymbolVal("lambda") :: ListVal(params) :: body) =>
        val paramNames = params.map {
          case SymbolVal(n) => n
          case other        => throw new EvalError(s"invalid parameter: ${other.display}")
        }
        (LambdaVal(paramNames, body, env), env)

      // and / or
      case ListVal(SymbolVal("and") :: args) => (evalAnd(args, env), env)
      case ListVal(SymbolVal("or") :: args)  => (evalOr(args, env), env)

      // function application
      case ListVal(head :: args) =>
        val (func, _)  = eval(head, env)
        val evaledArgs = args.map(a => eval(a, env)._1)
        (applyFunc(func, evaledArgs), env)

      case _: LambdaVal => (expr, env)

  private def applyFunc(
    func: SchemeValue,
    args: List[SchemeValue]
  ): SchemeValue =
    func match
      case lam @ LambdaVal(params, body, closure, selfName) =>
        if params.length != args.length then
          throw new EvalError(
            s"wrong number of arguments: expected ${params.length}, got ${args.length}"
          )
        val envWithSelf = selfName.fold(closure)(n => closure + (n -> lam))
        val localEnv    = envWithSelf ++ params.zip(args).toMap
        evalBody(body, localEnv)
      case SymbolVal(name) => applyBuiltin(name, args)
      case _               => throw new EvalError("not a procedure")

  private def evalBody(body: List[SchemeValue], env: Env): SchemeValue =
    body match
      case Nil         => Void
      case last :: Nil => eval(last, env)._1
      case head :: tail =>
        eval(head, env)
        evalBody(tail, env)

  private def evalAnd(args: List[SchemeValue], env: Env): SchemeValue =
    args match
      case Nil         => BoolVal(true)
      case last :: Nil => eval(last, env)._1
      case head :: tail =>
        val result = eval(head, env)._1
        if result.isTruthy then evalAnd(tail, env)
        else result

  private def evalOr(args: List[SchemeValue], env: Env): SchemeValue =
    args match
      case Nil         => BoolVal(false)
      case last :: Nil => eval(last, env)._1
      case head :: tail =>
        val result = eval(head, env)._1
        if result.isTruthy then result
        else evalOr(tail, env)

  private def applyBuiltin(
    name: String,
    args: List[SchemeValue]
  ): SchemeValue =
    name match
      case "+" => arithOp(args, 0L, _ + _)
      case "-" =>
        args match
          case Nil              => throw new EvalError("-: need at least 1 argument")
          case IntVal(n) :: Nil => IntVal(-n)
          case _                => arithOp(args.tail, asInt(args.head), _ - _)
      case "*" => arithOp(args, 1L, _ * _)
      case "/" =>
        args match
          case Nil => throw new EvalError("/: need at least 1 argument")
          case _ =>
            args.tail.foldLeft(asInt(args.head)) { (acc, v) =>
              val d = asInt(v)
              if d == 0 then throw new EvalError("division by zero")
              else acc / d
            } |> IntVal.apply
      case "<"  => cmpOp(args, _ < _)
      case ">"  => cmpOp(args, _ > _)
      case "="  => cmpOp(args, _ == _)
      case "<=" => cmpOp(args, _ <= _)
      case ">=" => cmpOp(args, _ >= _)
      case "not" =>
        args match
          case v :: Nil => BoolVal(!v.isTruthy)
          case _        => throw new EvalError("not: expects 1 argument")
      case _ => throw new EvalError(s"unknown procedure: $name")

  private def asInt(v: SchemeValue): Long = v match
    case IntVal(n) => n
    case other     => throw new EvalError(s"expected number, got: ${other.display}")

  private def arithOp(
    args: List[SchemeValue],
    init: Long,
    op: (Long, Long) => Long
  ): SchemeValue =
    IntVal(args.foldLeft(init)((acc, v) => op(acc, asInt(v))))

  private def cmpOp(
    args: List[SchemeValue],
    op: (Long, Long) => Boolean
  ): SchemeValue =
    args match
      case a :: b :: Nil => BoolVal(op(asInt(a), asInt(b)))
      case _             => throw new EvalError("comparison expects 2 arguments")

  extension [A](a: A) private def |>[B](f: A => B): B = f(a)
