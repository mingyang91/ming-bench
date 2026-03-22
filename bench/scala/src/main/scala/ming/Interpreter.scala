package ming

import SchemeValue.*

object Interpreter:

  type Env = Map[String, SchemeValue]

  val defaultEnv: Env = Map.empty

  private def fmtPos(pos: Option[(Int, Int)]): String =
    pos.map { case (l, c) => s" [$l:$c]" }.getOrElse("")

  private def hasPos(msg: String): Boolean =
    msg.matches(".*\\d+:\\d+.*")

  /** Evaluate an expression, returning (result, updated-env). */
  def eval(expr: SchemeValue, env: Env): (SchemeValue, Env) =
    expr match
      case IntVal(_)    => (expr, env)
      case BoolVal(_)   => (expr, env)
      case StringVal(_) => (expr, env)
      case Void         => (expr, env)
      case SymbolVal(name, pos) =>
        val v = env.getOrElse(
          name,
          throw new EvalError(s"unbound variable: $name${fmtPos(pos)}")
        )
        (v, env)
      case ListVal(Nil, _) => (expr, env)

      // quote
      case ListVal(SymbolVal("quote", _) :: arg :: Nil, _) => (arg, env)

      // if
      case ListVal(SymbolVal("if", _) :: rest, pos) => evalIf(rest, pos, env)

      // define
      case ListVal(SymbolVal("define", _) :: rest, pos) =>
        evalDefine(rest, pos, env)

      // lambda
      case ListVal(SymbolVal("lambda", _) :: ListVal(params, _) :: body, _) =>
        (LambdaVal(extractParams(params), body, env), env)

      // and / or
      case ListVal(SymbolVal("and", _) :: args, _) => (evalAnd(args, env), env)
      case ListVal(SymbolVal("or", _) :: args, _)  => (evalOr(args, env), env)

      // named let / let
      case ListVal(SymbolVal("let", _) :: rest, _) => evalLet(rest, env)

      // begin
      case ListVal(SymbolVal("begin", _) :: body, _) => evalSequence(body, env)

      // cond
      case ListVal(SymbolVal("cond", _) :: clauses, _) =>
        (evalCond(clauses, env), env)

      // function application
      case ListVal(head :: args, pos) => evalApplication(head, args, pos, env)

      case _: LambdaVal => (expr, env)
      case _: PairVal   => (expr, env)

  private def evalIf(
    rest: List[SchemeValue],
    pos: Option[(Int, Int)],
    env: Env
  ): (SchemeValue, Env) =
    rest match
      case cond :: thenBr :: elseBr :: Nil =>
        val (cv, _) = eval(cond, env)
        if cv.isTruthy then eval(thenBr, env) else eval(elseBr, env)
      case cond :: thenBr :: Nil =>
        val (cv, _) = eval(cond, env)
        if cv.isTruthy then eval(thenBr, env) else (Void, env)
      case _ => throw new EvalError(s"if: bad syntax${fmtPos(pos)}")

  private def evalDefine(
    rest: List[SchemeValue],
    pos: Option[(Int, Int)],
    env: Env
  ): (SchemeValue, Env) =
    rest match
      case SymbolVal(name, _) :: value :: Nil =>
        val (v, _) = eval(value, env)
        val bound = v match
          case LambdaVal(params, body, closure, _) =>
            LambdaVal(params, body, closure, Some(name))
          case other => other
        (Void, env + (name -> bound))
      case ListVal(SymbolVal(name, _) :: params, _) :: body =>
        val lambda = LambdaVal(extractParams(params), body, env, Some(name))
        (Void, env + (name -> lambda))
      case _ => throw new EvalError(s"define: bad syntax${fmtPos(pos)}")

  private def evalLet(
    rest: List[SchemeValue],
    env: Env
  ): (SchemeValue, Env) =
    rest match
      // named let
      case SymbolVal(name, _) :: ListVal(bindings, _) :: body =>
        val (paramNames, initVals) = bindings.map {
          case ListVal(SymbolVal(p, _) :: valExpr :: Nil, _) =>
            val (v, _) = eval(valExpr, env)
            (p, v)
          case _ => throw new EvalError("invalid let binding")
        }.unzip
        val lambda = LambdaVal(paramNames, body, env, Some(name))
        (applyFunc(lambda, initVals), env)
      // regular let
      case ListVal(bindings, _) :: body =>
        val letEnv = bindings.foldLeft(env) { (e, binding) =>
          binding match
            case ListVal(SymbolVal(name, _) :: valExpr :: Nil, _) =>
              val (v, _) = eval(valExpr, env)
              e + (name -> v)
            case _ => throw new EvalError("invalid let binding")
        }
        (evalBody(body, letEnv), env)
      case _ => throw new EvalError("invalid let syntax")

  private def evalApplication(
    head: SchemeValue,
    args: List[SchemeValue],
    pos: Option[(Int, Int)],
    env: Env
  ): (SchemeValue, Env) =
    val (func, _)  = eval(head, env)
    val evaledArgs = args.map(a => eval(a, env)._1)
    try (applyFunc(func, evaledArgs), env)
    catch
      case e: EvalError if !hasPos(e.getMessage) =>
        throw new EvalError(s"${e.getMessage}${fmtPos(pos)}")

  private def extractParams(params: List[SchemeValue]): List[String] =
    params.map {
      case SymbolVal(n, _) => n
      case other =>
        throw new EvalError(s"invalid parameter: ${other.display}")
    }

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
      case SymbolVal(name, _) => Builtins.applyBuiltin(name, args)
      case _                  => throw new EvalError("not a procedure")

  private def evalBody(body: List[SchemeValue], env: Env): SchemeValue =
    // Pre-scan for internal defines so they are mutually visible (letrec-like)
    val (defines, rest) = body.span(isDefine)
    val bodyEnv =
      if defines.isEmpty then env
      else
        // First pass: bind all names to Void placeholders
        val names               = defines.map(extractDefineName)
        val envWithPlaceholders = names.foldLeft(env)((e, n) => e + (n -> Void))
        // Second pass: evaluate definitions with all names in scope
        val envAfterDefs = defines.foldLeft(envWithPlaceholders) { (e, d) =>
          val (_, newE) = eval(d, e)
          newE
        }
        // Third pass: update lambda closures to include all siblings
        val finalEnv = names.foldLeft(envAfterDefs) { (e, name) =>
          e(name) match
            case LambdaVal(params, body, closure, selfName) =>
              val updatedClosure = closure ++ names.map(n => n -> e(n)).toMap
              e + (name -> LambdaVal(params, body, updatedClosure, selfName))
            case _ => e
        }
        finalEnv
    rest match
      case Nil         => Void
      case last :: Nil => eval(last, bodyEnv)._1
      case head :: tail =>
        val (_, newEnv) = eval(head, bodyEnv)
        evalBodySeq(tail, newEnv)

  private def evalBodySeq(body: List[SchemeValue], env: Env): SchemeValue =
    body match
      case Nil         => Void
      case last :: Nil => eval(last, env)._1
      case head :: tail =>
        val (_, newEnv) = eval(head, env)
        evalBodySeq(tail, newEnv)

  private def isDefine(expr: SchemeValue): Boolean = expr match
    case ListVal(SymbolVal("define", _) :: _, _) => true
    case _                                       => false

  private def extractDefineName(expr: SchemeValue): String = expr match
    case ListVal(SymbolVal("define", _) :: SymbolVal(name, _) :: _, _) => name
    case ListVal(
          SymbolVal("define", _) :: ListVal(SymbolVal(name, _) :: _, _) :: _,
          _
        ) =>
      name
    case _ => throw new EvalError("invalid define")

  /** Evaluate a sequence of expressions, threading env. */
  private def evalSequence(
    exprs: List[SchemeValue],
    env: Env
  ): (SchemeValue, Env) =
    exprs match
      case Nil         => (Void, env)
      case last :: Nil => eval(last, env)
      case head :: tail =>
        val (_, newEnv) = eval(head, env)
        evalSequence(tail, newEnv)

  private def evalCond(clauses: List[SchemeValue], env: Env): SchemeValue =
    clauses match
      case Nil => Void
      case ListVal(SymbolVal("else", _) :: body, _) :: _ =>
        evalBody(body, env)
      case ListVal(test :: body, _) :: rest =>
        val (tv, _) = eval(test, env)
        if tv.isTruthy then evalBody(body, env)
        else evalCond(rest, env)
      case _ => throw new EvalError("invalid cond clause")

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
