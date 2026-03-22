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

      // named let: (let name ((var init) ...) body ...)
      case ListVal(SymbolVal("let") :: SymbolVal(name) :: ListVal(bindings) :: body) =>
        val (paramNames, initVals) = bindings.map {
          case ListVal(SymbolVal(p) :: valExpr :: Nil) =>
            val (v, _) = eval(valExpr, env)
            (p, v)
          case _ => throw new EvalError("invalid let binding")
        }.unzip
        val lambda = LambdaVal(paramNames, body, env, Some(name))
        val result = applyFunc(lambda, initVals)
        (result, env)

      // let
      case ListVal(SymbolVal("let") :: ListVal(bindings) :: body) =>
        val letEnv = bindings.foldLeft(env) { (e, binding) =>
          binding match
            case ListVal(SymbolVal(name) :: valExpr :: Nil) =>
              val (v, _) = eval(valExpr, env)
              e + (name -> v)
            case _ => throw new EvalError("invalid let binding")
        }
        (evalBody(body, letEnv), env)

      // begin
      case ListVal(SymbolVal("begin") :: body) =>
        val (result, newEnv) = evalSequence(body, env)
        (result, newEnv)

      // cond
      case ListVal(SymbolVal("cond") :: clauses) =>
        (evalCond(clauses, env), env)

      // function application
      case ListVal(head :: args) =>
        val (func, _)  = eval(head, env)
        val evaledArgs = args.map(a => eval(a, env)._1)
        (applyFunc(func, evaledArgs), env)

      case _: LambdaVal => (expr, env)
      case _: PairVal   => (expr, env)

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
      case SymbolVal(name) => Builtins.applyBuiltin(name, args)
      case _               => throw new EvalError("not a procedure")

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
        // Third pass: update lambda closures to include all siblings (mutual recursion)
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
    case ListVal(SymbolVal("define") :: _) => true
    case _                                 => false

  private def extractDefineName(expr: SchemeValue): String = expr match
    case ListVal(SymbolVal("define") :: SymbolVal(name) :: _)               => name
    case ListVal(SymbolVal("define") :: ListVal(SymbolVal(name) :: _) :: _) => name
    case _                                                                  => throw new EvalError("invalid define")

  /** Evaluate a sequence of expressions, threading env (for begin with defines). */
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
      case ListVal(SymbolVal("else") :: body) :: _ =>
        evalBody(body, env)
      case ListVal(test :: body) :: rest =>
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
