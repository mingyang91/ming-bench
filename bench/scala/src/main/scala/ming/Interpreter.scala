package ming

import SchemeValue.*
import InterpreterUtils.*

object Interpreter:

  type Env    = Map[String, SchemeValue]
  type Output = String

  val defaultEnv: Env = Map.empty

  /** Evaluate an expression, returning (result, updated-env, output). */
  def eval(expr: SchemeValue, env: Env): (SchemeValue, Env, Output) =
    expr match
      case IntVal(_) | BoolVal(_) | StringVal(_) | MutableStringVal(_) | CharVal(_) | Void =>
        (expr, env, "")
      case SymbolVal(name, pos) =>
        val v = env.getOrElse(
          name,
          throw new EvalError(s"unbound variable: $name${fmtPos(pos)}")
        )
        (v, env, "")
      case ListVal(Nil, _) => (expr, env, "")

      // quote
      case ListVal(SymbolVal("quote", _) :: arg :: Nil, _) => (arg, env, "")

      // if
      case ListVal(SymbolVal("if", _) :: rest, pos) => evalIf(rest, pos, env)

      // define
      case ListVal(SymbolVal("define", _) :: rest, pos) =>
        evalDefine(rest, pos, env)

      // lambda
      case ListVal(SymbolVal("lambda", _) :: ListVal(params, _) :: body, _) =>
        (LambdaVal(extractParams(params), body, env), env, "")

      // and / or
      case ListVal(SymbolVal("and", _) :: args, _) =>
        val (r, o) = evalAnd(args, env)
        (r, env, o)
      case ListVal(SymbolVal("or", _) :: args, _) =>
        val (r, o) = evalOr(args, env)
        (r, env, o)

      // named let / let
      case ListVal(SymbolVal("let", _) :: rest, _) => evalLet(rest, env)

      // begin
      case ListVal(SymbolVal("begin", _) :: body, _) => evalSequence(body, env)

      // cond
      case ListVal(SymbolVal("cond", _) :: clauses, _) =>
        val (r, o) = evalCond(clauses, env)
        (r, env, o)

      // function application
      case ListVal(head :: args, pos) => evalApplication(head, args, pos, env)

      case _: LambdaVal => (expr, env, "")
      case _: PairVal   => (expr, env, "")

  private def evalIf(
    rest: List[SchemeValue],
    pos: Option[(Int, Int)],
    env: Env
  ): (SchemeValue, Env, Output) =
    rest match
      case cond :: thenBr :: elseBr :: Nil =>
        val (cv, _, o1) = eval(cond, env)
        val (rv, re, o2) =
          if cv.isTruthy then eval(thenBr, env) else eval(elseBr, env)
        (rv, re, o1 + o2)
      case cond :: thenBr :: Nil =>
        val (cv, _, o1) = eval(cond, env)
        if cv.isTruthy then
          val (rv, re, o2) = eval(thenBr, env)
          (rv, re, o1 + o2)
        else (Void, env, o1)
      case _ => throw new EvalError(s"if: bad syntax${fmtPos(pos)}")

  private def evalDefine(
    rest: List[SchemeValue],
    pos: Option[(Int, Int)],
    env: Env
  ): (SchemeValue, Env, Output) =
    rest match
      case SymbolVal(name, _) :: value :: Nil =>
        val (v, _, o) = eval(value, env)
        val bound = v match
          case LambdaVal(params, body, closure, _) =>
            LambdaVal(params, body, closure, Some(name))
          case other => other
        (Void, env + (name -> bound), o)
      case ListVal(SymbolVal(name, _) :: params, _) :: body =>
        val lambda = LambdaVal(extractParams(params), body, env, Some(name))
        (Void, env + (name -> lambda), "")
      case _ => throw new EvalError(s"define: bad syntax${fmtPos(pos)}")

  private def evalLet(
    rest: List[SchemeValue],
    env: Env
  ): (SchemeValue, Env, Output) =
    rest match
      // named let
      case SymbolVal(name, _) :: ListVal(bindings, _) :: body =>
        val (paramNames, initVals, bindOutput) =
          bindings.foldLeft((List.empty[String], List.empty[SchemeValue], "")) { case ((ps, vs, o), binding) =>
            binding match
              case ListVal(SymbolVal(p, _) :: valExpr :: Nil, _) =>
                val (v, _, vo) = eval(valExpr, env)
                (ps :+ p, vs :+ v, o + vo)
              case _ => throw new EvalError("invalid let binding")
          }
        val lambda   = LambdaVal(paramNames, body, env, Some(name))
        val (rv, ro) = applyFunc(lambda, initVals)
        (rv, env, bindOutput + ro)
      // regular let
      case ListVal(bindings, _) :: body =>
        val (letEnv, bindOutput) =
          bindings.foldLeft((env, "")) { case ((e, o), binding) =>
            binding match
              case ListVal(SymbolVal(name, _) :: valExpr :: Nil, _) =>
                val (v, _, vo) = eval(valExpr, env)
                (e + (name -> v), o + vo)
              case _ => throw new EvalError("invalid let binding")
          }
        val (rv, ro) = evalBodyWithOutput(body, letEnv)
        (rv, env, bindOutput + ro)
      case _ => throw new EvalError("invalid let syntax")

  private def evalApplication(
    head: SchemeValue,
    args: List[SchemeValue],
    pos: Option[(Int, Int)],
    env: Env
  ): (SchemeValue, Env, Output) =
    val (func, _, o1)    = eval(head, env)
    val (evaledArgs, o2) = evalArgs(args, env)
    try
      val (rv, o3) = applyFunc(func, evaledArgs)
      (rv, env, o1 + o2 + o3)
    catch
      case e: EvalError if !hasPos(e.getMessage) =>
        throw new EvalError(s"${e.getMessage}${fmtPos(pos)}")

  private def evalArgs(
    args: List[SchemeValue],
    env: Env
  ): (List[SchemeValue], Output) =
    args.foldLeft((List.empty[SchemeValue], "")) { case ((vs, o), arg) =>
      val (v, _, vo) = eval(arg, env)
      (vs :+ v, o + vo)
    }

  private def applyFunc(
    func: SchemeValue,
    args: List[SchemeValue]
  ): (SchemeValue, Output) =
    func match
      case lam @ LambdaVal(params, body, closure, selfName) =>
        if params.length != args.length then
          throw new EvalError(
            s"wrong number of arguments: expected ${params.length}, got ${args.length}"
          )
        val envWithSelf = selfName.fold(closure)(n => closure + (n -> lam))
        val localEnv    = envWithSelf ++ params.zip(args).toMap
        evalBodyWithOutput(body, localEnv)
      case SymbolVal(name, _) => Builtins.applyBuiltin(name, args)
      case _                  => throw new EvalError("not a procedure")

  private def evalBodyWithOutput(
    body: List[SchemeValue],
    env: Env
  ): (SchemeValue, Output) =
    // Pre-scan for internal defines so they are mutually visible (letrec-like)
    val (defines, rest) = body.span(isDefine)
    val (bodyEnv, defOutput) =
      if defines.isEmpty then (env, "")
      else
        // First pass: bind all names to Void placeholders
        val names               = defines.map(extractDefineName)
        val envWithPlaceholders = names.foldLeft(env)((e, n) => e + (n -> Void))
        // Second pass: evaluate definitions with all names in scope
        val (envAfterDefs, o) =
          defines.foldLeft((envWithPlaceholders, "")) { case ((e, o), d) =>
            val (_, newE, dOut) = eval(d, e)
            (newE, o + dOut)
          }
        // Third pass: update lambda closures to include all siblings
        val finalEnv = names.foldLeft(envAfterDefs) { (e, name) =>
          e(name) match
            case LambdaVal(params, body, closure, selfName) =>
              val updatedClosure = closure ++ names.map(n => n -> e(n)).toMap
              e + (name -> LambdaVal(params, body, updatedClosure, selfName))
            case _ => e
        }
        (finalEnv, o)
    rest match
      case Nil => (Void, defOutput)
      case last :: Nil =>
        val (rv, _, o) = eval(last, bodyEnv)
        (rv, defOutput + o)
      case head :: tail =>
        val (_, newEnv, o) = eval(head, bodyEnv)
        val (rv, ro)       = evalBodySeqWithOutput(tail, newEnv)
        (rv, defOutput + o + ro)

  private def evalBodySeqWithOutput(
    body: List[SchemeValue],
    env: Env
  ): (SchemeValue, Output) =
    body match
      case Nil => (Void, "")
      case last :: Nil =>
        val (rv, _, o) = eval(last, env)
        (rv, o)
      case head :: tail =>
        val (_, newEnv, o) = eval(head, env)
        val (rv, ro)       = evalBodySeqWithOutput(tail, newEnv)
        (rv, o + ro)

  /** Evaluate a sequence of expressions, threading env. */
  private def evalSequence(
    exprs: List[SchemeValue],
    env: Env
  ): (SchemeValue, Env, Output) =
    exprs match
      case Nil         => (Void, env, "")
      case last :: Nil => eval(last, env)
      case head :: tail =>
        val (_, newEnv, o) = eval(head, env)
        val (rv, re, ro)   = evalSequence(tail, newEnv)
        (rv, re, o + ro)

  private def evalCond(
    clauses: List[SchemeValue],
    env: Env
  ): (SchemeValue, Output) =
    clauses match
      case Nil => (Void, "")
      case ListVal(SymbolVal("else", _) :: body, _) :: _ =>
        evalBodyWithOutput(body, env)
      case ListVal(test :: body, _) :: rest =>
        val (tv, _, o1) = eval(test, env)
        if tv.isTruthy then
          val (rv, o2) = evalBodyWithOutput(body, env)
          (rv, o1 + o2)
        else
          val (rv, o2) = evalCond(rest, env)
          (rv, o1 + o2)
      case _ => throw new EvalError("invalid cond clause")

  private def evalAnd(
    args: List[SchemeValue],
    env: Env
  ): (SchemeValue, Output) =
    args match
      case Nil => (BoolVal(true), "")
      case last :: Nil =>
        val (rv, _, o) = eval(last, env)
        (rv, o)
      case head :: tail =>
        val (result, _, o1) = eval(head, env)
        if result.isTruthy then
          val (rv, o2) = evalAnd(tail, env)
          (rv, o1 + o2)
        else (result, o1)

  private def evalOr(
    args: List[SchemeValue],
    env: Env
  ): (SchemeValue, Output) =
    args match
      case Nil => (BoolVal(false), "")
      case last :: Nil =>
        val (rv, _, o) = eval(last, env)
        (rv, o)
      case head :: tail =>
        val (result, _, o1) = eval(head, env)
        if result.isTruthy then (result, o1)
        else
          val (rv, o2) = evalOr(tail, env)
          (rv, o1 + o2)
