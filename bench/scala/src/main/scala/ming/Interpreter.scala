package ming

import SchemeValue.*
import InterpreterUtils.*
import TcoResult.*
import scala.annotation.tailrec

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
        resolveToValue(TailEval.evalAndTail(args, env), env)
      case ListVal(SymbolVal("or", _) :: args, _) =>
        resolveToValue(TailEval.evalOrTail(args, env), env)

      // named let / let
      case ListVal(SymbolVal("let", _) :: rest, _) =>
        resolveToValue(TailEval.evalLetTail(rest, env), env)

      // begin
      case ListVal(SymbolVal("begin", _) :: body, _) => evalSequence(body, env)

      // cond
      case ListVal(SymbolVal("cond", _) :: clauses, _) =>
        resolveToValue(TailEval.evalCondTail(clauses, env), env)

      // function application
      case ListVal(head :: args, pos) => evalApplication(head, args, pos, env)

      case _: LambdaVal => (expr, env, "")
      case _: PairVal   => (expr, env, "")

  // ---------------------------------------------------------------------------
  // Non-tail helpers (used by eval)
  // ---------------------------------------------------------------------------

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

  private[ming] def evalDefine(
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

  private def evalApplication(
    head: SchemeValue,
    args: List[SchemeValue],
    pos: Option[(Int, Int)],
    env: Env
  ): (SchemeValue, Env, Output) =
    val (func, _, o1)    = eval(head, env)
    val (evaledArgs, o2) = evalArgs(args, env)
    try
      val (rv, o3) = applyFunc(func, evaledArgs, env)
      (rv, env, o1 + o2 + o3)
    catch
      case e: EvalError if !hasPos(e.getMessage) =>
        throw new EvalError(s"${e.getMessage}${fmtPos(pos)}")

  private[ming] def evalArgs(
    args: List[SchemeValue],
    env: Env
  ): (List[SchemeValue], Output) =
    args.foldLeft((List.empty[SchemeValue], "")) { case ((vs, o), arg) =>
      val (v, _, vo) = eval(arg, env)
      (vs :+ v, o + vo)
    }

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

  // ---------------------------------------------------------------------------
  // Trampoline for TCO
  // ---------------------------------------------------------------------------

  private def applyFunc(
    func: SchemeValue,
    args: List[SchemeValue],
    callingEnv: Env
  ): (SchemeValue, Output) =
    trampolineLoop(applyStep(func, args, callingEnv), "")

  @tailrec
  private def trampolineLoop(result: TcoResult, accOut: Output): (SchemeValue, Output) =
    result match
      case Value(v, o) => (v, accOut + o)
      case TailCall(func, args, callingEnv, o) =>
        trampolineLoop(applyStep(func, args, callingEnv), accOut + o)

  private[ming] def applyStep(
    func: SchemeValue,
    args: List[SchemeValue],
    callingEnv: Env
  ): TcoResult =
    func match
      case lam @ LambdaVal(params, body, closure, selfName) =>
        if params.length != args.length then
          throw new EvalError(
            s"wrong number of arguments: expected ${params.length}, got ${args.length}"
          )
        val merged      = callingEnv ++ closure
        val envWithSelf = selfName.fold(merged)(n => merged + (n -> lam))
        val localEnv    = envWithSelf ++ params.zip(args).toMap
        TailEval.evalBodyTail(body, localEnv)
      case SymbolVal(name, _) =>
        val (rv, o) = Builtins.applyBuiltin(name, args)
        Value(rv, o)
      case _ => throw new EvalError("not a procedure")

  /** Resolve a TcoResult in a non-tail context, using callerEnv. */
  private def resolveToValue(
    result: TcoResult,
    callerEnv: Env
  ): (SchemeValue, Env, Output) =
    result match
      case Value(v, o) => (v, callerEnv, o)
      case TailCall(func, args, callingEnv, o) =>
        val (rv, ro) = applyFunc(func, args, callingEnv)
        (rv, callerEnv, o + ro)

  /** Process internal defines for letrec-like mutual visibility. */
  private[ming] def processDefines(
    defines: List[SchemeValue],
    env: Env
  ): (Env, Output) =
    val names               = defines.map(extractDefineName)
    val envWithPlaceholders = names.foldLeft(env)((e, n) => e + (n -> Void))
    val (envAfterDefs, o) =
      defines.foldLeft((envWithPlaceholders, "")) { case ((e, o), d) =>
        val (_, newE, dOut) = eval(d, e)
        (newE, o + dOut)
      }
    val finalEnv = names.foldLeft(envAfterDefs) { (e, name) =>
      e(name) match
        case LambdaVal(params, body, closure, selfName) =>
          val updatedClosure = closure ++ names.map(n => n -> e(n)).toMap
          e + (name -> LambdaVal(params, body, updatedClosure, selfName))
        case _ => e
    }
    (finalEnv, o)
