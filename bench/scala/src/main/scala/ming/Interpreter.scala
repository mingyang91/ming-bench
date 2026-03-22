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
      case IntVal(_) | BoolVal(_) | StringVal(_) | MutableStringVal(_) | CharVal(_) | Void | _: Cell =>
        (expr, env, "")
      case SymbolVal(name, pos) =>
        val raw = env.getOrElse(
          name,
          throw new EvalError(s"unbound variable: $name${fmtPos(pos)}")
        )
        (deref(raw), env, "")
      case ListVal(Nil, _) => (expr, env, "")

      // quote
      case ListVal(SymbolVal("quote", _) :: arg :: Nil, _) => (arg, env, "")

      // if
      case ListVal(SymbolVal("if", _) :: rest, pos) => evalIf(rest, pos, env)

      // define
      case ListVal(SymbolVal("define", _) :: rest, pos) =>
        evalDefine(rest, pos, env)

      // set!
      case ListVal(SymbolVal("set!", _) :: SymbolVal(name, namePos) :: value :: Nil, _) =>
        evalSetBang(name, namePos, value, env)

      // lambda
      case ListVal(SymbolVal("lambda", _) :: ListVal(params, _) :: body, _) =>
        val (ps, rp) = extractParamsWithRest(params)
        (LambdaVal(ps, body, env, restParam = rp), env, "")

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

  private def evalSetBang(
    name: String,
    namePos: Option[(Int, Int)],
    valueExpr: SchemeValue,
    env: Env
  ): (SchemeValue, Env, Output) =
    val cell = env.getOrElse(
      name,
      throw new EvalError(s"set!: unbound variable: $name${fmtPos(namePos)}")
    )
    val (v, _, o) = eval(valueExpr, env)
    cell match
      case Cell(arr) =>
        arr(0) = v
        (Void, env, o)
      case _ => throw new EvalError(s"set!: invalid binding for $name")

  private[ming] def evalDefine(
    rest: List[SchemeValue],
    pos: Option[(Int, Int)],
    env: Env
  ): (SchemeValue, Env, Output) =
    rest match
      case SymbolVal(name, _) :: value :: Nil =>
        val (v, _, o) = eval(value, env)
        val bound = v match
          case LambdaVal(params, body, closure, _, restParam) =>
            LambdaVal(params, body, closure, Some(name), restParam)
          case other => other
        (Void, env + (name -> makeCell(bound)), o)
      case ListVal(SymbolVal(name, _) :: params, _) :: body =>
        val (ps, rp) = extractParamsWithRest(params)
        val lambda   = LambdaVal(ps, body, env, Some(name), rp)
        (Void, env + (name -> makeCell(lambda)), "")
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
      case lam @ LambdaVal(params, body, closure, selfName, restParam) =>
        val minArgs = params.length
        restParam match
          case None =>
            if args.length != minArgs then
              throw new EvalError(
                s"wrong number of arguments: expected $minArgs, got ${args.length}"
              )
          case Some(_) =>
            if args.length < minArgs then
              throw new EvalError(
                s"wrong number of arguments: expected at least $minArgs, got ${args.length}"
              )
        val merged            = callingEnv ++ closure
        val envWithSelf       = selfName.fold(merged)(n => merged + (n -> makeCell(lam)))
        val (required, extra) = args.splitAt(params.length)
        val localEnv          = envWithSelf ++ params.zip(required).map((p, a) => p -> makeCell(a)).toMap
        val finalEnv = restParam.fold(localEnv) { rp =>
          localEnv + (rp -> makeCell(Builtins.listToPairs(extra)))
        }
        TailEval.evalBodyTail(body, finalEnv)
      case SymbolVal(name, _) =>
        name match
          case "apply" => handleApply(args, callingEnv)
          case _ =>
            val (rv, o) = Builtins.applyBuiltin(name, args)
            Value(rv, o)
      case _ => throw new EvalError("not a procedure")

  private def handleApply(
    args: List[SchemeValue],
    callingEnv: Env
  ): TcoResult =
    args match
      case func :: rest if rest.nonEmpty =>
        val prefixArgs = rest.init
        val lastArg    = rest.last
        val listArgs   = schemeListToList(lastArg)
        applyStep(func, prefixArgs ++ listArgs, callingEnv)
      case _ => throw new EvalError("apply: need at least 2 arguments")

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
    val envWithPlaceholders = names.foldLeft(env)((e, n) => e + (n -> makeCell(Void)))
    val (envAfterDefs, o) =
      defines.foldLeft((envWithPlaceholders, "")) { case ((e, o), d) =>
        val (_, newE, dOut) = eval(d, e)
        (newE, o + dOut)
      }
    val finalEnv = names.foldLeft(envAfterDefs) { (e, name) =>
      e(name) match
        case Cell(arr) =>
          arr(0) match
            case LambdaVal(params, body, closure, selfName, restParam) =>
              val updatedClosure = closure ++ names.map(n => n -> e(n)).toMap
              arr(0) = LambdaVal(params, body, updatedClosure, selfName, restParam)
              e
            case _ => e
        case _ => e
    }
    (finalEnv, o)
