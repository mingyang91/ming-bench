package ming

import SchemeValue.*
import InterpreterUtils.*
import Bounce.*
import scala.annotation.tailrec

object CpsEval:
  type Env  = Map[String, SchemeValue]
  type Cont = (SchemeValue, Env) => Bounce

  @tailrec
  def run(b: Bounce): (SchemeValue, Env) = b match
    case Done(v, e) => (v, e)
    case More(f)    => run(f())

  def evalK(expr: SchemeValue, env: Env, out: Array[String], k: Cont): Bounce =
    expr match
      case IntVal(_) | BoolVal(_) | StringVal(_) | MutableStringVal(_) | CharVal(_) | Void =>
        More(() => k(expr, env))
      case _: Cell => More(() => k(expr, env))
      case SymbolVal(name, pos) =>
        val raw = env.getOrElse(
          name,
          throw new EvalError(s"unbound variable: $name${fmtPos(pos)}")
        )
        More(() => k(deref(raw), env))
      case ListVal(Nil, _) => More(() => k(expr, env))
      case ListVal(SymbolVal("quote", _) :: arg :: Nil, _) =>
        More(() => k(arg, env))
      case ListVal(SymbolVal("if", _) :: rest, pos) =>
        CpsSpecialForms.evalIfK(rest, pos, env, out, k)
      case ListVal(SymbolVal("define", _) :: rest, pos) =>
        CpsSpecialForms.evalDefineK(rest, pos, env, out, k)
      case ListVal(
            SymbolVal("set!", _) :: SymbolVal(name, namePos) :: value :: Nil,
            _
          ) =>
        CpsSpecialForms.evalSetBangK(name, namePos, value, env, out, k)
      case ListVal(
            SymbolVal("lambda", _) :: ListVal(params, _) :: body,
            _
          ) =>
        val (ps, rp) = extractParamsWithRest(params)
        More(() => k(LambdaVal(ps, body, env, restParam = rp), env))
      case ListVal(SymbolVal("and", _) :: args, _) =>
        CpsSpecialForms.evalAndK(args, env, out, k)
      case ListVal(SymbolVal("or", _) :: args, _) =>
        CpsSpecialForms.evalOrK(args, env, out, k)
      case ListVal(SymbolVal("let", _) :: rest, _) =>
        CpsSpecialForms.evalLetK(rest, env, out, k)
      case ListVal(SymbolVal("begin", _) :: body, _) =>
        evalSequenceK(body, env, out, k)
      case ListVal(SymbolVal("cond", _) :: clauses, _) =>
        CpsSpecialForms.evalCondK(clauses, env, out, k)
      case ListVal(head :: args, pos) =>
        evalApplicationK(head, args, pos, env, out, k)
      case _: LambdaVal       => More(() => k(expr, env))
      case _: PairVal         => More(() => k(expr, env))
      case _: ContinuationVal => More(() => k(expr, env))

  def evalSequenceK(
    exprs: List[SchemeValue],
    env: Env,
    out: Array[String],
    k: Cont
  ): Bounce =
    exprs match
      case Nil         => More(() => k(Void, env))
      case last :: Nil => evalK(last, env, out, k)
      case head :: tail =>
        evalK(head, env, out, (_, newEnv) => evalSequenceK(tail, newEnv, out, k))

  def evalBodyK(
    body: List[SchemeValue],
    env: Env,
    out: Array[String],
    k: Cont
  ): Bounce =
    val (defines, rest) = body.span(isDefine)
    if defines.isEmpty then evalSequenceK(rest, env, out, k)
    else
      processDefinesK(
        defines,
        env,
        out,
        bodyEnv => evalSequenceK(rest, bodyEnv, out, k)
      )

  private def processDefinesK(
    defines: List[SchemeValue],
    env: Env,
    out: Array[String],
    k: Env => Bounce
  ): Bounce =
    val names               = defines.map(extractDefineName)
    val envWithPlaceholders = names.foldLeft(env)((e, n) => e + (n -> makeCell(Void)))
    evalDefinesSeqK(
      defines,
      envWithPlaceholders,
      out,
      envAfterDefs =>
        val finalEnv = names.foldLeft(envAfterDefs) { (e, name) =>
          e(name) match
            case Cell(arr) =>
              arr(0) match
                case LambdaVal(params, body, closure, selfName, restParam) =>
                  val updatedClosure =
                    closure ++ names.map(n => n -> e(n)).toMap
                  arr(0) = LambdaVal(params, body, updatedClosure, selfName, restParam)
                  e
                case _ => e
            case _ => e
        }
        More(() => k(finalEnv))
    )

  private def evalDefinesSeqK(
    defines: List[SchemeValue],
    env: Env,
    out: Array[String],
    k: Env => Bounce
  ): Bounce =
    defines match
      case Nil => More(() => k(env))
      case head :: tail =>
        evalK(
          head,
          env,
          out,
          (_, newEnv) => evalDefinesSeqK(tail, newEnv, out, k)
        )

  private def evalApplicationK(
    head: SchemeValue,
    args: List[SchemeValue],
    pos: Option[(Int, Int)],
    env: Env,
    out: Array[String],
    k: Cont
  ): Bounce =
    evalK(
      head,
      env,
      out,
      (func, _) =>
        func match
          case SymbolVal("call/cc" | "call-with-current-continuation", _) =>
            handleCallCCInline(args, env, out, k, pos)
          case _ =>
            evalArgsK(
              args,
              env,
              out,
              evaledArgs =>
                try applyK(func, evaledArgs, env, out, k, pos)
                catch
                  case e: EvalError if !hasPos(e.getMessage) =>
                    throw new EvalError(s"${e.getMessage}${fmtPos(pos)}")
            )
    )

  private def handleCallCCInline(
    args: List[SchemeValue],
    env: Env,
    out: Array[String],
    k: Cont,
    pos: Option[(Int, Int)]
  ): Bounce =
    args match
      case fExpr :: Nil =>
        evalK(
          fExpr,
          env,
          out,
          (f, _) =>
            val contVal = ContinuationVal(k)
            applyK(f, List(contVal), env, out, k, pos)
        )
      case _ =>
        throw new EvalError(
          s"call/cc: expected 1 argument, got ${args.length}${fmtPos(pos)}"
        )

  private def evalArgsK(
    args: List[SchemeValue],
    env: Env,
    out: Array[String],
    k: List[SchemeValue] => Bounce
  ): Bounce =
    evalArgsRTL(args.reverse, env, out, Nil, k)

  private def evalArgsRTL(
    reversedArgs: List[SchemeValue],
    env: Env,
    out: Array[String],
    acc: List[SchemeValue],
    k: List[SchemeValue] => Bounce
  ): Bounce =
    reversedArgs match
      case Nil => More(() => k(acc))
      case head :: tail =>
        evalK(
          head,
          env,
          out,
          (v, _) => evalArgsRTL(tail, env, out, v :: acc, k)
        )

  def applyK(
    func: SchemeValue,
    args: List[SchemeValue],
    callingEnv: Env,
    out: Array[String],
    k: Cont,
    pos: Option[(Int, Int)]
  ): Bounce =
    func match
      case lam @ LambdaVal(params, body, closure, selfName, restParam) =>
        checkArity(params.length, args.length, restParam, pos)
        val merged            = callingEnv ++ closure
        val envWithSelf       = selfName.fold(merged)(n => merged + (n -> makeCell(lam)))
        val (required, extra) = args.splitAt(params.length)
        val localEnv =
          envWithSelf ++ params.zip(required).map((p, a) => p -> makeCell(a)).toMap
        val finalEnv = restParam.fold(localEnv) { rp =>
          localEnv + (rp -> makeCell(Builtins.listToPairs(extra)))
        }
        evalBodyK(body, finalEnv, out, k)

      case ContinuationVal(savedK) =>
        args match
          case value :: Nil => More(() => savedK(value, callingEnv))
          case _ =>
            throw new EvalError(
              s"continuation: expected 1 argument, got ${args.length}"
            )

      case SymbolVal(name, _) =>
        name match
          case "apply" => handleApplyK(args, callingEnv, out, k)
          case "call/cc" | "call-with-current-continuation" =>
            handleCallCCAsValue(args, callingEnv, out, k, pos)
          case _ =>
            val (rv, builtinOut) = Builtins.applyBuiltin(name, args)
            out(0) = out(0) + builtinOut
            More(() => k(rv, callingEnv))

      case _ => throw new EvalError(s"not a procedure${fmtPos(pos)}")

  private def handleCallCCAsValue(
    args: List[SchemeValue],
    env: Env,
    out: Array[String],
    k: Cont,
    pos: Option[(Int, Int)]
  ): Bounce =
    args match
      case f :: Nil =>
        val contVal = ContinuationVal(k)
        applyK(f, List(contVal), env, out, k, pos)
      case _ =>
        throw new EvalError(
          s"call/cc: expected 1 argument, got ${args.length}"
        )

  private def handleApplyK(
    args: List[SchemeValue],
    callingEnv: Env,
    out: Array[String],
    k: Cont
  ): Bounce =
    args match
      case func :: rest if rest.nonEmpty =>
        val prefixArgs = rest.init
        val lastArg    = rest.last
        val listArgs   = schemeListToList(lastArg)
        applyK(func, prefixArgs ++ listArgs, callingEnv, out, k, None)
      case _ => throw new EvalError("apply: need at least 2 arguments")

  private def checkArity(
    expected: Int,
    got: Int,
    restParam: Option[String],
    pos: Option[(Int, Int)]
  ): Unit =
    restParam match
      case None =>
        if got != expected then
          throw new EvalError(
            s"wrong number of arguments: expected $expected, got $got"
          )
      case Some(_) =>
        if got < expected then
          throw new EvalError(
            s"wrong number of arguments: expected at least $expected, got $got"
          )
