package ming

import SchemeValue.*
import Evaluator.{DoneS, EvalS, ReturnS, Step}

/** Continuation application and procedure dispatch. */
private[ming] object KontApply:

  def applyKont(
    value: SchemeValue,
    k: Kont,
    out: String
  ): Step = k match
    case Kont.Halt => DoneS(value, out)

    case Kont.Seq(remaining, seqEnv, nextK) =>
      remaining match
        case Nil         => ReturnS(value, nextK, out)
        case last :: Nil => EvalS(last, seqEnv, nextK, out)
        case head :: tail =>
          EvalS(head, seqEnv, Kont.Seq(tail, seqEnv, nextK), out)

    case Kont.EvalOp(argExprs, opEnv, nextK, callPos) =>
      applyEvalOp(value, argExprs, opEnv, nextK, callPos, out)

    case Kont.EvalArg(proc, evaled, remaining, argEnv, nextK, callPos) =>
      applyEvalArg(value, proc, evaled, remaining, argEnv, nextK, callPos, out)

    case Kont.IfK(thenE, elseE, ifEnv, nextK) =>
      if Evaluator.isFalsy(value) then
        elseE match
          case Some(e) => EvalS(e, ifEnv, nextK, out)
          case None    => ReturnS(SchemeVoid, nextK, out)
      else EvalS(thenE, ifEnv, nextK, out)

    case Kont.SetK(name, setEnv, nextK) =>
      setEnv.set(name, value)
      ReturnS(SchemeVoid, nextK, out)

    case Kont.DefineK(name, defEnv, nextK) =>
      val newEnv   = defEnv.extend(name, value)
      val updatedK = Evaluator.updateSeqEnv(nextK, newEnv)
      ReturnS(SchemeVoid, updatedK, out)

    case Kont.AndK(remaining, andEnv, nextK) =>
      if Evaluator.isFalsy(value) then ReturnS(value, nextK, out)
      else
        remaining match
          case last :: Nil => EvalS(last, andEnv, nextK, out)
          case head :: tail =>
            EvalS(head, andEnv, Kont.AndK(tail, andEnv, nextK), out)
          case Nil => ReturnS(value, nextK, out)

    case Kont.OrK(remaining, orEnv, nextK) =>
      if !Evaluator.isFalsy(value) then ReturnS(value, nextK, out)
      else
        remaining match
          case last :: Nil => EvalS(last, orEnv, nextK, out)
          case head :: tail =>
            EvalS(head, orEnv, Kont.OrK(tail, orEnv, nextK), out)
          case Nil => ReturnS(value, nextK, out)

    case Kont.NotK(nextK) =>
      ReturnS(SchemeBool(Evaluator.isFalsy(value)), nextK, out)

    case Kont.CallCCK(nextK) =>
      val kontVal = SchemeContinuation(nextK)
      applyProc(value, List(kontVal), nextK, out)

    case Kont.CondK(body, remaining, condEnv, nextK) =>
      if Evaluator.isFalsy(value) then SpecialForms.evalCondClauses(remaining, condEnv, nextK, out)
      else if body.isEmpty then ReturnS(value, nextK, out)
      else SpecialForms.startSequence(body, condEnv, nextK, out)

    case Kont.LetInitK(params, evaled, remaining, body, letEnv, nextK) =>
      applyLetInit(value, params, evaled, remaining, body, letEnv, nextK, out)

    case Kont.NamedLetInitK(name, params, evaled, remaining, body, letEnv, nextK) =>
      applyNamedLetInit(value, name, params, evaled, remaining, body, letEnv, nextK, out)

  // --- applyKont helpers for complex cases ---

  private def applyEvalOp(
    proc: SchemeValue,
    argExprs: List[SchemeValue],
    opEnv: Env,
    nextK: Kont,
    callPos: SourcePos,
    out: String
  ): Step =
    argExprs.reverse match
      case Nil =>
        try applyProc(proc, Nil, nextK, out)
        catch
          case e: EvalError if e.sourcePos == SourcePos.None =>
            throw new EvalError(e.baseMessage, callPos)
      case first :: rest =>
        EvalS(
          first,
          opEnv,
          Kont.EvalArg(proc, Nil, rest, opEnv, nextK, callPos),
          out
        )

  private def applyEvalArg(
    value: SchemeValue,
    proc: SchemeValue,
    evaled: List[SchemeValue],
    remaining: List[SchemeValue],
    argEnv: Env,
    nextK: Kont,
    callPos: SourcePos,
    out: String
  ): Step =
    val newEvaled = value :: evaled
    remaining match
      case Nil =>
        try applyProc(proc, newEvaled, nextK, out)
        catch
          case e: EvalError if e.sourcePos == SourcePos.None =>
            throw new EvalError(e.baseMessage, callPos)
      case next :: rest =>
        EvalS(
          next,
          argEnv,
          Kont.EvalArg(proc, newEvaled, rest, argEnv, nextK, callPos),
          out
        )

  private def applyLetInit(
    value: SchemeValue,
    params: List[String],
    evaled: List[SchemeValue],
    remaining: List[SchemeValue],
    body: List[SchemeValue],
    letEnv: Env,
    nextK: Kont,
    out: String
  ): Step =
    val newEvaled = evaled :+ value
    remaining match
      case Nil =>
        SpecialForms.startSequence(body, letEnv.extend(params, newEvaled), nextK, out)
      case next :: rest =>
        EvalS(
          next,
          letEnv,
          Kont.LetInitK(params, newEvaled, rest, body, letEnv, nextK),
          out
        )

  private def applyNamedLetInit(
    value: SchemeValue,
    name: String,
    params: List[String],
    evaled: List[SchemeValue],
    remaining: List[SchemeValue],
    body: List[SchemeValue],
    letEnv: Env,
    nextK: Kont,
    out: String
  ): Step =
    val newEvaled = evaled :+ value
    remaining match
      case Nil =>
        val recEnv = Env.RecursiveFrame(
          name,
          closure => SchemeLambda(params, None, body, closure),
          letEnv
        )
        SpecialForms.startSequence(body, recEnv.extend(params, newEvaled), nextK, out)
      case next :: rest =>
        EvalS(
          next,
          letEnv,
          Kont.NamedLetInitK(name, params, newEvaled, rest, body, letEnv, nextK),
          out
        )

  // --- Procedure application ---

  def applyProc(
    proc: SchemeValue,
    args: List[SchemeValue],
    k: Kont,
    out: String
  ): Step = proc match
    case SchemeLambda(params, restParam, body, closure) =>
      val localEnv = bindArgs(params, restParam, args, closure)
      SpecialForms.startSequence(body, localEnv, k, out)
    case SchemeContinuation(savedK) =>
      if args.length != 1 then throw new EvalError("continuation: expected 1 argument")
      ReturnS(args.head, savedK, out)
    case SchemeBuiltinProc("call/cc") | SchemeBuiltinProc("call-with-current-continuation") =>
      if args.length != 1 then throw new EvalError("call/cc: expected 1 argument")
      val kontVal = SchemeContinuation(k)
      applyProc(args.head, List(kontVal), k, out)
    case SchemeBuiltinProc("apply") =>
      applyApply(args, k, out)
    case SchemeBuiltinProc(name) =>
      val (result, bo) = Builtins.evalBuiltin(name, args)
      ReturnS(result, k, out + bo)
    case other =>
      throw new EvalError(s"not a procedure: ${other.display}")

  private def bindArgs(
    params: List[String],
    restParam: Option[String],
    args: List[SchemeValue],
    closure: Env
  ): Env = restParam match
    case None =>
      if params.length != args.length then
        throw new EvalError(
          s"expected ${params.length} arguments, got ${args.length}"
        )
      closure.extend(params, args)
    case Some(rest) =>
      if args.length < params.length then
        throw new EvalError(
          s"expected at least ${params.length} arguments, got ${args.length}"
        )
      val (fixed, remaining) = args.splitAt(params.length)
      closure.extend(params :+ rest, fixed :+ SchemeList(remaining))

  private def applyApply(
    args: List[SchemeValue],
    k: Kont,
    out: String
  ): Step =
    if args.length < 2 then throw new EvalError("apply: expected at least 2 arguments")
    val proc       = args.head
    val lastArg    = args.last
    val prefixArgs = args.drop(1).dropRight(1)
    val allArgs = lastArg match
      case SchemeList(es) => prefixArgs ++ es
      case other =>
        throw new EvalError(
          s"apply: last argument must be a list, got ${other.display}"
        )
    applyProc(proc, allArgs, k, out)
