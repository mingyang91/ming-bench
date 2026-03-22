package ming

import SchemeValue.*
import Evaluator.{DoneS, EvalS, ReturnS, Step}

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

    case Kont.CallCCK(nextK) =>
      val kontVal = SchemeContinuation(nextK)
      ProcApply.applyProc(value, List(kontVal), nextK, out)

    case Kont.CondK(body, remaining, condEnv, nextK) =>
      if Evaluator.isFalsy(value) then SpecialForms.evalCondClauses(remaining, condEnv, nextK, out)
      else if body.isEmpty then ReturnS(value, nextK, out)
      else SpecialForms.startSequence(body, condEnv, nextK, out)

    case Kont.LetInitK(params, evaled, remaining, body, letEnv, nextK) =>
      applyLetInit(value, params, evaled, remaining, body, letEnv, nextK, out)

    case Kont.NamedLetInitK(name, params, evaled, remaining, body, letEnv, nextK) =>
      applyNamedLetInit(value, name, params, evaled, remaining, body, letEnv, nextK, out)

    case Kont.MapK(proc, remainingGroups, accumulated, nextK) =>
      applyMapK(value, proc, remainingGroups, accumulated, nextK, out)

    case Kont.LetrecInitK(curName, remNames, remInits, body, frame, nextK) =>
      applyLetrecInit(value, curName, remNames, remInits, body, frame, nextK, out)

    case Kont.LetStarInitK(name, remaining, body, lsEnv, nextK) =>
      applyLetStarInit(value, name, remaining, body, lsEnv, nextK, out)

    case Kont.CaseK(clauses, caseEnv, nextK) =>
      DerivedForms.evalCaseClauses(value, clauses, caseEnv, nextK, out)

    case Kont.ForEachK(proc, remainingGroups, nextK) =>
      applyForEachK(value, proc, remainingGroups, nextK, out)

    case dk: (Kont.DynWindInK | Kont.DynWindMark | Kont.DynWindRetK | Kont.DynUnwindK | Kont.DynRewindK) =>
      applyDynWindKont(value, dk, out)

    case Kont.ExceptionHandlerK(_, nextK) => ReturnS(value, nextK, out)
    case Kont.GuardK(_, _, _, nextK)      => ReturnS(value, nextK, out)

    case Kont.GuardTestK(exnValue, body, remaining, variable, env, raiseK, guardK) =>
      ExceptionHandling.applyGuardTest(value, exnValue, body, remaining, variable, env, raiseK, guardK, out)

    case Kont.GuardClauseK(body, env, nextK) =>
      SpecialForms.startSequence(body, env, nextK, out)

    case Kont.CallWithValuesK(consumer, nextK) =>
      applyCallWithValues(value, consumer, nextK, out)

  private def applyDynWindKont(
    value: SchemeValue,
    dk: Kont.DynWindInK | Kont.DynWindMark | Kont.DynWindRetK | Kont.DynUnwindK | Kont.DynRewindK,
    out: String
  ): Step = dk match
    case Kont.DynWindInK(bodyThunk, entry, nextK) =>
      ProcApply.applyProc(bodyThunk, Nil, Kont.DynWindMark(entry, nextK), out)
    case Kont.DynWindMark(entry, nextK) =>
      ProcApply.applyProc(entry.outThunk, Nil, Kont.DynWindRetK(value, nextK), out)
    case Kont.DynWindRetK(bodyValue, nextK) =>
      ReturnS(bodyValue, nextK, out)
    case Kont.DynUnwindK(remaining, toRewind, savedValue, targetK) =>
      ProcApply.startUnwind(remaining, toRewind, savedValue, targetK, out)
    case Kont.DynRewindK(remaining, savedValue, targetK) =>
      ProcApply.startRewind(remaining, savedValue, targetK, out)

  private def applyLetrecInit(
    value: SchemeValue,
    curName: String,
    remNames: List[String],
    remInits: List[SchemeValue],
    body: List[SchemeValue],
    frame: Env,
    nextK: Kont,
    out: String
  ): Step =
    frame.set(curName, value)
    remNames match
      case Nil => SpecialForms.startSequence(body, frame, nextK, out)
      case nextName :: tailNames =>
        remInits match
          case nextInit :: tailInits =>
            EvalS(
              nextInit,
              frame,
              Kont.LetrecInitK(nextName, tailNames, tailInits, body, frame, nextK),
              out
            )
          case Nil => SpecialForms.startSequence(body, frame, nextK, out)

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
        try ProcApply.applyProc(proc, Nil, nextK, out)
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
        try ProcApply.applyProc(proc, newEvaled, nextK, out)
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

  private def applyLetStarInit(
    value: SchemeValue,
    name: String,
    remaining: List[(String, SchemeValue)],
    body: List[SchemeValue],
    lsEnv: Env,
    nextK: Kont,
    out: String
  ): Step =
    val newEnv = lsEnv.extend(name, value)
    remaining match
      case Nil => SpecialForms.startSequence(body, newEnv, nextK, out)
      case (nextName, nextInit) :: rest =>
        EvalS(
          nextInit,
          newEnv,
          Kont.LetStarInitK(nextName, rest, body, newEnv, nextK),
          out
        )

  private def applyMapK(
    value: SchemeValue,
    proc: SchemeValue,
    remainingGroups: List[List[SchemeValue]],
    accumulated: List[SchemeValue],
    nextK: Kont,
    out: String
  ): Step =
    val newAcc = accumulated :+ value
    remainingGroups match
      case Nil => ReturnS(SchemeList(newAcc), nextK, out)
      case group :: rest =>
        ProcApply.applyProc(proc, group, Kont.MapK(proc, rest, newAcc, nextK), out)

  private def applyForEachK(
    value: SchemeValue,
    proc: SchemeValue,
    remainingGroups: List[List[SchemeValue]],
    nextK: Kont,
    out: String
  ): Step = remainingGroups match
    case Nil => ReturnS(SchemeVoid, nextK, out)
    case group :: rest =>
      ProcApply.applyProc(proc, group, Kont.ForEachK(proc, rest, nextK), out)

  private def applyCallWithValues(
    value: SchemeValue,
    consumer: SchemeValue,
    nextK: Kont,
    out: String
  ): Step =
    val consumerArgs = value match
      case SchemeMultipleValues(vals) => vals
      case single                     => List(single)
    ProcApply.applyProc(consumer, consumerArgs, nextK, out)

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
