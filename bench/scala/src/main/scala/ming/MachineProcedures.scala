package ming

import BuiltinSupport.*

private[ming] object MachineProcedures:

  def invokeProcedure(machine: Machine, procedure: Value, args: List[Value], pos: SourcePos): Unit =
    procedure match
      case Value.BuiltinProc("call/cc") =>
        prepareCallCc(machine, "call/cc", args, pos)
      case Value.BuiltinProc("call-with-current-continuation") =>
        prepareCallCc(machine, "call-with-current-continuation", args, pos)
      case Value.BuiltinProc("apply") =>
        prepareApply(machine, args, pos)
      case Value.BuiltinProc("map") =>
        prepareMap(machine, args, pos)
      case Value.BuiltinProc("for-each") =>
        prepareForEach(machine, args, pos)
      case Value.BuiltinProc(name) =>
        machine.setValue(Builtins.invoke(name, args, pos, machine.context))
      case Value.RecordConstructor(recordType) =>
        machine.setValue(SchemeRecords.construct(recordType, args, pos))
      case Value.RecordPredicate(recordType) =>
        machine.setValue(SchemeRecords.test(recordType, args, pos))
      case Value.RecordAccessor(recordType, fieldIndex, name) =>
        machine.setValue(SchemeRecords.access(recordType, fieldIndex, name, args, pos))
      case Value.Closure(name, params, restParam, body, closureEnv) =>
        startClosureCall(machine, name, params, restParam, body, closureEnv, args, pos)
      case Value.CaseClosure(name, clauses, closureEnv) =>
        val clause = selectCaseLambdaClause(name, clauses, args.length, pos)
        startClosureCall(machine, name, clause.params, clause.restParam, clause.body, closureEnv, args, pos)
      case continuation: Value.ContinuationVal =>
        val capturedValue = requireSingleArg("continuation", args, pos)
        machine.frames = continuation.snapshot.frames
        machine.setValue(capturedValue)
      case _ =>
        throw EvalError.at(pos, "attempted to call a non-procedure")

  private def startClosureCall(
    machine: Machine,
    name: Option[String],
    params: List[String],
    restParam: Option[String],
    body: List[Expr],
    closureEnv: Env,
    args: List[Value],
    pos: SourcePos
  ): Unit =
    validateArity(name, params.length, restParam, args.length, pos)

    val callEnv = closureEnv.child()
    params.zip(args).foreach { case (param, value) =>
      callEnv.define(param, value)
    }
    restParam.foreach { param =>
      callEnv.define(param, ValueSemantics.listFrom(args.drop(params.length)))
    }
    MachineExpressions.startSequence(machine, body, callEnv)

  private def prepareCallCc(machine: Machine, name: String, args: List[Value], pos: SourcePos): Unit =
    val procedure = requireSingleArg(name, args, pos)
    val captured  = new Value.ContinuationVal(new ContinuationSnapshot(machine.frames))
    machine.setInvoke(procedure, List(captured), pos)

  private def prepareApply(machine: Machine, args: List[Value], pos: SourcePos): Unit =
    if args.lengthCompare(2) < 0 then
      throw EvalError.at(pos, s"apply expects at least 2 argument(s), got ${args.length}")

    val procedure  = args.head
    val prefixArgs = args.slice(1, args.length - 1)
    val listArgs   = ValueSemantics.toProperList("apply", args.last, pos)
    machine.setInvoke(procedure, prefixArgs ++ listArgs, pos)

  private def prepareMap(machine: Machine, args: List[Value], pos: SourcePos): Unit =
    if args.lengthCompare(2) < 0 then throw EvalError.at(pos, s"map expects at least 2 argument(s), got ${args.length}")

    val procedure = args.head
    val lists     = args.tail.map(ValueSemantics.toProperList("map", _, pos))
    val size      = lists.head.length
    if lists.exists(_.lengthCompare(size) != 0) then throw EvalError.at(pos, "map expected lists of equal length")

    val rows =
      if size == 0 then Nil
      else lists.transpose

    rows match
      case Nil =>
        machine.setValue(Value.EmptyList)
      case row :: tail =>
        machine.push(ContinuationFrame.MapResult(procedure, tail, Nil, pos))
        machine.setInvoke(procedure, row, pos)

  private def prepareForEach(machine: Machine, args: List[Value], pos: SourcePos): Unit =
    if args.lengthCompare(2) < 0 then
      throw EvalError.at(pos, s"for-each expects at least 2 argument(s), got ${args.length}")

    val procedure = args.head
    val lists     = args.tail.map(ValueSemantics.toProperList("for-each", _, pos))
    val size      = lists.head.length
    if lists.exists(_.lengthCompare(size) != 0) then throw EvalError.at(pos, "for-each expected lists of equal length")

    val rows =
      if size == 0 then Nil
      else lists.transpose

    rows match
      case Nil =>
        machine.setValue(Value.Void)
      case row :: tail =>
        machine.push(ContinuationFrame.ForEachResult(procedure, tail, pos))
        machine.setInvoke(procedure, row, pos)

  private def selectCaseLambdaClause(
    name: Option[String],
    clauses: List[CaseLambdaClause],
    actualArgCount: Int,
    pos: SourcePos
  ): CaseLambdaClause =
    clauses.find(_.matchesArgCount(actualArgCount)).getOrElse {
      val procName = name.getOrElse("case-lambda")
      throw EvalError.at(pos, s"$procName has no matching clause for $actualArgCount argument(s)")
    }

  private def validateArity(
    name: Option[String],
    fixedParamCount: Int,
    restParam: Option[String],
    actualArgCount: Int,
    pos: SourcePos
  ): Unit =
    restParam match
      case Some(_) if actualArgCount >= fixedParamCount =>
        ()
      case Some(_) =>
        val procName = name.getOrElse("lambda")
        throw EvalError.at(pos, s"$procName expects at least $fixedParamCount argument(s), got $actualArgCount")
      case None if actualArgCount == fixedParamCount =>
        ()
      case None =>
        val procName = name.getOrElse("lambda")
        throw EvalError.at(pos, s"$procName expects $fixedParamCount argument(s), got $actualArgCount")
