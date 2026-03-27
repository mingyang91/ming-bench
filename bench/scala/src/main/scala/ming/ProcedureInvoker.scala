package ming

import EvaluatorForms.*

private[ming] object ProcedureInvoker:

  def applyProcedure(
    procedure: Value,
    args: List[Expr],
    env: Env,
    pos: SourcePos,
    context: EvalContext
  ): Value =
    val evaluatedArgs = args.map(ExpressionEvaluator.eval(_, env, context))
    invokeProcedure(procedure, evaluatedArgs, pos, context)

  def invokeProcedure(
    procedure: Value,
    evaluatedArgs: List[Value],
    pos: SourcePos,
    context: EvalContext
  ): Value =
    procedure match
      case Value.BuiltinProc("apply") =>
        invokeApply(evaluatedArgs, pos, context)
      case Value.BuiltinProc("map") =>
        invokeMap(evaluatedArgs, pos, context)
      case Value.BuiltinProc(name) =>
        Builtins.invoke(name, evaluatedArgs, pos, context)
      case Value.RecordConstructor(recordType) =>
        SchemeRecords.construct(recordType, evaluatedArgs, pos)
      case Value.RecordPredicate(recordType) =>
        SchemeRecords.test(recordType, evaluatedArgs, pos)
      case Value.RecordAccessor(recordType, fieldIndex, name) =>
        SchemeRecords.access(recordType, fieldIndex, name, evaluatedArgs, pos)
      case Value.Closure(name, params, restParam, body, closureEnv) =>
        invokeUserProcedure(name, params, restParam, body, closureEnv, evaluatedArgs, pos, context)
      case Value.CaseClosure(name, clauses, closureEnv) =>
        val clause = selectCaseLambdaClause(name, clauses, evaluatedArgs.length, pos)
        invokeUserProcedure(name, clause.params, clause.restParam, clause.body, closureEnv, evaluatedArgs, pos, context)
      case _ =>
        throw EvalError.at(pos, "attempted to call a non-procedure")

  def parseCaseLambdaClauses(clauses: List[Expr]): List[CaseLambdaClause] =
    clauses.map {
      case Expr.ListExpr(paramsExpr :: body, _) if body.nonEmpty =>
        val formals = parseParameterSpec(paramsExpr)
        CaseLambdaClause(formals.params, formals.restParam, body)
      case Expr.ListExpr(_, clausePos) =>
        throw EvalError.at(clausePos, "case-lambda clause must include parameters and a body")
      case invalid =>
        throw EvalError.at(invalid.pos, "invalid case-lambda clause")
    }

  private def invokeApply(args: List[Value], pos: SourcePos, context: EvalContext): Value =
    if args.lengthCompare(2) < 0 then
      throw EvalError.at(pos, s"apply expects at least 2 argument(s), got ${args.length}")

    val procedure  = args.head
    val prefixArgs = args.slice(1, args.length - 1)
    val listArgs   = ValueSemantics.toProperList("apply", args.last, pos)
    invokeProcedure(procedure, prefixArgs ++ listArgs, pos, context)

  private def invokeMap(args: List[Value], pos: SourcePos, context: EvalContext): Value =
    if args.lengthCompare(2) < 0 then throw EvalError.at(pos, s"map expects at least 2 argument(s), got ${args.length}")

    val procedure = args.head
    val lists     = args.tail.map(ValueSemantics.toProperList("map", _, pos))
    val size      = lists.head.length
    if lists.exists(_.lengthCompare(size) != 0) then throw EvalError.at(pos, "map expected lists of equal length")

    val results =
      if size == 0 then Nil
      else lists.transpose.map(values => invokeProcedure(procedure, values, pos, context))

    ValueSemantics.listFrom(results)

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

  private def invokeUserProcedure(
    name: Option[String],
    params: List[String],
    restParam: Option[String],
    body: List[Expr],
    closureEnv: Env,
    evaluatedArgs: List[Value],
    pos: SourcePos,
    context: EvalContext
  ): Value =
    validateArity(name, params.length, restParam, evaluatedArgs.length, pos)

    val callEnv = closureEnv.child()
    params.zip(evaluatedArgs).foreach { case (param, value) =>
      callEnv.define(param, value)
    }
    restParam.foreach { param =>
      callEnv.define(param, ValueSemantics.listFrom(evaluatedArgs.drop(params.length)))
    }
    ExpressionEvaluator.evalSequence(body, callEnv, context)

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
