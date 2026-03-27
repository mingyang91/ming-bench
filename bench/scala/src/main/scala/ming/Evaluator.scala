package ming

import EvaluatorForms.*

/** Scheme interpreter entry point. Agents implement this object. */
object Evaluator:

  /** Evaluate one or more Scheme expressions and return the string representation of the last result.
    */
  def evalStr(input: String): String =
    val (result, _) = evalProgram(input)
    SchemeRenderer.render(result)

  /** Evaluate Scheme expressions and return both the result string and any captured output from display/write/newline.
    */
  def evalStrWithOutput(input: String): (String, String) =
    val (result, output) = evalProgram(input)
    (SchemeRenderer.render(result), output)

  private def evalProgram(input: String): (Value, String) =
    val expressions = SchemeParser.parseProgram(input)
    if expressions.isEmpty then throw EvalError.at(SourcePos(1, 1), "empty input")

    val globalEnv = Env.topLevel()
    val context   = new EvalContext
    val result = expressions.foldLeft[Value](Value.Void) { (_, expr) =>
      eval(expr, globalEnv, context)
    }
    (result, context.capturedOutput)

  private def eval(expr: Expr, env: Env, context: EvalContext): Value =
    expr match
      case Expr.IntLit(value, _)                       => Value.IntVal(value)
      case Expr.RationalLit(numerator, denominator, _) => Value.RationalVal(numerator, denominator)
      case Expr.InexactLit(value, _)                   => Value.InexactVal(value)
      case Expr.BoolLit(value, _)                      => Value.BoolVal(value)
      case Expr.StringLit(value, _)                    => Value.StringVal(MutableString.from(value))
      case Expr.CharLit(value, _)                      => Value.CharVal(value)
      case Expr.Symbol(name, pos) =>
        env.lookup(name).getOrElse(throw EvalError.at(pos, s"unbound variable: $name"))
      case Expr.ListExpr(items, pos) =>
        evalList(items, env, pos, context)

  private def evalList(items: List[Expr], env: Env, pos: SourcePos, context: EvalContext): Value =
    items match
      case Nil => throw EvalError.at(pos, "cannot evaluate empty list")
      case Expr.Symbol("define-syntax", formPos) :: args =>
        evalDefineSyntax(args, env, formPos)
      case Expr.Symbol("define-record-type", formPos) :: args =>
        evalDefineRecordType(args, env, formPos)
      case Expr.Symbol("define", formPos) :: args =>
        evalDefine(args, env, formPos, context)
      case Expr.Symbol("if", formPos) :: args =>
        evalIf(args, env, formPos, context)
      case Expr.Symbol("quote", formPos) :: args =>
        evalQuote(args, formPos)
      case Expr.Symbol("lambda", formPos) :: args =>
        evalLambda(args, env, formPos)
      case Expr.Symbol("set!", formPos) :: args =>
        evalSet(args, env, formPos, context)
      case Expr.Symbol("begin", _) :: args =>
        evalBegin(args, env, context)
      case Expr.Symbol("let", formPos) :: args =>
        evalLet(args, env, formPos, context)
      case Expr.Symbol("cond", formPos) :: args =>
        evalCond(args, env, formPos, context)
      case Expr.Symbol("and", _) :: args =>
        evalAnd(args, env, Value.BoolVal(true), context)
      case Expr.Symbol("or", _) :: args =>
        evalOr(args, env, context)
      case Expr.Symbol(name, _) :: _ if env.lookupSyntax(name).isDefined =>
        val expanded = env.lookupSyntax(name).get.expand(Expr.ListExpr(items, pos))
        eval(expanded, env, context)
      case head :: args =>
        applyProcedure(eval(head, env, context), args, env, head.pos, context)

  private def evalDefineSyntax(args: List[Expr], env: Env, pos: SourcePos): Value =
    args match
      case Expr.Symbol(name, _) :: transformerExpr :: Nil =>
        env.defineSyntax(name, SchemeMacros.parseSyntaxRules(name, transformerExpr, env, pos))
        Value.Void
      case _ =>
        throw EvalError.at(pos, "invalid define-syntax")

  private def evalDefineRecordType(args: List[Expr], env: Env, pos: SourcePos): Value =
    SchemeRecords.defineRecordType(args, env, pos)

  private def evalDefine(args: List[Expr], env: Env, pos: SourcePos, context: EvalContext): Value =
    args match
      case Expr.Symbol(name, _) :: valueExpr :: Nil =>
        env.define(name, eval(valueExpr, env, context))
        Value.Void
      case Expr.ListExpr(Expr.Symbol(name, _) :: params, _) :: body if body.nonEmpty =>
        val formals = parseParameterSpec(params)
        val closure =
          Value.Closure(
            name = Some(name),
            params = formals.params,
            restParam = formals.restParam,
            body = body,
            env = env
          )
        env.define(name, closure)
        Value.Void
      case _ =>
        throw EvalError.at(pos, "invalid define")

  private def evalIf(args: List[Expr], env: Env, pos: SourcePos, context: EvalContext): Value =
    args match
      case conditionExpr :: thenExpr :: elseExpr :: Nil =>
        if ValueSemantics.isTruthy(eval(conditionExpr, env, context)) then eval(thenExpr, env, context)
        else eval(elseExpr, env, context)
      case _ =>
        throw EvalError.at(pos, "if expects exactly 3 arguments")

  private def evalQuote(args: List[Expr], pos: SourcePos): Value =
    args match
      case expr :: Nil => ValueSemantics.quote(expr)
      case _           => throw EvalError.at(pos, "quote expects exactly 1 argument")

  private def evalLambda(args: List[Expr], env: Env, pos: SourcePos): Value =
    args match
      case paramsExpr :: body if body.nonEmpty =>
        val formals = parseParameterSpec(paramsExpr)
        Value.Closure(name = None, params = formals.params, restParam = formals.restParam, body = body, env = env)
      case _ =>
        throw EvalError.at(pos, "lambda expects parameters and at least one body expression")

  private def evalSet(args: List[Expr], env: Env, pos: SourcePos, context: EvalContext): Value =
    args match
      case Expr.Symbol(name, symbolPos) :: valueExpr :: Nil =>
        val value = eval(valueExpr, env, context)
        if env.assign(name, value) then Value.Void
        else throw EvalError.at(symbolPos, s"unbound variable: $name")
      case _ =>
        throw EvalError.at(pos, "invalid set!")

  private def evalBegin(args: List[Expr], env: Env, context: EvalContext): Value =
    evalSequence(args, env, context)

  private def evalLet(args: List[Expr], env: Env, pos: SourcePos, context: EvalContext): Value =
    args match
      case Expr.ListExpr(bindings, _) :: body if body.nonEmpty =>
        val letEnv = env.child()
        bindLetValues(letEnv, parseLetBindings(bindings), env, context)
        evalSequence(body, letEnv, context)
      case Expr.Symbol(name, _) :: Expr.ListExpr(bindings, _) :: body if body.nonEmpty =>
        val parsedBindings = parseLetBindings(bindings)
        val evaluatedArgs  = parsedBindings.map(binding => eval(binding.valueExpr, env, context))
        val letEnv         = env.child()
        val closure =
          Value.Closure(
            name = Some(name),
            params = parsedBindings.map(_.name),
            restParam = None,
            body = body,
            env = letEnv
          )
        letEnv.define(name, closure)
        invokeProcedure(closure, evaluatedArgs, pos, context)
      case _ =>
        throw EvalError.at(pos, "invalid let")

  private def bindLetValues(targetEnv: Env, bindings: List[LetBinding], evalEnv: Env, context: EvalContext): Unit =
    bindings.foreach { binding =>
      targetEnv.define(binding.name, eval(binding.valueExpr, evalEnv, context))
    }

  private def evalCond(clauses: List[Expr], env: Env, pos: SourcePos, context: EvalContext): Value =
    clauses match
      case Nil =>
        Value.Void
      case Expr.ListExpr(Expr.Symbol("else", _) :: body, clausePos) :: remaining =>
        if remaining.nonEmpty then throw EvalError.at(clausePos, "else clause must be last")
        if body.isEmpty then throw EvalError.at(clausePos, "else clause must have a body")
        evalSequence(body, env, context)
      case Expr.ListExpr(testExpr :: Nil, _) :: remaining =>
        val testValue = eval(testExpr, env, context)
        if ValueSemantics.isTruthy(testValue) then testValue
        else evalCond(remaining, env, pos, context)
      case Expr.ListExpr(testExpr :: body, _) :: remaining =>
        val testValue = eval(testExpr, env, context)
        if ValueSemantics.isTruthy(testValue) then evalSequence(body, env, context)
        else evalCond(remaining, env, pos, context)
      case invalid :: _ =>
        throw EvalError.at(invalid.pos, "invalid cond clause")

  private def applyProcedure(
    procedure: Value,
    args: List[Expr],
    env: Env,
    pos: SourcePos,
    context: EvalContext
  ): Value =
    val evaluatedArgs = args.map(eval(_, env, context))
    invokeProcedure(procedure, evaluatedArgs, pos, context)

  private def invokeProcedure(
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
        validateArity(name, params.length, restParam, evaluatedArgs.length, pos)

        val callEnv = closureEnv.child()
        params.zip(evaluatedArgs).foreach { case (param, value) =>
          callEnv.define(param, value)
        }
        restParam.foreach { param =>
          callEnv.define(param, ValueSemantics.listFrom(evaluatedArgs.drop(params.length)))
        }
        evalSequence(body, callEnv, context)
      case _ =>
        throw EvalError.at(pos, "attempted to call a non-procedure")

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

  private def evalSequence(exprs: List[Expr], env: Env, context: EvalContext): Value =
    exprs.foldLeft[Value](Value.Void) { (_, expr) =>
      eval(expr, env, context)
    }

  @annotation.tailrec
  private def evalAnd(args: List[Expr], env: Env, lastValue: Value, context: EvalContext): Value =
    args match
      case Nil => lastValue
      case head :: tail =>
        val value = eval(head, env, context)
        if ValueSemantics.isTruthy(value) then evalAnd(tail, env, value, context)
        else value

  @annotation.tailrec
  private def evalOr(args: List[Expr], env: Env, context: EvalContext): Value =
    args match
      case Nil => Value.BoolVal(false)
      case head :: tail =>
        val value = eval(head, env, context)
        if ValueSemantics.isTruthy(value) then value
        else evalOr(tail, env, context)
