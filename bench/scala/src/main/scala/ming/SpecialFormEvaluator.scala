package ming

import EvaluatorForms.*

private[ming] object SpecialFormEvaluator:

  def evalListStep(items: List[Expr], env: Env, pos: SourcePos, context: EvalContext): EvalStep =
    items match
      case Nil => throw EvalError.at(pos, "cannot evaluate empty list")
      case Expr.Symbol("define-syntax", formPos) :: args =>
        EvalStep.Done(evalDefineSyntax(args, env, formPos))
      case Expr.Symbol("define-record-type", formPos) :: args =>
        EvalStep.Done(evalDefineRecordType(args, env, formPos))
      case Expr.Symbol("define", formPos) :: args =>
        EvalStep.Done(evalDefine(args, env, formPos, context))
      case Expr.Symbol("if", formPos) :: args =>
        evalIfStep(args, env, formPos, context)
      case Expr.Symbol("quote", formPos) :: args =>
        EvalStep.Done(evalQuote(args, formPos))
      case Expr.Symbol("lambda", formPos) :: args =>
        EvalStep.Done(evalLambda(args, env, formPos))
      case Expr.Symbol("case-lambda", formPos) :: args =>
        EvalStep.Done(evalCaseLambda(args, env, formPos))
      case Expr.Symbol("set!", formPos) :: args =>
        EvalStep.Done(evalSet(args, env, formPos, context))
      case Expr.Symbol("begin", _) :: args =>
        EvalStep.EvalSequence(args, env)
      case Expr.Symbol("let", formPos) :: args =>
        evalLetStep(args, env, formPos, context)
      case Expr.Symbol("let*", formPos) :: args =>
        evalLetStarStep(args, env, formPos, context)
      case Expr.Symbol("letrec", formPos) :: args =>
        evalLetrecStep(args, env, formPos, context, sequential = false)
      case Expr.Symbol("letrec*", formPos) :: args =>
        evalLetrecStep(args, env, formPos, context, sequential = true)
      case Expr.Symbol("cond", formPos) :: args =>
        evalCondStep(args, env, formPos, context)
      case Expr.Symbol("case", formPos) :: args =>
        evalCaseStep(args, env, formPos, context)
      case Expr.Symbol("do", formPos) :: args =>
        EvalStep.Done(evalDo(args, env, formPos, context))
      case Expr.Symbol("and", _) :: args =>
        evalAndStep(args, env, context)
      case Expr.Symbol("or", _) :: args =>
        evalOrStep(args, env, context)
      case Expr.Symbol(name, _) :: _ if env.lookupSyntax(name).isDefined =>
        val expanded = env.lookupSyntax(name).get.expand(Expr.ListExpr(items, pos))
        EvalStep.EvalExpr(expanded, env)
      case head :: args =>
        val procedure     = ExpressionEvaluator.eval(head, env, context)
        val evaluatedArgs = args.map(ExpressionEvaluator.eval(_, env, context))
        EvalStep.InvokeProcedure(procedure, evaluatedArgs, head.pos)

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
        env.define(name, ExpressionEvaluator.eval(valueExpr, env, context))
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

  private def evalIfStep(args: List[Expr], env: Env, pos: SourcePos, context: EvalContext): EvalStep =
    args match
      case conditionExpr :: thenExpr :: Nil =>
        if ValueSemantics.isTruthy(ExpressionEvaluator.eval(conditionExpr, env, context)) then
          EvalStep.EvalExpr(thenExpr, env)
        else EvalStep.Done(Value.Void)
      case conditionExpr :: thenExpr :: elseExpr :: Nil =>
        if ValueSemantics.isTruthy(ExpressionEvaluator.eval(conditionExpr, env, context)) then
          EvalStep.EvalExpr(thenExpr, env)
        else EvalStep.EvalExpr(elseExpr, env)
      case _ =>
        throw EvalError.at(pos, "if expects 2 or 3 arguments")

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

  private def evalCaseLambda(args: List[Expr], env: Env, pos: SourcePos): Value =
    Value.CaseClosure(name = None, clauses = ProcedureInvoker.parseCaseLambdaClauses(args), env = env)

  private def evalSet(args: List[Expr], env: Env, pos: SourcePos, context: EvalContext): Value =
    args match
      case Expr.Symbol(name, symbolPos) :: valueExpr :: Nil =>
        val value = ExpressionEvaluator.eval(valueExpr, env, context)
        if env.assign(name, value) then Value.Void
        else throw EvalError.at(symbolPos, s"unbound variable: $name")
      case _ =>
        throw EvalError.at(pos, "invalid set!")

  private def evalLetStep(args: List[Expr], env: Env, pos: SourcePos, context: EvalContext): EvalStep =
    args match
      case Expr.ListExpr(bindings, _) :: body if body.nonEmpty =>
        val letEnv = env.child()
        bindLetValues(letEnv, parseLetBindings(bindings), env, context)
        EvalStep.EvalSequence(body, letEnv)
      case Expr.Symbol(name, _) :: Expr.ListExpr(bindings, _) :: body if body.nonEmpty =>
        val parsedBindings = parseLetBindings(bindings)
        val evaluatedArgs = parsedBindings.map { binding =>
          ExpressionEvaluator.eval(binding.valueExpr, env, context)
        }
        val letEnv = env.child()
        val closure =
          Value.Closure(
            name = Some(name),
            params = parsedBindings.map(_.name),
            restParam = None,
            body = body,
            env = letEnv
          )
        letEnv.define(name, closure)
        EvalStep.InvokeProcedure(closure, evaluatedArgs, pos)
      case _ =>
        throw EvalError.at(pos, "invalid let")

  private def bindLetValues(targetEnv: Env, bindings: List[LetBinding], evalEnv: Env, context: EvalContext): Unit =
    bindings.foreach { binding =>
      targetEnv.define(binding.name, ExpressionEvaluator.eval(binding.valueExpr, evalEnv, context))
    }

  private def evalLetStarStep(args: List[Expr], env: Env, pos: SourcePos, context: EvalContext): EvalStep =
    args match
      case Expr.ListExpr(bindings, _) :: body if body.nonEmpty =>
        val letEnv = env.child()
        parseLetBindings(bindings).foreach { binding =>
          letEnv.define(binding.name, ExpressionEvaluator.eval(binding.valueExpr, letEnv, context))
        }
        EvalStep.EvalSequence(body, letEnv)
      case _ =>
        throw EvalError.at(pos, "invalid let*")

  private def evalLetrecStep(
    args: List[Expr],
    env: Env,
    pos: SourcePos,
    context: EvalContext,
    sequential: Boolean
  ): EvalStep =
    args match
      case Expr.ListExpr(bindings, _) :: body if body.nonEmpty =>
        val recursiveEnv = env.child()
        val preparedBindings = parseLetBindings(bindings).map { binding =>
          val cell = BindingCell(Value.Void)
          recursiveEnv.defineAlias(binding.name, cell)
          (binding, cell)
        }

        if sequential then
          preparedBindings.foreach { case (binding, cell) =>
            cell.value = ExpressionEvaluator.eval(binding.valueExpr, recursiveEnv, context)
          }
        else
          val values = preparedBindings.map { case (binding, _) =>
            ExpressionEvaluator.eval(binding.valueExpr, recursiveEnv, context)
          }
          preparedBindings.zip(values).foreach { case ((_, cell), value) =>
            cell.value = value
          }

        EvalStep.EvalSequence(body, recursiveEnv)
      case _ =>
        throw EvalError.at(pos, s"invalid ${if sequential then "letrec*" else "letrec"}")

  private def evalCondStep(clauses: List[Expr], env: Env, pos: SourcePos, context: EvalContext): EvalStep =
    clauses match
      case Nil =>
        EvalStep.Done(Value.Void)
      case Expr.ListExpr(Expr.Symbol("else", _) :: body, clausePos) :: remaining =>
        if remaining.nonEmpty then throw EvalError.at(clausePos, "else clause must be last")
        if body.isEmpty then throw EvalError.at(clausePos, "else clause must have a body")
        EvalStep.EvalSequence(body, env)
      case Expr.ListExpr(testExpr :: Nil, _) :: remaining =>
        val testValue = ExpressionEvaluator.eval(testExpr, env, context)
        if ValueSemantics.isTruthy(testValue) then EvalStep.Done(testValue)
        else evalCondStep(remaining, env, pos, context)
      case Expr.ListExpr(testExpr :: body, _) :: remaining =>
        val testValue = ExpressionEvaluator.eval(testExpr, env, context)
        if ValueSemantics.isTruthy(testValue) then EvalStep.EvalSequence(body, env)
        else evalCondStep(remaining, env, pos, context)
      case invalid :: _ =>
        throw EvalError.at(invalid.pos, "invalid cond clause")

  private def evalCaseStep(args: List[Expr], env: Env, pos: SourcePos, context: EvalContext): EvalStep =
    args match
      case keyExpr :: clauses =>
        evalCaseClausesStep(ExpressionEvaluator.eval(keyExpr, env, context), clauses, env, pos, context)
      case _ =>
        throw EvalError.at(pos, "case expects a key expression")

  @annotation.tailrec
  private def evalCaseClausesStep(
    key: Value,
    clauses: List[Expr],
    env: Env,
    pos: SourcePos,
    context: EvalContext
  ): EvalStep =
    clauses match
      case Nil =>
        EvalStep.Done(Value.Void)
      case Expr.ListExpr(Expr.Symbol("else", _) :: body, clausePos) :: remaining =>
        if remaining.nonEmpty then throw EvalError.at(clausePos, "else clause must be last")
        EvalStep.EvalSequence(body, env)
      case Expr.ListExpr(Expr.ListExpr(datums, _) :: body, _) :: remaining =>
        if datums.exists(datum => ValueSemantics.isEqv(key, ValueSemantics.quote(datum))) then
          EvalStep.EvalSequence(body, env)
        else evalCaseClausesStep(key, remaining, env, pos, context)
      case invalid :: _ =>
        throw EvalError.at(invalid.pos, "invalid case clause")

  private def evalDo(args: List[Expr], env: Env, pos: SourcePos, context: EvalContext): Value =
    args match
      case Expr.ListExpr(bindings, _) :: Expr.ListExpr(testExpr :: resultExprs, _) :: body =>
        val loopEnv = env.child()
        val loopBindings = parseDoBindings(bindings).map { binding =>
          val cell = BindingCell(ExpressionEvaluator.eval(binding.initExpr, env, context))
          loopEnv.defineAlias(binding.name, cell)
          (binding, cell)
        }

        var result: Value = Value.Void
        var done          = false
        while !done do
          if ValueSemantics.isTruthy(ExpressionEvaluator.eval(testExpr, loopEnv, context)) then
            result = ExpressionEvaluator.evalSequence(resultExprs, loopEnv, context)
            done = true
          else
            ExpressionEvaluator.evalSequence(body, loopEnv, context)
            val nextValues = loopBindings.map { case (binding, cell) =>
              binding.stepExpr match
                case Some(stepExpr) =>
                  ExpressionEvaluator.eval(stepExpr, loopEnv, context)
                case None =>
                  cell.value
            }
            loopBindings.zip(nextValues).foreach { case ((_, cell), value) =>
              cell.value = value
            }

        result
      case _ =>
        throw EvalError.at(pos, "invalid do")

  private def evalAndStep(args: List[Expr], env: Env, context: EvalContext): EvalStep =
    args match
      case Nil =>
        EvalStep.Done(Value.BoolVal(true))
      case _ =>
        var remaining = args
        while remaining.tail.nonEmpty do
          val value = ExpressionEvaluator.eval(remaining.head, env, context)
          if !ValueSemantics.isTruthy(value) then return EvalStep.Done(value)
          remaining = remaining.tail
        EvalStep.EvalExpr(remaining.head, env)

  private def evalOrStep(args: List[Expr], env: Env, context: EvalContext): EvalStep =
    args match
      case Nil =>
        EvalStep.Done(Value.BoolVal(false))
      case _ =>
        var remaining = args
        while remaining.tail.nonEmpty do
          val value = ExpressionEvaluator.eval(remaining.head, env, context)
          if ValueSemantics.isTruthy(value) then return EvalStep.Done(value)
          remaining = remaining.tail
        EvalStep.EvalExpr(remaining.head, env)
