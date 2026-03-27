package ming

import EvaluatorForms.*

private[ming] object SpecialFormEvaluator:

  def evalList(items: List[Expr], env: Env, pos: SourcePos, context: EvalContext): Value =
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
      case Expr.Symbol("case-lambda", formPos) :: args =>
        evalCaseLambda(args, env, formPos)
      case Expr.Symbol("set!", formPos) :: args =>
        evalSet(args, env, formPos, context)
      case Expr.Symbol("begin", _) :: args =>
        ExpressionEvaluator.evalSequence(args, env, context)
      case Expr.Symbol("let", formPos) :: args =>
        evalLet(args, env, formPos, context)
      case Expr.Symbol("letrec", formPos) :: args =>
        evalLetrec(args, env, formPos, context, sequential = false)
      case Expr.Symbol("letrec*", formPos) :: args =>
        evalLetrec(args, env, formPos, context, sequential = true)
      case Expr.Symbol("cond", formPos) :: args =>
        evalCond(args, env, formPos, context)
      case Expr.Symbol("case", formPos) :: args =>
        evalCase(args, env, formPos, context)
      case Expr.Symbol("do", formPos) :: args =>
        evalDo(args, env, formPos, context)
      case Expr.Symbol("and", _) :: args =>
        ExpressionEvaluator.evalAnd(args, env, Value.BoolVal(true), context)
      case Expr.Symbol("or", _) :: args =>
        ExpressionEvaluator.evalOr(args, env, context)
      case Expr.Symbol(name, _) :: _ if env.lookupSyntax(name).isDefined =>
        val expanded = env.lookupSyntax(name).get.expand(Expr.ListExpr(items, pos))
        ExpressionEvaluator.eval(expanded, env, context)
      case head :: args =>
        ProcedureInvoker.applyProcedure(ExpressionEvaluator.eval(head, env, context), args, env, head.pos, context)

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

  private def evalIf(args: List[Expr], env: Env, pos: SourcePos, context: EvalContext): Value =
    args match
      case conditionExpr :: thenExpr :: Nil =>
        if ValueSemantics.isTruthy(ExpressionEvaluator.eval(conditionExpr, env, context)) then
          ExpressionEvaluator.eval(thenExpr, env, context)
        else Value.Void
      case conditionExpr :: thenExpr :: elseExpr :: Nil =>
        if ValueSemantics.isTruthy(ExpressionEvaluator.eval(conditionExpr, env, context)) then
          ExpressionEvaluator.eval(thenExpr, env, context)
        else ExpressionEvaluator.eval(elseExpr, env, context)
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

  private def evalLet(args: List[Expr], env: Env, pos: SourcePos, context: EvalContext): Value =
    args match
      case Expr.ListExpr(bindings, _) :: body if body.nonEmpty =>
        val letEnv = env.child()
        bindLetValues(letEnv, parseLetBindings(bindings), env, context)
        ExpressionEvaluator.evalSequence(body, letEnv, context)
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
        ProcedureInvoker.invokeProcedure(closure, evaluatedArgs, pos, context)
      case _ =>
        throw EvalError.at(pos, "invalid let")

  private def bindLetValues(targetEnv: Env, bindings: List[LetBinding], evalEnv: Env, context: EvalContext): Unit =
    bindings.foreach { binding =>
      targetEnv.define(binding.name, ExpressionEvaluator.eval(binding.valueExpr, evalEnv, context))
    }

  private def evalLetrec(
    args: List[Expr],
    env: Env,
    pos: SourcePos,
    context: EvalContext,
    sequential: Boolean
  ): Value =
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

        ExpressionEvaluator.evalSequence(body, recursiveEnv, context)
      case _ =>
        throw EvalError.at(pos, s"invalid ${if sequential then "letrec*" else "letrec"}")

  private def evalCond(clauses: List[Expr], env: Env, pos: SourcePos, context: EvalContext): Value =
    clauses match
      case Nil =>
        Value.Void
      case Expr.ListExpr(Expr.Symbol("else", _) :: body, clausePos) :: remaining =>
        if remaining.nonEmpty then throw EvalError.at(clausePos, "else clause must be last")
        if body.isEmpty then throw EvalError.at(clausePos, "else clause must have a body")
        ExpressionEvaluator.evalSequence(body, env, context)
      case Expr.ListExpr(testExpr :: Nil, _) :: remaining =>
        val testValue = ExpressionEvaluator.eval(testExpr, env, context)
        if ValueSemantics.isTruthy(testValue) then testValue
        else evalCond(remaining, env, pos, context)
      case Expr.ListExpr(testExpr :: body, _) :: remaining =>
        val testValue = ExpressionEvaluator.eval(testExpr, env, context)
        if ValueSemantics.isTruthy(testValue) then ExpressionEvaluator.evalSequence(body, env, context)
        else evalCond(remaining, env, pos, context)
      case invalid :: _ =>
        throw EvalError.at(invalid.pos, "invalid cond clause")

  private def evalCase(args: List[Expr], env: Env, pos: SourcePos, context: EvalContext): Value =
    args match
      case keyExpr :: clauses =>
        evalCaseClauses(ExpressionEvaluator.eval(keyExpr, env, context), clauses, env, pos, context)
      case _ =>
        throw EvalError.at(pos, "case expects a key expression")

  @annotation.tailrec
  private def evalCaseClauses(
    key: Value,
    clauses: List[Expr],
    env: Env,
    pos: SourcePos,
    context: EvalContext
  ): Value =
    clauses match
      case Nil =>
        Value.Void
      case Expr.ListExpr(Expr.Symbol("else", _) :: body, clausePos) :: remaining =>
        if remaining.nonEmpty then throw EvalError.at(clausePos, "else clause must be last")
        ExpressionEvaluator.evalSequence(body, env, context)
      case Expr.ListExpr(Expr.ListExpr(datums, _) :: body, _) :: remaining =>
        if datums.exists(datum => ValueSemantics.isEqv(key, ValueSemantics.quote(datum))) then
          ExpressionEvaluator.evalSequence(body, env, context)
        else evalCaseClauses(key, remaining, env, pos, context)
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
