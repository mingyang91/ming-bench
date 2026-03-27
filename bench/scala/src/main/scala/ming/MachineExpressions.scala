package ming

import ContinuationFrame.*

private[ming] object MachineExpressions:

  def evalExpr(machine: Machine, expr: Expr, env: Env): Unit =
    expr match
      case Expr.IntLit(value, _) =>
        machine.setValue(Value.IntVal(value))
      case Expr.RationalLit(numerator, denominator, _) =>
        machine.setValue(Value.RationalVal(numerator, denominator))
      case Expr.InexactLit(value, _) =>
        machine.setValue(Value.InexactVal(value))
      case Expr.BoolLit(value, _) =>
        machine.setValue(Value.BoolVal(value))
      case Expr.StringLit(value, _) =>
        machine.setValue(Value.StringVal(MutableString.immutable(value)))
      case Expr.CharLit(value, _) =>
        machine.setValue(Value.CharVal(value))
      case Expr.Symbol(name, pos) =>
        machine.setValue(env.lookup(name).getOrElse(throw EvalError.at(pos, s"unbound variable: $name")))
      case vectorExpr @ Expr.VectorExpr(_, _) =>
        machine.setValue(ValueSemantics.quote(vectorExpr))
      case Expr.ListExpr(items, pos) =>
        evalList(machine, items, env, pos)

  def startSequence(machine: Machine, exprs: List[Expr], env: Env): Unit =
    exprs match
      case Nil =>
        machine.setValue(Value.Void)
      case head :: Nil =>
        machine.setExpr(head, env)
      case head :: tail =>
        machine.push(Sequence(tail, env))
        machine.setExpr(head, env)

  def startCond(machine: Machine, clauses: List[Expr], env: Env, pos: SourcePos): Unit =
    clauses match
      case Nil =>
        machine.setValue(Value.Void)
      case Expr.ListExpr(Expr.Symbol("else", _) :: body, clausePos) :: remaining =>
        if remaining.nonEmpty then throw EvalError.at(clausePos, "else clause must be last")
        if body.isEmpty then throw EvalError.at(clausePos, "else clause must have a body")
        startSequence(machine, body, env)
      case Expr.ListExpr(testExpr :: body, _) :: remaining =>
        machine.push(CondClause(body, remaining, env, pos))
        machine.setExpr(testExpr, env)
      case invalid :: _ =>
        throw EvalError.at(invalid.pos, "invalid cond clause")

  @annotation.tailrec
  def startCaseClauses(machine: Machine, key: Value, clauses: List[Expr], env: Env, pos: SourcePos): Unit =
    clauses match
      case Nil =>
        machine.setValue(Value.Void)
      case Expr.ListExpr(Expr.Symbol("else", _) :: body, clausePos) :: remaining =>
        if remaining.nonEmpty then throw EvalError.at(clausePos, "else clause must be last")
        startSequence(machine, body, env)
      case Expr.ListExpr(Expr.ListExpr(datums, _) :: body, _) :: remaining =>
        if datums.exists(datum => ValueSemantics.isEqv(key, ValueSemantics.quote(datum))) then
          startSequence(machine, body, env)
        else startCaseClauses(machine, key, remaining, env, pos)
      case invalid :: _ =>
        throw EvalError.at(invalid.pos, "invalid case clause")

  private def evalList(machine: Machine, items: List[Expr], env: Env, pos: SourcePos): Unit =
    items match
      case Nil =>
        throw EvalError.at(pos, "cannot evaluate empty list")
      case Expr.Symbol("define-syntax", formPos) :: args =>
        machine.setValue(evalDefineSyntax(args, env, formPos))
      case Expr.Symbol("define-record-type", formPos) :: args =>
        machine.setValue(SchemeRecords.defineRecordType(args, env, formPos))
      case Expr.Symbol("define", formPos) :: args =>
        startDefine(machine, args, env, formPos)
      case Expr.Symbol("if", formPos) :: args =>
        startIf(machine, args, env, formPos)
      case Expr.Symbol("quote", formPos) :: args =>
        machine.setValue(evalQuote(args, formPos))
      case Expr.Symbol("lambda", formPos) :: args =>
        machine.setValue(evalLambda(args, env, formPos))
      case Expr.Symbol("case-lambda", formPos) :: args =>
        machine.setValue(
          Value.CaseClosure(name = None, clauses = ProcedureInvoker.parseCaseLambdaClauses(args), env = env)
        )
      case Expr.Symbol("set!", formPos) :: args =>
        startSet(machine, args, env, formPos)
      case Expr.Symbol("begin", _) :: args =>
        startSequence(machine, args, env)
      case Expr.Symbol("let", formPos) :: args =>
        machine.setExpr(SpecialFormTransforms.desugarLet(args, formPos), env)
      case Expr.Symbol("let*", formPos) :: args =>
        machine.setExpr(SpecialFormTransforms.desugarLetStar(args, formPos), env)
      case Expr.Symbol("letrec", formPos) :: args =>
        startLetrec(machine, args, env, formPos, sequential = false)
      case Expr.Symbol("letrec*", formPos) :: args =>
        startLetrec(machine, args, env, formPos, sequential = true)
      case Expr.Symbol("cond", formPos) :: args =>
        startCond(machine, args, env, formPos)
      case Expr.Symbol("guard", formPos) :: args =>
        machine.setExpr(SpecialFormTransforms.desugarGuard(args, formPos), env)
      case Expr.Symbol("case", formPos) :: args =>
        startCase(machine, args, env, formPos)
      case Expr.Symbol("do", formPos) :: args =>
        machine.setExpr(SpecialFormTransforms.desugarDo(args, formPos), env)
      case Expr.Symbol("and", _) :: args =>
        startAnd(machine, args, env)
      case Expr.Symbol("or", _) :: args =>
        startOr(machine, args, env)
      case Expr.Symbol(name, _) :: _ =>
        env.lookupSyntax(name) match
          case Some(transformer) =>
            machine.setExpr(transformer.expand(Expr.ListExpr(items, pos)), env)
          case None =>
            startApplication(machine, items.head, items.tail, env)
      case head :: args =>
        startApplication(machine, head, args, env)

  private def startApplication(machine: Machine, head: Expr, args: List[Expr], env: Env): Unit =
    machine.push(CallHead(args, env, head.pos))
    machine.setExpr(head, env)

  private def startDefine(machine: Machine, args: List[Expr], env: Env, pos: SourcePos): Unit =
    args match
      case Expr.Symbol(name, _) :: valueExpr :: Nil =>
        machine.push(DefineValue(name, env))
        machine.setExpr(valueExpr, env)
      case Expr.ListExpr(Expr.Symbol(name, _) :: params, _) :: body if body.nonEmpty =>
        val formals = EvaluatorForms.parseParameterSpec(params)
        env.define(
          name,
          Value.Closure(
            name = Some(name),
            params = formals.params,
            restParam = formals.restParam,
            body = body,
            env = env
          )
        )
        machine.setValue(Value.Void)
      case _ =>
        throw EvalError.at(pos, "invalid define")

  private def startSet(machine: Machine, args: List[Expr], env: Env, pos: SourcePos): Unit =
    args match
      case Expr.Symbol(name, symbolPos) :: valueExpr :: Nil =>
        machine.push(SetValue(name, symbolPos, env))
        machine.setExpr(valueExpr, env)
      case _ =>
        throw EvalError.at(pos, "invalid set!")

  private def startIf(machine: Machine, args: List[Expr], env: Env, pos: SourcePos): Unit =
    args match
      case conditionExpr :: thenExpr :: Nil =>
        machine.push(IfBranch(thenExpr, None, env))
        machine.setExpr(conditionExpr, env)
      case conditionExpr :: thenExpr :: elseExpr :: Nil =>
        machine.push(IfBranch(thenExpr, Some(elseExpr), env))
        machine.setExpr(conditionExpr, env)
      case _ =>
        throw EvalError.at(pos, "if expects 2 or 3 arguments")

  private def startAnd(machine: Machine, args: List[Expr], env: Env): Unit =
    args match
      case Nil =>
        machine.setValue(Value.BoolVal(true))
      case head :: Nil =>
        machine.setExpr(head, env)
      case head :: tail =>
        machine.push(And(tail, env))
        machine.setExpr(head, env)

  private def startOr(machine: Machine, args: List[Expr], env: Env): Unit =
    args match
      case Nil =>
        machine.setValue(Value.BoolVal(false))
      case head :: Nil =>
        machine.setExpr(head, env)
      case head :: tail =>
        machine.push(Or(tail, env))
        machine.setExpr(head, env)

  private def startCase(machine: Machine, args: List[Expr], env: Env, pos: SourcePos): Unit =
    args match
      case keyExpr :: clauses =>
        machine.push(CaseKey(clauses, env, pos))
        machine.setExpr(keyExpr, env)
      case _ =>
        throw EvalError.at(pos, "case expects a key expression")

  private def startLetrec(
    machine: Machine,
    args: List[Expr],
    env: Env,
    pos: SourcePos,
    sequential: Boolean
  ): Unit =
    args match
      case Expr.ListExpr(bindings, _) :: body if body.nonEmpty =>
        val recursiveEnv = env.child()
        val preparedBindings = EvaluatorForms.parseLetBindings(bindings).map { binding =>
          val cell = BindingCell(Value.Void)
          recursiveEnv.defineAlias(binding.name, cell)
          (binding, cell)
        }

        preparedBindings match
          case Nil =>
            startSequence(machine, body, recursiveEnv)
          case (binding, cell) :: remaining =>
            if sequential then machine.push(LetrecSequentialValue(cell, remaining, recursiveEnv, body))
            else machine.push(LetrecParallelValue(cell, remaining, Nil, recursiveEnv, body))
            machine.setExpr(binding.valueExpr, recursiveEnv)
      case _ =>
        throw EvalError.at(pos, s"invalid ${if sequential then "letrec*" else "letrec"}")

  private def evalDefineSyntax(args: List[Expr], env: Env, pos: SourcePos): Value =
    args match
      case Expr.Symbol(name, _) :: transformerExpr :: Nil =>
        env.defineSyntax(name, SchemeMacros.parseTransformer(name, transformerExpr, env, pos))
        Value.Void
      case _ =>
        throw EvalError.at(pos, "invalid define-syntax")

  private def evalQuote(args: List[Expr], pos: SourcePos): Value =
    args match
      case expr :: Nil =>
        ValueSemantics.quote(expr)
      case _ =>
        throw EvalError.at(pos, "quote expects exactly 1 argument")

  private def evalLambda(args: List[Expr], env: Env, pos: SourcePos): Value =
    args match
      case paramsExpr :: body if body.nonEmpty =>
        val formals = EvaluatorForms.parseParameterSpec(paramsExpr)
        Value.Closure(name = None, params = formals.params, restParam = formals.restParam, body = body, env = env)
      case _ =>
        throw EvalError.at(pos, "lambda expects parameters and at least one body expression")
