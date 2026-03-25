package ming

private[ming] object SchemeInterpreter extends SchemeInterpreterTypes:

  import SchemeInterpreterBindingForms.*
  import SchemeInterpreterSpecialForms.*

  sealed private[ming] trait EvalTarget

  private[ming] object EvalTarget:
    final case class ExprTarget(expr: Expr, env: Env, macros: MacroScope)                  extends EvalTarget
    final case class SequenceTarget(expressions: List[Expr], env: Env, macros: MacroScope) extends EvalTarget

  sealed private[ming] trait EvalStep

  private[ming] object EvalStep:
    final case class Done(value: Value)           extends EvalStep
    final case class Continue(target: EvalTarget) extends EvalStep

  def evalProgram(input: String): Value =
    runProgram(input)._1

  def evalProgramWithOutput(input: String): (Value, String) =
    val (result, runtime) = runProgram(input)
    (result, runtime.capturedOutput)

  def render(value: Value): String =
    SchemeRendering.render(value)

  def renderDisplay(value: Value): String =
    SchemeRendering.renderDisplay(value)

  private def runProgram(input: String): (Value, Runtime) =
    val expressions = SchemeReader.readAll(input)
    if expressions.isEmpty then throw EvalError.at(SourcePos(1, 1), "empty input")
    val runtime    = Runtime()
    val env        = SchemeRuntimeSupport.initialEnv(runtime)
    val macroScope = MacroScope.root()
    (evalSequence(expressions, env, macroScope), runtime)

  private[ming] def evalSequence(expressions: List[Expr], env: Env, macros: MacroScope): Value =
    evalLoop(EvalTarget.SequenceTarget(expressions, env, macros))

  private def eval(expr: Expr, env: Env, macros: MacroScope): Value =
    evalLoop(EvalTarget.ExprTarget(expr, env, macros))

  private def evalLoop(initialTarget: EvalTarget): Value =
    var current = initialTarget

    while true do
      current match
        case EvalTarget.ExprTarget(expr, env, macros) =>
          evalExprStep(expr, env, macros) match
            case EvalStep.Done(value)      => return value
            case EvalStep.Continue(target) => current = target
        case EvalTarget.SequenceTarget(expressions, env, macros) =>
          expressions match
            case Nil =>
              return Value.Void
            case head :: Nil =>
              current = EvalTarget.ExprTarget(head, env, macros)
            case head :: tail =>
              eval(head, env, macros)
              current = EvalTarget.SequenceTarget(tail, env, macros)

    throw new IllegalStateException("unreachable")

  private def evalExprStep(expr: Expr, env: Env, macros: MacroScope): EvalStep =
    expr match
      case Expr.Number(value, _)    => EvalStep.Done(Value.Number(value))
      case Expr.Bool(value, _)      => EvalStep.Done(Value.Bool(value))
      case Expr.StringLit(value, _) => EvalStep.Done(Value.StringLit(value))
      case Expr.Character(value, _) => EvalStep.Done(Value.Character(value))
      case Expr.Symbol(name, pos)   => EvalStep.Done(env.lookup(name, pos))
      case Expr.ListExpr(items, pos) =>
        items match
          case Nil => throw EvalError.at(pos, "cannot evaluate empty list")
          case Expr.Symbol("define-record-type", _) :: args =>
            EvalStep.Done(SchemeRecords.evalDefineRecordType(args, env, pos))
          case Expr.Symbol("define-syntax", _) :: args =>
            EvalStep.Done(evalDefineSyntax(args, env, macros, pos))
          case Expr.Symbol("define", _) :: args =>
            EvalStep.Done(evalDefine(args, env, macros, pos, eval))
          case Expr.Symbol("set!", _) :: args =>
            EvalStep.Done(evalSet(args, env, macros, pos, eval))
          case Expr.Symbol("begin", _) :: args   => evalBegin(args, env, macros)
          case Expr.Symbol("if", _) :: args      => evalIf(args, env, macros, pos, eval)
          case Expr.Symbol("let", _) :: args     => evalLet(args, env, macros, pos, eval, applyProcedureStep)
          case Expr.Symbol("let*", _) :: args    => evalLetStar(args, env, macros, pos, eval)
          case Expr.Symbol("letrec", _) :: args  => evalLetrec(args, env, macros, pos, eval)
          case Expr.Symbol("letrec*", _) :: args => evalLetrecStar(args, env, macros, pos, eval)
          case Expr.Symbol("cond", _) :: args    => evalCond(args, env, macros, pos, eval)
          case Expr.Symbol("case", _) :: args    => evalCase(args, env, macros, pos, eval)
          case Expr.Symbol("quote", _) :: args =>
            EvalStep.Done(evalQuote(args, pos))
          case Expr.Symbol("lambda", _) :: args =>
            EvalStep.Done(evalLambda(args, env, macros, pos))
          case Expr.Symbol("case-lambda", _) :: args =>
            EvalStep.Done(SchemeProcedures.evalCaseLambda(args, env, macros, pos))
          case Expr.Symbol("do", _) :: args =>
            EvalStep.Done(SchemeInterpreterDoSupport.eval(args, env, macros, pos, eval, evalSequence))
          case Expr.Symbol("and", _) :: args => evalAnd(args, env, macros, eval)
          case Expr.Symbol("or", _) :: args  => evalOr(args, env, macros, eval)
          case (symbol @ Expr.Symbol(name, _)) :: args =>
            macros.lookup(name) match
              case Some(macroDef) =>
                val expanded = macroDef.expand(Expr.ListExpr(items, pos), pos)
                continueExpr(expanded, env, macros)
              case None =>
                evalProcedureCall(symbol, args, env, macros, pos)
          case head :: args =>
            evalProcedureCall(head, args, env, macros, pos)

  private[ming] def continueExpr(expr: Expr, env: Env, macros: MacroScope): EvalStep =
    EvalStep.Continue(EvalTarget.ExprTarget(expr, env, macros))

  private[ming] def continueSequence(expressions: List[Expr], env: Env, macros: MacroScope): EvalStep =
    EvalStep.Continue(EvalTarget.SequenceTarget(expressions, env, macros))

  private def evalProcedureCall(
    procedureExpr: Expr,
    argExprs: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos
  ): EvalStep =
    val procedure = eval(procedureExpr, env, macros)
    val values    = argExprs.map(eval(_, env, macros))
    applyProcedureStep(procedure, values, pos)

  private[ming] def applyProcedure(value: Value, args: List[Value], pos: SourcePos): Value =
    applyProcedureStep(value, args, pos) match
      case EvalStep.Done(result)     => result
      case EvalStep.Continue(target) => evalLoop(target)

  private def applyProcedureStep(value: Value, args: List[Value], pos: SourcePos): EvalStep =
    value match
      case Value.Builtin(_, impl) =>
        EvalStep.Done(impl(args, pos))
      case Value.Closure(params, body, closureEnv, closureMacros) =>
        val prepared = SchemeProcedures.prepareUserProcedure(
          params,
          body,
          closureEnv,
          closureMacros,
          args,
          pos,
          "lambda"
        )
        continueSequence(prepared.body, prepared.env, prepared.macros)
      case Value.CaseLambda(clauses, closureEnv, closureMacros) =>
        val Value.CaseLambdaClause(params, body) =
          SchemeProcedures.selectCaseLambdaClause(clauses, args.length, pos)
        val prepared = SchemeProcedures.prepareUserProcedure(
          params,
          body,
          closureEnv,
          closureMacros,
          args,
          pos,
          "case-lambda"
        )
        continueSequence(prepared.body, prepared.env, prepared.macros)
      case other =>
        throw EvalError.at(pos, s"not a procedure: ${render(other)}")
