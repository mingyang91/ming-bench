package ming

import scala.annotation.tailrec

private[ming] object InterpreterEvaluator:

  private val halt: Continuation = value => FinalStep(value)

  def evalSequence(expressions: List[Expr], env: Environment): Value =
    ExceptionRuntime.withExceptionState(
      DynamicWindRuntime.withWindState(run(deferSequence(expressions, env, halt)))
    )

  def eval(expression: Expr, env: Environment): Value =
    ExceptionRuntime.withExceptionState(
      DynamicWindRuntime.withWindState(run(deferExpr(expression, env, halt)))
    )

  def applyFunction(function: Value, arguments: List[Value], position: Position): Value =
    ExceptionRuntime.withExceptionState(
      DynamicWindRuntime.withWindState(run(deferApplication(function, arguments, position, halt)))
    )

  def done(value: Value, continuation: Continuation): EvaluationStep =
    ReturnStep(value, continuation)

  def deferExpr(
    expression: Expr,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    ContinueStep(EvalExprControl(expression, env, continuation))

  def deferSequence(
    expressions: List[Expr],
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    ContinueStep(EvalSequenceControl(expressions, env, continuation))

  def deferApplication(
    function: Value,
    arguments: List[Value],
    position: Position,
    continuation: Continuation
  ): EvaluationStep =
    ContinueStep(ApplyControl(function, arguments, position, continuation))

  @tailrec
  private def run(step: EvaluationStep): Value =
    step match
      case FinalStep(value) =>
        value
      case ReturnStep(value, continuation) =>
        run(continuation(value))
      case ContinueStep(control) =>
        run(stepControl(control))

  private def stepControl(control: EvaluationControl): EvaluationStep =
    control match
      case EvalExprControl(expression, env, continuation) =>
        evalStep(expression, env, continuation)
      case EvalSequenceControl(expressions, env, continuation) =>
        evalSequenceStep(expressions, env, continuation)
      case ApplyControl(function, arguments, position, continuation) =>
        applyFunctionStep(function, arguments, position, continuation)

  private def evalStep(
    expression: Expr,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    expression match
      case IntExpr(value, _) =>
        done(IntValue(value), continuation)
      case RationalExpr(numerator, denominator, _) =>
        done(NumericSupport.exactValue(numerator, denominator), continuation)
      case InexactExpr(value, _) =>
        done(InexactValue(value), continuation)
      case BoolExpr(value, _) =>
        done(BoolValue(value), continuation)
      case StringExpr(value, _) =>
        done(StringValue(value), continuation)
      case CharExpr(value, _) =>
        done(CharValue(value), continuation)
      case SymbolExpr(name, position) =>
        done(env.lookup(name, position), continuation)
      case ListExpr(items, position) =>
        evalListStep(items, position, env, continuation)

  private def evalSequenceStep(
    expressions: List[Expr],
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    expressions match
      case Nil =>
        done(VoidValue, continuation)
      case expression :: Nil =>
        deferExpr(expression, env, continuation)
      case expression :: rest =>
        deferExpr(
          expression,
          env,
          value => continueSequence(expression, value, rest, env, continuation)
        )

  private def continueSequence(
    expression: Expr,
    value: Value,
    rest: List[Expr],
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    if InterpreterSequencePause.shouldPauseAfter(expression, value) then done(value, continuation)
    else deferSequence(rest, env, continuation)

  private def applyFunctionStep(
    function: Value,
    arguments: List[Value],
    position: Position,
    continuation: Continuation
  ): EvaluationStep =
    ProcedureApplicationEvaluator.apply(function, arguments, position, continuation)

  private def evalListStep(
    items: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    items match
      case Nil =>
        SchemeFailure.raise("cannot evaluate an empty list", position)
      case SymbolExpr("and", _) :: rest =>
        SpecialFormEvaluator.evalAnd(rest, env, continuation)
      case SymbolExpr("begin", _) :: rest =>
        deferSequence(rest, env, continuation)
      case SymbolExpr("case", _) :: rest =>
        SpecialFormEvaluator.evalCase(rest, position, env, continuation)
      case SymbolExpr("cond", _) :: rest =>
        SpecialFormEvaluator.evalCond(rest, env, continuation)
      case SymbolExpr("do", _) :: rest =>
        SpecialFormEvaluator.evalDo(rest, position, env, continuation)
      case SymbolExpr("or", _) :: rest =>
        SpecialFormEvaluator.evalOr(rest, env, continuation)
      case SymbolExpr("define", _) :: rest =>
        SpecialFormEvaluator.evalDefine(rest, position, env, continuation)
      case SymbolExpr("define-record-type", _) :: rest =>
        done(RecordTypeEvaluator.evalDefineRecordType(rest, position, env), continuation)
      case SymbolExpr("define-syntax", _) :: rest =>
        SpecialFormEvaluator.evalDefineSyntax(rest, position, env, continuation)
      case SymbolExpr("guard", _) :: rest =>
        SpecialFormEvaluator.evalGuard(rest, position, env, continuation)
      case SymbolExpr("if", _) :: rest =>
        SpecialFormEvaluator.evalIf(rest, position, env, continuation)
      case SymbolExpr("case-lambda", _) :: rest =>
        SpecialFormEvaluator.evalCaseLambda(rest, position, env, continuation)
      case SymbolExpr("let", _) :: rest =>
        SpecialFormEvaluator.evalLet(rest, position, env, continuation)
      case SymbolExpr("let*", _) :: rest =>
        SpecialFormEvaluator.evalLetStar(rest, position, env, continuation)
      case SymbolExpr("letrec", _) :: rest =>
        SpecialFormEvaluator.evalLetrec(rest, position, env, continuation)
      case SymbolExpr("letrec*", _) :: rest =>
        SpecialFormEvaluator.evalLetrecStar(rest, position, env, continuation)
      case SymbolExpr("quasiquote", _) :: rest if env.lookupMacro("quasiquote").isEmpty =>
        SpecialFormEvaluator.evalQuasiquote(rest, position, env, continuation)
      case SymbolExpr("quote", _) :: rest =>
        SpecialFormEvaluator.evalQuote(rest, position, continuation)
      case SymbolExpr("lambda", _) :: rest =>
        SpecialFormEvaluator.evalLambda(rest, position, env, continuation)
      case SymbolExpr("set!", _) :: rest =>
        SpecialFormEvaluator.evalSet(rest, position, env, continuation)
      case SymbolExpr("syntax", _) :: rest =>
        SpecialFormEvaluator.evalSyntax(rest, position, env, continuation)
      case SymbolExpr("syntax-case", _) :: rest =>
        SpecialFormEvaluator.evalSyntaxCase(rest, position, env, continuation)
      case SymbolExpr("with-syntax", _) :: rest =>
        SpecialFormEvaluator.evalWithSyntax(rest, position, env, continuation)
      case (operator @ SymbolExpr(name, _)) :: arguments =>
        env.lookupMacro(name) match
          case Some(macroDefinition) =>
            evalMacroApplication(ListExpr(operator :: arguments, position), macroDefinition, env, continuation)
          case None =>
            evalApplicationStep(operator, arguments, position, env, continuation)
      case operator :: arguments =>
        evalApplicationStep(operator, arguments, position, env, continuation)

  private def evalMacroApplication(
    application: ListExpr,
    macroDefinition: MacroDefinition,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    val expansion =
      macroDefinition match
        case syntaxRulesMacro: SyntaxRulesMacro =>
          MacroExpander.expand(application, syntaxRulesMacro)
        case ProcedureMacro(_, transformer, definitionEnv) =>
          val result =
            MacroRuntime.withDefinitionEnv(definitionEnv):
              applyFunction(
                transformer,
                List(SyntaxObjectValue(MacroExpansion(application, Nil))),
                application.position
              )
          RuntimeSupport
            .expectSyntaxObject(
              RuntimeSupport.expectSingleValue(result, "macro transformer", application.position),
              "macro transformer",
              application.position
            )
            .expansion
    expansion.aliases.foreach((alias, cell) => env.defineAlias(alias, cell))
    deferExpr(expansion.expr, env, continuation)

  private def evalApplicationStep(
    operator: Expr,
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    deferExpr(
      operator,
      env,
      function =>
        evalArgumentExpressions(
          RuntimeSupport.expectSingleValue(function, "procedure application", operator.position),
          arguments.reverse,
          env,
          position,
          continuation
        )
    )

  private def evalArgumentExpressions(
    function: Value,
    arguments: List[Expr],
    env: Environment,
    position: Position,
    continuation: Continuation,
    evaluatedArguments: List[Value] = Nil
  ): EvaluationStep =
    arguments match
      case Nil =>
        deferApplication(function, evaluatedArguments, position, continuation)
      case argument :: rest =>
        deferExpr(
          argument,
          env,
          value =>
            evalArgumentExpressions(
              function,
              rest,
              env,
              position,
              continuation,
              RuntimeSupport.expectSingleValue(value, "procedure application", argument.position) :: evaluatedArguments
            )
        )
