package ming

private[ming] object SpecialFormEvaluator:

  def evalAnd(
    expressions: List[Expr],
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    SpecialFormConditionalEvaluator.evalAnd(expressions, env, continuation)

  def evalOr(
    expressions: List[Expr],
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    SpecialFormConditionalEvaluator.evalOr(expressions, env, continuation)

  def evalCond(
    clauses: List[Expr],
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    SpecialFormConditionalEvaluator.evalCond(clauses, env, continuation)

  def evalDefine(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    SpecialFormDefinitionEvaluator.evalDefine(arguments, position, env, continuation)

  def evalDefineSyntax(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    SpecialFormDefinitionEvaluator.evalDefineSyntax(arguments, position, env, continuation)

  def evalIf(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    SpecialFormConditionalEvaluator.evalIf(arguments, position, env, continuation)

  def evalGuard(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    SpecialFormGuardEvaluator.evalGuard(arguments, position, env, continuation)

  def evalLet(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    SpecialFormBindingEvaluator.evalLet(arguments, position, env, continuation)

  def evalLetrec(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    SpecialFormBindingEvaluator.evalLetrec(arguments, position, env, continuation)

  def evalLetrecStar(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    SpecialFormBindingEvaluator.evalLetrecStar(arguments, position, env, continuation)

  def evalLetStar(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    SpecialFormBindingEvaluator.evalLetStar(arguments, position, env, continuation)

  def evalQuote(
    arguments: List[Expr],
    position: Position,
    continuation: Continuation
  ): EvaluationStep =
    InterpreterEvaluator.done(
      SpecialFormQuoteEvaluator.evalQuote(arguments, position),
      continuation
    )

  def evalLambda(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    InterpreterEvaluator.done(
      SpecialFormProcedureEvaluator.evalLambda(arguments, position, env),
      continuation
    )

  def evalCaseLambda(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    InterpreterEvaluator.done(
      SpecialFormProcedureEvaluator.evalCaseLambda(arguments, position, env),
      continuation
    )

  def evalCase(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    SpecialFormConditionalEvaluator.evalCase(arguments, position, env, continuation)

  def evalDo(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    DoEvaluator.eval(arguments, position, env, continuation)

  def evalSyntax(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    SpecialFormSyntaxEvaluator.evalSyntax(arguments, position, env, continuation)

  def evalSyntaxCase(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    SpecialFormSyntaxEvaluator.evalSyntaxCase(arguments, position, env, continuation)

  def evalWithSyntax(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    SpecialFormSyntaxEvaluator.evalWithSyntax(arguments, position, env, continuation)

  def evalSet(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    SpecialFormDefinitionEvaluator.evalSet(arguments, position, env, continuation)
