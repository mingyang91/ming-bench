package ming

import RuntimeSupport.isTruthy
import SpecialFormBindingSyntax.*

private[ming] object SpecialFormBindingEvaluator:

  def evalLet(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    arguments match
      case SymbolExpr(name, _) :: bindingsExpression :: body if body.nonEmpty =>
        evalNamedLet(name, bindingsExpression, body, position, env, continuation)
      case bindingsExpression :: body if body.nonEmpty =>
        evalUnnamedLet(bindingsExpression, body, position, env, continuation)
      case _ =>
        SchemeFailure.raise("let expected bindings and body", position)

  def evalLetrec(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    evalRecursiveLet(arguments, position, env, "letrec", RecursiveLetMode.Parallel, continuation)

  def evalLetrecStar(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    evalRecursiveLet(arguments, position, env, "letrec*", RecursiveLetMode.Sequential, continuation)

  def evalLetStar(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    arguments match
      case bindingsExpression :: body if body.nonEmpty =>
        val bindings = parseBindings(bindingsExpression, position, "let*")
        val childEnv = Environment.child(env)
        evalLetStarBindings(bindings, childEnv, body, continuation)
      case _ =>
        SchemeFailure.raise("let* expected bindings and body", position)

  def evalDo(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    arguments match
      case bindingsExpression :: testClauseExpression :: body =>
        val bindings     = parseDoBindings(bindingsExpression, position)
        val doTestClause = parseDoTestClause(testClauseExpression)
        evalExpressionList(
          bindings.map(_.initExpression),
          env,
          initialValues =>
            val loopEnv = Environment.child(env, bindings.map(_.name).zip(initialValues))
            evalDoLoop(bindings, doTestClause, body, loopEnv, continuation)
        )
      case _ =>
        SchemeFailure.raise("do expected bindings and a test clause", position)

  private def evalUnnamedLet(
    bindingsExpression: Expr,
    body: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    val bindings = parseBindings(bindingsExpression, position, "let")
    evalExpressionList(
      bindings.map(_.valueExpression),
      env,
      boundValues =>
        val childEnv = Environment.child(env, bindings.map(_.name).zip(boundValues))
        InterpreterEvaluator.deferSequence(body, childEnv, continuation)
    )

  private def evalNamedLet(
    name: String,
    bindingsExpression: Expr,
    body: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    val bindings = parseBindings(bindingsExpression, position, "let")
    evalExpressionList(
      bindings.map(_.valueExpression),
      env,
      arguments =>
        val closureEnv = Environment.child(env)
        val closure    = ClosureValue(bindings.map(_.name), None, body, closureEnv, Some(name))
        closureEnv.define(name, closure)
        InterpreterEvaluator.deferApplication(closure, arguments, position, continuation)
    )

  private def evalRecursiveLet(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    formName: String,
    mode: RecursiveLetMode,
    continuation: Continuation
  ): EvaluationStep =
    arguments match
      case bindingsExpression :: body if body.nonEmpty =>
        val bindings = parseBindings(bindingsExpression, position, formName)
        val childEnv = Environment.child(env)
        reserveBindings(bindings, childEnv)
        initializeRecursiveBindings(
          bindings,
          childEnv,
          position,
          mode,
          () => InterpreterEvaluator.deferSequence(body, childEnv, continuation)
        )
      case _ =>
        SchemeFailure.raise(s"$formName expected bindings and body", position)

  private def reserveBindings(bindings: List[Binding], env: Environment): Unit =
    bindings.foreach(binding => env.reserve(binding.name))

  private def initializeRecursiveBindings(
    bindings: List[Binding],
    env: Environment,
    position: Position,
    mode: RecursiveLetMode,
    finish: () => EvaluationStep
  ): EvaluationStep =
    mode match
      case RecursiveLetMode.Sequential =>
        initializeRecursiveBindingsSequential(bindings, env, position, finish)
      case RecursiveLetMode.Parallel =>
        evalExpressionList(
          bindings.map(_.valueExpression),
          env,
          values =>
            bindings
              .zip(values)
              .foreach: (binding, value) =>
                env.assign(binding.name, value, position)
            finish()
        )

  private def initializeRecursiveBindingsSequential(
    bindings: List[Binding],
    env: Environment,
    position: Position,
    finish: () => EvaluationStep
  ): EvaluationStep =
    bindings match
      case Nil =>
        finish()
      case binding :: rest =>
        InterpreterEvaluator.deferExpr(
          binding.valueExpression,
          env,
          value =>
            env.assign(binding.name, value, position)
            initializeRecursiveBindingsSequential(rest, env, position, finish)
        )

  private def evalLetStarBindings(
    bindings: List[Binding],
    childEnv: Environment,
    body: List[Expr],
    continuation: Continuation
  ): EvaluationStep =
    bindings match
      case Nil =>
        InterpreterEvaluator.deferSequence(body, childEnv, continuation)
      case binding :: rest =>
        InterpreterEvaluator.deferExpr(
          binding.valueExpression,
          childEnv,
          value =>
            childEnv.define(binding.name, value)
            evalLetStarBindings(rest, childEnv, body, continuation)
        )

  private def evalDoLoop(
    bindings: List[DoBinding],
    testClause: DoTestClause,
    body: List[Expr],
    loopEnv: Environment,
    continuation: Continuation
  ): EvaluationStep =
    InterpreterEvaluator.deferExpr(
      testClause.testExpression,
      loopEnv,
      testValue =>
        if isTruthy(testValue) then evalDoFinalExpressions(testClause.finalExpressions, loopEnv, continuation)
        else evalDoBodyAndAdvance(bindings, testClause, body, loopEnv, continuation)
    )

  private def evalDoFinalExpressions(
    finalExpressions: List[Expr],
    loopEnv: Environment,
    continuation: Continuation
  ): EvaluationStep =
    finalExpressions match
      case Nil =>
        InterpreterEvaluator.done(VoidValue, continuation)
      case _ =>
        InterpreterEvaluator.deferSequence(finalExpressions, loopEnv, continuation)

  private def evalDoBodyAndAdvance(
    bindings: List[DoBinding],
    testClause: DoTestClause,
    body: List[Expr],
    loopEnv: Environment,
    continuation: Continuation
  ): EvaluationStep =
    val finish = () =>
      advanceDoBindings(bindings, loopEnv, () => evalDoLoop(bindings, testClause, body, loopEnv, continuation))

    if body.nonEmpty then InterpreterEvaluator.deferSequence(body, loopEnv, _ => finish())
    else finish()

  private def advanceDoBindings(
    bindings: List[DoBinding],
    loopEnv: Environment,
    finish: () => EvaluationStep
  ): EvaluationStep =
    evalDoNextValues(
      bindings,
      loopEnv,
      nextValues =>
        bindings
          .zip(nextValues)
          .foreach: (binding, value) =>
            loopEnv.assign(binding.name, value, binding.position)
        finish()
    )

  private def evalDoNextValues(
    bindings: List[DoBinding],
    loopEnv: Environment,
    finish: List[Value] => EvaluationStep,
    reversedValues: List[Value] = Nil
  ): EvaluationStep =
    bindings match
      case Nil =>
        finish(reversedValues.reverse)
      case binding :: rest =>
        binding.stepExpression match
          case Some(stepExpression) =>
            InterpreterEvaluator.deferExpr(
              stepExpression,
              loopEnv,
              value => evalDoNextValues(rest, loopEnv, finish, value :: reversedValues)
            )
          case None =>
            evalDoNextValues(
              rest,
              loopEnv,
              finish,
              loopEnv.lookup(binding.name, binding.position) :: reversedValues
            )

  private def evalExpressionList(
    expressions: List[Expr],
    env: Environment,
    finish: List[Value] => EvaluationStep,
    reversedValues: List[Value] = Nil
  ): EvaluationStep =
    expressions match
      case Nil =>
        finish(reversedValues.reverse)
      case expression :: rest =>
        InterpreterEvaluator.deferExpr(
          expression,
          env,
          value => evalExpressionList(rest, env, finish, value :: reversedValues)
        )
