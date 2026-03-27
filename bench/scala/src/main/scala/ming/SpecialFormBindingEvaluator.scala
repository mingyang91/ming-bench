package ming

import RuntimeSupport.expectSingleValue
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

  private def evalUnnamedLet(
    bindingsExpression: Expr,
    body: List[Expr],
    position: Position,
    env: Environment,
    continuation: Continuation
  ): EvaluationStep =
    val bindings = parseBindings(bindingsExpression, position, "let")
    SingleValueExpressionEvaluator.evalAll(
      bindings.map(_.valueExpression),
      env,
      "binding",
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
    SingleValueExpressionEvaluator.evalAll(
      bindings.map(_.valueExpression),
      env,
      "binding",
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
        SingleValueExpressionEvaluator.evalAll(
          bindings.map(_.valueExpression),
          env,
          "binding",
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
            env.assign(
              binding.name,
              expectSingleValue(value, "binding", binding.valueExpression.position),
              position
            )
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
            childEnv.define(
              binding.name,
              expectSingleValue(value, "let*", binding.valueExpression.position)
            )
            evalLetStarBindings(rest, childEnv, body, continuation)
        )
