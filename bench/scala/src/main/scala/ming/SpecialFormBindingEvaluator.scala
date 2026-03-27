package ming

import RuntimeSupport.isTruthy

import scala.annotation.tailrec

private[ming] object SpecialFormBindingEvaluator:

  final private case class Binding(name: String, valueExpression: Expr)

  final private case class DoBinding(
    name: String,
    initExpression: Expr,
    stepExpression: Option[Expr],
    position: Position
  )

  final private case class DoTestClause(
    testExpression: Expr,
    finalExpressions: List[Expr]
  )

  private enum RecursiveLetMode:
    case Parallel
    case Sequential

  def evalLet(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    arguments match
      case SymbolExpr(name, _) :: bindingsExpression :: body if body.nonEmpty =>
        evalNamedLet(name, bindingsExpression, body, position, env)
      case bindingsExpression :: body if body.nonEmpty =>
        evalUnnamedLet(bindingsExpression, body, position, env)
      case _ =>
        SchemeFailure.raise("let expected bindings and body", position)

  def evalLetrec(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    evalRecursiveLet(arguments, position, env, "letrec", RecursiveLetMode.Parallel)

  def evalLetrecStar(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    evalRecursiveLet(arguments, position, env, "letrec*", RecursiveLetMode.Sequential)

  def evalDo(
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    arguments match
      case bindingsExpression :: testClauseExpression :: body =>
        val bindings      = parseDoBindings(bindingsExpression, position)
        val doTestClause  = parseDoTestClause(testClauseExpression)
        val initialValues = bindings.map(binding => InterpreterEvaluator.eval(binding.initExpression, env))
        val loopEnv       = Environment.child(env, bindings.map(_.name).zip(initialValues))
        evalDoLoop(bindings, doTestClause, body, loopEnv)
      case _ =>
        SchemeFailure.raise("do expected bindings and a test clause", position)

  private def evalUnnamedLet(
    bindingsExpression: Expr,
    body: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    val bindings    = parseBindings(bindingsExpression, position, "let")
    val boundValues = bindings.map(binding => InterpreterEvaluator.eval(binding.valueExpression, env))
    val childEnv    = Environment.child(env, bindings.map(_.name).zip(boundValues))
    InterpreterEvaluator.evalSequence(body, childEnv)

  private def evalNamedLet(
    name: String,
    bindingsExpression: Expr,
    body: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    val bindings   = parseBindings(bindingsExpression, position, "let")
    val arguments  = bindings.map(binding => InterpreterEvaluator.eval(binding.valueExpression, env))
    val closureEnv = Environment.child(env)
    val closure    = ClosureValue(bindings.map(_.name), None, body, closureEnv, Some(name))
    closureEnv.define(name, closure)
    InterpreterEvaluator.applyFunction(closure, arguments, position)

  private def evalRecursiveLet(
    arguments: List[Expr],
    position: Position,
    env: Environment,
    formName: String,
    mode: RecursiveLetMode
  ): Value =
    arguments match
      case bindingsExpression :: body if body.nonEmpty =>
        val bindings = parseBindings(bindingsExpression, position, formName)
        val childEnv = Environment.child(env)
        reserveBindings(bindings, childEnv)
        initializeRecursiveBindings(bindings, childEnv, position, mode)
        InterpreterEvaluator.evalSequence(body, childEnv)
      case _ =>
        SchemeFailure.raise(s"$formName expected bindings and body", position)

  private def reserveBindings(bindings: List[Binding], env: Environment): Unit =
    bindings.foreach(binding => env.reserve(binding.name))

  private def initializeRecursiveBindings(
    bindings: List[Binding],
    env: Environment,
    position: Position,
    mode: RecursiveLetMode
  ): Unit =
    mode match
      case RecursiveLetMode.Sequential =>
        bindings.foreach: binding =>
          val value = InterpreterEvaluator.eval(binding.valueExpression, env)
          env.assign(binding.name, value, position)
      case RecursiveLetMode.Parallel =>
        val values = bindings.map(binding => InterpreterEvaluator.eval(binding.valueExpression, env))
        bindings
          .zip(values)
          .foreach: (binding, value) =>
            env.assign(binding.name, value, position)

  @tailrec
  private def evalDoLoop(
    bindings: List[DoBinding],
    testClause: DoTestClause,
    body: List[Expr],
    loopEnv: Environment
  ): Value =
    if isTruthy(InterpreterEvaluator.eval(testClause.testExpression, loopEnv)) then
      evalDoFinalExpressions(testClause.finalExpressions, loopEnv)
    else
      evalDoBody(body, loopEnv)
      advanceDoBindings(bindings, loopEnv)
      evalDoLoop(bindings, testClause, body, loopEnv)

  private def evalDoFinalExpressions(
    finalExpressions: List[Expr],
    loopEnv: Environment
  ): Value =
    finalExpressions match
      case Nil =>
        VoidValue
      case _ =>
        InterpreterEvaluator.evalSequence(finalExpressions, loopEnv)

  private def evalDoBody(body: List[Expr], loopEnv: Environment): Unit =
    if body.nonEmpty then InterpreterEvaluator.evalSequence(body, loopEnv)

  private def advanceDoBindings(bindings: List[DoBinding], loopEnv: Environment): Unit =
    val nextValues = bindings.map(nextDoValue(_, loopEnv))
    bindings
      .zip(nextValues)
      .foreach: (binding, value) =>
        loopEnv.assign(binding.name, value, binding.position)

  private def nextDoValue(binding: DoBinding, loopEnv: Environment): Value =
    binding.stepExpression match
      case Some(stepExpression) =>
        InterpreterEvaluator.eval(stepExpression, loopEnv)
      case None =>
        loopEnv.lookup(binding.name, binding.position)

  private def parseBindings(
    bindingsExpression: Expr,
    position: Position,
    formName: String
  ): List[Binding] =
    bindingsExpression match
      case ListExpr(bindings, _) =>
        bindings.map(parseBinding(_, formName))
      case _ =>
        SchemeFailure.raise(s"$formName expected a binding list", position)

  private def parseBinding(binding: Expr, formName: String): Binding =
    binding match
      case ListExpr(List(SymbolExpr(name, _), valueExpression), _) =>
        Binding(name, valueExpression)
      case _ =>
        SchemeFailure.raise(
          s"$formName expected bindings of the form (name expr)",
          binding.position
        )

  private def parseDoBindings(
    bindingsExpression: Expr,
    position: Position
  ): List[DoBinding] =
    bindingsExpression match
      case ListExpr(bindings, _) =>
        bindings.map(parseDoBinding)
      case _ =>
        SchemeFailure.raise("do expected a binding list", position)

  private def parseDoBinding(binding: Expr): DoBinding =
    binding match
      case ListExpr(List(SymbolExpr(name, _), initExpression), bindingPosition) =>
        DoBinding(name, initExpression, None, bindingPosition)
      case ListExpr(List(SymbolExpr(name, _), initExpression, stepExpression), bindingPosition) =>
        DoBinding(name, initExpression, Some(stepExpression), bindingPosition)
      case _ =>
        SchemeFailure.raise(
          "do expected bindings of the form (name init) or (name init step)",
          binding.position
        )

  private def parseDoTestClause(testClauseExpression: Expr): DoTestClause =
    testClauseExpression match
      case ListExpr(testExpression :: finalExpressions, _) =>
        DoTestClause(testExpression, finalExpressions)
      case _ =>
        SchemeFailure.raise(
          "do expected a test clause of the form (test expr ...)",
          testClauseExpression.position
        )
