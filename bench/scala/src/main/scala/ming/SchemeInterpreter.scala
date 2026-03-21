package ming

import scala.annotation.tailrec

object SchemeInterpreter:

  def evaluateProgram(input: String): Value =
    evaluateProgramWithOutput(input)._1

  def evaluateProgramWithOutput(input: String): (Value, String) =
    val (state, value) = evaluateSequence(SchemeReader.readAll(input), EvalState.empty)
    (value, state.renderedOutput)

  private def evaluateSequence(expressions: List[Expr], state: EvalState): (EvalState, Value) = expressions match
    case Nil =>
      (state, Value.Void)
    case head :: tail =>
      val (nextState, value) = evaluateSequenceStep(head, state)
      evaluateSequenceTail(tail, nextState, value)

  @tailrec
  private def evaluateSequenceTail(
    remaining: List[Expr],
    state: EvalState,
    current: Value
  ): (EvalState, Value) =
    remaining match
      case Nil =>
        (state, current)
      case head :: tail =>
        val (nextState, value) = evaluateSequenceStep(head, state)
        evaluateSequenceTail(tail, nextState, value)

  private def evaluateSequenceStep(expr: Expr, state: EvalState): (EvalState, Value) = expr match
    case Expr.ListExpr(Expr.Symbol("define", position) :: arguments, _) =>
      (evaluateDefine(arguments, state, position), Value.Void)
    case Expr.ListExpr(Expr.Symbol("begin", _) :: arguments, _) =>
      evaluateSequence(arguments, state)
    case _ =>
      evaluate(expr, state)

  private def evaluate(expr: Expr, state: EvalState): (EvalState, Value) = expr match
    case Expr.Literal(value, _) =>
      (state, value)
    case Expr.Symbol(name, position) =>
      (
        state,
        lookup(name, state.env).getOrElse(throw EvalError.at(position, s"unbound symbol '$name'"))
      )
    case Expr.ListExpr(Nil, position) =>
      throw EvalError.at(position, "cannot evaluate empty list")
    case Expr.ListExpr(operator :: arguments, _) =>
      operator match
        case Expr.Symbol("if", position) =>
          evaluateIf(arguments, state, position)
        case Expr.Symbol("quote", position) =>
          (state, evaluateQuote(arguments, position))
        case Expr.Symbol("lambda", position) =>
          (state, evaluateLambda(arguments, state.env, position))
        case Expr.Symbol("and", _) =>
          evaluateAnd(arguments, state)
        case Expr.Symbol("or", _) =>
          evaluateOr(arguments, state)
        case Expr.Symbol("begin", _) =>
          evaluateBegin(arguments, state)
        case Expr.Symbol("let", position) =>
          evaluateLet(arguments, state, position)
        case Expr.Symbol("cond", _) =>
          evaluateCond(arguments, state)
        case Expr.Symbol("define", position) =>
          throw EvalError.at(position, "define is only allowed within a sequence")
        case _ =>
          val (nextState, procedure) = evaluate(operator, state)
          applyProcedure(procedure, arguments, nextState, operator.sourcePos)

  private def evaluateDefine(arguments: List[Expr], state: EvalState, position: SourcePos): EvalState =
    arguments match
      case Expr.Symbol(name, _) :: valueExpr :: Nil =>
        evaluateValueDefine(name, valueExpr, state)
      case Expr.ListExpr(Expr.Symbol(name, _) :: parameters, _) :: body if body.nonEmpty =>
        val parameterNames = parameters.map(expectParameterName)
        state.withEnv(
          bindRecursive(state.env, name)(recursiveEnv => Value.Closure(parameterNames, body, recursiveEnv))
        )
      case _ =>
        throw EvalError.at(position, "invalid define form")

  private def evaluateValueDefine(name: String, valueExpr: Expr, state: EvalState): EvalState =
    lazy val evaluated: (EvalState, Value) =
      evaluate(valueExpr, state.withEnv(recursiveEnv))
    lazy val recursiveEnv: Env =
      state.env.define(name, evaluated._2)

    val (nextState, _) = evaluated
    nextState.withEnv(recursiveEnv)

  private def bindRecursive(env: Env, name: String)(build: Env => Value): Env =
    lazy val recursiveEnv: Env = env.define(name, build(recursiveEnv))
    recursiveEnv

  private def evaluateBegin(arguments: List[Expr], state: EvalState): (EvalState, Value) =
    withScopedEnv(state, state.env)(evaluateSequence(arguments, _))

  private def evaluateIf(
    arguments: List[Expr],
    state: EvalState,
    position: SourcePos
  ): (EvalState, Value) =
    arguments match
      case condition :: thenBranch :: elseBranch :: Nil =>
        val (nextState, conditionValue) = evaluate(condition, state)
        if conditionValue.isTruthy then evaluate(thenBranch, nextState)
        else evaluate(elseBranch, nextState)
      case _ =>
        throw EvalError.at(position, "'if' expects exactly 3 arguments")

  private def evaluateQuote(arguments: List[Expr], position: SourcePos): Value =
    arguments match
      case datum :: Nil =>
        quoteToValue(datum)
      case _ =>
        throw EvalError.at(position, "'quote' expects exactly 1 argument")

  private def evaluateLambda(arguments: List[Expr], env: Env, position: SourcePos): Value =
    arguments match
      case parameterExpr :: body if body.nonEmpty =>
        Value.Closure(readParameters(parameterExpr, position), body, env)
      case _ =>
        throw EvalError.at(position, "invalid lambda form")

  private def evaluateLet(
    arguments: List[Expr],
    state: EvalState,
    position: SourcePos
  ): (EvalState, Value) =
    arguments match
      case bindingsExpr :: body if body.nonEmpty =>
        val (bindingState, bindings) = readLetBindings(bindingsExpr, state, position)
        withScopedEnv(bindingState, state.env.extend(bindings))(evaluateBody(body, _))
      case _ =>
        throw EvalError.at(position, "invalid let form")

  private def readLetBindings(
    bindingsExpr: Expr,
    state: EvalState,
    position: SourcePos
  ): (EvalState, List[(String, Value)]) = bindingsExpr match
    case Expr.ListExpr(bindings, _) =>
      readLetBindingList(bindings, state, state.env)
    case _ =>
      throw EvalError.at(position, "let bindings must be a list")

  private def readLetBindingList(
    bindings: List[Expr],
    state: EvalState,
    baseEnv: Env
  ): (EvalState, List[(String, Value)]) = bindings match
    case Nil =>
      (state, Nil)
    case binding :: tail =>
      val (nextState, entry)    = readLetBinding(binding, state, baseEnv)
      val (finalState, entries) = readLetBindingList(tail, nextState, baseEnv)
      (finalState, entry :: entries)

  private def readLetBinding(
    binding: Expr,
    state: EvalState,
    baseEnv: Env
  ): (EvalState, (String, Value)) = binding match
    case Expr.ListExpr(Expr.Symbol(name, _) :: valueExpr :: Nil, _) =>
      val (nextState, value) = withScopedEnv(state, baseEnv)(evaluate(valueExpr, _))
      (nextState, (name, value))
    case _ =>
      throw EvalError.at(binding.sourcePos, "invalid let binding")

  private def evaluateCond(arguments: List[Expr], state: EvalState): (EvalState, Value) = arguments match
    case Nil =>
      (state, Value.Void)
    case clause :: remaining =>
      evaluateCondClause(clause, remaining, state)

  private def evaluateCondClause(
    clause: Expr,
    remaining: List[Expr],
    state: EvalState
  ): (EvalState, Value) = clause match
    case Expr.ListExpr(Nil, clausePosition) =>
      throw EvalError.at(clausePosition, "cond clause cannot be empty")
    case Expr.ListExpr(Expr.Symbol("else", clausePosition) :: body, _) =>
      if remaining.nonEmpty then throw EvalError.at(clausePosition, "else clause must be last")
      else if body.isEmpty then throw EvalError.at(clausePosition, "else clause must not be empty")
      else withScopedEnv(state, state.env)(evaluateBody(body, _))
    case Expr.ListExpr(test :: body, _) =>
      val (nextState, conditionValue) = evaluate(test, state)
      if conditionValue.isTruthy then
        if body.isEmpty then (nextState, conditionValue)
        else withScopedEnv(nextState, nextState.env)(evaluateBody(body, _))
      else evaluateCond(remaining, nextState)
    case _ =>
      throw EvalError.at(clause.sourcePos, "cond clause must be a list")

  private def readParameters(parameterExpr: Expr, position: SourcePos): List[String] =
    parameterExpr match
      case Expr.ListExpr(parameters, _) =>
        parameters.map(expectParameterName)
      case _ =>
        throw EvalError.at(position, "lambda parameters must be a list")

  private def expectParameterName(expr: Expr): String = expr match
    case Expr.Symbol(name, _) => name
    case _                    => throw EvalError.at(expr.sourcePos, "parameter must be a symbol")

  private def quoteToValue(expr: Expr): Value = expr match
    case Expr.Literal(value, _) =>
      value
    case Expr.Symbol(name, _) =>
      Value.Symbol(name)
    case Expr.ListExpr(items, _) =>
      items.foldRight(Value.EmptyList: Value) { (item, tail) =>
        Value.Pair(quoteToValue(item), tail)
      }

  private def applyProcedure(
    procedure: Value,
    arguments: List[Expr],
    state: EvalState,
    position: SourcePos
  ): (EvalState, Value) =
    procedure match
      case Value.Builtin(name) =>
        val (nextState, evaluatedArgs) = evaluateArguments(arguments, state)
        applyBuiltin(name, evaluatedArgs, nextState, position)
      case Value.Closure(parameters, body, closureEnv) =>
        val (nextState, evaluatedArgs) = evaluateArguments(arguments, state)
        val argumentValues             = evaluatedArgs.map(_.value)
        if parameters.length != argumentValues.length then
          throw EvalError.at(
            position,
            s"wrong argument count: expected ${parameters.length}, got ${argumentValues.length}"
          )
        else withScopedEnv(nextState, closureEnv.extend(parameters.zip(argumentValues)))(evaluateBody(body, _))
      case _ =>
        throw EvalError.at(position, "attempted to call a non-procedure")

  private def evaluateBody(expressions: List[Expr], state: EvalState): (EvalState, Value) = expressions match
    case Nil =>
      (state, Value.Void)
    case _ =>
      evaluateSequence(expressions, state)

  private def lookup(name: String, env: Env): Option[Value] =
    env.lookup(name).orElse(BuiltinProcedure.resolve(name))

  private def applyBuiltin(
    name: String,
    arguments: List[EvaluatedArg],
    state: EvalState,
    position: SourcePos
  ): (EvalState, Value) =
    val result = BuiltinProcedure(name, arguments, position)
    (state.appendOutput(result.output), result.value)

  private def evaluateArguments(
    arguments: List[Expr],
    state: EvalState
  ): (EvalState, List[EvaluatedArg]) = arguments match
    case Nil =>
      (state, Nil)
    case argument :: remaining =>
      val (nextState, value) = evaluate(argument, state)
      val (finalState, rest) = evaluateArguments(remaining, nextState)
      (finalState, EvaluatedArg(value, argument.sourcePos) :: rest)

  private def evaluateAnd(arguments: List[Expr], state: EvalState): (EvalState, Value) =
    arguments match
      case Nil =>
        (state, Value.Bool(true))
      case argument :: Nil =>
        evaluate(argument, state)
      case argument :: rest =>
        val (nextState, value) = evaluate(argument, state)
        if value.isTruthy then evaluateAnd(rest, nextState) else (nextState, value)

  private def evaluateOr(arguments: List[Expr], state: EvalState): (EvalState, Value) =
    arguments match
      case Nil =>
        (state, Value.Bool(false))
      case argument :: Nil =>
        evaluate(argument, state)
      case argument :: rest =>
        val (nextState, value) = evaluate(argument, state)
        if value.isTruthy then (nextState, value) else evaluateOr(rest, nextState)

  private def withScopedEnv[A](state: EvalState, scopedEnv: Env)(
    evaluateScope: EvalState => (EvalState, A)
  ): (EvalState, A) =
    val (scopedState, value) = evaluateScope(state.withEnv(scopedEnv))
    (scopedState.withEnv(state.env), value)
