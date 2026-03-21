package ming
import scala.util.control.TailCalls.{TailRec, done, tailcall}

object SchemeInterpreter:
  private type EvalResult = (EvalState, Value); private type BindingResult = (EvalState, List[(String, Value)])
  def evaluateProgram(input: String): (EvalState, Value) = evaluateSequence(SchemeReader.readAll(input), EvalState.empty).result

  private def evaluateSequence(expressions: List[Expr], state: EvalState): TailRec[EvalResult] =
    readFunctionDefineGroup(expressions, state).fold(expressions match
      case Nil => done((state, Value.Void))
      case head :: tail =>
        tailcall(evaluateSequenceStep(head, state)).flatMap:
          case (nextState, value) =>
            if tail.isEmpty then done((nextState, value))
            else tailcall(evaluateSequence(tail, nextState))
    ):
      case (nextState, remaining) =>
        if remaining.isEmpty then done((nextState, Value.Void))
        else tailcall(evaluateSequence(remaining, nextState))
  private def readFunctionDefineGroup(expressions: List[Expr], state: EvalState): Option[(EvalState, List[Expr])] =
    val (definitions, remaining) = expressions.span:
      case Expr.ListExpr(Expr.Symbol("define", _) :: Expr.ListExpr(Expr.Symbol(_, _) :: _, _) :: body, _) =>
        body.nonEmpty
      case _ =>
        false
    Option.when(definitions.nonEmpty):
      lazy val recursiveEnv: Env = definitions.foldLeft(state.env):
        case (env, Expr.ListExpr(Expr.Symbol("define", _) :: Expr.ListExpr(Expr.Symbol(name, _) :: parameters, _) :: body, _)) =>
          env.define(name, Value.Closure(parameters.map(expectParameterName), body, recursiveEnv))
        case (env, _) =>
          env
      (state.withEnv(recursiveEnv), remaining)
  private def evaluateSequenceStep(expr: Expr, state: EvalState): TailRec[EvalResult] = expr match
    case Expr.ListExpr(Expr.Symbol("define", position) :: arguments, _) =>
      tailcall(evaluateDefine(arguments, state, position)).map(nextState => (nextState, Value.Void))
    case Expr.ListExpr(Expr.Symbol("begin", _) :: arguments, _) =>
      tailcall(evaluateSequence(arguments, state))
    case _ =>
      tailcall(evaluate(expr, state))
  private def evaluate(expr: Expr, state: EvalState): TailRec[EvalResult] = expr match
    case Expr.Literal(value, _) =>
      done((state, value))
    case Expr.Symbol(name, position) =>
      done((state, lookup(name, state.env).getOrElse(throw EvalError.at(position, s"unbound symbol '$name'"))))
    case Expr.ListExpr(Nil, position) =>
      throw EvalError.at(position, "cannot evaluate empty list")
    case Expr.ListExpr(operator :: arguments, _) =>
      evaluateList(operator, arguments, state)
  private def evaluateList(operator: Expr, arguments: List[Expr], state: EvalState): TailRec[EvalResult] =
    operator match
      case Expr.Symbol("if", position) =>
        tailcall(evaluateIf(arguments, state, position))
      case Expr.Symbol("quote", position) =>
        done((state, evaluateQuote(arguments, position)))
      case Expr.Symbol("lambda", position) =>
        done((state, evaluateLambda(arguments, state.env, position)))
      case Expr.Symbol("and", _) =>
        tailcall(evaluateAnd(arguments, state))
      case Expr.Symbol("or", _) =>
        tailcall(evaluateOr(arguments, state))
      case Expr.Symbol("begin", _) =>
        tailcall(evaluateBegin(arguments, state))
      case Expr.Symbol("let", position) =>
        tailcall(evaluateLet(arguments, state, position))
      case Expr.Symbol("cond", _) =>
        tailcall(evaluateCond(arguments, state))
      case Expr.Symbol("define", position) =>
        throw EvalError.at(position, "define is only allowed within a sequence")
      case _ =>
        tailcall(evaluate(operator, state)).flatMap:
          case (nextState, procedure) =>
            tailcall(applyProcedure(procedure, arguments, nextState, operator.sourcePos))
  private def evaluateDefine(arguments: List[Expr], state: EvalState, position: SourcePos): TailRec[EvalState] =
    arguments match
      case Expr.Symbol(name, _) :: valueExpr :: Nil =>
        done(evaluateValueDefine(name, valueExpr, state))
      case Expr.ListExpr(Expr.Symbol(name, _) :: parameters, _) :: body if body.nonEmpty =>
        val parameterNames = parameters.map(expectParameterName)
        done(
          state.withEnv(
            bindRecursive(state.env, name)(recursiveEnv => Value.Closure(parameterNames, body, recursiveEnv))
          )
        )
      case _ =>
        throw EvalError.at(position, "invalid define form")
  private def evaluateValueDefine(name: String, valueExpr: Expr, state: EvalState): EvalState =
    lazy val evaluated: EvalResult =
      evaluate(valueExpr, state.withEnv(recursiveEnv)).result
    lazy val recursiveEnv: Env =
      state.env.define(name, evaluated._2)
    val (nextState, _) = evaluated
    nextState.withEnv(recursiveEnv)
  private def bindRecursive(env: Env, name: String)(build: Env => Value): Env =
    lazy val recursiveEnv: Env = env.define(name, build(recursiveEnv))
    recursiveEnv
  private def evaluateBegin(arguments: List[Expr], state: EvalState): TailRec[EvalResult] =
    withScopedEnv(state, state.env)(evaluateSequence(arguments, _))
  private def evaluateIf(
    arguments: List[Expr],
    state: EvalState,
    position: SourcePos
  ): TailRec[EvalResult] =
    arguments match
      case condition :: thenBranch :: elseBranch :: Nil =>
        tailcall(evaluate(condition, state)).flatMap:
          case (nextState, conditionValue) =>
            if conditionValue.isTruthy then tailcall(evaluate(thenBranch, nextState))
            else tailcall(evaluate(elseBranch, nextState))
      case _ =>
        throw EvalError.at(position, "'if' expects exactly 3 arguments")
  private def evaluateQuote(arguments: List[Expr], position: SourcePos): Value =
    arguments match
      case datum :: Nil => quoteToValue(datum)
      case _            => throw EvalError.at(position, "'quote' expects exactly 1 argument")
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
  ): TailRec[EvalResult] =
    arguments match
      case Expr.Symbol(name, _) :: bindingsExpr :: body if body.nonEmpty =>
        tailcall(evaluateNamedLet(name, bindingsExpr, body, state, position))
      case bindingsExpr :: body if body.nonEmpty =>
        tailcall(readLetBindings(bindingsExpr, state, position)).flatMap:
          case (bindingState, bindings) =>
            tailcall(withScopedEnv(bindingState, state.env.extend(bindings))(evaluateBody(body, _)))
      case _ =>
        throw EvalError.at(position, "invalid let form")
  private def evaluateNamedLet(
    name: String,
    bindingsExpr: Expr,
    body: List[Expr],
    state: EvalState,
    position: SourcePos
  ): TailRec[EvalResult] =
    tailcall(readLetBindings(bindingsExpr, state, position)).flatMap:
      case (bindingState, bindings) =>
        val parameterNames = bindings.map(_._1)
        val recursiveEnv =
          bindRecursive(state.env, name)(closureEnv => Value.Closure(parameterNames, body, closureEnv))
        tailcall(withScopedEnv(bindingState, recursiveEnv.extend(bindings))(evaluateBody(body, _)))
  private def readLetBindings(
    bindingsExpr: Expr,
    state: EvalState,
    position: SourcePos
  ): TailRec[BindingResult] =
    bindingsExpr match
      case Expr.ListExpr(bindings, _) =>
        tailcall(readLetBindingList(bindings, state, state.env))
      case _ =>
        throw EvalError.at(position, "let bindings must be a list")
  private def readLetBindingList(
    bindings: List[Expr],
    state: EvalState,
    baseEnv: Env
  ): TailRec[BindingResult] =
    bindings match
      case Nil =>
        done((state, Nil))
      case binding :: tail =>
        tailcall(readLetBinding(binding, state, baseEnv)).flatMap:
          case (nextState, entry) =>
            tailcall(readLetBindingList(tail, nextState, baseEnv)).map:
              case (finalState, entries) =>
                (finalState, entry :: entries)
  private def readLetBinding(
    binding: Expr,
    state: EvalState,
    baseEnv: Env
  ): TailRec[(EvalState, (String, Value))] =
    binding match
      case Expr.ListExpr(Expr.Symbol(name, _) :: valueExpr :: Nil, _) =>
        tailcall(withScopedEnv(state, baseEnv)(evaluate(valueExpr, _))).map:
          case (nextState, value) =>
            (nextState, (name, value))
      case _ =>
        throw EvalError.at(binding.sourcePos, "invalid let binding")
  private def evaluateCond(arguments: List[Expr], state: EvalState): TailRec[EvalResult] =
    arguments match
      case Nil =>
        done((state, Value.Void))
      case clause :: remaining =>
        tailcall(evaluateCondClause(clause, remaining, state))
  private def evaluateCondClause(
    clause: Expr,
    remaining: List[Expr],
    state: EvalState
  ): TailRec[EvalResult] =
    clause match
      case Expr.ListExpr(Nil, clausePosition) =>
        throw EvalError.at(clausePosition, "cond clause cannot be empty")
      case Expr.ListExpr(Expr.Symbol("else", clausePosition) :: body, _) =>
        if remaining.nonEmpty then throw EvalError.at(clausePosition, "else clause must be last")
        else if body.isEmpty then throw EvalError.at(clausePosition, "else clause must not be empty")
        else tailcall(withScopedEnv(state, state.env)(evaluateBody(body, _)))
      case Expr.ListExpr(test :: body, _) =>
        tailcall(evaluate(test, state)).flatMap:
          case (nextState, conditionValue) =>
            if conditionValue.isTruthy then
              if body.isEmpty then done((nextState, conditionValue))
              else tailcall(withScopedEnv(nextState, nextState.env)(evaluateBody(body, _)))
            else tailcall(evaluateCond(remaining, nextState))
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
  ): TailRec[EvalResult] =
    procedure match
      case Value.Builtin(name) =>
        tailcall(evaluateArguments(arguments, state)).map:
          case (nextState, evaluatedArgs) =>
            BuiltinProcedure(name, evaluatedArgs, nextState, position)
      case Value.Closure(parameters, body, closureEnv) =>
        tailcall(evaluateArguments(arguments, state)).flatMap:
          case (nextState, evaluatedArgs) =>
            val argumentValues = evaluatedArgs.map(_.value)
            if parameters.length != argumentValues.length then
              throw EvalError.at(
                position,
                s"wrong argument count: expected ${parameters.length}, got ${argumentValues.length}"
              )
            else
              tailcall(
                withScopedEnv(nextState, closureEnv.extend(parameters.zip(argumentValues)))(evaluateBody(body, _))
              )
      case _ =>
        throw EvalError.at(position, "attempted to call a non-procedure")
  private def evaluateBody(expressions: List[Expr], state: EvalState): TailRec[EvalResult] =
    if expressions.isEmpty then done((state, Value.Void))
    else tailcall(evaluateSequence(expressions, state))
  private def lookup(name: String, env: Env): Option[Value] =
    env.lookup(name).orElse(BuiltinProcedure.resolve(name))
  private def evaluateArguments(
    arguments: List[Expr],
    state: EvalState
  ): TailRec[(EvalState, List[EvaluatedArg])] =
    arguments match
      case Nil =>
        done((state, Nil))
      case argument :: remaining =>
        tailcall(evaluate(argument, state)).flatMap:
          case (nextState, value) =>
            tailcall(evaluateArguments(remaining, nextState)).map:
              case (finalState, rest) =>
                (finalState, EvaluatedArg(value, argument.sourcePos) :: rest)
  private def evaluateAnd(arguments: List[Expr], state: EvalState): TailRec[EvalResult] =
    arguments match
      case Nil =>
        done((state, Value.Bool(true)))
      case argument :: Nil =>
        tailcall(evaluate(argument, state))
      case argument :: rest =>
        tailcall(evaluate(argument, state)).flatMap:
          case (nextState, value) =>
            if value.isTruthy then tailcall(evaluateAnd(rest, nextState))
            else done((nextState, value))
  private def evaluateOr(arguments: List[Expr], state: EvalState): TailRec[EvalResult] =
    arguments match
      case Nil =>
        done((state, Value.Bool(false)))
      case argument :: Nil =>
        tailcall(evaluate(argument, state))
      case argument :: rest =>
        tailcall(evaluate(argument, state)).flatMap:
          case (nextState, value) =>
            if value.isTruthy then done((nextState, value))
            else tailcall(evaluateOr(rest, nextState))
  private def withScopedEnv[A](state: EvalState, scopedEnv: Env)(
    evaluateScope: EvalState => TailRec[(EvalState, A)]
  ): TailRec[(EvalState, A)] =
    tailcall(evaluateScope(state.withEnv(scopedEnv))).map:
      case (scopedState, value) =>
        (scopedState.withEnv(state.env), value)
