package ming

private[ming] object SchemeInterpreterSpecialForms:

  import SchemeInterpreter.{EvalState, Expr, Resume, Value}
  import SchemeInterpreterSyntax.*

  private type EvalExprState =
    (Expr, Env, MacroScope, Resume) => EvalState

  private type EvalSequenceState =
    (List[Expr], Env, MacroScope, Resume) => EvalState

  def evalDefineSyntaxState(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    cont: Resume
  ): EvalState =
    args match
      case Expr.Symbol(name, _) :: transformerExpr :: Nil =>
        macros.define(name, SyntaxRules.parse(name, transformerExpr, env, macros))
        cont(Value.Void)
      case _ =>
        throw EvalError.at(pos, "invalid define-syntax")

  def evalDefineState(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    cont: Resume,
    evalExprState: EvalExprState
  ): EvalState =
    args match
      case Expr.Symbol(name, _) :: valueExpr :: Nil =>
        evalExprState(
          valueExpr,
          env,
          macros,
          value =>
            env.define(name, value)
            cont(Value.Void)
        )
      case Expr.ListExpr(Expr.Symbol(name, _) :: params, _) :: body if body.nonEmpty =>
        env.define(name, Value.Closure(readParamList(params), body, env, macros))
        cont(Value.Void)
      case _ =>
        throw EvalError.at(pos, "invalid define")

  def evalSetState(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    cont: Resume,
    evalExprState: EvalExprState
  ): EvalState =
    args match
      case Expr.Symbol(name, _) :: valueExpr :: Nil =>
        evalExprState(
          valueExpr,
          env,
          macros,
          value =>
            env.assign(name, value, pos)
            cont(Value.Void)
        )
      case _ =>
        throw EvalError.at(pos, "invalid set!")

  def evalIfState(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    cont: Resume,
    evalExprState: EvalExprState
  ): EvalState =
    args match
      case condition :: ifTrue :: Nil =>
        evalExprState(
          condition,
          env,
          macros,
          value =>
            if isTruthy(value) then evalExprState(ifTrue, env, macros, cont)
            else cont(Value.Void)
        )
      case condition :: ifTrue :: ifFalse :: Nil =>
        evalExprState(
          condition,
          env,
          macros,
          value =>
            if isTruthy(value) then evalExprState(ifTrue, env, macros, cont)
            else evalExprState(ifFalse, env, macros, cont)
        )
      case _ =>
        throw EvalError.at(pos, s"if expected 2 or 3 arguments, got ${args.length}")

  def evalCondState(
    clauses: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    cont: Resume,
    evalExprState: EvalExprState,
    evalSequenceState: EvalSequenceState
  ): EvalState =
    clauses match
      case Nil =>
        cont(Value.Void)
      case Expr.ListExpr(Expr.Symbol("else", _) :: body, clausePos) :: rest =>
        if rest.nonEmpty then throw EvalError.at(clausePos, "cond else clause must be last")
        if body.isEmpty then throw EvalError.at(clausePos, "cond else clause requires a body")
        evalSequenceState(body, env, macros, cont)
      case Expr.ListExpr(test :: body, _) :: rest =>
        evalExprState(
          test,
          env,
          macros,
          value =>
            if isTruthy(value) then
              if body.isEmpty then cont(value)
              else evalSequenceState(body, env, macros, cont)
            else evalCondState(rest, env, macros, pos, cont, evalExprState, evalSequenceState)
        )
      case _ =>
        throw EvalError.at(pos, "invalid cond clause")

  def evalCaseState(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    cont: Resume,
    evalExprState: EvalExprState,
    evalSequenceState: EvalSequenceState
  ): EvalState =
    args match
      case keyExpr :: clauses =>
        evalExprState(
          keyExpr,
          env,
          macros,
          key => evalCaseClausesState(key, clauses, env, macros, pos, cont, evalSequenceState)
        )
      case _ =>
        throw EvalError.at(pos, "invalid case")

  def evalQuoteState(args: List[Expr], pos: SourcePos, cont: Resume): EvalState =
    args match
      case value :: Nil =>
        cont(quote(value))
      case _ =>
        throw EvalError.at(pos, s"quote expected 1 argument, got ${args.length}")

  def evalLambdaState(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    cont: Resume
  ): EvalState =
    args match
      case formals :: body if body.nonEmpty =>
        cont(Value.Closure(readParams(formals), body, env, macros))
      case _ =>
        throw EvalError.at(pos, "invalid lambda")

  def evalAndState(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    cont: Resume,
    evalExprState: EvalExprState
  ): EvalState =
    shortCircuitState(args, env, macros, cont, Value.Bool(true), value => !isTruthy(value), evalExprState)

  def evalOrState(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    cont: Resume,
    evalExprState: EvalExprState
  ): EvalState =
    shortCircuitState(args, env, macros, cont, Value.Bool(false), isTruthy, evalExprState)

  private def evalCaseClausesState(
    key: Value,
    clauses: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    cont: Resume,
    evalSequenceState: EvalSequenceState
  ): EvalState =
    clauses match
      case Nil =>
        cont(Value.Void)
      case Expr.ListExpr(Expr.Symbol("else", _) :: body, clausePos) :: rest =>
        if rest.nonEmpty then throw EvalError.at(clausePos, "case else clause must be last")
        if body.isEmpty then throw EvalError.at(clausePos, "case else clause requires a body")
        evalSequenceState(body, env, macros, cont)
      case Expr.ListExpr(Expr.ListExpr(datums, _) :: body, _) :: rest =>
        if body.isEmpty then throw EvalError.at(pos, "case clause requires a body")
        if datums.exists(datum => EqualityBuiltins.eqvValues(key, quote(datum))) then
          evalSequenceState(body, env, macros, cont)
        else evalCaseClausesState(key, rest, env, macros, pos, cont, evalSequenceState)
      case _ =>
        throw EvalError.at(pos, "invalid case clause")

  private def shortCircuitState(
    expressions: List[Expr],
    env: Env,
    macros: MacroScope,
    cont: Resume,
    onEmpty: Value,
    isTerminal: Value => Boolean,
    evalExprState: EvalExprState
  ): EvalState =
    expressions match
      case Nil =>
        cont(onEmpty)
      case head :: Nil =>
        evalExprState(head, env, macros, cont)
      case head :: tail =>
        evalExprState(
          head,
          env,
          macros,
          value =>
            if isTerminal(value) then cont(value)
            else shortCircuitState(tail, env, macros, cont, onEmpty, isTerminal, evalExprState)
        )
