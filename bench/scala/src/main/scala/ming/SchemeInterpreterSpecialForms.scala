package ming

private[ming] object SchemeInterpreterSpecialForms:

  import SchemeInterpreter.{EvalState, Expr, Resume, Value}
  import SchemeInterpreterSyntax.*

  private type EvalExprState =
    (Expr, Env, MacroScope, Resume) => EvalState

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

  def evalQuoteState(args: List[Expr], pos: SourcePos, cont: Resume): EvalState =
    args match
      case value :: Nil =>
        cont(quote(value))
      case _ =>
        throw EvalError.at(pos, s"quote expected 1 argument, got ${args.length}")

  def evalSyntaxState(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    cont: Resume
  ): EvalState =
    cont(SchemeSyntax.evalSyntax(args, env, macros, pos))

  def evalSyntaxCaseState(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    cont: Resume
  ): EvalState =
    cont(SchemeSyntax.evalSyntaxCase(args, env, macros, pos))

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

  def evalWithSyntaxState(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    cont: Resume
  ): EvalState =
    cont(SchemeSyntax.evalWithSyntax(args, env, macros, pos))
