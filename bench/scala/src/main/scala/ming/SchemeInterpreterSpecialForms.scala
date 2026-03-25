package ming

import scala.annotation.tailrec

private[ming] object SchemeInterpreterSpecialForms:

  import SchemeInterpreter.{continueExpr, continueSequence, EvalStep, Expr, Value}
  import SchemeInterpreterSyntax.*

  private type EvalExpr       = (Expr, Env, MacroScope) => Value
  private type ApplyProcedure = (Value, List[Value], SourcePos) => EvalStep

  def evalDefineSyntax(args: List[Expr], env: Env, macros: MacroScope, pos: SourcePos): Value =
    args match
      case Expr.Symbol(name, _) :: transformerExpr :: Nil =>
        macros.define(name, SyntaxRules.parse(name, transformerExpr, env, macros))
        Value.Void
      case _ =>
        throw EvalError.at(pos, "invalid define-syntax")

  def evalDefine(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    evalExpr: EvalExpr
  ): Value =
    args match
      case Expr.Symbol(name, _) :: valueExpr :: Nil =>
        env.define(name, evalExpr(valueExpr, env, macros))
        Value.Void
      case Expr.ListExpr(Expr.Symbol(name, _) :: params, _) :: body if body.nonEmpty =>
        env.define(name, Value.Closure(readParamList(params), body, env, macros))
        Value.Void
      case _ =>
        throw EvalError.at(pos, "invalid define")

  def evalSet(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    evalExpr: EvalExpr
  ): Value =
    args match
      case Expr.Symbol(name, _) :: valueExpr :: Nil =>
        env.assign(name, evalExpr(valueExpr, env, macros), pos)
        Value.Void
      case _ =>
        throw EvalError.at(pos, "invalid set!")

  def evalBegin(args: List[Expr], env: Env, macros: MacroScope): EvalStep =
    continueSequence(args, env, macros)

  def evalIf(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    evalExpr: EvalExpr
  ): EvalStep =
    args match
      case condition :: ifTrue :: Nil =>
        if isTruthy(evalExpr(condition, env, macros)) then continueExpr(ifTrue, env, macros)
        else EvalStep.Done(Value.Void)
      case condition :: ifTrue :: ifFalse :: Nil =>
        if isTruthy(evalExpr(condition, env, macros)) then continueExpr(ifTrue, env, macros)
        else continueExpr(ifFalse, env, macros)
      case _ =>
        throw EvalError.at(pos, s"if expected 2 or 3 arguments, got ${args.length}")

  def evalCond(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    evalExpr: EvalExpr
  ): EvalStep =
    @tailrec
    def loop(clauses: List[Expr]): EvalStep =
      clauses match
        case Nil =>
          EvalStep.Done(Value.Void)
        case Expr.ListExpr(Expr.Symbol("else", _) :: body, clausePos) :: rest =>
          if rest.nonEmpty then throw EvalError.at(clausePos, "cond else clause must be last")
          if body.isEmpty then throw EvalError.at(clausePos, "cond else clause requires a body")
          continueSequence(body, env, macros)
        case Expr.ListExpr(test :: body, _) :: rest =>
          val testValue = evalExpr(test, env, macros)
          if isTruthy(testValue) then
            if body.isEmpty then EvalStep.Done(testValue)
            else continueSequence(body, env, macros)
          else loop(rest)
        case _ =>
          throw EvalError.at(pos, "invalid cond clause")

    loop(args)

  def evalCase(
    args: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos,
    evalExpr: EvalExpr
  ): EvalStep =
    args match
      case keyExpr :: clauses =>
        val key = evalExpr(keyExpr, env, macros)
        evalCaseClauses(key, clauses, env, macros, pos)
      case _ =>
        throw EvalError.at(pos, "invalid case")

  def evalQuote(args: List[Expr], pos: SourcePos): Value =
    args match
      case value :: Nil => quote(value)
      case _ =>
        throw EvalError.at(pos, s"quote expected 1 argument, got ${args.length}")

  def evalLambda(args: List[Expr], env: Env, macros: MacroScope, pos: SourcePos): Value =
    args match
      case formals :: body if body.nonEmpty =>
        Value.Closure(readParams(formals), body, env, macros)
      case _ =>
        throw EvalError.at(pos, "invalid lambda")

  def evalAnd(args: List[Expr], env: Env, macros: MacroScope, evalExpr: EvalExpr): EvalStep =
    shortCircuit(
      expressions = args,
      env = env,
      macros = macros,
      onEmpty = Value.Bool(true),
      isTerminal = value => !isTruthy(value),
      evalExpr = evalExpr
    )

  def evalOr(args: List[Expr], env: Env, macros: MacroScope, evalExpr: EvalExpr): EvalStep =
    shortCircuit(
      expressions = args,
      env = env,
      macros = macros,
      onEmpty = Value.Bool(false),
      isTerminal = isTruthy,
      evalExpr = evalExpr
    )

  private def evalCaseClauses(
    key: Value,
    clauses: List[Expr],
    env: Env,
    macros: MacroScope,
    pos: SourcePos
  ): EvalStep =
    @tailrec
    def loop(remaining: List[Expr]): EvalStep =
      remaining match
        case Nil =>
          EvalStep.Done(Value.Void)
        case Expr.ListExpr(Expr.Symbol("else", _) :: body, clausePos) :: rest =>
          if rest.nonEmpty then throw EvalError.at(clausePos, "case else clause must be last")
          if body.isEmpty then throw EvalError.at(clausePos, "case else clause requires a body")
          continueSequence(body, env, macros)
        case Expr.ListExpr(Expr.ListExpr(datums, _) :: body, _) :: rest =>
          if body.isEmpty then throw EvalError.at(pos, "case clause requires a body")
          if datums.exists(datum => EqualityBuiltins.eqvValues(key, quote(datum))) then
            continueSequence(body, env, macros)
          else loop(rest)
        case _ =>
          throw EvalError.at(pos, "invalid case clause")

    loop(clauses)

  private def shortCircuit(
    expressions: List[Expr],
    env: Env,
    macros: MacroScope,
    onEmpty: Value,
    isTerminal: Value => Boolean,
    evalExpr: EvalExpr
  ): EvalStep =
    @tailrec
    def loop(remaining: List[Expr]): EvalStep =
      remaining match
        case Nil =>
          EvalStep.Done(onEmpty)
        case head :: Nil =>
          continueExpr(head, env, macros)
        case head :: tail =>
          val value = evalExpr(head, env, macros)
          if isTerminal(value) then EvalStep.Done(value)
          else loop(tail)

    loop(expressions)
