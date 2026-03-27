package ming

import scala.annotation.tailrec

private[ming] object InterpreterEvaluator:

  def evalSequence(expressions: List[Expr], env: Environment): Value =
    run(EvalSequenceControl(expressions, env))

  def eval(expression: Expr, env: Environment): Value =
    run(EvalExprControl(expression, env))

  def applyFunction(function: Value, arguments: List[Value], position: Position): Value =
    run(ApplyControl(function, arguments, position))

  def done(value: Value): EvaluationStep =
    ReturnStep(value)

  def deferExpr(expression: Expr, env: Environment): EvaluationStep =
    ContinueStep(EvalExprControl(expression, env))

  def deferSequence(expressions: List[Expr], env: Environment): EvaluationStep =
    ContinueStep(EvalSequenceControl(expressions, env))

  def deferApplication(function: Value, arguments: List[Value], position: Position): EvaluationStep =
    ContinueStep(ApplyControl(function, arguments, position))

  @tailrec
  private def run(control: EvaluationControl): Value =
    step(control) match
      case ReturnStep(value)     => value
      case ContinueStep(nextJob) => run(nextJob)

  private def step(control: EvaluationControl): EvaluationStep =
    control match
      case EvalExprControl(expression, env) =>
        evalStep(expression, env)
      case EvalSequenceControl(expressions, env) =>
        evalSequenceStep(expressions, env)
      case ApplyControl(function, arguments, position) =>
        applyFunctionStep(function, arguments, position)

  private def evalStep(expression: Expr, env: Environment): EvaluationStep =
    expression match
      case IntExpr(value, _) =>
        done(IntValue(value))
      case RationalExpr(numerator, denominator, _) =>
        done(NumericSupport.exactValue(numerator, denominator))
      case InexactExpr(value, _) =>
        done(InexactValue(value))
      case BoolExpr(value, _) =>
        done(BoolValue(value))
      case StringExpr(value, _) =>
        done(StringValue(value))
      case CharExpr(value, _) =>
        done(CharValue(value))
      case SymbolExpr(name, position) =>
        done(env.lookup(name, position))
      case ListExpr(items, position) =>
        evalListStep(items, position, env)

  @tailrec
  private def evalSequenceStep(expressions: List[Expr], env: Environment): EvaluationStep =
    expressions match
      case Nil =>
        done(VoidValue)
      case expression :: Nil =>
        deferExpr(expression, env)
      case expression :: rest =>
        eval(expression, env)
        evalSequenceStep(rest, env)

  private def applyFunctionStep(
    function: Value,
    arguments: List[Value],
    position: Position
  ): EvaluationStep =
    function match
      case BuiltinValue(_, implementation) =>
        done(implementation(arguments, position))
      case ClosureValue(parameters, restParameter, body, closureEnv, _) =>
        applyClosure(parameters, restParameter, body, closureEnv, arguments, position)
      case CaseLambdaValue(clauses, _) =>
        applyCaseLambda(clauses, arguments, position)
      case other =>
        SchemeFailure.raise(
          s"attempted to call a non-procedure value: ${other.render}",
          position
        )

  private def evalListStep(
    items: List[Expr],
    position: Position,
    env: Environment
  ): EvaluationStep =
    items match
      case Nil =>
        SchemeFailure.raise("cannot evaluate an empty list", position)
      case SymbolExpr("and", _) :: rest =>
        SpecialFormEvaluator.evalAnd(rest, env)
      case SymbolExpr("begin", _) :: rest =>
        deferSequence(rest, env)
      case SymbolExpr("case", _) :: rest =>
        SpecialFormEvaluator.evalCase(rest, position, env)
      case SymbolExpr("cond", _) :: rest =>
        SpecialFormEvaluator.evalCond(rest, env)
      case SymbolExpr("do", _) :: rest =>
        SpecialFormEvaluator.evalDo(rest, position, env)
      case SymbolExpr("or", _) :: rest =>
        SpecialFormEvaluator.evalOr(rest, env)
      case SymbolExpr("define", _) :: rest =>
        SpecialFormEvaluator.evalDefine(rest, position, env)
      case SymbolExpr("define-record-type", _) :: rest =>
        done(RecordTypeEvaluator.evalDefineRecordType(rest, position, env))
      case SymbolExpr("define-syntax", _) :: rest =>
        SpecialFormEvaluator.evalDefineSyntax(rest, position, env)
      case SymbolExpr("if", _) :: rest =>
        SpecialFormEvaluator.evalIf(rest, position, env)
      case SymbolExpr("case-lambda", _) :: rest =>
        SpecialFormEvaluator.evalCaseLambda(rest, position, env)
      case SymbolExpr("let", _) :: rest =>
        SpecialFormEvaluator.evalLet(rest, position, env)
      case SymbolExpr("letrec", _) :: rest =>
        SpecialFormEvaluator.evalLetrec(rest, position, env)
      case SymbolExpr("letrec*", _) :: rest =>
        SpecialFormEvaluator.evalLetrecStar(rest, position, env)
      case SymbolExpr("quote", _) :: rest =>
        SpecialFormEvaluator.evalQuote(rest, position)
      case SymbolExpr("lambda", _) :: rest =>
        SpecialFormEvaluator.evalLambda(rest, position, env)
      case SymbolExpr("set!", _) :: rest =>
        SpecialFormEvaluator.evalSet(rest, position, env)
      case (operator @ SymbolExpr(name, _)) :: arguments =>
        env.lookupMacro(name) match
          case Some(macroDefinition) =>
            evalMacroApplication(ListExpr(operator :: arguments, position), macroDefinition, env)
          case None =>
            evalApplicationStep(operator, arguments, position, env)
      case operator :: arguments =>
        evalApplicationStep(operator, arguments, position, env)

  private def evalMacroApplication(
    application: ListExpr,
    macroDefinition: SyntaxRulesMacro,
    env: Environment
  ): EvaluationStep =
    val expansion = MacroExpander.expand(application, macroDefinition)
    expansion.aliases.foreach((alias, cell) => env.defineAlias(alias, cell))
    deferExpr(expansion.expr, env)

  private def evalApplicationStep(
    operator: Expr,
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): EvaluationStep =
    val function           = eval(operator, env)
    val evaluatedArguments = arguments.map(argument => eval(argument, env))
    deferApplication(function, evaluatedArguments, position)

  private def applyCaseLambda(
    clauses: List[ClosureValue],
    arguments: List[Value],
    position: Position
  ): EvaluationStep =
    clauses.find(clauseMatches(_, arguments.length)) match
      case Some(ClosureValue(parameters, restParameter, body, closureEnv, _)) =>
        applyClosure(parameters, restParameter, body, closureEnv, arguments, position)
      case None =>
        SchemeFailure.raise(
          s"case-lambda did not match ${arguments.length} argument(s)",
          position
        )

  private def applyClosure(
    parameters: List[String],
    restParameter: Option[String],
    body: List[Expr],
    closureEnv: Environment,
    arguments: List[Value],
    position: Position
  ): EvaluationStep =
    restParameter match
      case None =>
        if arguments.length != parameters.length then
          SchemeFailure.raise(
            s"procedure expected ${parameters.length} argument(s), got ${arguments.length}",
            position
          )

      case Some(_) =>
        if arguments.length < parameters.length then
          SchemeFailure.raise(
            s"procedure expected at least ${parameters.length} argument(s), got ${arguments.length}",
            position
          )

    val fixedBindings = parameters.zip(arguments.take(parameters.length))
    val bindings =
      restParameter match
        case Some(restName) =>
          fixedBindings ++ List(restName -> RuntimeSupport.buildList(arguments.drop(parameters.length)))
        case None =>
          fixedBindings
    val callEnv = Environment.child(closureEnv, bindings)
    deferSequence(body, callEnv)

  private def clauseMatches(clause: ClosureValue, argumentCount: Int): Boolean =
    clause.restParameter match
      case Some(_) => argumentCount >= clause.parameters.length
      case None    => argumentCount == clause.parameters.length
