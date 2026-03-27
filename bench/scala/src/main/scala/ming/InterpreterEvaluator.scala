package ming

private[ming] object InterpreterEvaluator:

  def evalSequence(expressions: List[Expr], env: Environment): Value =
    expressions.foldLeft[Value](VoidValue): (_, expression) =>
      eval(expression, env)

  def eval(expression: Expr, env: Environment): Value =
    expression match
      case IntExpr(value, _) => IntValue(value)
      case RationalExpr(numerator, denominator, _) =>
        NumericSupport.exactValue(numerator, denominator)
      case InexactExpr(value, _) => InexactValue(value)
      case BoolExpr(value, _)    => BoolValue(value)
      case StringExpr(value, _)  => StringValue(value)
      case CharExpr(value, _)    => CharValue(value)
      case SymbolExpr(name, position) =>
        env.lookup(name, position)
      case ListExpr(items, position) =>
        evalList(items, position, env)

  def applyFunction(function: Value, arguments: List[Value], position: Position): Value =
    function match
      case BuiltinValue(_, implementation) =>
        implementation(arguments, position)
      case ClosureValue(parameters, restParameter, body, closureEnv, _) =>
        applyClosure(parameters, restParameter, body, closureEnv, arguments, position)
      case other =>
        SchemeFailure.raise(
          s"attempted to call a non-procedure value: ${other.render}",
          position
        )

  private def evalList(items: List[Expr], position: Position, env: Environment): Value =
    items match
      case Nil =>
        SchemeFailure.raise("cannot evaluate an empty list", position)
      case SymbolExpr("and", _) :: rest =>
        SpecialFormEvaluator.evalAnd(rest, env)
      case SymbolExpr("begin", _) :: rest =>
        evalSequence(rest, env)
      case SymbolExpr("cond", _) :: rest =>
        SpecialFormEvaluator.evalCond(rest, env)
      case SymbolExpr("or", _) :: rest =>
        SpecialFormEvaluator.evalOr(rest, env)
      case SymbolExpr("define", _) :: rest =>
        SpecialFormEvaluator.evalDefine(rest, position, env)
      case SymbolExpr("define-syntax", _) :: rest =>
        SpecialFormEvaluator.evalDefineSyntax(rest, position, env)
      case SymbolExpr("if", _) :: rest =>
        SpecialFormEvaluator.evalIf(rest, position, env)
      case SymbolExpr("let", _) :: rest =>
        SpecialFormEvaluator.evalLet(rest, position, env)
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
            evalApplication(operator, arguments, position, env)
      case operator :: arguments =>
        evalApplication(operator, arguments, position, env)

  private def evalMacroApplication(
    application: ListExpr,
    macroDefinition: SyntaxRulesMacro,
    env: Environment
  ): Value =
    val expansion = MacroExpander.expand(application, macroDefinition)
    expansion.aliases.foreach((alias, cell) => env.defineAlias(alias, cell))
    eval(expansion.expr, env)

  private def evalApplication(
    operator: Expr,
    arguments: List[Expr],
    position: Position,
    env: Environment
  ): Value =
    val function           = eval(operator, env)
    val evaluatedArguments = arguments.map(argument => eval(argument, env))
    applyFunction(function, evaluatedArguments, position)

  private def applyClosure(
    parameters: List[String],
    restParameter: Option[String],
    body: List[Expr],
    closureEnv: Environment,
    arguments: List[Value],
    position: Position
  ): Value =
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
    evalSequence(body, callEnv)
