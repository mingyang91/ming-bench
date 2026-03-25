package ming

private[ming] object Interpreter:

  def evaluate(input: String): (Value, String) =
    val expressions = SchemeParser.parseProgram(input)
    if expressions.isEmpty then throw new EvalError("1:1: expected expression")

    val runtime = RuntimeContext()
    val macros  = MacroState()
    val env     = Env.root(Builtins.globalEnv(runtime, macros))
    val result  = evalSequence(expressions, env, macros)
    (result, runtime.capturedOutput)

  private def eval(expr: Expr, env: Env, macros: MacroState): Value =
    MacroExpander.expand(expr, macros) match
      case expanded if expanded != expr =>
        eval(expanded, env, macros)

      case current =>
        evalExpanded(current, env, macros)

  private def evalExpanded(expr: Expr, env: Env, macros: MacroState): Value =
    val forms = new InterpreterForms(
      env = env,
      evalExpr = expr => eval(expr, env, macros),
      evalSequenceIn = (expressions, scope) => evalSequence(expressions, scope, macros),
      applyProcedure = (proc, args, pos) => applyProcedure(proc, args, pos, macros)
    )

    expr match
      case Expr.IntAtom(value, _) =>
        Value.IntVal(value)

      case Expr.RationalAtom(numerator, denominator, _) =>
        Value.RationalVal(numerator, denominator)

      case Expr.InexactAtom(value, _) =>
        Value.InexactVal(value)

      case Expr.BoolAtom(value, _) =>
        Value.BoolVal(value)

      case Expr.StringAtom(value, _) =>
        Value.StringVal(value.toCharArray)

      case Expr.CharAtom(value, _) =>
        Value.CharVal(value)

      case Expr.Symbol(name, pos) =>
        env.lookup(name, pos)

      case Expr.ListExpr(Nil, pos) =>
        throw EvalError.at(pos, "cannot evaluate an empty list")

      case Expr.ListExpr(Expr.Symbol("define", _) :: args, pos) =>
        forms.evalDefine(args, pos)

      case Expr.ListExpr(Expr.Symbol("define-record-type", _) :: args, pos) =>
        RecordSupport.define(args, env, pos)

      case Expr.ListExpr(Expr.Symbol("define-syntax", _) :: args, pos) =>
        MacroExpander.define(args, env, macros, pos)

      case Expr.ListExpr(Expr.Symbol("set!", _) :: args, pos) =>
        forms.evalSet(args, pos)

      case Expr.ListExpr(Expr.Symbol("if", _) :: args, pos) =>
        forms.evalIf(args, pos)

      case Expr.ListExpr(Expr.Symbol("quote", _) :: args, pos) =>
        forms.evalQuote(args, pos)

      case Expr.ListExpr(Expr.Symbol("lambda", _) :: args, pos) =>
        forms.evalLambda(args, pos)

      case Expr.ListExpr(Expr.Symbol("case-lambda", _) :: args, pos) =>
        forms.evalCaseLambda(args, pos)

      case Expr.ListExpr(Expr.Symbol("begin", _) :: args, _) =>
        evalBegin(args, env, macros)

      case Expr.ListExpr(Expr.Symbol("let", _) :: args, pos) =>
        forms.evalLet(args, pos)

      case Expr.ListExpr(Expr.Symbol("letrec", _) :: args, pos) =>
        forms.evalLetrec(args, pos, sequential = false)

      case Expr.ListExpr(Expr.Symbol("letrec*", _) :: args, pos) =>
        forms.evalLetrec(args, pos, sequential = true)

      case Expr.ListExpr(Expr.Symbol("cond", _) :: args, pos) =>
        forms.evalCond(args, pos)

      case Expr.ListExpr(Expr.Symbol("case", _) :: args, pos) =>
        forms.evalCase(args, pos)

      case Expr.ListExpr(Expr.Symbol("and", _) :: args, _) =>
        forms.evalAnd(args)

      case Expr.ListExpr(Expr.Symbol("or", _) :: args, _) =>
        forms.evalOr(args)

      case Expr.ListExpr(Expr.Symbol("do", _) :: args, pos) =>
        forms.evalDo(args, pos)

      case Expr.ListExpr(head :: args, pos) =>
        applyProcedure(eval(head, env, macros), args.map(arg => eval(arg, env, macros)), pos, macros)

  private def evalSequence(expressions: List[Expr], env: Env, macros: MacroState): Value =
    expressions.foldLeft[Value](Value.VoidVal) { (_, expr) =>
      eval(expr, env, macros)
    }

  private def evalBegin(args: List[Expr], env: Env, macros: MacroState): Value =
    evalSequence(args, env, macros)

  private[ming] def applyProcedure(proc: Value, args: List[Value], pos: SourcePos, macros: MacroState): Value =
    proc match
      case Value.Builtin(_, fn) =>
        fn(args, pos)

      case Value.Closure(params, restParam, body, closureEnv) =>
        applyClosure(params, restParam, body, closureEnv, args, pos, macros)

      case Value.CaseClosure(clauses, closureEnv) =>
        clauses.find(_.matchesArity(args.length)) match
          case Some(ProcedureClause(params, restParam, body)) =>
            applyClosure(params, restParam, body, closureEnv, args, pos, macros)

          case None =>
            throw EvalError.at(pos, s"case-lambda has no matching clause for ${args.length} arguments")

      case other =>
        throw EvalError.at(pos, s"attempted to call a ${other.typeName} value")

  private def applyClosure(
    params: List[String],
    restParam: Option[String],
    body: List[Expr],
    closureEnv: Env,
    args: List[Value],
    pos: SourcePos,
    macros: MacroState
  ): Value =
    restParam match
      case None =>
        if params.length != args.length then
          throw EvalError.at(pos, s"expected ${params.length} arguments, got ${args.length}")

        evalSequence(body, closureEnv.extend(params, args), macros)

      case Some(restName) =>
        if args.length < params.length then
          throw EvalError.at(pos, s"expected at least ${params.length} arguments, got ${args.length}")

        val fixedArgs = args.take(params.length)
        val restArgs  = Value.list(args.drop(params.length))
        evalSequence(body, closureEnv.extend(params :+ restName, fixedArgs :+ restArgs), macros)
