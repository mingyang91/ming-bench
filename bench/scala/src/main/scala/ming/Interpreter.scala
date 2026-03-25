package ming

import scala.annotation.tailrec

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
        evalDefine(args, env, macros, pos)

      case Expr.ListExpr(Expr.Symbol("define-record-type", _) :: args, pos) =>
        RecordSupport.define(args, env, pos)

      case Expr.ListExpr(Expr.Symbol("define-syntax", _) :: args, pos) =>
        MacroExpander.define(args, env, macros, pos)

      case Expr.ListExpr(Expr.Symbol("set!", _) :: args, pos) =>
        evalSet(args, env, macros, pos)

      case Expr.ListExpr(Expr.Symbol("if", _) :: args, pos) =>
        evalIf(args, env, macros, pos)

      case Expr.ListExpr(Expr.Symbol("quote", _) :: args, pos) =>
        evalQuote(args, pos)

      case Expr.ListExpr(Expr.Symbol("lambda", _) :: args, pos) =>
        evalLambda(args, env, pos)

      case Expr.ListExpr(Expr.Symbol("begin", _) :: args, _) =>
        evalBegin(args, env, macros)

      case Expr.ListExpr(Expr.Symbol("let", _) :: args, pos) =>
        evalLet(args, env, macros, pos)

      case Expr.ListExpr(Expr.Symbol("cond", _) :: args, pos) =>
        evalCond(args, env, macros, pos)

      case Expr.ListExpr(Expr.Symbol("and", _) :: args, _) =>
        evalAnd(args, env, macros)

      case Expr.ListExpr(Expr.Symbol("or", _) :: args, _) =>
        evalOr(args, env, macros)

      case Expr.ListExpr(head :: args, pos) =>
        applyProcedure(eval(head, env, macros), args.map(arg => eval(arg, env, macros)), pos, macros)

  private def evalSequence(expressions: List[Expr], env: Env, macros: MacroState): Value =
    expressions.foldLeft[Value](Value.VoidVal) { (_, expr) =>
      eval(expr, env, macros)
    }

  private def evalDefine(args: List[Expr], env: Env, macros: MacroState, pos: SourcePos): Value =
    args match
      case Expr.Symbol(name, _) :: valueExpr :: Nil =>
        env.define(name, eval(valueExpr, env, macros))
        Value.VoidVal

      case Expr.ListExpr(Expr.Symbol(name, _) :: params, signaturePos) :: body if body.nonEmpty =>
        val (fixedParams, restParam) = parseParameters(params, signaturePos)
        env.define(name, Value.Closure(fixedParams, restParam, body, env))
        Value.VoidVal

      case _ =>
        throw EvalError.at(pos, "define expects a name and expression")

  private def evalSet(args: List[Expr], env: Env, macros: MacroState, pos: SourcePos): Value =
    args match
      case Expr.Symbol(name, _) :: valueExpr :: Nil =>
        env.set(name, eval(valueExpr, env, macros), pos)
        Value.VoidVal

      case _ =>
        throw EvalError.at(pos, "set! expects a name and expression")

  private def evalIf(args: List[Expr], env: Env, macros: MacroState, pos: SourcePos): Value =
    args match
      case condition :: thenBranch :: elseBranch :: Nil =>
        if Value.isTruthy(eval(condition, env, macros)) then eval(thenBranch, env, macros)
        else eval(elseBranch, env, macros)

      case _ =>
        throw EvalError.at(pos, "if expects exactly 3 arguments")

  private def evalQuote(args: List[Expr], pos: SourcePos): Value =
    args match
      case quoted :: Nil =>
        Value.fromQuotedExpr(quoted)

      case _ =>
        throw EvalError.at(pos, "quote expects exactly 1 argument")

  private def evalLambda(args: List[Expr], env: Env, pos: SourcePos): Value =
    args match
      case Expr.ListExpr(params, paramsPos) :: body if body.nonEmpty =>
        val (fixedParams, restParam) = parseParameters(params, paramsPos)
        Value.Closure(fixedParams, restParam, body, env)

      case _ =>
        throw EvalError.at(pos, "lambda expects a parameter list and body")

  private def evalBegin(args: List[Expr], env: Env, macros: MacroState): Value =
    evalSequence(args, env, macros)

  private def evalLet(args: List[Expr], env: Env, macros: MacroState, pos: SourcePos): Value =
    args match
      case Expr.Symbol(name, _) :: Expr.ListExpr(bindings, bindingsPos) :: body if body.nonEmpty =>
        evalNamedLet(name, bindings, body, env, macros, bindingsPos, pos)

      case Expr.ListExpr(bindings, bindingsPos) :: body if body.nonEmpty =>
        val parsedBindings = parseBindings(bindings, bindingsPos)
        val names          = parsedBindings.map(_._1)
        val values         = parsedBindings.map { case (_, valueExpr) => eval(valueExpr, env, macros) }
        evalSequence(body, env.extend(names, values), macros)

      case _ =>
        throw EvalError.at(pos, "let expects bindings and a body")

  private def evalNamedLet(
    name: String,
    bindings: List[Expr],
    body: List[Expr],
    env: Env,
    macros: MacroState,
    bindingsPos: SourcePos,
    pos: SourcePos
  ): Value =
    val parsedBindings = parseBindings(bindings, bindingsPos)
    val names          = parsedBindings.map(_._1)
    val values         = parsedBindings.map { case (_, valueExpr) => eval(valueExpr, env, macros) }
    val loopEnv        = Env.child(env)
    val closure        = Value.Closure(names, None, body, loopEnv)

    loopEnv.define(name, closure)
    applyProcedure(closure, values, pos, macros)

  private def evalCond(clauses: List[Expr], env: Env, macros: MacroState, pos: SourcePos): Value =
    clauses match
      case Nil =>
        Value.VoidVal

      case Expr.ListExpr(Nil, clausePos) :: _ =>
        throw EvalError.at(clausePos, "cond clause cannot be empty")

      case Expr.ListExpr(Expr.Symbol("else", _) :: body, clausePos) :: rest =>
        if rest.nonEmpty then throw EvalError.at(clausePos, "else must be the last cond clause")
        else if body.isEmpty then Value.VoidVal
        else evalSequence(body, env, macros)

      case Expr.ListExpr(test :: body, _) :: rest =>
        val testValue = eval(test, env, macros)
        if Value.isTruthy(testValue) then if body.isEmpty then testValue else evalSequence(body, env, macros)
        else evalCond(rest, env, macros, pos)

      case other :: _ =>
        throw EvalError.at(pos, s"invalid cond clause: ${other}")

  private def parseBindings(bindings: List[Expr], pos: SourcePos): List[(String, Expr)] =
    bindings.map {
      case Expr.ListExpr(Expr.Symbol(name, _) :: valueExpr :: Nil, _) =>
        (name, valueExpr)

      case Expr.ListExpr(_, bindingPos) =>
        throw EvalError.at(bindingPos, "binding must contain exactly a name and expression")

      case _ =>
        throw EvalError.at(pos, "bindings must be lists")
    }

  private def parseParameters(params: List[Expr], pos: SourcePos): (List[String], Option[String]) =
    @tailrec
    def loop(
      remaining: List[Expr],
      acc: List[String]
    ): (List[String], Option[String]) =
      remaining match
        case Nil =>
          (acc.reverse, None)

        case Expr.Symbol(".", dotPos) :: Expr.Symbol(restName, restPos) :: Nil =>
          if restName == "." then throw EvalError.at(restPos, "parameter list contains an invalid rest parameter")
          (acc.reverse, Some(restName))

        case Expr.Symbol(".", dotPos) :: _ =>
          throw EvalError.at(dotPos, "dot must be followed by exactly one rest parameter")

        case Expr.Symbol(name, _) :: tail =>
          if name == "." then throw EvalError.at(pos, "parameter list contains an invalid dot")
          loop(tail, name :: acc)

        case _ =>
          throw EvalError.at(pos, "parameter list must contain only symbols")

    loop(params, Nil)

  @tailrec
  private def evalAnd(
    args: List[Expr],
    env: Env,
    macros: MacroState,
    result: Value = Value.BoolVal(true)
  ): Value =
    args match
      case Nil =>
        result

      case _ if !Value.isTruthy(result) =>
        result

      case expr :: rest =>
        evalAnd(rest, env, macros, eval(expr, env, macros))

  @tailrec
  private def evalOr(
    args: List[Expr],
    env: Env,
    macros: MacroState,
    result: Value = Value.BoolVal(false)
  ): Value =
    args match
      case Nil =>
        result

      case _ if Value.isTruthy(result) =>
        result

      case expr :: rest =>
        evalOr(rest, env, macros, eval(expr, env, macros))

  private[ming] def applyProcedure(proc: Value, args: List[Value], pos: SourcePos, macros: MacroState): Value =
    proc match
      case Value.Builtin(_, fn) =>
        fn(args, pos)

      case Value.Closure(params, restParam, body, closureEnv) =>
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

      case other =>
        throw EvalError.at(pos, s"attempted to call a ${other.typeName} value")
