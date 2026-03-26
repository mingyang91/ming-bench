package ming

import SchemeBuiltinSupport.*
import SchemeModel.*
import SchemeRuntime.*

object SchemeInterpreter:

  def evalToString(input: String): String =
    val output = new StringBuilder
    render(evalProgram(input, output))

  def evalToStringWithOutput(input: String): (String, String) =
    val output = new StringBuilder
    val result = evalProgram(input, output)
    (render(result), output.result())

  private def evalProgram(input: String, output: StringBuilder): Value =
    val expressions = SchemeParser.parseProgram(input)
    if expressions.isEmpty then throw new EvalError("empty program")
    evalSequence(expressions, baseEnv(output))

  private def eval(expr: Expr, env: Env): Value =
    withErrorContext(expr.pos) {
      expr match
        case Expr.IntegerLiteral(value, _) => Value.IntegerValue(value)
        case Expr.BooleanLiteral(value, _) => Value.BooleanValue(value)
        case Expr.StringLiteral(value, _)  => Value.StringValue(SchemeString.fromText(value))
        case Expr.CharLiteral(value, _)    => Value.CharValue(value)
        case Expr.Symbol(name, _)          => env.lookup(name)
        case Expr.ListExpr(Nil, _) =>
          throw new EvalError("cannot evaluate an empty list")
        case Expr.ListExpr(Expr.Symbol("quote", _) :: args, _) =>
          evalQuote(args)
        case Expr.ListExpr(Expr.Symbol("if", _) :: args, _) =>
          evalIf(args, env)
        case Expr.ListExpr(Expr.Symbol("define", _) :: args, _) =>
          evalDefine(args, env)
        case Expr.ListExpr(Expr.Symbol("lambda", _) :: args, _) =>
          evalLambda(args, env)
        case Expr.ListExpr(Expr.Symbol("set!", _) :: args, _) =>
          evalSet(args, env)
        case Expr.ListExpr(Expr.Symbol("and", _) :: rest, _) =>
          evalAnd(rest, env)
        case Expr.ListExpr(Expr.Symbol("or", _) :: rest, _) =>
          evalOr(rest, env)
        case Expr.ListExpr(Expr.Symbol("begin", _) :: rest, _) =>
          evalBegin(rest, env)
        case Expr.ListExpr(Expr.Symbol("cond", _) :: rest, _) =>
          evalCond(rest, env)
        case Expr.ListExpr(Expr.Symbol("let", _) :: rest, _) =>
          evalLet(rest, env)
        case Expr.ListExpr(operator :: args, _) =>
          applyProcedure(eval(operator, env), args.map(arg => eval(arg, env)))
    }

  private def withErrorContext[T](pos: SourcePos)(thunk: => T): T =
    try thunk
    catch
      case error: EvalError if error.position.isEmpty =>
        throw error.withPosition(pos)

  private def evalQuote(args: List[Expr]): Value =
    args match
      case expr :: Nil => quoteExpr(expr)
      case _ =>
        throw new EvalError(s"quote expected 1 argument, got ${args.length}")

  private def evalIf(args: List[Expr], env: Env): Value =
    args match
      case condition :: whenTrue :: whenFalse :: Nil =>
        if isTruthy(eval(condition, env)) then eval(whenTrue, env)
        else eval(whenFalse, env)
      case _ =>
        throw new EvalError(s"if expected 3 arguments, got ${args.length}")

  private def evalDefine(args: List[Expr], env: Env): Value =
    args match
      case Expr.Symbol(name, _) :: valueExpr :: Nil =>
        env.define(name, eval(valueExpr, env))
        Value.VoidValue
      case Expr.ListExpr(Expr.Symbol(name, _) :: params, _) :: body if body.nonEmpty =>
        env.define(name, buildClosure(params, body, env, Some(name)))
        Value.VoidValue
      case _ =>
        throw new EvalError("invalid define form")

  private def evalLambda(args: List[Expr], env: Env): Value =
    args match
      case Expr.ListExpr(params, _) :: body if body.nonEmpty =>
        buildClosure(params, body, env, None)
      case _ =>
        throw new EvalError("invalid lambda form")

  private def evalSet(args: List[Expr], env: Env): Value =
    args match
      case Expr.Symbol(name, _) :: valueExpr :: Nil =>
        env.set(name, eval(valueExpr, env))
        Value.VoidValue
      case _ =>
        throw new EvalError("invalid set! form")

  private def evalBegin(args: List[Expr], env: Env): Value =
    evalSequence(args, env)

  private def evalCond(clauses: List[Expr], env: Env): Value =
    @annotation.tailrec
    def loop(remaining: List[Expr]): Value =
      remaining match
        case Nil =>
          Value.VoidValue
        case Expr.ListExpr(Nil, _) :: _ =>
          throw new EvalError("cond clauses must be non-empty lists")
        case Expr.ListExpr(Expr.Symbol("else", _) :: expressions, _) :: tail =>
          if tail.nonEmpty then throw new EvalError("cond else clause must be last")
          evalSequence(expressions, env)
        case Expr.ListExpr(testExpr :: expressions, _) :: tail =>
          val testValue = eval(testExpr, env)
          if isTruthy(testValue) then if expressions.isEmpty then testValue else evalSequence(expressions, env)
          else loop(tail)
        case _ =>
          throw new EvalError("cond clauses must be non-empty lists")

    loop(clauses)

  private def evalLet(args: List[Expr], env: Env): Value =
    args match
      case Expr.Symbol(name, _) :: bindingsExpr :: body if body.nonEmpty =>
        evalNamedLet(name, bindingsExpr, body, env)
      case bindingsExpr :: body if body.nonEmpty =>
        evalPlainLet(bindingsExpr, body, env)
      case _ =>
        throw new EvalError("invalid let form")

  private def evalPlainLet(bindingsExpr: Expr, body: List[Expr], env: Env): Value =
    val bindings = parseBindings(bindingsExpr)
    val values = bindings.map { case (_, valueExpr) =>
      eval(valueExpr, env)
    }
    val letEnv = new Env(Some(env))

    bindings.zip(values).foreach { case ((name, _), value) =>
      letEnv.define(name, value)
    }

    evalSequence(body, letEnv)

  private def evalNamedLet(name: String, bindingsExpr: Expr, body: List[Expr], env: Env): Value =
    val bindings = parseBindings(bindingsExpr)
    val params   = bindings.map(_._1)
    val args = bindings.map { case (_, valueExpr) =>
      eval(valueExpr, env)
    }
    val letEnv                 = new Env(Some(env))
    val closure: Value.Closure = Value.Closure(Some(name), params, None, body, letEnv)

    letEnv.define(name, closure)
    applyClosure(closure, args)

  private def parseBindings(bindingsExpr: Expr): List[(String, Expr)] =
    bindingsExpr match
      case Expr.ListExpr(bindings, _) =>
        val parsed = bindings.map {
          case Expr.ListExpr(List(Expr.Symbol(name, _), valueExpr), _) =>
            (name, valueExpr)
          case Expr.ListExpr(List(_, _), _) =>
            throw new EvalError("let bindings must have symbol names")
          case _ =>
            throw new EvalError("let bindings must contain (name value) pairs")
        }
        ensureDistinct(parsed.map(_._1), "let bindings")
        parsed
      case _ =>
        throw new EvalError("let bindings must be a list")

  private def buildClosure(
    paramsExpr: List[Expr],
    body: List[Expr],
    env: Env,
    name: Option[String]
  ): Value =
    val (fixedParams, restParam) = parseClosureParams(paramsExpr)
    ensureDistinct(fixedParams ++ restParam.toList, "lambda parameters")
    Value.Closure(name, fixedParams, restParam, body, env)

  private def parseClosureParams(paramsExpr: List[Expr]): (List[String], Option[String]) =
    val dotIndex = paramsExpr.indexWhere {
      case Expr.Symbol(".", _) => true
      case _                   => false
    }

    if dotIndex < 0 then (paramsExpr.map(requireParamName), None)
    else
      val fixedParams = paramsExpr.take(dotIndex).map(requireParamName)
      paramsExpr.drop(dotIndex) match
        case Expr.Symbol(".", _) :: Expr.Symbol(restName, _) :: Nil if restName != "." =>
          (fixedParams, Some(restName))
        case _ =>
          throw new EvalError("invalid lambda parameter list")

  private def requireParamName(expr: Expr): String =
    expr match
      case Expr.Symbol(name, _) if name != "." => name
      case _                                   => throw new EvalError("lambda parameters must be symbols")

  private def quoteExpr(expr: Expr): Value =
    expr match
      case Expr.IntegerLiteral(value, _) => Value.IntegerValue(value)
      case Expr.BooleanLiteral(value, _) => Value.BooleanValue(value)
      case Expr.StringLiteral(value, _)  => Value.StringValue(SchemeString.fromText(value))
      case Expr.CharLiteral(value, _)    => Value.CharValue(value)
      case Expr.Symbol(name, _)          => Value.SymbolValue(name)
      case Expr.ListExpr(items, _) =>
        makeList(items.map(quoteExpr))

  private def evalAnd(args: List[Expr], env: Env): Value =
    @annotation.tailrec
    def loop(remaining: List[Expr], result: Value): Value =
      remaining match
        case Nil =>
          result
        case head :: tail =>
          val next = eval(head, env)
          if isTruthy(next) then loop(tail, next) else next

    loop(args, Value.BooleanValue(true))

  private def evalOr(args: List[Expr], env: Env): Value =
    @annotation.tailrec
    def loop(remaining: List[Expr], result: Value): Value =
      remaining match
        case Nil =>
          result
        case head :: tail =>
          val next = eval(head, env)
          if isTruthy(next) then next else loop(tail, next)

    loop(args, Value.BooleanValue(false))

  private[ming] def applyProcedure(procedure: Value, args: List[Value]): Value =
    procedure match
      case Value.Builtin(_, implementation) => implementation(args)
      case closure: Value.Closure           => applyClosure(closure, args)
      case other =>
        throw new EvalError(s"not a procedure: ${render(other)}")

  private def applyClosure(closure: Value.Closure, args: List[Value]): Value =
    val name = closure.name.getOrElse("lambda")
    closure.restParam match
      case Some(_) =>
        requireMinArgCount(name, args, closure.fixedParams.length)
      case None =>
        requireArgCount(name, args, closure.fixedParams.length)

    val callEnv = new Env(Some(closure.env))
    closure.fixedParams.zip(args).foreach { case (name, value) =>
      callEnv.define(name, value)
    }
    closure.restParam.foreach { restName =>
      callEnv.define(restName, makeList(args.drop(closure.fixedParams.length)))
    }
    evalSequence(closure.body, callEnv)

  private def evalSequence(expressions: List[Expr], env: Env): Value =
    expressions.foldLeft(Value.VoidValue: Value) { (_, expr) =>
      eval(expr, env)
    }
