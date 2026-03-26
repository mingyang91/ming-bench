package ming

import SchemeModel.*
import SchemeRuntime.*

object SchemeInterpreter:

  def evalToString(input: String): String =
    render(evalProgram(input))

  def evalToStringWithOutput(input: String): (String, String) =
    (render(evalProgram(input)), "")

  private def evalProgram(input: String): Value =
    val expressions = SchemeParser.parseProgram(input)
    if expressions.isEmpty then throw new EvalError("empty program")
    evalSequence(expressions, baseEnv())

  private def eval(expr: Expr, env: Env): Value =
    expr match
      case Expr.IntegerLiteral(value) => Value.IntegerValue(value)
      case Expr.BooleanLiteral(value) => Value.BooleanValue(value)
      case Expr.StringLiteral(value)  => Value.StringValue(value)
      case Expr.Symbol(name)          => env.lookup(name)
      case Expr.ListExpr(Nil) =>
        throw new EvalError("cannot evaluate an empty list")
      case Expr.ListExpr(Expr.Symbol("quote") :: args) =>
        evalQuote(args)
      case Expr.ListExpr(Expr.Symbol("if") :: args) =>
        evalIf(args, env)
      case Expr.ListExpr(Expr.Symbol("define") :: args) =>
        evalDefine(args, env)
      case Expr.ListExpr(Expr.Symbol("lambda") :: args) =>
        evalLambda(args, env)
      case Expr.ListExpr(Expr.Symbol("and") :: rest) =>
        evalAnd(rest, env)
      case Expr.ListExpr(Expr.Symbol("or") :: rest) =>
        evalOr(rest, env)
      case Expr.ListExpr(Expr.Symbol("begin") :: rest) =>
        evalBegin(rest, env)
      case Expr.ListExpr(Expr.Symbol("cond") :: rest) =>
        evalCond(rest, env)
      case Expr.ListExpr(Expr.Symbol("let") :: rest) =>
        evalLet(rest, env)
      case Expr.ListExpr(operator :: args) =>
        apply(eval(operator, env), args.map(arg => eval(arg, env)))

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
      case Expr.Symbol(name) :: valueExpr :: Nil =>
        env.define(name, eval(valueExpr, env))
        Value.VoidValue
      case Expr.ListExpr(Expr.Symbol(name) :: params) :: body if body.nonEmpty =>
        env.define(name, buildClosure(params, body, env, Some(name)))
        Value.VoidValue
      case _ =>
        throw new EvalError("invalid define form")

  private def evalLambda(args: List[Expr], env: Env): Value =
    args match
      case Expr.ListExpr(params) :: body if body.nonEmpty =>
        buildClosure(params, body, env, None)
      case _ =>
        throw new EvalError("invalid lambda form")

  private def evalBegin(args: List[Expr], env: Env): Value =
    evalSequence(args, env)

  private def evalCond(clauses: List[Expr], env: Env): Value =
    @annotation.tailrec
    def loop(remaining: List[Expr]): Value =
      remaining match
        case Nil =>
          Value.VoidValue
        case Expr.ListExpr(Nil) :: _ =>
          throw new EvalError("cond clauses must be non-empty lists")
        case Expr.ListExpr(Expr.Symbol("else") :: expressions) :: tail =>
          if tail.nonEmpty then throw new EvalError("cond else clause must be last")
          evalSequence(expressions, env)
        case Expr.ListExpr(testExpr :: expressions) :: tail =>
          val testValue = eval(testExpr, env)
          if isTruthy(testValue) then if expressions.isEmpty then testValue else evalSequence(expressions, env)
          else loop(tail)
        case _ =>
          throw new EvalError("cond clauses must be non-empty lists")

    loop(clauses)

  private def evalLet(args: List[Expr], env: Env): Value =
    args match
      case Expr.Symbol(name) :: bindingsExpr :: body if body.nonEmpty =>
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
    val closure: Value.Closure = Value.Closure(Some(name), params, body, letEnv)

    letEnv.define(name, closure)
    applyClosure(closure, args)

  private def parseBindings(bindingsExpr: Expr): List[(String, Expr)] =
    bindingsExpr match
      case Expr.ListExpr(bindings) =>
        val parsed = bindings.map {
          case Expr.ListExpr(List(Expr.Symbol(name), valueExpr)) =>
            (name, valueExpr)
          case Expr.ListExpr(List(_, _)) =>
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
    val params = paramsExpr.map {
      case Expr.Symbol(paramName) => paramName
      case _                      => throw new EvalError("lambda parameters must be symbols")
    }
    ensureDistinct(params, "lambda parameters")
    Value.Closure(name, params, body, env)

  private def quoteExpr(expr: Expr): Value =
    expr match
      case Expr.IntegerLiteral(value) => Value.IntegerValue(value)
      case Expr.BooleanLiteral(value) => Value.BooleanValue(value)
      case Expr.StringLiteral(value)  => Value.StringValue(value)
      case Expr.Symbol(name)          => Value.SymbolValue(name)
      case Expr.ListExpr(items) =>
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

  private def apply(procedure: Value, args: List[Value]): Value =
    procedure match
      case Value.Builtin(_, implementation) => implementation(args)
      case closure: Value.Closure           => applyClosure(closure, args)
      case other =>
        throw new EvalError(s"not a procedure: ${render(other)}")

  private def applyClosure(closure: Value.Closure, args: List[Value]): Value =
    requireArgCount(closure.name.getOrElse("lambda"), args, closure.params.length)
    val callEnv = new Env(Some(closure.env))
    closure.params.zip(args).foreach { case (name, value) =>
      callEnv.define(name, value)
    }
    evalSequence(closure.body, callEnv)

  private def evalSequence(expressions: List[Expr], env: Env): Value =
    expressions.foldLeft(Value.VoidValue: Value) { (_, expr) =>
      eval(expr, env)
    }
