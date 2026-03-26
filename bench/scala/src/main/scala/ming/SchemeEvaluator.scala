package ming

import SchemeBuiltinSupport.*
import SchemeEvaluatorSupport.*
import SchemeMacros.*
import SchemeModel.*
import SchemeRecords.*
import SchemeRuntime.*

private[ming] object SchemeEvaluator:

  def evalSequence(expressions: List[Expr], env: Env): Value =
    expressions.foldLeft(Value.VoidValue: Value) { (_, expr) =>
      eval(expr, env)
    }

  def applyProcedure(procedure: Value, args: List[Value]): Value =
    procedure match
      case Value.Builtin(_, implementation) => implementation(args)
      case caseClosure: Value.CaseClosure   => applyCaseClosure(caseClosure, args)
      case closure: Value.Closure           => applyClosure(closure, args)
      case other =>
        throw new EvalError(s"not a procedure: ${SchemeRuntime.render(other)}")

  private def eval(expr: Expr, env: Env): Value =
    withErrorContext(expr.pos) {
      expr match
        case Expr.IntegerLiteral(value, _) => Value.IntegerValue(value)
        case Expr.RationalLiteral(numerator, denominator, _) =>
          SchemeNumbers.exactRational(numerator, denominator)
        case Expr.InexactLiteral(value, _) => Value.InexactValue(value)
        case Expr.BooleanLiteral(value, _) => Value.BooleanValue(value)
        case Expr.StringLiteral(value, _)  => Value.StringValue(SchemeString.fromText(value))
        case Expr.CharLiteral(value, _)    => Value.CharValue(value)
        case Expr.Symbol(name, _)          => env.lookup(name)
        case Expr.ListExpr(Nil, _) =>
          throw new EvalError("cannot evaluate an empty list")
        case list @ Expr.ListExpr(operator :: args, _) =>
          evalCompoundExpression(list, operator, args, env)
    }

  private def evalCompoundExpression(
    list: Expr.ListExpr,
    operator: Expr,
    args: List[Expr],
    env: Env
  ): Value =
    operator match
      case Expr.Symbol(name, _) =>
        env.lookupSyntax(name) match
          case Some(transformer) =>
            val expanded = transformer.expand(list, env)
            eval(expanded.expr, expanded.env)
          case None =>
            evalApplication(operator, args, env)
      case _ =>
        evalApplication(operator, args, env)

  private def evalApplication(operator: Expr, args: List[Expr], env: Env): Value =
    operator match
      case Expr.Symbol("quote", _) =>
        evalQuote(args)
      case Expr.Symbol("if", _) =>
        evalIf(args, env)
      case Expr.Symbol("define", _) =>
        evalDefine(args, env)
      case Expr.Symbol("define-syntax", _) =>
        evalDefineSyntax(args, env)
      case Expr.Symbol("define-record-type", _) =>
        defineRecordType(args, env)
      case Expr.Symbol("lambda", _) =>
        evalLambda(args, env)
      case Expr.Symbol("case-lambda", _) =>
        evalCaseLambda(args, env)
      case Expr.Symbol("set!", _) =>
        evalSet(args, env)
      case Expr.Symbol("and", _) =>
        evalAnd(args, env)
      case Expr.Symbol("or", _) =>
        evalOr(args, env)
      case Expr.Symbol("begin", _) =>
        evalBegin(args, env)
      case Expr.Symbol("cond", _) =>
        evalCond(args, env)
      case Expr.Symbol("let", _) =>
        evalLet(args, env)
      case _ =>
        applyProcedure(eval(operator, env), args.map(arg => eval(arg, env)))

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

  private def evalDefineSyntax(args: List[Expr], env: Env): Value =
    args match
      case Expr.Symbol(name, _) :: transformer :: Nil =>
        env.defineSyntax(name, parseSyntaxRules(name, transformer, env))
        Value.VoidValue
      case _ =>
        throw new EvalError("invalid define-syntax form")

  private def evalLambda(args: List[Expr], env: Env): Value =
    args match
      case Expr.ListExpr(params, _) :: body if body.nonEmpty =>
        buildClosure(params, body, env, None)
      case _ =>
        throw new EvalError("invalid lambda form")

  private def evalCaseLambda(args: List[Expr], env: Env): Value =
    buildCaseClosure(args, env)

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

  private def applyClosure(closure: Value.Closure, args: List[Value]): Value =
    applyProcedureBody(
      closure.name.getOrElse("lambda"),
      closure.fixedParams,
      closure.restParam,
      closure.body,
      closure.env,
      args
    )

  private def applyCaseClosure(caseClosure: Value.CaseClosure, args: List[Value]): Value =
    caseClosure.clauses.find(clauseMatchesArgCount(_, args.length)) match
      case Some(clause) =>
        applyProcedureBody(
          "case-lambda",
          clause.fixedParams,
          clause.restParam,
          clause.body,
          clause.env,
          args
        )
      case None =>
        throw new EvalError(s"case-lambda expected a matching clause for ${args.length} argument(s)")

  private def clauseMatchesArgCount(clause: CaseLambdaClause, argCount: Int): Boolean =
    clause.restParam match
      case Some(_) => argCount >= clause.fixedParams.length
      case None    => argCount == clause.fixedParams.length

  private def applyProcedureBody(
    name: String,
    fixedParams: List[String],
    restParam: Option[String],
    body: List[Expr],
    closureEnv: Env,
    args: List[Value]
  ): Value =
    restParam match
      case Some(_) =>
        requireMinArgCount(name, args, fixedParams.length)
      case None =>
        requireArgCount(name, args, fixedParams.length)

    val callEnv = new Env(Some(closureEnv))
    fixedParams.zip(args).foreach { case (paramName, value) =>
      callEnv.define(paramName, value)
    }
    restParam.foreach { restName =>
      callEnv.define(restName, makeList(args.drop(fixedParams.length)))
    }
    evalSequence(body, callEnv)
