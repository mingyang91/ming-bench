package ming

import scala.annotation.tailrec

/** Core evaluation logic for the Scheme interpreter. */
object Interpreter:

  import SchemeValue.*

  private val builtinNames: Set[String] =
    Set(
      "+",
      "-",
      "*",
      "/",
      "<",
      ">",
      "=",
      "<=",
      ">=",
      "not",
      "cons",
      "car",
      "cdr",
      "null?",
      "list",
      "length",
      "string?",
      "number?",
      "boolean?",
      "pair?",
      "symbol?"
    )

  val defaultEnv: Environment =
    builtinNames.foldLeft(Environment.empty) { (env, name) =>
      env.define(name, SchemeSymbol(name))
    }

  /** Recursively strip all SchemeLocated wrappers from a value. */
  private def strip(v: SchemeValue): SchemeValue = v match
    case SchemeLocated(inner, _, _) => strip(inner)
    case SchemeList(elems)          => SchemeList(elems.map(strip))
    case other                      => other

  /** Evaluate an expression, returning the result and (possibly updated) environment.
    */
  def eval(
    expr: SchemeValue,
    env: Environment
  ): (SchemeValue, Environment) =
    expr match
      case SchemeInt(_)    => (expr, env)
      case SchemeBool(_)   => (expr, env)
      case SchemeString(_) => (expr, env)
      case SchemeNil       => (expr, env)
      case SchemeVoid      => (expr, env)
      case _: SchemeLambda => (expr, env)
      case SchemeLocated(inner, line, col) =>
        try eval(inner, env)
        catch
          case e: EvalError if !e.hasPosition =>
            throw new EvalError(s"$line:$col: ${e.getMessage}", hasPosition = true)
      case SchemeSymbol(name) =>
        env.lookup(name) match
          case Some(v) => (v, env)
          case None =>
            throw new EvalError(s"unbound variable: $name")
      case SchemeList(elements) => evalList(elements.map(strip), env)

  private def evalList(
    elements: List[SchemeValue],
    env: Environment
  ): (SchemeValue, Environment) =
    elements match
      case Nil                            => (SchemeNil, env)
      case SchemeSymbol("define") :: rest => evalDefine(rest, env)
      case SchemeSymbol("if") :: rest     => evalIf(rest, env)
      case SchemeSymbol("quote") :: rest  => (evalQuote(rest), env)
      case SchemeSymbol("and") :: args    => (evalAnd(args, env), env)
      case SchemeSymbol("or") :: args     => (evalOr(args, env), env)
      case SchemeSymbol("lambda") :: rest => (evalLambda(rest, env), env)
      case SchemeSymbol("let") :: rest    => evalLet(rest, env)
      case SchemeSymbol("begin") :: rest  => evalBegin(rest, env)
      case SchemeSymbol("cond") :: rest   => evalCond(rest, env)
      case head :: args =>
        val (func, _)     = eval(head, env)
        val evaluatedArgs = args.map(a => eval(a, env)._1)
        (applyProc(func, evaluatedArgs), env)

  private def evalDefine(
    args: List[SchemeValue],
    env: Environment
  ): (SchemeValue, Environment) =
    args match
      case SchemeList(SchemeSymbol(name) :: params) :: body =>
        val paramNames = extractParamNames(params)
        val lambda =
          SchemeLambda(paramNames, body, env, Some(name))
        val newEnv = env.define(name, lambda)
        (SchemeVoid, newEnv)
      case SchemeSymbol(name) :: valueExpr :: Nil =>
        val (value, _) = eval(valueExpr, env)
        val named = value match
          case SchemeLambda(p, b, c, None) =>
            SchemeLambda(p, b, c, Some(name))
          case other => other
        val newEnv = env.define(name, named)
        (SchemeVoid, newEnv)
      case _ =>
        throw new EvalError("bad define syntax")

  private def extractParamNames(
    params: List[SchemeValue]
  ): List[String] =
    params.map {
      case SchemeSymbol(n) => n
      case other =>
        throw new EvalError(
          s"expected parameter name, got: ${other.display}"
        )
    }

  private def evalIf(
    args: List[SchemeValue],
    env: Environment
  ): (SchemeValue, Environment) =
    args match
      case cond :: thenBranch :: elseBranch =>
        val (condVal, _) = eval(cond, env)
        val isFalse = condVal match
          case SchemeBool(false) => true
          case _                 => false
        if isFalse then
          elseBranch match
            case elseExpr :: Nil => (eval(elseExpr, env)._1, env)
            case Nil             => (SchemeVoid, env)
            case _ =>
              throw new EvalError("if: too many arguments")
        else (eval(thenBranch, env)._1, env)
      case _ =>
        throw new EvalError("if requires at least 2 arguments")

  private def evalQuote(args: List[SchemeValue]): SchemeValue =
    args match
      case List(value) => value
      case _ =>
        throw new EvalError("quote expects exactly 1 argument")

  private def evalAnd(
    args: List[SchemeValue],
    env: Environment
  ): SchemeValue =
    args match
      case Nil         => SchemeBool(true)
      case last :: Nil => eval(last, env)._1
      case head :: tail =>
        eval(head, env)._1 match
          case SchemeBool(false) => SchemeBool(false)
          case _                 => evalAnd(tail, env)

  private def evalOr(
    args: List[SchemeValue],
    env: Environment
  ): SchemeValue =
    args match
      case Nil         => SchemeBool(false)
      case last :: Nil => eval(last, env)._1
      case head :: tail =>
        val result = eval(head, env)._1
        result match
          case SchemeBool(false) => evalOr(tail, env)
          case _                 => result

  private def evalLambda(
    args: List[SchemeValue],
    env: Environment
  ): SchemeValue =
    args match
      case SchemeList(params) :: body if body.nonEmpty =>
        val paramNames = extractParamNames(params)
        SchemeLambda(paramNames, body, env)
      case _ => throw new EvalError("bad lambda syntax")

  private def evalLet(
    args: List[SchemeValue],
    env: Environment
  ): (SchemeValue, Environment) =
    args match
      case SchemeList(bindings) :: body if body.nonEmpty =>
        val letEnv = bindings.foldLeft(env) {
          case (acc, SchemeList(List(SchemeSymbol(name), expr))) =>
            val (v, _) = eval(expr, env)
            acc.define(name, v)
          case _ => throw new EvalError("bad let binding")
        }
        (evalBodyWithEnv(body, letEnv), env)
      case _ => throw new EvalError("bad let syntax")

  private def evalBegin(
    exprs: List[SchemeValue],
    env: Environment
  ): (SchemeValue, Environment) =
    exprs match
      case Nil         => (SchemeVoid, env)
      case last :: Nil => eval(last, env)
      case head :: tail =>
        val (_, newEnv) = eval(head, env)
        evalBegin(tail, newEnv)

  @tailrec
  private def evalCond(
    clauses: List[SchemeValue],
    env: Environment
  ): (SchemeValue, Environment) =
    clauses match
      case Nil => (SchemeVoid, env)
      case SchemeList(SchemeSymbol("else") :: body) :: _ =>
        (evalBodyWithEnv(body, env), env)
      case SchemeList(test :: body) :: rest =>
        val (testVal, _) = eval(test, env)
        testVal match
          case SchemeBool(false) => evalCond(rest, env)
          case _ =>
            if body.isEmpty then (testVal, env)
            else (evalBodyWithEnv(body, env), env)
      case _ => throw new EvalError("bad cond syntax")

  private def evalBodyWithEnv(
    body: List[SchemeValue],
    env: Environment
  ): SchemeValue =
    body match
      case Nil         => SchemeVoid
      case last :: Nil => eval(last, env)._1
      case head :: tail =>
        val (_, newEnv) = eval(head, env)
        evalBodyWithEnv(tail, newEnv)

  def evalBody(
    body: List[SchemeValue],
    env: Environment
  ): SchemeValue =
    body match
      case Nil         => SchemeVoid
      case last :: Nil => eval(last, env)._1
      case head :: tail =>
        val (_, newEnv) = eval(head, env)
        evalBody(tail, newEnv)

  def applyProc(
    func: SchemeValue,
    args: List[SchemeValue]
  ): SchemeValue =
    func match
      case SchemeSymbol(name) =>
        Builtins.applyNamedBuiltin(name, args)
      case lam @ SchemeLambda(params, body, closure, nameOpt) =>
        if params.length != args.length then
          throw new EvalError(
            s"expected ${params.length} args, got ${args.length}"
          )
        val closureWithSelf = nameOpt match
          case Some(n) => closure.define(n, lam)
          case None    => closure
        val innerEnv = closureWithSelf.extend(params, args)
        evalBody(body, innerEnv)
      case _ =>
        throw new EvalError(s"not a procedure: ${func.display}")
