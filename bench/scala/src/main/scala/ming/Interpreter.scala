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
      "symbol?",
      "char?",
      "display",
      "write",
      "newline",
      "string-append",
      "string-length",
      "substring",
      "string->number",
      "number->string",
      "symbol->string",
      "string->symbol",
      "string-ref"
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

  /** Evaluate an expression, returning the result, environment, and output.
    */
  def eval(
    expr: SchemeValue,
    env: Environment
  ): (SchemeValue, Environment, String) =
    expr match
      case SchemeInt(_)    => (expr, env, "")
      case SchemeBool(_)   => (expr, env, "")
      case SchemeString(_) => (expr, env, "")
      case SchemeNil       => (expr, env, "")
      case SchemeVoid      => (expr, env, "")
      case _: SchemeLambda => (expr, env, "")
      case SchemeChar(_)   => (expr, env, "")
      case SchemeLocated(inner, line, col) =>
        try eval(inner, env)
        catch
          case e: EvalError if !e.hasPosition =>
            throw new EvalError(
              s"$line:$col: ${e.getMessage}",
              hasPosition = true
            )
      case SchemeSymbol(name) =>
        env.lookup(name) match
          case Some(v) => (v, env, "")
          case None =>
            throw new EvalError(s"unbound variable: $name")
      case SchemeList(elements) => evalList(elements.map(strip), env)

  private def evalList(
    elements: List[SchemeValue],
    env: Environment
  ): (SchemeValue, Environment, String) =
    elements match
      case Nil                            => (SchemeNil, env, "")
      case SchemeSymbol("define") :: rest => evalDefine(rest, env)
      case SchemeSymbol("if") :: rest     => evalIf(rest, env)
      case SchemeSymbol("quote") :: rest  => (evalQuote(rest), env, "")
      case SchemeSymbol("and") :: args =>
        val (v, o) = evalAnd(args, env)
        (v, env, o)
      case SchemeSymbol("or") :: args =>
        val (v, o) = evalOr(args, env)
        (v, env, o)
      case SchemeSymbol("lambda") :: rest =>
        (evalLambda(rest, env), env, "")
      case SchemeSymbol("let") :: rest   => evalLet(rest, env)
      case SchemeSymbol("begin") :: rest => evalBegin(rest, env, "")
      case SchemeSymbol("cond") :: rest  => evalCond(rest, env, "")
      case head :: args =>
        val (func, _, funcOut)       = eval(head, env)
        val (evaluatedArgs, argsOut) = evalArgs(args, env)
        val (result, resultOut)      = Builtins.applyProc(func, evaluatedArgs)
        (result, env, funcOut + argsOut + resultOut)

  private def evalDefine(
    args: List[SchemeValue],
    env: Environment
  ): (SchemeValue, Environment, String) =
    args match
      case SchemeList(SchemeSymbol(name) :: params) :: body =>
        val paramNames = extractParamNames(params)
        val lambda =
          SchemeLambda(paramNames, body, env, Some(name))
        val newEnv = env.define(name, lambda)
        (SchemeVoid, newEnv, "")
      case SchemeSymbol(name) :: valueExpr :: Nil =>
        val (value, _, out) = eval(valueExpr, env)
        val named = value match
          case SchemeLambda(p, b, c, None) =>
            SchemeLambda(p, b, c, Some(name))
          case other => other
        val newEnv = env.define(name, named)
        (SchemeVoid, newEnv, out)
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
  ): (SchemeValue, Environment, String) =
    args match
      case cond :: thenBranch :: elseBranch =>
        val (condVal, _, condOut) = eval(cond, env)
        val isFalse = condVal match
          case SchemeBool(false) => true
          case _                 => false
        if isFalse then
          elseBranch match
            case elseExpr :: Nil =>
              val (v, _, o) = eval(elseExpr, env)
              (v, env, condOut + o)
            case Nil => (SchemeVoid, env, condOut)
            case _ =>
              throw new EvalError("if: too many arguments")
        else
          val (v, _, o) = eval(thenBranch, env)
          (v, env, condOut + o)
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
  ): (SchemeValue, String) =
    args match
      case Nil => (SchemeBool(true), "")
      case last :: Nil =>
        val (v, _, o) = eval(last, env)
        (v, o)
      case head :: tail =>
        val (v, _, o) = eval(head, env)
        v match
          case SchemeBool(false) => (SchemeBool(false), o)
          case _ =>
            val (result, tailOut) = evalAnd(tail, env)
            (result, o + tailOut)

  private def evalOr(
    args: List[SchemeValue],
    env: Environment
  ): (SchemeValue, String) =
    args match
      case Nil => (SchemeBool(false), "")
      case last :: Nil =>
        val (v, _, o) = eval(last, env)
        (v, o)
      case head :: tail =>
        val (result, _, o) = eval(head, env)
        result match
          case SchemeBool(false) =>
            val (v, tailOut) = evalOr(tail, env)
            (v, o + tailOut)
          case _ => (result, o)

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
  ): (SchemeValue, Environment, String) =
    args match
      case SchemeList(bindings) :: body if body.nonEmpty =>
        val (letEnv, bindOut) = bindings.foldLeft((env, "")) {
          case ((acc, accOut), SchemeList(List(SchemeSymbol(name), expr))) =>
            val (v, _, o) = eval(expr, env)
            (acc.define(name, v), accOut + o)
          case _ => throw new EvalError("bad let binding")
        }
        val (v, bodyOut) = evalBodyWithEnv(body, letEnv)
        (v, env, bindOut + bodyOut)
      case _ => throw new EvalError("bad let syntax")

  private def evalBegin(
    exprs: List[SchemeValue],
    env: Environment,
    accOut: String
  ): (SchemeValue, Environment, String) =
    exprs match
      case Nil => (SchemeVoid, env, accOut)
      case last :: Nil =>
        val (v, e, o) = eval(last, env)
        (v, e, accOut + o)
      case head :: tail =>
        val (_, newEnv, o) = eval(head, env)
        evalBegin(tail, newEnv, accOut + o)

  @tailrec
  private def evalCond(
    clauses: List[SchemeValue],
    env: Environment,
    accOut: String
  ): (SchemeValue, Environment, String) =
    clauses match
      case Nil => (SchemeVoid, env, accOut)
      case SchemeList(SchemeSymbol("else") :: body) :: _ =>
        val (v, o) = evalBody(body, env)
        (v, env, accOut + o)
      case SchemeList(test :: body) :: rest =>
        val (testVal, _, testOut) = eval(test, env)
        testVal match
          case SchemeBool(false) =>
            evalCond(rest, env, accOut + testOut)
          case _ =>
            if body.isEmpty then (testVal, env, accOut + testOut)
            else
              val (v, o) = evalBody(body, env)
              (v, env, accOut + testOut + o)
      case _ => throw new EvalError("bad cond syntax")

  private def evalBodyWithEnv(
    body: List[SchemeValue],
    env: Environment
  ): (SchemeValue, String) =
    body match
      case Nil => (SchemeVoid, "")
      case last :: Nil =>
        val (v, _, o) = eval(last, env)
        (v, o)
      case head :: tail =>
        val (_, newEnv, o) = eval(head, env)
        val (v, tailOut)   = evalBodyWithEnv(tail, newEnv)
        (v, o + tailOut)

  def evalBody(
    body: List[SchemeValue],
    env: Environment
  ): (SchemeValue, String) =
    body match
      case Nil => (SchemeVoid, "")
      case last :: Nil =>
        val (v, _, o) = eval(last, env)
        (v, o)
      case head :: tail =>
        val (_, newEnv, o) = eval(head, env)
        val (v, tailOut)   = evalBody(tail, newEnv)
        (v, o + tailOut)

  private def evalArgs(
    args: List[SchemeValue],
    env: Environment
  ): (List[SchemeValue], String) =
    val (revVals, out) = args.foldLeft((List.empty[SchemeValue], "")) { case ((vals, accOut), arg) =>
      val (v, _, o) = eval(arg, env)
      (v :: vals, accOut + o)
    }
    (revVals.reverse, out)
