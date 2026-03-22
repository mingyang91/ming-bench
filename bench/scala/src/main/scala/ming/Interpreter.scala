package ming

import scala.annotation.tailrec

/** Core evaluation logic for the Scheme interpreter. */
object Interpreter:

  import SchemeValue.*

  private val builtinNames: Set[String] =
    Set("+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not")

  val defaultEnv: Environment =
    builtinNames.foldLeft(Environment.empty) { (env, name) =>
      env.define(name, SchemeSymbol(name))
    }

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
      case SchemeSymbol(name) =>
        env.lookup(name) match
          case Some(v) => (v, env)
          case None =>
            throw new EvalError(s"unbound variable: $name")
      case SchemeList(elements) => evalList(elements, env)

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

  def evalBody(
    body: List[SchemeValue],
    env: Environment
  ): SchemeValue =
    body match
      case Nil         => SchemeVoid
      case last :: Nil => eval(last, env)._1
      case head :: tail =>
        eval(head, env)
        evalBody(tail, env)

  def applyProc(
    func: SchemeValue,
    args: List[SchemeValue]
  ): SchemeValue =
    func match
      case SchemeSymbol(name) => applyNamedBuiltin(name, args)
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

  private def applyNamedBuiltin(
    name: String,
    args: List[SchemeValue]
  ): SchemeValue = name match
    case "+"   => arithmeticOp(args, _ + _, 0)
    case "*"   => arithmeticOp(args, _ * _, 1)
    case "-"   => subtractOp(args)
    case "/"   => divideOp(args)
    case "<"   => comparisonOp(args, _ < _)
    case ">"   => comparisonOp(args, _ > _)
    case "="   => comparisonOp(args, _ == _)
    case "<="  => comparisonOp(args, _ <= _)
    case ">="  => comparisonOp(args, _ >= _)
    case "not" => evalNot(args)
    case _     => throw new EvalError(s"unbound variable: $name")

  private def requireInt(v: SchemeValue): Long = v match
    case SchemeInt(n) => n
    case _ =>
      throw new EvalError(s"expected number, got: ${v.display}")

  private def arithmeticOp(
    args: List[SchemeValue],
    op: (Long, Long) => Long,
    identity: Long
  ): SchemeValue =
    SchemeInt(
      args.foldLeft(identity)((acc, v) => op(acc, requireInt(v)))
    )

  private def subtractOp(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil          => throw new EvalError("- requires at least 1 argument")
      case List(single) => SchemeInt(-requireInt(single))
      case head :: tail =>
        val first = requireInt(head)
        SchemeInt(
          tail.foldLeft(first)((acc, v) => acc - requireInt(v))
        )

  private def divideOp(args: List[SchemeValue]): SchemeValue =
    args match
      case Nil =>
        throw new EvalError("/ requires at least 1 argument")
      case List(single) =>
        val n = requireInt(single)
        if n == 0 then throw new EvalError("division by zero")
        SchemeInt(1 / n)
      case head :: tail =>
        val first = requireInt(head)
        SchemeInt(tail.foldLeft(first) { (acc, v) =>
          val n = requireInt(v)
          if n == 0 then throw new EvalError("division by zero")
          acc / n
        })

  private def comparisonOp(
    args: List[SchemeValue],
    op: (Long, Long) => Boolean
  ): SchemeValue =
    args match
      case Nil | _ :: Nil =>
        throw new EvalError(
          "comparison requires at least 2 arguments"
        )
      case _ =>
        val nums = args.map(requireInt)
        SchemeBool(
          nums.zip(nums.tail).forall((a, b) => op(a, b))
        )

  private def evalNot(args: List[SchemeValue]): SchemeValue =
    args match
      case List(single) =>
        single match
          case SchemeBool(false) => SchemeBool(true)
          case _                 => SchemeBool(false)
      case _ =>
        throw new EvalError("not expects exactly 1 argument")
