package ming

/** Define, lambda, set!, and define-syntax special forms. */
object DefineForms:

  def evalDefine(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.SSymbol(name) :: valueExpr :: Nil =>
        env.define(name, Evaluator.eval(valueExpr, env))
        SchemeVal.SVoid
      case SchemeVal.SList(SchemeVal.SSymbol(name) :: params) :: body if body.nonEmpty =>
        val (paramNames, restParam) = parseParams(params)
        env.define(name, SchemeVal.SLambda(paramNames, restParam, body, env))
        SchemeVal.SVoid
      case _ => throw new EvalError("define: bad syntax")

  def parseParams(
    params: List[SchemeVal]
  ): (List[String], Option[String]) =
    val dotIdx = params.indexWhere(_ == SchemeVal.SSymbol("."))
    if dotIdx >= 0 then
      if dotIdx != params.length - 2 then throw new EvalError("bad dot syntax in parameter list")
      val fixed = params.take(dotIdx).map {
        case SchemeVal.SSymbol(n) => n
        case other =>
          throw new EvalError(s"expected parameter name, got ${other.display}")
      }
      params(dotIdx + 1) match
        case SchemeVal.SSymbol(rest) => (fixed, Some(rest))
        case other =>
          throw new EvalError(s"expected parameter name, got ${other.display}")
    else
      val names = params.map {
        case SchemeVal.SSymbol(n) => n
        case other =>
          throw new EvalError(s"expected parameter name, got ${other.display}")
      }
      (names, None)

  def evalLambda(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.SList(params) :: body if body.nonEmpty =>
        val (paramNames, restParam) = parseParams(params)
        SchemeVal.SLambda(paramNames, restParam, body, env)
      case SchemeVal.SSymbol(rest) :: body if body.nonEmpty =>
        SchemeVal.SLambda(Nil, Some(rest), body, env)
      case _ => throw new EvalError("lambda: bad syntax")

  def evalSet(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.SSymbol(name) :: valueExpr :: Nil =>
        env.set(name, Evaluator.eval(valueExpr, env))
        SchemeVal.SVoid
      case _ => throw new EvalError("set!: bad syntax")

  def evalDefineSyntax(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.SSymbol(name) :: SchemeVal.SList(
            SchemeVal.SSymbol("syntax-rules") :: SchemeVal.SList(literals) :: clauses
          ) :: Nil =>
        val litNames = literals.collect { case SchemeVal.SSymbol(n) => n }.toSet
        val parsedClauses = clauses.map {
          case SchemeVal.SList(pattern :: template :: Nil) => (pattern, template)
          case other =>
            throw new EvalError(s"syntax-rules: bad clause ${other.display}")
        }
        env.define(name, SchemeVal.SMacro(litNames, parsedClauses, env))
        SchemeVal.SVoid
      case SchemeVal.SSymbol(name) :: transformerExpr :: Nil =>
        val bound       = env.definedNames
        val transformer = Evaluator.eval(transformerExpr, env)
        env.define(name, SchemeVal.STransformerMacro(transformer, env, bound))
        SchemeVal.SVoid
      case _ => throw new EvalError("define-syntax: bad syntax")
