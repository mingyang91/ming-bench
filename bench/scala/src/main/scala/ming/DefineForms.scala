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
      case (nameAndParams @ SchemeVal.SPair(_)) :: body if body.nonEmpty =>
        val (name, paramNames, restParam) = parseNameAndParams(nameAndParams)
        env.define(name, SchemeVal.SLambda(paramNames, restParam, body, env))
        SchemeVal.SVoid
      case _ => throw new EvalError("define: bad syntax")

  /** Extract (name, paramNames, restParam) from a dotted-pair name+params form like SPair(f, SPair(a, rest)). */
  def parseNameAndParams(v: SchemeVal): (String, List[String], Option[String]) =
    val (elems, tail) = flattenDottedList(v)
    elems match
      case SchemeVal.SSymbol(name) :: params =>
        val paramNames = params.map {
          case SchemeVal.SSymbol(n) => n
          case other => throw new EvalError(s"expected parameter name, got ${other.display}")
        }
        val restParam = tail.map {
          case SchemeVal.SSymbol(n) => n
          case other => throw new EvalError(s"expected parameter name, got ${other.display}")
        }
        (name, paramNames, restParam)
      case _ => throw new EvalError("define: bad syntax")

  /** Flatten a potentially dotted list into (proper elements, optional tail). */
  private[ming] def flattenDottedList(v: SchemeVal): (List[SchemeVal], Option[SchemeVal]) =
    v match
      case SchemeVal.SList(elems) => (elems, None)
      case SchemeVal.SPair(cell) =>
        val (rest, tail) = flattenDottedList(cell.cdr)
        (cell.car :: rest, tail)
      case other => (Nil, Some(other))

  def parseParams(
    params: List[SchemeVal]
  ): (List[String], Option[String]) =
    val names = params.map {
      case SchemeVal.SSymbol(n) => n
      case other =>
        throw new EvalError(s"expected parameter name, got ${other.display}")
    }
    (names, None)

  /** Parse params from either a proper list (SList) or a dotted pair (SPair). */
  def parseParamsFromVal(v: SchemeVal): (List[String], Option[String]) =
    v match
      case SchemeVal.SList(params) => parseParams(params)
      case SchemeVal.SSymbol(rest) => (Nil, Some(rest))
      case _ =>
        val (elems, tail) = flattenDottedList(v)
        val paramNames = elems.map {
          case SchemeVal.SSymbol(n) => n
          case other => throw new EvalError(s"expected parameter name, got ${other.display}")
        }
        val restParam = tail.map {
          case SchemeVal.SSymbol(n) => n
          case other => throw new EvalError(s"expected parameter name, got ${other.display}")
        }
        (paramNames, restParam)

  def evalLambda(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case paramSpec :: body if body.nonEmpty =>
        val (paramNames, restParam) = parseParamsFromVal(paramSpec)
        SchemeVal.SLambda(paramNames, restParam, body, env)
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
