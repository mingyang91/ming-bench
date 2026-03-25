package ming

object SpecialForms:
  import Builtins.isFalsy

  private[ming] def isMacro(name: String, env: Env): Boolean =
    env.lookupOpt(name).exists(v => v.isInstanceOf[Expr.Macro] || v.isInstanceOf[Expr.TransformerMacro])

  private[ming] def lookupMacro(name: String, env: Env): Expr.Macro =
    env.lookup(name) match
      case mac: Expr.Macro => mac
      case other           => throw EvalError(s"$name is not a macro")

  private[ming] def evalDefine(args: List[Expr], env: Env): Expr = args match
    case Expr.Sym(name) :: value :: Nil =>
      env.define(name, Evaluator.eval(value, env))
      Expr.Bool(false)
    case Expr.Lst(Expr.Sym(name) :: params) :: body if body.nonEmpty =>
      val (paramNames, restParam) = ParamUtils.extractParamsWithRest("define", params)
      env.define(name, Expr.Lambda(paramNames, restParam, body, env))
      Expr.Bool(false)
    case _ => throw EvalError("define: invalid syntax")

  private[ming] def evalSet(args: List[Expr], env: Env): Expr = args match
    case Expr.Sym(name) :: value :: Nil =>
      env.set(name, Evaluator.eval(value, env))
      Expr.Bool(false)
    case _ => throw EvalError("set!: invalid syntax")

  private[ming] def evalLambda(args: List[Expr], env: Env): Expr = args match
    case Expr.Lst(params) :: body if body.nonEmpty =>
      val (paramNames, restParam) = ParamUtils.extractParamsWithRest("lambda", params)
      Expr.Lambda(paramNames, restParam, body, env)
    case Expr.Sym(restName) :: body if body.nonEmpty =>
      Expr.Lambda(Nil, Some(restName), body, env)
    case _ => throw EvalError("lambda: invalid syntax")

  private[ming] def evalDefineSyntax(args: List[Expr], env: Env): Expr = args match
    case Expr.Sym(name) :: Expr.Lst(
          Expr.Sym("syntax-rules") :: Expr.Lst(literals) :: rules
        ) :: Nil =>
      val litNames = literals.map {
        case Expr.Sym(s) => s
        case _           => throw EvalError("define-syntax: literals must be symbols")
      }
      val rulesList = rules.map {
        case Expr.Lst(List(Expr.Lst(_ :: patternArgs), template)) =>
          (patternArgs, template)
        case _ => throw EvalError("define-syntax: invalid rule")
      }
      env.define(name, Expr.Macro(litNames, rulesList, env))
      Expr.Bool(false)
    case Expr.Sym(name) :: transformerExpr :: Nil =>
      val transformer = Evaluator.eval(transformerExpr, env)
      env.define(name, Expr.TransformerMacro(transformer, env))
      Expr.Bool(false)
    case _ => throw EvalError("define-syntax: invalid syntax")

  private[ming] def evalCaseLambda(clauses: List[Expr], env: Env): Expr =
    val parsed = clauses.map {
      case Expr.Lst(Expr.Lst(params) :: body) if body.nonEmpty =>
        val (paramNames, restParam) =
          ParamUtils.extractParamsWithRest("case-lambda", params)
        (paramNames, restParam, body)
      case _ => throw EvalError("case-lambda: invalid clause")
    }
    Expr.CaseLambda(parsed, env)

  private[ming] def evalDo(args: List[Expr], env: Env): Expr = args match
    case Expr.Lst(varSpecs) :: Expr.Lst(testAndExprs) :: body =>
      if testAndExprs.isEmpty then throw EvalError("do: need test expression")
      val test        = testAndExprs.head
      val resultExprs = testAndExprs.tail
      val vars        = parseDoVarSpecs(varSpecs)
      val doEnv       = env.child()
      for (name, init, _) <- vars do doEnv.define(name, Evaluator.eval(init, env))
      while true do
        val testResult = Evaluator.eval(test, doEnv)
        if !isFalsy(testResult) then
          if resultExprs.isEmpty then return testResult
          else return Evaluator.evalBody(resultExprs, doEnv)
        for expr <- body do Evaluator.eval(expr, doEnv)
        val newVals = vars.map {
          case (_, _, Some(step)) => Some(Evaluator.eval(step, doEnv))
          case (_, _, None)       => None
        }
        for ((name, _, _), newVal) <- vars.zip(newVals) do newVal.foreach(v => doEnv.define(name, v))
      Expr.Bool(false) // unreachable
    case _ => throw EvalError("do: invalid syntax")

  private def parseDoVarSpecs(
    varSpecs: List[Expr]
  ): List[(String, Expr, Option[Expr])] =
    varSpecs.map {
      case Expr.Lst(Expr.Sym(name) :: init :: step :: Nil) =>
        (name, init, Some(step))
      case Expr.Lst(Expr.Sym(name) :: init :: Nil) => (name, init, None)
      case _                                       => throw EvalError("do: invalid variable spec")
    }
