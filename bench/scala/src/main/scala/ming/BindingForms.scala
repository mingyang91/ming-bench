package ming

/** Binding and iteration special forms extracted from Evaluator. */
object BindingForms:

  def evalLet(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      // Named let: (let name ((var init) ...) body ...)
      case SchemeVal.SSymbol(name) :: SchemeVal.SList(bindings) :: body if body.nonEmpty =>
        val paramNames = bindings.map {
          case SchemeVal.SList(SchemeVal.SSymbol(n) :: _ :: Nil) => n
          case _                                                 => throw new EvalError("let: bad binding")
        }
        val initVals = bindings.map {
          case SchemeVal.SList(_ :: initExpr :: Nil) => Evaluator.eval(initExpr, env)
          case _                                     => throw new EvalError("let: bad binding")
        }
        val letEnv = Env(Some(env))
        letEnv.define(
          name,
          SchemeVal.SLambda(paramNames, None, body, letEnv)
        )
        paramNames.zip(initVals).foreach((p, v) => letEnv.define(p, v))
        Evaluator.evalBody(body, letEnv)
      // Regular let: (let ((var init) ...) body ...)
      case SchemeVal.SList(bindings) :: body if body.nonEmpty =>
        val pairs = bindings.map {
          case SchemeVal.SList(SchemeVal.SSymbol(n) :: initExpr :: Nil) =>
            (n, Evaluator.eval(initExpr, env))
          case _ => throw new EvalError("let: bad binding")
        }
        val letEnv = Env(Some(env))
        pairs.foreach((n, v) => letEnv.define(n, v))
        Evaluator.evalBody(body, letEnv)
      case _ => throw new EvalError("let: bad syntax")

  def evalLetStar(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.SList(bindings) :: body if body.nonEmpty =>
        val letEnv = Env(Some(env))
        bindings.foreach {
          case SchemeVal.SList(SchemeVal.SSymbol(n) :: initExpr :: Nil) =>
            letEnv.define(n, Evaluator.eval(initExpr, letEnv))
          case _ => throw new EvalError("let*: bad binding")
        }
        Evaluator.evalBody(body, letEnv)
      case _ => throw new EvalError("let*: bad syntax")

  def evalLetrec(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.SList(bindings) :: body if body.nonEmpty =>
        val letEnv = Env(Some(env))
        val names = bindings.map {
          case SchemeVal.SList(SchemeVal.SSymbol(n) :: _ :: Nil) => n
          case _                                                 => throw new EvalError("letrec: bad binding")
        }
        names.foreach(n => letEnv.define(n, SchemeVal.SVoid))
        bindings.foreach {
          case SchemeVal.SList(SchemeVal.SSymbol(n) :: initExpr :: Nil) =>
            letEnv.set(n, Evaluator.eval(initExpr, letEnv))
          case _ => throw new EvalError("letrec: bad binding")
        }
        Evaluator.evalBody(body, letEnv)
      case _ => throw new EvalError("letrec: bad syntax")

  def evalLetrecStar(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.SList(bindings) :: body if body.nonEmpty =>
        val letEnv = Env(Some(env))
        bindings.foreach {
          case SchemeVal.SList(SchemeVal.SSymbol(n) :: initExpr :: Nil) =>
            letEnv.define(n, Evaluator.eval(initExpr, letEnv))
          case _ => throw new EvalError("letrec*: bad binding")
        }
        Evaluator.evalBody(body, letEnv)
      case _ => throw new EvalError("letrec*: bad syntax")

  def evalCaseLambda(args: List[SchemeVal], env: Env): SchemeVal =
    val clauses = args.map {
      case SchemeVal.SList(SchemeVal.SList(params) :: body) if body.nonEmpty =>
        val (paramNames, restParam) = Evaluator.parseParams(params)
        (paramNames, restParam, body)
      case SchemeVal.SList(SchemeVal.SSymbol(rest) :: body) if body.nonEmpty =>
        (Nil, Some(rest), body)
      case SchemeVal.SList(SchemeVal.SList(Nil) :: body) if body.nonEmpty =>
        (Nil, None, body)
      case _ => throw new EvalError("case-lambda: bad clause")
    }
    SchemeVal.SCaseLambda(clauses, env)

  def evalCase(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case keyExpr :: clauses if clauses.nonEmpty =>
        val key = Evaluator.eval(keyExpr, env)
        evalCaseClauses(key, clauses, env)
      case _ => throw new EvalError("case: bad syntax")

  private def evalCaseClauses(key: SchemeVal, clauses: List[SchemeVal], env: Env): SchemeVal =
    clauses match
      case Nil => SchemeVal.SVoid
      case clause :: rest =>
        clause match
          case SchemeVal.SList(SchemeVal.SSymbol("else") :: body) =>
            Evaluator.evalBody(body, env)
          case SchemeVal.SList(SchemeVal.SList(datums) :: body) =>
            if datums.exists(d => Builtins.schemeEqv(key, d)) then
              if body.isEmpty then SchemeVal.SVoid
              else Evaluator.evalBody(body, env)
            else evalCaseClauses(key, rest, env)
          case _ => throw new EvalError("case: bad clause")

  def evalDo(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.SList(varClauses) :: SchemeVal.SList(testAndExprs) :: body =>
        val doEnv = Env(Some(env))
        val vars = varClauses.map {
          case SchemeVal.SList(SchemeVal.SSymbol(name) :: init :: step :: Nil) =>
            (name, init, Some(step))
          case SchemeVal.SList(SchemeVal.SSymbol(name) :: init :: Nil) =>
            (name, init, None)
          case _ => throw new EvalError("do: bad variable clause")
        }
        val initVals = vars.map((_, init, _) => Evaluator.eval(init, env))
        vars.zip(initVals).foreach { case ((name, _, _), v) => doEnv.define(name, v) }
        val test        = testAndExprs.headOption.getOrElse(throw new EvalError("do: missing test"))
        val resultExprs = testAndExprs.tail
        while !Evaluator.isTruthy(Evaluator.eval(test, doEnv)) do
          body.foreach(Evaluator.eval(_, doEnv))
          val stepVals = vars.map {
            case (_, _, Some(step)) => Some(Evaluator.eval(step, doEnv))
            case (_, _, None)       => None
          }
          vars.zip(stepVals).foreach {
            case ((name, _, _), Some(v)) => doEnv.set(name, v)
            case _                       => ()
          }
        if resultExprs.isEmpty then SchemeVal.SVoid
        else Evaluator.evalBody(resultExprs, doEnv)
      case _ => throw new EvalError("do: bad syntax")
