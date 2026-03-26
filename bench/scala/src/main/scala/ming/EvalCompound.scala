package ming

import SchemeTypes.{errAt, isTruthy, Env, Pos, Value}

/** Compound special forms extracted from Evaluator: letrec, letrec*, case, do. */
private[ming] object EvalCompound:

  def evalLetrec(
    rest: List[Expr],
    env: Env,
    pos: Pos,
    eval: (Expr, Env) => Value,
    evalBody: (List[Expr], Env) => Value
  ): Value = rest match
    case Expr.SList(bindings, _) :: body if body.nonEmpty =>
      val letEnv = env.child()
      val names = bindings.map {
        case Expr.SList(Expr.Symbol(name, _) :: _ :: Nil, _) => name
        case _                                               => throw errAt(pos, "invalid letrec binding")
      }
      names.foreach(n => letEnv.define(n, Value.VVoid))
      bindings.foreach {
        case Expr.SList(Expr.Symbol(name, _) :: initExpr :: Nil, _) =>
          letEnv.define(name, eval(initExpr, letEnv))
        case _ => throw errAt(pos, "invalid letrec binding")
      }
      evalBody(body, letEnv)
    case _ => throw errAt(pos, "invalid letrec")

  def evalLetrecStar(
    rest: List[Expr],
    env: Env,
    pos: Pos,
    eval: (Expr, Env) => Value,
    evalBody: (List[Expr], Env) => Value
  ): Value = rest match
    case Expr.SList(bindings, _) :: body if body.nonEmpty =>
      val letEnv = env.child()
      bindings.foreach {
        case Expr.SList(Expr.Symbol(name, _) :: initExpr :: Nil, _) =>
          letEnv.define(name, eval(initExpr, letEnv))
        case _ => throw errAt(pos, "invalid letrec* binding")
      }
      evalBody(body, letEnv)
    case _ => throw errAt(pos, "invalid letrec*")

  def evalCase(
    rest: List[Expr],
    env: Env,
    pos: Pos,
    eval: (Expr, Env) => Value,
    evalBody: (List[Expr], Env) => Value,
    posOf: Expr => Pos
  ): Value = rest match
    case keyExpr :: clauses =>
      val key = eval(keyExpr, env)
      def matchClauses(cls: List[Expr]): Value = cls match
        case Nil => Value.VVoid
        case Expr.SList(Expr.Symbol("else", _) :: body, _) :: _ =>
          evalBody(body, env)
        case Expr.SList(Expr.SList(datums, _) :: body, _) :: rest =>
          val matched = datums.exists { d =>
            ListUtilBuiltins.eqvCheck(key, EvalForms.quoteToValue(d))
          }
          if matched then evalBody(body, env)
          else matchClauses(rest)
        case e :: _ => throw errAt(posOf(e), "invalid case clause")
      matchClauses(clauses)
    case _ => throw errAt(pos, "invalid case")

  def evalDo(
    rest: List[Expr],
    env: Env,
    pos: Pos,
    eval: (Expr, Env) => Value,
    evalBody: (List[Expr], Env) => Value,
    posOf: Expr => Pos
  ): Value = rest match
    case Expr.SList(varClauses, _) :: Expr.SList(testAndExprs, _) :: body =>
      if testAndExprs.isEmpty then throw errAt(pos, "invalid do: empty test clause")
      val testExpr    = testAndExprs.head
      val resultExprs = testAndExprs.tail
      val vars = varClauses.map {
        case Expr.SList(Expr.Symbol(name, _) :: init :: step :: Nil, _) =>
          (name, init, Some(step))
        case Expr.SList(Expr.Symbol(name, _) :: init :: Nil, _) =>
          (name, init, None)
        case e => throw errAt(posOf(e), "invalid do variable clause")
      }
      val doEnv = env.child()
      vars.foreach { (name, init, _) =>
        doEnv.define(name, eval(init, env))
      }
      while !isTruthy(eval(testExpr, doEnv)) do
        body.foreach(e => eval(e, doEnv))
        val newVals = vars.map { (name, _, step) =>
          step match
            case Some(s) => Some(eval(s, doEnv))
            case None    => None
        }
        vars.zip(newVals).foreach { case ((name, _, _), newVal) =>
          newVal.foreach(v => doEnv.define(name, v))
        }
      if resultExprs.isEmpty then Value.VVoid
      else evalBody(resultExprs, doEnv)
    case _ => throw errAt(pos, "invalid do")
