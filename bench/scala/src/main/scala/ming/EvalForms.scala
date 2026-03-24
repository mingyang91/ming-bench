package ming

import scala.collection.mutable

object EvalForms:

  def evalCase(
    keyVal: SchemeVal,
    clauses: List[Expr],
    env: Env
  ): SchemeVal =
    clauses match
      case Nil => SchemeVal.Void
      case Expr.SList(Expr.Symbol("else") :: body) :: _ =>
        Evaluator.evalBody(body, env)
      case Expr.SList(Expr.SList(datums) :: body) :: rest =>
        val matched = datums.exists(d => Equality.eqvCheck(keyVal, Evaluator.quoteToVal(d)))
        if matched then Evaluator.evalBody(body, env)
        else evalCase(keyVal, rest, env)
      case _ => throw new EvalError("case: invalid clause")

  def evalDo(
    varClauses: List[Expr],
    testAndResult: List[Expr],
    bodyExprs: List[Expr],
    env: Env
  ): SchemeVal =
    val parsed = varClauses.map {
      case Expr.SList(Expr.Symbol(name) :: init :: step :: Nil) => (name, init, Some(step))
      case Expr.SList(Expr.Symbol(name) :: init :: Nil)         => (name, init, None)
      case _                                                    => throw new EvalError("do: invalid variable clause")
    }
    val test        = testAndResult.head
    val resultExprs = testAndResult.tail

    val doEnv = new Env(mutable.Map.empty, Some(env))
    for (name, init, _) <- parsed do doEnv.define(name, Evaluator.eval(init, env))

    while !Evaluator.isTruthy(Evaluator.eval(test, doEnv)) do
      for e <- bodyExprs do Evaluator.eval(e, doEnv)
      val newVals = parsed.map { (name, _, step) =>
        step match
          case Some(s) => Some(Evaluator.eval(s, doEnv))
          case None    => None
      }
      parsed.zip(newVals).foreach { case ((name, _, _), newVal) =>
        newVal.foreach(v => doEnv.define(name, v))
      }

    if resultExprs.isEmpty then SchemeVal.Void
    else Evaluator.evalBody(resultExprs, doEnv)
