package ming

import Evaluator.{Done, EvalResult}
import scala.annotation.tailrec

/** Exception handling: raise, guard, with-exception-handler (L17). */
object ExceptionHandling:

  def evalRaise(
    args: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    args match
      case expr :: Nil =>
        val (v, _, out2) = Evaluator.eval(expr, env, out)
        throw new SchemeRaised(v, out2)
      case _ =>
        throw new EvalError("raise requires exactly 1 argument")

  def evalGuard(
    args: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    args match
      case Value.PairVal(Value.Symbol(varName, _), clausesPair, _) :: body =>
        val clauses = Evaluator.toList(clausesPair)
        try
          val (v, _, out2) = evalBodyFull(body, env, out)
          Done(v, env, out2)
        catch
          case raised: SchemeRaised =>
            val guardEnv = env.define(varName, raised.value)
            evalGuardClauses(clauses, raised.value, guardEnv, raised.output)
      case _ =>
        throw new EvalError("bad guard syntax")

  def evalWithExceptionHandler(
    args: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    args match
      case handlerExpr :: thunkExpr :: Nil =>
        val (handler, _, out1) = Evaluator.eval(handlerExpr, env, out)
        val (thunk, _, out2)   = Evaluator.eval(thunkExpr, env, out1)
        try
          val (v, e, out3) = DynamicWind.callThunk(thunk, out2)
          Done(v, e, out3)
        catch
          case raised: SchemeRaised =>
            Evaluator.applyProcTail(
              handler,
              List(raised.value),
              None,
              raised.output
            )
      case _ =>
        throw new EvalError(
          "with-exception-handler requires exactly 2 arguments"
        )

  @tailrec
  private def evalGuardClauses(
    clauses: List[Value],
    raisedValue: Value,
    env: Env,
    out: String
  ): EvalResult =
    clauses match
      case Nil =>
        throw new SchemeRaised(raisedValue, out)
      case clause :: rest =>
        val parts = Evaluator.toList(clause)
        parts match
          case Value.Symbol("else", _) :: exprs =>
            Evaluator.evalBodyTail(exprs, env, out)
          case test :: exprs =>
            val (testVal, _, out2) = Evaluator.eval(test, env, out)
            if !Evaluator.isFalsy(testVal) then
              if exprs.isEmpty then Done(testVal, env, out2)
              else Evaluator.evalBodyTail(exprs, env, out2)
            else evalGuardClauses(rest, raisedValue, env, out2)
          case _ =>
            throw new EvalError("bad guard clause")

  private def evalBodyFull(
    body: List[Value],
    env: Env,
    out: String
  ): (Value, Env, String) =
    body match
      case Nil         => (Value.VoidVal, env, out)
      case last :: Nil => Evaluator.eval(last, env, out)
      case head :: tail =>
        val (_, _, out2) = Evaluator.eval(head, env, out)
        evalBodyFull(tail, env, out2)
