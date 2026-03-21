package ming

import Evaluator.{Bounce, Done, EvalResult, GuardBounce}
import scala.annotation.tailrec

/** Exception handling: raise, guard, with-exception-handler (L17). */
object ExceptionHandling:

  case class GuardHandler(
    clauses: List[Value],
    varName: String,
    env: Env
  )

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
        GuardBounce(
          Evaluator.evalBodyTail(body, env, out),
          clauses,
          varName,
          env
        )
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

  /** Run a guard body with a single try/catch and @tailrec inner loop.
    *
    * Tail-recursive guard calls produce GuardBounce results that are absorbed into the same loop, avoiding per-guard
    * JVM stack frames.
    */
  private[ming] def guardLoop(
    initial: GuardBounce
  ): (Value, Env, String) =
    val handlers = Array(
      List(GuardHandler(initial.clauses, initial.varName, initial.guardEnv))
    )
    try
      @tailrec
      def loop(result: EvalResult): (Value, Env, String) =
        result match
          case Done(v, e, o) => (v, e, o)
          case Bounce(e2, env2, o) =>
            loop(Evaluator.evalStep(e2, env2, o))
          case gb: GuardBounce =>
            handlers(0) = GuardHandler(gb.clauses, gb.varName, gb.guardEnv) :: handlers(0)
            loop(gb.bodyResult)
      loop(initial.bodyResult)
    catch
      case raised: SchemeRaised =>
        handleGuardCatch(raised, handlers(0))

  private[ming] def handleGuardCatch(
    raised: SchemeRaised,
    handlers: List[GuardHandler]
  ): (Value, Env, String) =
    handlers match
      case Nil => throw raised
      case h :: rest =>
        val guardEnv = h.env.define(h.varName, raised.value)
        try
          evalGuardClauses(
            h.clauses,
            raised.value,
            guardEnv,
            raised.output
          ) match
            case Done(v, e, o)       => (v, e, o)
            case Bounce(e2, env2, o) => Evaluator.eval(e2, env2, o)
            case gb: GuardBounce     => guardLoop(gb)
        catch case reRaised: SchemeRaised => handleGuardCatch(reRaised, rest)

  @tailrec
  private[ming] def evalGuardClauses(
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
