package ming

import SchemeValue.*
import Interpreter.EvalResult
import Interpreter.EvalResult.*

/** Iteration-related special forms (do). */
object IterationForms:

  /** Format an error message with optional position info. */
  private def posMsg(msg: String, pos: Option[SourcePos]): String =
    pos match
      case Some(p) => s"$msg [$p]"
      case None    => msg

  def evalDo(
    args: List[SchemeValue],
    pos: Option[SourcePos],
    env: Environment
  ): EvalResult =
    args match
      case ListVal(varSpecs, _) :: ListVal(testAndExprs, _) :: body =>
        if testAndExprs.isEmpty then throw new EvalError(posMsg("do: missing test", pos))
        val test        = testAndExprs.head
        val resultExprs = testAndExprs.tail
        // Parse variable specs: (var init [step])
        val specs = varSpecs.map {
          case ListVal(SymbolVal(name, _) :: init :: step :: Nil, _) => (name, init, Some(step))
          case ListVal(SymbolVal(name, _) :: init :: Nil, _)         => (name, init, None)
          case _ => throw new EvalError(posMsg("do: bad variable spec", pos))
        }
        // Initialize
        val doEnv = env.child()
        specs.foreach { (name, init, _) =>
          doEnv.define(name, Interpreter.eval(init, env))
        }
        // Iterate
        while true do
          val testResult = Interpreter.eval(test, doEnv)
          if testResult.isTruthy then
            if resultExprs.isEmpty then return Done(Void)
            Interpreter.evalBodyInit(resultExprs, doEnv)
            return TailCall(resultExprs.last, doEnv)
          // Execute body
          body.foreach(expr => Interpreter.eval(expr, doEnv))
          // Parallel step: evaluate all steps with OLD values, then update
          val newVals = specs.map { (name, _, stepOpt) =>
            stepOpt match
              case Some(step) => Some(Interpreter.eval(step, doEnv))
              case None       => None
          }
          specs.zip(newVals).foreach { case ((name, _, _), valOpt) =>
            valOpt.foreach(v => doEnv.define(name, v))
          }
        Done(Void) // unreachable
      case _ => throw new EvalError(posMsg("do: bad syntax", pos))
