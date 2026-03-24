package ming

import Evaluator.*
import Evaluator.Val.*

/** Public API and non-CPS wrappers — delegates to the CPS evaluator core. */
object Interpreter:

  private[ming] def eval(expr: Val, env: Env): Val =
    Evaluator.trampoline(Evaluator.evalK(expr, env, v => BDone(v)))

  /** Non-CPS function application — used by builtins (map, for-each, etc.). */
  private[ming] def applyFunc(func: Val, args: List[Val]): Val = func match
    case _: ContinuationVal =>
      throw new ContinuationJump(Evaluator.applyK(func, args, v => BDone(v)))
    case Builtin(f) => Evaluator.applyBuiltinChecked(f, args)
    case _          => Evaluator.trampoline(Evaluator.applyK(func, args, v => BDone(v)))

  private def defaultEnv(): Env =
    val env = Env.empty()
    Builtins.all.foreach((name, v) => env.define(name, v))
    env.define("call/cc", CallCCVal)
    env.define("call-with-current-continuation", CallCCVal)
    env

  private def runProgram(input: String): Val =
    Evaluator.windingStack = List.empty
    Evaluator.raiseHandlers = List.empty
    val parser = new Parser(input)
    val exprs  = parser.parseAllWithPositions()
    if exprs.isEmpty then throw new EvalError("no expressions")
    val env = defaultEnv()
    def evalTopLevel(remaining: List[(Val, Int, Int)], k: Cont): Bounce =
      remaining match
        case scala.Nil => k(Void)
        case (expr, line, col) :: scala.Nil =>
          Evaluator.lastPos = s"$line:$col"
          Evaluator.evalK(expr, env, k)
        case (expr, line, col) :: rest =>
          Evaluator.lastPos = s"$line:$col"
          Evaluator.evalK(expr, env, _ => BMore(() => evalTopLevel(rest, k)))
    Evaluator.trampoline(evalTopLevel(exprs, v => BDone(v)))

  def evalStr(input: String): String =
    Display.write(runProgram(input))

  def evalStrWithOutput(input: String): (String, String) =
    Evaluator.outputBuffer.clear()
    val result = runProgram(input)
    val output = Evaluator.outputBuffer.toString
    Evaluator.outputBuffer.clear()
    (Display.write(result), output)
