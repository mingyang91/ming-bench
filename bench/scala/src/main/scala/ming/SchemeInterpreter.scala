package ming

import SchemeModel.*
import SchemeRuntime.*

object SchemeInterpreter:

  def evalToString(input: String): String =
    val output = new StringBuilder
    SchemeRuntime.render(evalProgram(input, output))

  def evalToStringWithOutput(input: String): (String, String) =
    val output = new StringBuilder
    val result = evalProgram(input, output)
    (SchemeRuntime.render(result), output.result())

  private[ming] def applyProcedure(procedure: Value, args: List[Value]): Value =
    SchemeEvaluator.applyProcedure(procedure, args)

  private def evalProgram(input: String, output: StringBuilder): Value =
    val expressions = SchemeParser.parseProgram(input)
    if expressions.isEmpty then throw new EvalError("empty program")
    SchemeEvaluator.evalSequence(expressions, baseEnv(output))
