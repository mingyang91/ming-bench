package ming

import scala.annotation.tailrec

/** Scheme interpreter entry point. Agents implement this object. */
object Evaluator:

  private val builtinNames: List[String] = List(
    "+",
    "-",
    "*",
    "/",
    "<",
    ">",
    "=",
    "<=",
    ">=",
    "not",
    "cons",
    "car",
    "cdr",
    "null?",
    "list",
    "length",
    "string?",
    "number?",
    "boolean?",
    "pair?",
    "symbol?",
    "char?",
    "display",
    "write",
    "newline",
    "string-append",
    "string-length",
    "substring",
    "string->number",
    "number->string",
    "symbol->string",
    "string->symbol",
    "string-ref",
    "string-copy",
    "string-set!",
    "string->list",
    "list->string",
    "char->integer",
    "integer->char",
    "map",
    "apply",
    "call/cc",
    "call-with-current-continuation",
    "equal?",
    "eqv?",
    "eq?",
    "vector",
    "make-vector",
    "vector-ref",
    "vector-set!",
    "vector-length",
    "vector?",
    "vector->list",
    "list->vector",
    "abs",
    "modulo",
    "remainder",
    "quotient",
    "min",
    "max",
    "expt",
    "zero?",
    "positive?",
    "negative?",
    "odd?",
    "even?",
    "list-ref",
    "list-tail",
    "list?",
    "assoc",
    "char-alphabetic?",
    "char-numeric?",
    "char-upcase",
    "char-downcase",
    "char=?",
    "char<?",
    "string=?",
    "string<?",
    "string-ci=?",
    "string-upcase",
    "string-downcase",
    "dynamic-wind",
    "reverse",
    "raise",
    "with-exception-handler",
    "values",
    "call-with-values",
    "exact?",
    "inexact?",
    "exact->inexact",
    "inexact->exact",
    "numerator",
    "denominator",
    "integer?",
    "rational?",
    "set-car!",
    "set-cdr!",
    "caar",
    "cadr",
    "cdar",
    "cddr",
    "syntax->datum",
    "datum->syntax"
  )

  private val builtinBindings: Map[String, Array[Value]] =
    builtinNames.map(n => n -> Array[Value](Value.Symbol(n))).toMap

  private def freshEnv(): Env = Env(
    builtinBindings,
    None,
    Some(Array(Map.empty[String, Value]))
  )

  /** Evaluate one or more Scheme expressions and return the string representation of the last result.
    */
  def evalStr(input: String): String =
    ContState.reset()
    val exprs = Parser.parseWithPos(input)
    if exprs.isEmpty then throw new EvalError("no expressions")
    val (result, _, _) = evalAllCC(exprs, freshEnv())
    result match
      case Value.Void => ""
      case v          => v.display

  /** Evaluate Scheme expressions and return both the result string and any captured output.
    */
  def evalStrWithOutput(input: String): (String, String) =
    ContState.reset()
    val exprs = Parser.parseWithPos(input)
    if exprs.isEmpty then throw new EvalError("no expressions")
    val (result, _, output) = evalAllCC(exprs, freshEnv())
    val resultStr = result match
      case Value.Void => ""
      case v          => v.display
    (resultStr, output)

  private def evalAllCC(
    exprs: List[(Value, Pos)],
    env: Env,
    last: Value = Value.Void,
    out: String = ""
  ): (Value, Env, String) =
    try evalAll(exprs, env, last, out)
    catch
      case e: ContinuationInvoked =>
        ContState.checkpoints(0).get(e.id) match
          case Some((forms, ckEnv, ckOut)) =>
            val bodyRef = ContState.callbackBodies(0).get(e.id)
            ContState.pendingValue(0) = Some((bodyRef, e.value))
            evalAllCC(forms, ckEnv, Value.Void, ckOut)
          case None => throw e

  @tailrec
  private def evalAll(
    exprs: List[(Value, Pos)],
    env: Env,
    last: Value = Value.Void,
    out: String = ""
  ): (Value, Env, String) = exprs match
    case Nil => (last, env, out)
    case (expr, pos) :: tail =>
      ContState.currentCheckpoint(0) = Some((exprs, env, out))
      val (value, newEnv, o) = evalWithPos(expr, pos, env)
      evalAll(tail, newEnv, value, out + o)

  private def evalWithPos(
    expr: Value,
    pos: Pos,
    env: Env
  ): (Value, Env, String) =
    try Eval.evalTopLevel(expr, env)
    catch
      case e: EvalError =>
        val msg = e.getMessage
        if msg.matches(".*\\d+:\\d+.*") then throw e
        else throw new EvalError(s"$pos: $msg")
