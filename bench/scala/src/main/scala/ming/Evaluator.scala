package ming

import scala.collection.mutable

/** Scheme value types */
enum SchemeVal:
  case SInt(value: Long)
  case SBool(value: Boolean)
  case SString(value: String)
  case SSymbol(name: String)
  case SList(elems: List[SchemeVal])
  case SVoid
  case SLambda(params: List[String], body: List[SchemeVal], closure: Env)

  def display: String = this match
    case SInt(v)          => v.toString
    case SBool(v)         => if v then "#t" else "#f"
    case SString(v)       => s""""$v""""
    case SSymbol(n)       => n
    case SList(es)        => "(" + es.map(_.display).mkString(" ") + ")"
    case SVoid            => ""
    case SLambda(_, _, _) => "#<procedure>"

/** Environment with parent chain */
class Env(val parent: Option[Env] = None):
  private val bindings = mutable.HashMap[String, SchemeVal]()

  def get(name: String): SchemeVal =
    bindings.get(name) match
      case Some(v) => v
      case None =>
        parent match
          case Some(p) => p.get(name)
          case None    => throw new EvalError(s"unbound variable: $name")

  def define(name: String, value: SchemeVal): Unit =
    bindings(name) = value

  def set(name: String, value: SchemeVal): Unit =
    if bindings.contains(name) then bindings(name) = value
    else
      parent match
        case Some(p) => p.set(name, value)
        case None    => throw new EvalError(s"unbound variable: $name")

/** Scheme interpreter entry point. */
object Evaluator:

  private def isTruthy(v: SchemeVal): Boolean = v match
    case SchemeVal.SBool(false) => false
    case _                      => true

  def eval(expr: SchemeVal, env: Env): SchemeVal = expr match
    case SchemeVal.SInt(_) | SchemeVal.SBool(_) | SchemeVal.SString(_) | SchemeVal.SVoid => expr
    case SchemeVal.SSymbol(name)                                                         => env.get(name)
    case SchemeVal.SList(elems) =>
      elems match
        case Nil => throw new EvalError("empty application")
        case SchemeVal.SSymbol("quote") :: args =>
          if args.length != 1 then throw new EvalError("quote: expected 1 argument")
          args.head
        case SchemeVal.SSymbol("if") :: args     => evalIf(args, env)
        case SchemeVal.SSymbol("define") :: args => evalDefine(args, env)
        case SchemeVal.SSymbol("lambda") :: args => evalLambda(args, env)
        case SchemeVal.SSymbol("and") :: args    => evalAnd(args, env)
        case SchemeVal.SSymbol("or") :: args     => evalOr(args, env)
        case head :: args =>
          val op         = eval(head, env)
          val evaledArgs = args.map(eval(_, env))
          applyProc(op, evaledArgs)
    case other => other

  private def evalIf(args: List[SchemeVal], env: Env): SchemeVal =
    if args.length < 2 || args.length > 3 then throw new EvalError("if: expected 2 or 3 arguments")
    val cond = eval(args(0), env)
    if isTruthy(cond) then eval(args(1), env)
    else if args.length == 3 then eval(args(2), env)
    else SchemeVal.SVoid

  private def evalDefine(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.SSymbol(name) :: valueExpr :: Nil =>
        env.define(name, eval(valueExpr, env))
        SchemeVal.SVoid
      case SchemeVal.SList(SchemeVal.SSymbol(name) :: params) :: body if body.nonEmpty =>
        val paramNames = params.map {
          case SchemeVal.SSymbol(n) => n
          case other                => throw new EvalError(s"define: expected parameter name, got ${other.display}")
        }
        env.define(name, SchemeVal.SLambda(paramNames, body, env))
        SchemeVal.SVoid
      case _ => throw new EvalError("define: bad syntax")

  private def evalLambda(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.SList(params) :: body if body.nonEmpty =>
        val paramNames = params.map {
          case SchemeVal.SSymbol(n) => n
          case other                => throw new EvalError(s"lambda: expected parameter name, got ${other.display}")
        }
        SchemeVal.SLambda(paramNames, body, env)
      case _ => throw new EvalError("lambda: bad syntax")

  private def evalAnd(args: List[SchemeVal], env: Env): SchemeVal =
    if args.isEmpty then SchemeVal.SBool(true)
    else
      var result: SchemeVal = SchemeVal.SBool(true)
      val iter              = args.iterator
      var done              = false
      while iter.hasNext && !done do
        result = eval(iter.next(), env)
        if !isTruthy(result) then done = true
      result

  private def evalOr(args: List[SchemeVal], env: Env): SchemeVal =
    if args.isEmpty then SchemeVal.SBool(false)
    else
      var result: SchemeVal = SchemeVal.SBool(false)
      val iter              = args.iterator
      var found             = false
      while iter.hasNext && !found do
        result = eval(iter.next(), env)
        if isTruthy(result) then found = true
      result

  private def applyProc(op: SchemeVal, args: List[SchemeVal]): SchemeVal =
    op match
      case SchemeVal.SLambda(params, body, closure) =>
        if params.length != args.length then
          throw new EvalError(s"expected ${params.length} arguments, got ${args.length}")
        val callEnv = Env(Some(closure))
        params.zip(args).foreach((p, a) => callEnv.define(p, a))
        var result: SchemeVal = SchemeVal.SVoid
        for expr <- body do result = eval(expr, callEnv)
        result
      case SchemeVal.SSymbol(name) => Builtins.applyBuiltin(name, args)
      case _                       => throw new EvalError(s"not a procedure: ${op.display}")

  private def makeGlobalEnv(): Env =
    val env = Env()
    for name <- Builtins.names do env.define(name, SchemeVal.SSymbol(name))
    env

  /** Evaluate one or more Scheme expressions and return the string representation of the last result. */
  def evalStr(input: String): String =
    val exprs = Parser.parseAll(input)
    if exprs.isEmpty then throw new EvalError("empty input")
    val env               = makeGlobalEnv()
    var result: SchemeVal = SchemeVal.SVoid
    for expr <- exprs do result = eval(expr, env)
    result.display

  /** Evaluate Scheme expressions and return both the result string and any captured output. */
  def evalStrWithOutput(input: String): (String, String) =
    (evalStr(input), "")
