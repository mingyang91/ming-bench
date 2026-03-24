package ming

import scala.annotation.tailrec

import Evaluator.Val
import Evaluator.Val.*

/** TCO-aware special form handlers that return trampoline signals. */
object TcoForms:

  /** Result of a TCO-aware evaluation step. */
  enum TcoResult:
    /** Continue the trampoline with a new expression and environment. */
    case Continue(expr: Val, env: Env)

    /** Return a final value. */
    case Done(value: Val)

  import TcoResult.*

  /** Evaluate an if form with TCO. */
  def evalIf(
    rest: Val,
    env: Env,
    eval: (Val, Env) => Val,
    error: String => Nothing
  ): TcoResult =
    rest match
      case Pair(cond, Pair(thenExpr, Pair(elseExpr, Nil))) =>
        val condVal = eval(cond, env)
        if condVal != Bool(false) then Continue(thenExpr, env)
        else Continue(elseExpr, env)
      case Pair(cond, Pair(thenExpr, Nil)) =>
        val condVal = eval(cond, env)
        if condVal != Bool(false) then Continue(thenExpr, env)
        else Done(Void)
      case _ => error("bad if syntax")

  /** Evaluate an and form with TCO. */
  def evalAnd(
    args: Val,
    env: Env,
    eval: (Val, Env) => Val
  ): TcoResult =
    val argList = Evaluator.toList(args)
    evalAndLoop(argList, env, eval)

  @tailrec
  private def evalAndLoop(
    remaining: List[Val],
    env: Env,
    eval: (Val, Env) => Val
  ): TcoResult =
    remaining match
      case scala.Nil         => Done(Bool(true))
      case last :: scala.Nil => Continue(last, env)
      case head :: tail =>
        val v = eval(head, env)
        if v == Bool(false) then Done(Bool(false))
        else evalAndLoop(tail, env, eval)

  /** Evaluate an or form with TCO. */
  def evalOr(
    args: Val,
    env: Env,
    eval: (Val, Env) => Val
  ): TcoResult =
    val argList = Evaluator.toList(args)
    evalOrLoop(argList, env, eval)

  @tailrec
  private def evalOrLoop(
    remaining: List[Val],
    env: Env,
    eval: (Val, Env) => Val
  ): TcoResult =
    remaining match
      case scala.Nil         => Done(Bool(false))
      case last :: scala.Nil => Continue(last, env)
      case head :: tail =>
        val v = eval(head, env)
        if v != Bool(false) then Done(v)
        else evalOrLoop(tail, env, eval)

  /** Dispatch a let form (named or regular). */
  def evalLet(
    rest: Val,
    env: Env,
    eval: (Val, Env) => Val,
    error: String => Nothing
  ): TcoResult =
    rest match
      case Pair(Symbol(name), Pair(bindings, body)) =>
        evalNamedLet(name, bindings, body, env, eval, error)
      case Pair(bindings, body) =>
        evalRegularLet(bindings, body, env, eval, error)
      case _ => error("bad let syntax")

  /** Evaluate a cond form, returning a trampoline signal. */
  def evalCond(
    clauses: Val,
    env: Env,
    eval: (Val, Env) => Val,
    error: String => Nothing
  ): TcoResult =
    var cls = clauses
    while true do
      cls match
        case Nil => return Done(Void)
        case Pair(clause, rest) =>
          val clauseList = Evaluator.toList(clause)
          if clauseList.isEmpty then error("bad cond clause")
          clauseList.head match
            case Symbol("else") =>
              return evalBodyTco(clauseList.tail, env, eval)
            case test =>
              val v = eval(test, env)
              if v != Bool(false) then
                if clauseList.tail.isEmpty then return Done(v)
                return evalBodyTco(clauseList.tail, env, eval)
              else cls = rest
        case _ => error("bad cond syntax")
    throw new RuntimeException("unreachable")

  /** Evaluate a named let form: (let name ((var init) ...) body ...). */
  private def evalNamedLet(
    name: String,
    bindings: Val,
    body: Val,
    env: Env,
    eval: (Val, Env) => Val,
    error: String => Nothing
  ): TcoResult =
    val bindingList = Evaluator.toList(bindings)
    val paramNames = bindingList.map {
      case Pair(Symbol(p), Pair(_, Nil)) => p
      case _                             => error("bad named let binding")
    }
    val initVals = bindingList.map {
      case Pair(_, Pair(v, Nil)) => eval(v, env)
      case _                     => error("bad named let binding")
    }
    val bodyList = Evaluator.toList(body)
    if bodyList.isEmpty then error("let: empty body")
    val closureEnvForName = Env.empty(Some(env))
    val closure           = Closure(paramNames, None, bodyList, closureEnvForName)
    closureEnvForName.define(name, closure)
    val callEnv = Env.empty(Some(closureEnvForName))
    paramNames.zip(initVals).foreach((p, a) => callEnv.define(p, a))
    evalBodyTco(bodyList, callEnv, eval)

  /** Evaluate a regular let form: (let ((var init) ...) body ...). */
  private def evalRegularLet(
    bindings: Val,
    body: Val,
    env: Env,
    eval: (Val, Env) => Val,
    error: String => Nothing
  ): TcoResult =
    val childEnv = Env.empty(Some(env))
    for binding <- Evaluator.toList(bindings) do
      binding match
        case Pair(Symbol(name), Pair(valueExpr, Nil)) =>
          childEnv.define(name, eval(valueExpr, env))
        case _ => error("bad let binding")
    val bodyList = Evaluator.toList(body)
    if bodyList.isEmpty then error("let: empty body")
    evalBodyTco(bodyList, childEnv, eval)

  /** Apply a closure with TCO. */
  def applyClosureTco(
    params: List[String],
    restParam: Option[String],
    body: List[Val],
    closureEnv: Env,
    args: List[Val],
    eval: (Val, Env) => Val,
    error: String => Nothing
  ): TcoResult =
    val callEnv =
      Evaluator.setupClosureEnv(params, restParam, body, closureEnv, args)
    evalBodyTco(body, callEnv, eval)

  /** Apply a function (closure or builtin), returning a TcoResult. */
  def applyFuncTco(
    func: Val,
    argList: List[Val],
    eval: (Val, Env) => Val,
    error: String => Nothing,
    applyBuiltin: (List[Val] => Val, List[Val]) => Val
  ): TcoResult =
    func match
      case Closure(params, restParam, body, closureEnv) =>
        applyClosureTco(params, restParam, body, closureEnv, argList, eval, error)
      case Builtin(f) => Done(applyBuiltin(f, argList))
      case _          => error(s"not a procedure: ${Display.write(func)}")

  /** Evaluate body expressions for TCO: all but last eagerly, last as Continue. */
  private def evalBodyTco(
    body: List[Val],
    env: Env,
    eval: (Val, Env) => Val
  ): TcoResult =
    if body.isEmpty then Done(Void)
    else
      for e <- body.init do eval(e, env)
      Continue(body.last, env)
