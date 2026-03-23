package ming

import SchemeValue.*
import Interpreter.EvalResult
import Interpreter.EvalResult.*

/** Let-family and case special forms, extracted from SpecialForms. */
object SpecialFormsLet:

  /** Format an error message with optional position info. */
  private def posMsg(msg: String, pos: Option[SourcePos]): String =
    pos match
      case Some(p) => s"$msg [$p]"
      case None    => msg

  def evalLetrec(
    args: List[SchemeValue],
    pos: Option[SourcePos],
    env: Environment
  ): EvalResult =
    args match
      case ListVal(bindings, _) :: body if body.nonEmpty =>
        val childEnv = env.child()
        // Define all bindings as Void first (mutual visibility)
        val names = bindings.map {
          case ListVal(SymbolVal(n, _) :: _ :: Nil, _) => n
          case _                                       => throw new EvalError(posMsg("letrec: bad binding", pos))
        }
        names.foreach(n => childEnv.define(n, Void))
        // Evaluate init exprs in the child env and set
        bindings.foreach {
          case ListVal(SymbolVal(n, _) :: value :: Nil, _) =>
            childEnv.define(n, Interpreter.eval(value, childEnv))
          case _ => throw new EvalError(posMsg("letrec: bad binding", pos))
        }
        Interpreter.evalBodyInit(body, childEnv)
        TailCall(body.last, childEnv)
      case _ => throw new EvalError(posMsg("letrec: bad syntax", pos))

  def evalLetrecStar(
    args: List[SchemeValue],
    pos: Option[SourcePos],
    env: Environment
  ): EvalResult =
    args match
      case ListVal(bindings, _) :: body if body.nonEmpty =>
        val childEnv = env.child()
        // Define and evaluate sequentially (each visible to the next)
        bindings.foreach {
          case ListVal(SymbolVal(n, _) :: value :: Nil, _) =>
            childEnv.define(n, Interpreter.eval(value, childEnv))
          case _ => throw new EvalError(posMsg("letrec*: bad binding", pos))
        }
        Interpreter.evalBodyInit(body, childEnv)
        TailCall(body.last, childEnv)
      case _ => throw new EvalError(posMsg("letrec*: bad syntax", pos))

  def evalCase(
    args: List[SchemeValue],
    pos: Option[SourcePos],
    env: Environment
  ): EvalResult =
    args match
      case keyExpr :: clauses if clauses.nonEmpty =>
        val key = Interpreter.eval(keyExpr, env)
        evalCaseClauses(key, clauses, pos, env)
      case _ => throw new EvalError(posMsg("case: bad syntax", pos))

  private def evalCaseClauses(
    key: SchemeValue,
    clauses: List[SchemeValue],
    pos: Option[SourcePos],
    env: Environment
  ): EvalResult =
    var remaining = clauses
    while remaining.nonEmpty do
      remaining.head match
        case ListVal(SymbolVal("else", _) :: body, _) =>
          if body.isEmpty then return Done(Void)
          Interpreter.evalBodyInit(body, env)
          return TailCall(body.last, env)
        case ListVal(ListVal(datums, _) :: body, _) =>
          if datums.exists(d => Builtins.schemeEqv(key, d)) then
            if body.isEmpty then return Done(Void)
            Interpreter.evalBodyInit(body, env)
            return TailCall(body.last, env)
          remaining = remaining.tail
        case _ => throw new EvalError(posMsg("case: bad clause", pos))
    Done(Void)

  def evalLetStar(
    args: List[SchemeValue],
    pos: Option[SourcePos],
    env: Environment
  ): EvalResult =
    args match
      case ListVal(bindings, _) :: body if body.nonEmpty =>
        val childEnv = env.child()
        bindings.foreach {
          case ListVal(SymbolVal(n, _) :: value :: Nil, _) =>
            childEnv.define(n, Interpreter.eval(value, childEnv))
          case _ => throw new EvalError(posMsg("let*: bad binding", pos))
        }
        Interpreter.evalBodyInit(body, childEnv)
        TailCall(body.last, childEnv)
      case _ => throw new EvalError(posMsg("let*: bad syntax", pos))
