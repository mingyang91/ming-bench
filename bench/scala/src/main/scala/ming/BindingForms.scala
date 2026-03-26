package ming

import scala.collection.mutable
import scala.util.boundary
import scala.util.boundary.break

/** Binding and iteration special forms extracted from Evaluator. */
private[ming] object BindingForms:

  def evalLet(args: List[Expr], env: Env): SchemeVal =
    args match
      case Symbol(name, _) :: SList(bindings, _) :: body if body.nonEmpty =>
        val parsed   = parseBindings(bindings, env)
        val localEnv = new Env(mutable.Map.empty, Some(env))
        val lambda   = SchemeLambda(parsed.map(_._1), None, body, localEnv)
        localEnv.set(name, lambda)
        Evaluator.applyProc(lambda, parsed.map(_._2))
      case SList(bindings, _) :: body if body.nonEmpty =>
        val localEnv = new Env(mutable.Map.empty, Some(env))
        for b <- bindings do
          b match
            case SList(Symbol(n, _) :: initExpr :: Nil, _) =>
              localEnv.set(n, Evaluator.eval(initExpr, env))
            case _ => throw new EvalError("let: bad binding")
        Evaluator.evalBody(body, localEnv)
      case _ => throw new EvalError("let: bad syntax")

  private def parseBindings(
    bindings: List[Expr],
    env: Env
  ): List[(String, SchemeVal)] =
    bindings.map {
      case SList(Symbol(n, _) :: initExpr :: Nil, _) =>
        (n, Evaluator.eval(initExpr, env))
      case _ => throw new EvalError("let: bad binding")
    }

  def evalLetStar(args: List[Expr], env: Env): SchemeVal =
    args match
      case SList(bindings, _) :: body if body.nonEmpty =>
        val localEnv = new Env(mutable.Map.empty, Some(env))
        for b <- bindings do
          b match
            case SList(Symbol(n, _) :: initExpr :: Nil, _) =>
              localEnv.set(n, Evaluator.eval(initExpr, localEnv))
            case _ => throw new EvalError("let*: bad binding")
        Evaluator.evalBody(body, localEnv)
      case _ => throw new EvalError("let*: bad syntax")

  def evalLetrec(args: List[Expr], env: Env): SchemeVal =
    args match
      case SList(bindings, _) :: body if body.nonEmpty =>
        val localEnv = new Env(mutable.Map.empty, Some(env))
        val names = bindings.map {
          case SList(Symbol(n, _) :: _ :: Nil, _) => n
          case _                                  => throw new EvalError("letrec: bad binding")
        }
        for n <- names do localEnv.set(n, SchemeVoid)
        for b <- bindings do
          b match
            case SList(Symbol(n, _) :: initExpr :: Nil, _) =>
              localEnv.set(n, Evaluator.eval(initExpr, localEnv))
            case _ => throw new EvalError("letrec: bad binding")
        Evaluator.evalBody(body, localEnv)
      case _ => throw new EvalError("letrec: bad syntax")

  def evalLetrecStar(args: List[Expr], env: Env): SchemeVal =
    args match
      case SList(bindings, _) :: body if body.nonEmpty =>
        val localEnv = new Env(mutable.Map.empty, Some(env))
        for b <- bindings do
          b match
            case SList(Symbol(n, _) :: initExpr :: Nil, _) =>
              localEnv.set(n, Evaluator.eval(initExpr, localEnv))
            case _ => throw new EvalError("letrec*: bad binding")
        Evaluator.evalBody(body, localEnv)
      case _ => throw new EvalError("letrec*: bad syntax")

  def evalCond(clauses: List[Expr], env: Env): SchemeVal =
    boundary:
      for clause <- clauses do
        clause match
          case SList(Symbol("else", _) :: body, _) =>
            break(Evaluator.evalBody(body, env))
          case SList(test :: body, _) =>
            val testVal = Evaluator.eval(test, env)
            if !Evaluator.isFalsy(testVal) then
              break(
                if body.isEmpty then testVal else Evaluator.evalBody(body, env)
              )
          case _ => throw new EvalError("cond: bad clause")
      SchemeVoid

  def evalCase(args: List[Expr], env: Env): SchemeVal =
    if args.isEmpty then throw new EvalError("case: bad syntax")
    val key = Evaluator.eval(args.head, env)
    boundary:
      for clause <- args.tail do
        clause match
          case SList(Symbol("else", _) :: body, _) =>
            break(Evaluator.evalBody(body, env))
          case SList(SList(datums, _) :: body, _) =>
            val matched = datums.exists { d =>
              val dv = EvalHelpers.exprToVal(d)
              ListBuiltins.schemeEqv(key, dv)
            }
            if matched then
              break(
                if body.isEmpty then SchemeVoid
                else Evaluator.evalBody(body, env)
              )
          case _ => throw new EvalError("case: bad clause")
      SchemeVoid

  def evalDo(args: List[Expr], env: Env): SchemeVal =
    args match
      case SList(varSpecs, _) :: SList(testAndExprs, _) :: commands =>
        if testAndExprs.isEmpty then throw new EvalError("do: bad syntax")
        val test        = testAndExprs.head
        val resultExprs = testAndExprs.tail
        case class DoVar(name: String, init: Expr, step: Option[Expr])
        val vars = varSpecs.map {
          case SList(Symbol(n, _) :: init :: step :: Nil, _) =>
            DoVar(n, init, Some(step))
          case SList(Symbol(n, _) :: init :: Nil, _) => DoVar(n, init, None)
          case _                                     => throw new EvalError("do: bad variable spec")
        }
        val localEnv = new Env(mutable.Map.empty, Some(env))
        for v <- vars do localEnv.set(v.name, Evaluator.eval(v.init, env))
        while Evaluator.isFalsy(Evaluator.eval(test, localEnv)) do
          for cmd <- commands do Evaluator.eval(cmd, localEnv)
          val newVals = vars.map { v =>
            v.step match
              case Some(s) => Some(Evaluator.eval(s, localEnv))
              case None    => None
          }
          vars.zip(newVals).foreach { (v, nv) =>
            nv.foreach(value => localEnv.set(v.name, value))
          }
        if resultExprs.isEmpty then SchemeVoid
        else Evaluator.evalBody(resultExprs, localEnv)
      case _ => throw new EvalError("do: bad syntax")

  def evalWhen(args: List[Expr], env: Env): SchemeVal =
    if args.size < 2 then throw new EvalError("when: bad syntax")
    if !Evaluator.isFalsy(Evaluator.eval(args.head, env)) then Evaluator.evalBody(args.tail, env)
    else SchemeVoid

  def evalUnless(args: List[Expr], env: Env): SchemeVal =
    if args.size < 2 then throw new EvalError("unless: bad syntax")
    if Evaluator.isFalsy(Evaluator.eval(args.head, env)) then Evaluator.evalBody(args.tail, env)
    else SchemeVoid
