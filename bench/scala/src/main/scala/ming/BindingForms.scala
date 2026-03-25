package ming

import scala.annotation.tailrec

/** Binding and control-flow special forms extracted from Evaluator. */
object BindingForms:

  def evalLet(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.Symbol(name) :: SchemeVal.SList(bindings) :: body if body.nonEmpty =>
        val (params, inits) = parseBindings(bindings, env)
        val localEnv        = new Env(scala.collection.mutable.Map.empty, Some(env))
        val proc            = SchemeVal.LambdaProc(params, body, localEnv)
        localEnv.define(name, proc)
        params.zip(inits).foreach((p, v) => localEnv.define(p, v))
        body.foldLeft(SchemeVal.Void: SchemeVal)((_, e) => Evaluator.eval(e, localEnv))
      case SchemeVal.SList(bindings) :: body if body.nonEmpty =>
        val localEnv = new Env(scala.collection.mutable.Map.empty, Some(env))
        for b <- bindings do
          b match
            case SchemeVal.SList(List(SchemeVal.Symbol(name), valueExpr)) =>
              localEnv.define(name, Evaluator.eval(valueExpr, env))
            case _ => throw new EvalError("let: bad binding syntax")
        body.foldLeft(SchemeVal.Void: SchemeVal)((_, e) => Evaluator.eval(e, localEnv))
      case _ => throw new EvalError("let: bad syntax")

  private def parseBindings(
    bindings: List[SchemeVal],
    env: Env
  ): (List[String], List[SchemeVal]) =
    val pairs = bindings.map {
      case SchemeVal.SList(List(SchemeVal.Symbol(p), valueExpr)) => (p, Evaluator.eval(valueExpr, env))
      case _                                                     => throw new EvalError("let: bad binding syntax")
    }
    pairs.unzip

  def evalLetrec(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.SList(bindings) :: body if body.nonEmpty =>
        val localEnv = new Env(scala.collection.mutable.Map.empty, Some(env))
        val names = bindings.map {
          case SchemeVal.SList(List(SchemeVal.Symbol(name), _)) => name
          case _                                                => throw new EvalError("letrec: bad binding syntax")
        }
        names.foreach(n => localEnv.define(n, SchemeVal.Void))
        val values = bindings.map {
          case SchemeVal.SList(List(_, valueExpr)) => Evaluator.eval(valueExpr, localEnv)
          case _                                   => throw new EvalError("letrec: bad binding syntax")
        }
        names.zip(values).foreach((n, v) => localEnv.define(n, v))
        body.foldLeft(SchemeVal.Void: SchemeVal)((_, e) => Evaluator.eval(e, localEnv))
      case _ => throw new EvalError("letrec: bad syntax")

  def evalLetrecStar(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.SList(bindings) :: body if body.nonEmpty =>
        val localEnv = new Env(scala.collection.mutable.Map.empty, Some(env))
        for b <- bindings do
          b match
            case SchemeVal.SList(List(SchemeVal.Symbol(name), valueExpr)) =>
              localEnv.define(name, Evaluator.eval(valueExpr, localEnv))
            case _ => throw new EvalError("letrec*: bad binding syntax")
        body.foldLeft(SchemeVal.Void: SchemeVal)((_, e) => Evaluator.eval(e, localEnv))
      case _ => throw new EvalError("letrec*: bad syntax")

  def evalLetStar(args: List[SchemeVal], env: Env): SchemeVal =
    args match
      case SchemeVal.SList(bindings) :: body if body.nonEmpty =>
        val localEnv = new Env(scala.collection.mutable.Map.empty, Some(env))
        for b <- bindings do
          b match
            case SchemeVal.SList(List(SchemeVal.Symbol(name), valueExpr)) =>
              localEnv.define(name, Evaluator.eval(valueExpr, localEnv))
            case _ => throw new EvalError("let*: bad binding syntax")
        body.foldLeft(SchemeVal.Void: SchemeVal)((_, e) => Evaluator.eval(e, localEnv))
      case _ => throw new EvalError("let*: bad syntax")

  @tailrec
  def evalCond(clauses: List[SchemeVal], env: Env): SchemeVal =
    clauses match
      case Nil => SchemeVal.Void
      case clause :: rest =>
        clause match
          case SchemeVal.SList(elems) if elems.nonEmpty =>
            elems.head match
              case SchemeVal.Symbol("else") =>
                elems.tail.foldLeft(SchemeVal.Void: SchemeVal)((_, e) => Evaluator.eval(e, env))
              case test =>
                val testVal = Evaluator.eval(test, env)
                if Evaluator.isTruthy(testVal) then
                  if elems.tail.isEmpty then testVal
                  else elems.tail.foldLeft(SchemeVal.Void: SchemeVal)((_, e) => Evaluator.eval(e, env))
                else evalCond(rest, env)
          case _ => throw new EvalError("cond: bad clause")

  def evalCase(args: List[SchemeVal], env: Env): SchemeVal =
    if args.isEmpty then throw new EvalError("case: bad syntax")
    val key = Evaluator.eval(args.head, env)
    evalCaseClauses(key, args.tail, env)

  @tailrec
  private def evalCaseClauses(key: SchemeVal, clauses: List[SchemeVal], env: Env): SchemeVal =
    clauses match
      case Nil => SchemeVal.Void
      case clause :: rest =>
        clause match
          case SchemeVal.SList(elems) if elems.nonEmpty =>
            elems.head match
              case SchemeVal.Symbol("else") =>
                elems.tail.foldLeft(SchemeVal.Void: SchemeVal)((_, e) => Evaluator.eval(e, env))
              case SchemeVal.SList(datums) =>
                if datums.exists(d => schemeEqv(key, d)) then
                  elems.tail.foldLeft(SchemeVal.Void: SchemeVal)((_, e) => Evaluator.eval(e, env))
                else evalCaseClauses(key, rest, env)
              case _ => throw new EvalError("case: bad clause")
          case _ => throw new EvalError("case: bad clause")

  private def schemeEqv(a: SchemeVal, b: SchemeVal): Boolean = (a, b) match
    case (SchemeVal.Symbol(x), SchemeVal.Symbol(y))                     => x == y
    case (SchemeVal.IntVal(x), SchemeVal.IntVal(y))                     => x == y
    case (SchemeVal.RationalVal(n1, d1), SchemeVal.RationalVal(n2, d2)) => n1 == n2 && d1 == d2
    case (SchemeVal.FloatVal(x), SchemeVal.FloatVal(y))                 => x == y
    case (SchemeVal.BoolVal(x), SchemeVal.BoolVal(y))                   => x == y
    case (SchemeVal.CharVal(x), SchemeVal.CharVal(y))                   => x == y
    case (SchemeVal.SList(Nil), SchemeVal.SList(Nil))                   => true
    case _                                                              => a eq b

  def evalDo(args: List[SchemeVal], env: Env): SchemeVal =
    if args.size < 2 then throw new EvalError("do: bad syntax")
    val bindings = args(0) match
      case SchemeVal.SList(bs) => bs
      case _                   => throw new EvalError("do: expected bindings list")
    val testClause = args(1) match
      case SchemeVal.SList(elems) if elems.nonEmpty => elems
      case _                                        => throw new EvalError("do: expected test clause")
    val body = args.drop(2)

    val parsed = bindings.map {
      case SchemeVal.SList(List(SchemeVal.Symbol(name), init)) =>
        (name, init, None: Option[SchemeVal])
      case SchemeVal.SList(List(SchemeVal.Symbol(name), init, step)) =>
        (name, init, Some(step))
      case _ => throw new EvalError("do: bad binding syntax")
    }

    val localEnv = new Env(scala.collection.mutable.Map.empty, Some(env))
    val initVals = parsed.map((_, init, _) => Evaluator.eval(init, env))
    parsed.zip(initVals).foreach { case ((name, _, _), v) => localEnv.define(name, v) }

    var test  = testClause.head
    var exprs = testClause.tail
    while !Evaluator.isTruthy(Evaluator.eval(test, localEnv)) do
      body.foreach(e => Evaluator.eval(e, localEnv))
      val newVals = parsed.map {
        case (_, _, Some(step)) => Some(Evaluator.eval(step, localEnv))
        case (_, _, None)       => None
      }
      parsed.zip(newVals).foreach {
        case ((name, _, _), Some(v)) => localEnv.define(name, v)
        case _                       => ()
      }
    if exprs.isEmpty then SchemeVal.Void
    else exprs.foldLeft(SchemeVal.Void: SchemeVal)((_, e) => Evaluator.eval(e, localEnv))
