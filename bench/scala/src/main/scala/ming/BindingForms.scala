package ming

import Evaluator.{Bounce, Cont, Done, More}

/** Binding and control-flow special forms — CPS versions. */
object BindingForms:

  def evalLetK(args: List[SchemeVal], env: Env, k: Cont): Bounce =
    args match
      case SchemeVal.Symbol(name) :: SchemeVal.SList(bindings) :: body if body.nonEmpty =>
        // Named let
        evalBindingsK(
          bindings,
          env,
          pairs =>
            val (params, inits) = pairs.unzip
            val localEnv        = new Env(scala.collection.mutable.Map.empty, Some(env))
            val proc            = SchemeVal.LambdaProc(params, body, localEnv)
            localEnv.define(name, proc)
            params.zip(inits).foreach((p, v) => localEnv.define(p, v))
            Evaluator.evalBodyK(body, localEnv, k)
        )
      case SchemeVal.SList(bindings) :: body if body.nonEmpty =>
        evalBindingsK(
          bindings,
          env,
          pairs =>
            val localEnv = new Env(scala.collection.mutable.Map.empty, Some(env))
            pairs.foreach((name, value) => localEnv.define(name, value))
            Evaluator.evalBodyK(body, localEnv, k)
        )
      case _ => throw new EvalError("let: bad syntax")

  /** Evaluate bindings left-to-right in the given env, collect (name, value) pairs. */
  private def evalBindingsK(
    bindings: List[SchemeVal],
    env: Env,
    k: List[(String, SchemeVal)] => Bounce
  ): Bounce =
    def go(remaining: List[SchemeVal], acc: List[(String, SchemeVal)]): Bounce =
      remaining match
        case Nil => k(acc.reverse)
        case SchemeVal.SList(List(SchemeVal.Symbol(name), valueExpr)) :: tail =>
          More(() => Evaluator.evalK(valueExpr, env, v => go(tail, (name, v) :: acc)))
        case _ => throw new EvalError("let: bad binding syntax")
    go(bindings, Nil)

  def evalLetStarK(args: List[SchemeVal], env: Env, k: Cont): Bounce =
    args match
      case SchemeVal.SList(bindings) :: body if body.nonEmpty =>
        val localEnv = new Env(scala.collection.mutable.Map.empty, Some(env))
        evalSeqBindingsK(bindings, localEnv, () => Evaluator.evalBodyK(body, localEnv, k))
      case _ => throw new EvalError("let*: bad syntax")

  /** Evaluate bindings sequentially, defining each in localEnv before the next. */
  private def evalSeqBindingsK(bindings: List[SchemeVal], localEnv: Env, k: () => Bounce): Bounce =
    bindings match
      case Nil => k()
      case SchemeVal.SList(List(SchemeVal.Symbol(name), valueExpr)) :: tail =>
        More(() =>
          Evaluator.evalK(
            valueExpr,
            localEnv,
            v =>
              localEnv.define(name, v)
              evalSeqBindingsK(tail, localEnv, k)
          )
        )
      case _ => throw new EvalError("bad binding syntax")

  def evalLetrecK(args: List[SchemeVal], env: Env, k: Cont): Bounce =
    args match
      case SchemeVal.SList(bindings) :: body if body.nonEmpty =>
        val localEnv = new Env(scala.collection.mutable.Map.empty, Some(env))
        val names = bindings.map {
          case SchemeVal.SList(List(SchemeVal.Symbol(name), _)) => name
          case _                                                => throw new EvalError("letrec: bad binding syntax")
        }
        val valueExprs = bindings.map {
          case SchemeVal.SList(List(_, ve)) => ve
          case _                            => throw new EvalError("letrec: bad binding syntax")
        }
        names.foreach(n => localEnv.define(n, SchemeVal.Void))
        Evaluator.evalListK(
          valueExprs,
          localEnv,
          values =>
            names.zip(values).foreach((n, v) => localEnv.define(n, v))
            Evaluator.evalBodyK(body, localEnv, k)
        )
      case _ => throw new EvalError("letrec: bad syntax")

  def evalLetrecStarK(args: List[SchemeVal], env: Env, k: Cont): Bounce =
    args match
      case SchemeVal.SList(bindings) :: body if body.nonEmpty =>
        val localEnv = new Env(scala.collection.mutable.Map.empty, Some(env))
        evalSeqBindingsK(bindings, localEnv, () => Evaluator.evalBodyK(body, localEnv, k))
      case _ => throw new EvalError("letrec*: bad syntax")

  def evalCondK(clauses: List[SchemeVal], env: Env, k: Cont): Bounce =
    clauses match
      case Nil => k(SchemeVal.Void)
      case clause :: rest =>
        clause match
          case SchemeVal.SList(elems) if elems.nonEmpty =>
            elems.head match
              case SchemeVal.Symbol("else") =>
                Evaluator.evalBodyK(elems.tail, env, k)
              case test =>
                More(() =>
                  Evaluator.evalK(
                    test,
                    env,
                    testVal =>
                      if Evaluator.isTruthy(testVal) then
                        if elems.tail.isEmpty then k(testVal)
                        else Evaluator.evalBodyK(elems.tail, env, k)
                      else More(() => evalCondK(rest, env, k))
                  )
                )
          case _ => throw new EvalError("cond: bad clause")

  def evalCaseK(args: List[SchemeVal], env: Env, k: Cont): Bounce =
    if args.isEmpty then throw new EvalError("case: bad syntax")
    Evaluator.evalK(args.head, env, key => evalCaseClausesK(key, args.tail, env, k))

  private def evalCaseClausesK(key: SchemeVal, clauses: List[SchemeVal], env: Env, k: Cont): Bounce =
    clauses match
      case Nil => k(SchemeVal.Void)
      case clause :: rest =>
        clause match
          case SchemeVal.SList(elems) if elems.nonEmpty =>
            elems.head match
              case SchemeVal.Symbol("else") =>
                Evaluator.evalBodyK(elems.tail, env, k)
              case SchemeVal.SList(datums) =>
                if datums.exists(d => schemeEqv(key, d)) then Evaluator.evalBodyK(elems.tail, env, k)
                else evalCaseClausesK(key, rest, env, k)
              case _ => throw new EvalError("case: bad clause")
          case _ => throw new EvalError("case: bad clause")

  def evalDoK(args: List[SchemeVal], env: Env, k: Cont): Bounce =
    if args.size < 2 then throw new EvalError("do: bad syntax")
    val bindings = args(0) match
      case SchemeVal.SList(bs) => bs
      case _                   => throw new EvalError("do: expected bindings list")
    val testClause = args(1) match
      case SchemeVal.SList(elems) if elems.nonEmpty => elems
      case _                                        => throw new EvalError("do: expected test clause")
    val body  = args.drop(2)
    val test  = testClause.head
    val exprs = testClause.tail

    val parsed = bindings.map {
      case SchemeVal.SList(List(SchemeVal.Symbol(name), init)) =>
        (name, init, None: Option[SchemeVal])
      case SchemeVal.SList(List(SchemeVal.Symbol(name), init, step)) =>
        (name, init, Some(step))
      case _ => throw new EvalError("do: bad binding syntax")
    }

    val localEnv = new Env(scala.collection.mutable.Map.empty, Some(env))
    // Evaluate init values in outer env
    evalDoInitsK(
      parsed,
      env,
      initVals =>
        parsed.zip(initVals).foreach { case ((name, _, _), v) => localEnv.define(name, v) }
        doLoopK(parsed, test, exprs, body, localEnv, k)
    )

  private def evalDoInitsK(
    parsed: List[(String, SchemeVal, Option[SchemeVal])],
    env: Env,
    k: List[SchemeVal] => Bounce
  ): Bounce =
    def go(remaining: List[(String, SchemeVal, Option[SchemeVal])], acc: List[SchemeVal]): Bounce =
      remaining match
        case Nil => k(acc.reverse)
        case (_, init, _) :: tail =>
          More(() => Evaluator.evalK(init, env, v => go(tail, v :: acc)))
    go(parsed, Nil)

  private def doLoopK(
    parsed: List[(String, SchemeVal, Option[SchemeVal])],
    test: SchemeVal,
    exprs: List[SchemeVal],
    body: List[SchemeVal],
    localEnv: Env,
    k: Cont
  ): Bounce =
    More(() =>
      Evaluator.evalK(
        test,
        localEnv,
        testVal =>
          if Evaluator.isTruthy(testVal) then
            if exprs.isEmpty then k(SchemeVal.Void)
            else Evaluator.evalBodyK(exprs, localEnv, k)
          else
            Evaluator.evalBodyK(
              body,
              localEnv,
              _ =>
                evalDoStepsK(
                  parsed,
                  localEnv,
                  newVals =>
                    parsed.zip(newVals).foreach {
                      case ((name, _, _), Some(v)) => localEnv.define(name, v)
                      case _                       => ()
                    }
                    doLoopK(parsed, test, exprs, body, localEnv, k)
                )
            )
      )
    )

  private def evalDoStepsK(
    parsed: List[(String, SchemeVal, Option[SchemeVal])],
    localEnv: Env,
    k: List[Option[SchemeVal]] => Bounce
  ): Bounce =
    def go(remaining: List[(String, SchemeVal, Option[SchemeVal])], acc: List[Option[SchemeVal]]): Bounce =
      remaining match
        case Nil => k(acc.reverse)
        case (_, _, Some(step)) :: tail =>
          More(() => Evaluator.evalK(step, localEnv, v => go(tail, Some(v) :: acc)))
        case (_, _, None) :: tail =>
          go(tail, None :: acc)
    go(parsed, Nil)

  private def schemeEqv(a: SchemeVal, b: SchemeVal): Boolean = (a, b) match
    case (SchemeVal.Symbol(x), SchemeVal.Symbol(y))                     => x == y
    case (SchemeVal.IntVal(x), SchemeVal.IntVal(y))                     => x == y
    case (SchemeVal.RationalVal(n1, d1), SchemeVal.RationalVal(n2, d2)) => n1 == n2 && d1 == d2
    case (SchemeVal.FloatVal(x), SchemeVal.FloatVal(y))                 => x == y
    case (SchemeVal.BoolVal(x), SchemeVal.BoolVal(y))                   => x == y
    case (SchemeVal.CharVal(x), SchemeVal.CharVal(y))                   => x == y
    case (SchemeVal.SList(Nil), SchemeVal.SList(Nil))                   => true
    case _                                                              => a eq b
