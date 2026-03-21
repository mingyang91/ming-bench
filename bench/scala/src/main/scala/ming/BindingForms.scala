package ming

import Evaluator.{Bounce, Done, EvalResult}

/** Binding forms: do, let, let-values, receive. */
private[ming] object BindingForms:

  def evalDo(
    args: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    args match
      case bindings :: testClause :: body =>
        val bindingList = Evaluator.toList(bindings)
        val parsed      = bindingList.map(parseDoBinding(_, env))
        val (names, inits, steps, out2) =
          parsed.foldLeft(
            (List.empty[String], List.empty[Value], List.empty[Option[Value]], out)
          ) { case ((ns, is, ss, o), (n, initExpr, stepExpr)) =>
            val (v, _, o2) = Evaluator.eval(initExpr, env, o)
            (ns :+ n, is :+ v, ss :+ stepExpr, o2)
          }
        val testParts = Evaluator.toList(testClause)
        val (testExpr, resultExprs) = testParts match
          case t :: rest => (t, rest)
          case _         => throw new EvalError("bad do test clause")
        doLoop(names, steps, testExpr, resultExprs, body, env, inits, out2)
      case _ => throw new EvalError("bad do syntax")

  private def parseDoBinding(
    binding: Value,
    env: Env
  ): (String, Value, Option[Value]) =
    Evaluator.toList(binding) match
      case Value.Symbol(name, _) :: init :: step :: Nil =>
        (name, init, Some(step))
      case Value.Symbol(name, _) :: init :: Nil =>
        (name, init, None)
      case _ => throw new EvalError("bad do binding")

  @scala.annotation.tailrec
  private def doLoop(
    names: List[String],
    steps: List[Option[Value]],
    testExpr: Value,
    resultExprs: List[Value],
    body: List[Value],
    outerEnv: Env,
    vals: List[Value],
    out: String
  ): EvalResult =
    val loopEnv = names.zip(vals).foldLeft(outerEnv) { case (e, (n, v)) =>
      e.define(n, v)
    }
    val (testVal, _, out2) = Evaluator.eval(testExpr, loopEnv, out)
    if !Evaluator.isFalsy(testVal) then
      resultExprs match
        case Nil      => Done(Value.VoidVal, loopEnv, out2)
        case _ :: Nil => Bounce(resultExprs.head, loopEnv, out2)
        case _ =>
          Evaluator.evalBodyTail(resultExprs, loopEnv, out2)
    else
      val out3 = body.foldLeft(out2) { (o, expr) =>
        val (_, _, o2) = Evaluator.eval(expr, loopEnv, o)
        o2
      }
      val newVals = names.zip(steps).zip(vals).map {
        case ((_, Some(stepExpr)), _) =>
          val (v, _, _) = Evaluator.eval(stepExpr, loopEnv, out3)
          v
        case ((_, None), oldVal) => oldVal
      }
      doLoop(names, steps, testExpr, resultExprs, body, outerEnv, newVals, out3)

  def evalLetValues(
    args: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    args match
      case clauses :: body if body.nonEmpty =>
        val clauseList = Evaluator.toList(clauses)
        val (localEnv, out2) =
          clauseList.foldLeft((env, out)) { case ((acc, o), clause) =>
            val elems = Evaluator.toList(clause)
            elems match
              case formals :: producer :: Nil =>
                val names = Evaluator.toList(formals).map {
                  case Value.Symbol(n, _) => n
                  case _                  => throw new EvalError("bad let-values formals")
                }
                val (v, _, o2) = Evaluator.eval(producer, acc, o)
                val vals = v match
                  case Value.MultipleValues(vs) => vs
                  case single                   => List(single)
                if vals.length != names.length then
                  throw new EvalError(
                    s"let-values: expected ${names.length} values, got ${vals.length}"
                  )
                val newEnv = names.zip(vals).foldLeft(acc) { case (e, (n, v2)) =>
                  e.define(n, v2)
                }
                (newEnv, o2)
              case _ => throw new EvalError("bad let-values clause")
          }
        Evaluator.evalBodyTail(body, localEnv, out2)
      case _ => throw new EvalError("bad let-values syntax")

  def evalReceive(
    args: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    args match
      case formals :: producer :: body if body.nonEmpty =>
        val (params, rest) = Forms.extractParamsWithRest(formals)
        val (v, _, out2)   = Evaluator.eval(producer, env, out)
        val vals = v match
          case Value.MultipleValues(vs) => vs
          case single                   => List(single)
        if vals.length < params.length then
          throw new EvalError(
            s"receive: expected at least ${params.length} values, got ${vals.length}"
          )
        val localEnv = params.zip(vals).foldLeft(env) { case (e, (n, v2)) =>
          e.define(n, v2)
        }
        val finalEnv = rest match
          case Some(restName) =>
            val restVals = vals.drop(params.length)
            val restList = restVals.foldRight(Value.NilVal: Value) { (v2, acc) =>
              Value.PairVal(v2, acc)
            }
            localEnv.define(restName, restList)
          case None =>
            if vals.length != params.length then
              throw new EvalError(
                s"receive: expected ${params.length} values, got ${vals.length}"
              )
            localEnv
        Evaluator.evalBodyTail(body, finalEnv, out2)
      case _ => throw new EvalError("bad receive syntax")

  def evalLet(
    args: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    args match
      case Value.Symbol(name, _) :: bindings :: body if body.nonEmpty =>
        evalNamedLet(name, bindings, body, env, out)
      case bindings :: body if body.nonEmpty =>
        evalRegularLet(bindings, body, env, out)
      case _ => throw new EvalError("bad let syntax")

  private def evalNamedLet(
    name: String,
    bindings: Value,
    body: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    val bindingList = Evaluator.toList(bindings)
    val (params, inits, out2) =
      bindingList.foldLeft(
        (List.empty[String], List.empty[Value], out)
      ) { case ((ps, is, o), binding) =>
        val pair = Evaluator.toList(binding)
        pair match
          case Value.Symbol(p, _) :: initExpr :: Nil =>
            val (v, _, o2) = Evaluator.eval(initExpr, env, o)
            (ps :+ p, is :+ v, o2)
          case _ => throw new EvalError("bad let binding")
      }
    val envRef = () => env
    val lambda = Value.LambdaVal(params, body, envRef, Some(name))
    Evaluator.applyProcTail(lambda, inits, None, out2)

  private def evalRegularLet(
    bindings: Value,
    body: List[Value],
    env: Env,
    out: String
  ): EvalResult =
    val bindingList = Evaluator.toList(bindings)
    val (localEnv, out2) =
      bindingList.foldLeft((env, out)) { case ((acc, o), binding) =>
        val pair = Evaluator.toList(binding)
        pair match
          case Value.Symbol(name, _) :: valExpr :: Nil =>
            val (v, _, o2) = Evaluator.eval(valExpr, env, o)
            (acc.define(name, v), o2)
          case _ => throw new EvalError("bad let binding")
      }
    Evaluator.evalBodyTail(body, localEnv, out2)
