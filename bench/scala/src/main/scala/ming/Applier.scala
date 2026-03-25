package ming

object Applier:
  import Builtins.applyBuiltin

  private def display(e: Expr): String = Display.display(e)

  private[ming] def applyProc(func: Expr, args: List[Expr]): Expr =
    func match
      case Expr.Sym(name) if name == "apply" => applyApply(args)
      case Expr.Sym(name) if name == "map"   => applyMap(args)
      case Expr.Sym(name) if name.startsWith("%%record-") =>
        RecordOps.applyRecordOp(name, args)
      case Expr.Sym(name) => applyBuiltin(name, args)
      case Expr.Lambda(params, restParam, body, closure) =>
        val localEnv =
          Evaluator.bindLambdaParams(params, restParam, args, closure)
        Evaluator.evalBody(body, localEnv)
      case Expr.CaseLambda(clauses, closure) =>
        applyCaseLambda(clauses, closure, args)
      case _ => throw EvalError(s"not a procedure: ${display(func)}")

  private[ming] def matchCaseLambda(
    clauses: List[(List[String], Option[String], List[Expr])],
    args: List[Expr]
  ): Option[(List[String], Option[String], List[Expr])] =
    clauses.find { case (params, restParam, _) =>
      restParam match
        case None    => args.length == params.length
        case Some(_) => args.length >= params.length
    }

  private def applyCaseLambda(
    clauses: List[(List[String], Option[String], List[Expr])],
    closure: Env,
    args: List[Expr]
  ): Expr =
    matchCaseLambda(clauses, args) match
      case Some((params, restParam, body)) =>
        val localEnv =
          Evaluator.bindLambdaParams(params, restParam, args, closure)
        Evaluator.evalBody(body, localEnv)
      case None =>
        throw EvalError(
          s"case-lambda: no matching clause for ${args.length} arguments"
        )

  private def applyMap(args: List[Expr]): Expr =
    if args.length < 2 then throw EvalError("map: need at least 2 arguments")
    val proc = args.head
    val lists = args.tail.map {
      case Expr.Lst(elems) => elems
      case other =>
        throw EvalError(s"map: not a list: ${display(other)}")
    }
    val len = lists.head.length
    if !lists.forall(_.length == len) then throw EvalError("map: lists must have equal length")
    val result = (0 until len).toList.map { i =>
      val argSlice = lists.map(_(i))
      applyProc(proc, argSlice)
    }
    Expr.Lst(result)

  private def applyApply(args: List[Expr]): Expr =
    if args.length < 2 then throw EvalError("apply: need at least 2 arguments")
    val func = args.head
    val lastArg = args.last match
      case Expr.Lst(elems) => elems
      case other =>
        throw EvalError(s"apply: last argument must be a list")
    val prefixArgs = args.slice(1, args.length - 1)
    applyProc(func, prefixArgs ++ lastArg)
